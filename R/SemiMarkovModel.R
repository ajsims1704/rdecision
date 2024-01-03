#' @title A semi-Markov model for cohort simulation
#' @description An R6 class representing a semi-Markov model for
#' cohort simulation.
#' @details A class to represent a continuous time semi-Markov chain, modelled
#' using cohort simulation. As interpreted in \pkg{rdecision}, semi-Markov
#' models
#' may include temporary states and transitions are defined by per-cycle
#' probabilities. Although used widely in health economic modelling, the
#' differences between semi-Markov models and Markov processes introduce
#' some caveats for modellers:
#' \itemize{
#' \item{If there are temporary states, the result will depend on cycle length.}
#' \item{Transitions are specified by their conditional probability, which
#' is a \emph{per-cycle} probability of starting a cycle in one state and
#' ending it in another; if the cycle length changes, the probabilities should
#' change, too.}
#' \item{Probabilities and rates cannot be linked by the Kolmogorov forward
#' equation, where the per-cycle probabilities are given by the matrix
#' exponential of the transition rate matrix, because this equation does not
#' apply if there are temporary states. In creating semi-Markov models, it is
#' the  modeller's task to estimate probabilities from published data on
#' event rates.}
#' \item{The cycle time cannot be changed during the simulation.}
#' }
#' @section Graph theory:
#' A Markov model is a directed multidigraph permitting loops (a loop
#' multidigraph), optionally labelled, or a \dfn{quiver}. It is a
#' multidigraph because there are potentially two edges between each pair of
#' nodes {A,B} representing the transition probabilities from A to B and
#' \emph{vice versa}. It is a directed graph because the transition
#' probabilities refer to transitions in one direction. Each edge can be
#' optionally labelled. It permits self-loops (edges whose source and target
#' are the same node) to represent patients that remain in the same state
#' between cycles.
#' @section Transition rates and probabilities:
#' \subsection{Why semi-Markov?}{
#' Beck and Pauker (1983) and later Sonnenberg and Beck (1993) proposed the
#' use of Markov processes to model the health economics of medical
#' interventions. Further, they introduced the additional concept of temporary
#' states, to which patients who transition remain for exactly one cycle. This
#' breaks the principle that Markov processes are memoryless
#' and thus the underlying mathematical formalism, first
#' developed by Kolmogorov, is not applicable. For example, ensuring that all
#' patients leave a temporary state requires its transition rate to be infinite.
#' Hence, such models are usually labelled as semi-Markov processes.
#' }
#' \subsection{Rates and probabilities}{
#' Miller and Homan (1994) and Fleurence & Hollenbeak (2007) provide advice
#' on estimating probabilities from rates. Jones (2017) and Welton (2005)
#' describe methods for estimating probabilities in multi-state,
#' multi-transition models, although those methods may not apply to
#' semi-Markov models with temporary states. In particular note that the
#' "simple" equation, \eqn{p = 1-e^{-rt}} (Briggs 2006) applies only in a
#' two-state, one transition model.
#' }
#' \subsection{Uncertainty in rates}{
#' In semi-Markov models, the conditional probabilities of the transitions
#' from each state are usually modelled by a Dirichlet distribution. In
#' \pkg{rdecision}, create a Dirichlet distribution for each state and
#' optionally create model variables for each conditional probability
#' (\eqn{\rho_{ij}}) linked to an applicable Dirichlet distribution.
#' }
#' @references{
#'   Beck JR and Pauker SG. The Markov Process in Medical Prognosis.
#'   \emph{Med Decision Making}, 1983;\strong{3}:419–458.
#'
#'   Briggs A, Claxton K, Sculpher M. Decision modelling for health economic
#'   evaluation. Oxford, UK: Oxford University Press; 2006.
#'
#'   Fleurence RL and Hollenbeak CS. Rates and probabilities in economic
#'   modelling. \emph{PharmacoEconomics}, 2007;\strong{25}:3--6.
#'
#'   Jones E, Epstein D and García-Mochón L. A procedure for deriving
#'   formulas to convert transition rates to probabilities for multistate
#'   Markov models. \emph{Medical Decision Making} 2017;\strong{37}:779–789.
#'
#'   Miller DK and Homan SM. Determining transition probabilities: confusion
#'   and suggestions. \emph{Medical Decision Making} 1994;\strong{14}:52-58.
#'
#'   Sonnenberg FA, Beck JR. Markov models in medical decision making: a
#'   practical guide. \emph{Medical Decision Making}, 1993:\strong{13}:322.
#'
#'   Welton NJ and Ades A. Estimation of Markov chain transition probabilities
#'   and rates from fully and partially observed data: uncertainty
#'   propagation, evidence synthesis, and model calibration.
#'   \emph{Medical Decision Making}, 2005;\strong{25}:633-645.
#' }
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
SemiMarkovModel <- R6::R6Class(
  classname = "SemiMarkovModel",
  lock_class = TRUE,
  inherit = Digraph,
  private = list(

    # private class variables
    smm.tcycle = NULL,
    smm.Pt = NULL,
    smm.discost = NULL,
    smm.disutil = NULL,
    smm.pop = NULL,
    smm.icycle = NULL,
    smm.elapsed = NULL,

    # @description Low-level model cycling function
    # @details Iterates the model for a single cycle. Updates the state
    # populations, cycle number and elapsed time.
    # return Matrix of transition counts between states. Row and column names
    # are state names.
    cycle_pop = function() {
      # find the number of transitions between each pair of states by
      # multiplying state populations with each column of the transition
      # matrix
      p_t <- self$transition_probabilities()
      n_t <- private$smm.pop * p_t
      # update the state populations, cycle count and elapsed time
      private$smm.pop <- drop(private$smm.pop %*% p_t)
      private$smm.icycle <- private$smm.icycle + 1L
      private$smm.elapsed <- private$smm.elapsed + private$smm.tcycle
      # return the transition matrix
      return(n_t)
    },

    # @description Low-level model cycling function that calculates values,
    # using reference costs supplied as arguments, i.e. without doing graph
    # traversal to get values which tend not to change between cycles.
    # @details Iterates the model for a single cycle. Updates the state
    # populations, cycle number and elapsed time, and calculates values (costs
    # and utilities).
    # @param hcc.pop See function cycle
    # @param hcc.cost See fuction cycle
    # @param m_transition_costs Square matrix of order number of states, with
    # each cell being the cost of transition between a pair of states, as
    # returned by self$transition_cost().
    # v_state_utilities Vector of utilities, by state.
    # v_state_costs Vector of annual occupancy costs, by state.
    # @return Matrix with one row per state and columns for population used for
    # calculating utility, population used for calculating cost, occupancy cost,
    # entry cost, total cost and QALY.
    cycle_pop_with_values = function(hcc.pop, hcc.cost, m_transition_costs,
                                     v_state_utilities, v_state_costs) {
      # get the cycle time, in years
      dty <- as.numeric(private$smm.tcycle, units = "days") / 365.25
      # save the state populations and elapsed time before cycling
      v_pop_start <- self$get_populations()
      elapsed_start <- as.numeric(private$smm.elapsed, units = "days") / 365.25
      # cycle the population and get the number of transitions between states
      m_nt <- private$cycle_pop()
      # calculate transition costs as number of transitions between each
      # pair of states, Hadamard multiplied by transition costs, summed by
      # the state being entered (entry costs)
      m_ct <- m_nt * m_transition_costs
      v_ec <- colSums(m_ct)
      # get the state populations and elapsed time after cycling
      v_pop_end <- self$get_populations()
      elapsed_end <- as.numeric(private$smm.elapsed, units = "days") / 365.25
      # apply half cycle correction to populations and utility
      if (hcc.pop) {
        v_upop <- (v_pop_start + v_pop_end) / 2.0
        elapsed_u <- (elapsed_start + elapsed_end) / 2.0
      } else {
        v_upop <- v_pop_end
        elapsed_u <- elapsed_end
      }
      dfu <- 1.0 / (1.0 + private$smm.disutil) ^ elapsed_u
      v_qaly <- v_upop * v_state_utilities * dty * dfu
      # apply half cycle correction to costs
      if (hcc.cost) {
        v_cpop <- (v_pop_start + v_pop_end) / 2.0
        elapsed_c <- (elapsed_start + elapsed_end) / 2.0
      } else {
        v_cpop <- v_pop_end
        elapsed_c <- elapsed_end
      }
      dfc <- 1.0 / (1.0 + private$smm.discost) ^ elapsed_c
      v_oc <- v_cpop * v_state_costs * dty * dfc
      v_ec <- v_ec * dfc
      # return the populations, costs and utilities, by state
      N <- sum(v_pop_end)
      rm <- matrix(
        data = c(
          v_upop,
          v_cpop,
          v_oc / N,
          v_ec / N,
          (v_oc + v_ec) / N,
          v_qaly / N
        ),
        nrow = length(v_upop), ncol = 6L, byrow = FALSE,
        dimnames = list(
          rownames(m_transition_costs),
          c("PopU", "PopC", "OccCost", "EntryCost", "Cost", "QALY")
        )
      )
      return(rm)
    }
  ),

  public = list(

    #' @description Creates a semi-Markov model for cohort simulation.
    #' @details A semi-Markov model must meet the following conditions:
    #' \enumerate{
    #'   \item It must have at least one node and at least one edge.
    #'   \item All nodes must be of class \code{MarkovState};
    #'   \item All edges must be of class \code{Transition};
    #'   \item The nodes and edges must form a digraph whose underlying
    #'   graph is connected;
    #'   \item Each state must have at least one outgoing transition (for
    #'   absorbing states this is a self-loop);
    #'   \item For each state the sum of outgoing conditional transition
    #'   probabilities must be one. For convenience, one outgoing transition
    #'   probability from each state may be set to NA when the
    #'   probabilities are defined. Typically, probabilities for self
    #'   loops would be set to NA. Transition probabilities in \eqn{Pt}
    #'   associated with transitions that are not defined as edges in the
    #'   graph are zero. Probabilities can be changed between cycles.
    #'   \item No two edges may share the same source and target nodes (i.e.
    #'   the digraph may not have multiple edges). This is to ensure that there
    #'   are no more transitions than cells in the transition matrix.
    #'   \item The node labels must be unique to the graph.
    #' }
    #' @param V A list of nodes (\code{MarkovState}s).
    #' @param E A list of edges (\code{Transition}s).
    #' @param tcycle Cycle length, expressed as an R \code{difftime} object.
    #' @param discount.cost Annual discount rate for future costs. Note this
    #' is a rate, not a probability (i.e. use 0.035 for 3.5\%).
    #' @param discount.utility Annual discount rate for future incremental
    #' utility. Note this is a rate, not a probability (i.e. use 0.035
    #' for 3.5\%).
    #' @return A \code{SemiMarkovModel} object. The population of the first
    #' state is set to 1000 and from each state there is an equal
    #' conditional probability of each allowed transition.
    initialize = function(V, E, tcycle = as.difftime(365.25, units = "days"),
                          discount.cost = 0.0, discount.utility = 0.0) {
      # initialize the base class(es)
      super$initialize(V, E)
      # check minimum number of nodes and edges
      abortifnot(self$order() >= 1L && self$size() >= 1L,
        message = "The model must have at least 1 node and 1 edge",
        class = "invalid_graph"
      )
      # check that all nodes inherit from MarkovState
      S <- which(
        vapply(V, FUN.VALUE = TRUE, FUN = inherits, what = "MarkovState")
      )
      abortifnot(setequal(seq_along(V), S),
        message = "Each node must be a 'MarkovState'.",
        class = "invalid_state"
      )
      # check that all edges inherit from Transition
      tedges <- which(
        vapply(E, FUN.VALUE = TRUE, inherits, what = "Transition")
      )
      abortifnot(setequal(seq_along(E), tedges),
        message = "Each edge must be a 'Transition'.",
        class = "invalid_transition"
      )
      # check that the underlying graph is connected
      abortifnot(self$is_weakly_connected(),
        message = "The underlying graph of {V,E} must be connected",
        class = "invalid_graph"
      )
      # check that there are no multiple edges
      A <- self$digraph_adjacency_matrix()
      abortif(any(A > 1L),
        message = "The digraph must not have multiple edges",
        class = "multiple_edges"
      )
      # check that there is at least one outgoing transition from each state
      abortif(any(rowSums(A) < 1L),
        message = "Every state must have at least one outgoing transition",
        class = "missing_transition"
      )
      # check that the node names are unique
      abortifnot(length(unique(self$get_statenames())) == self$order(),
        message = "State labels must be unique",
        class = "invalid_state_names"
      )
      # check that the cycle time is an interval
      abortifnot(inherits(tcycle, what = "difftime"),
        message = "Argument 'tcycle' must be of class 'difftime'.",
        class = "invalid_tcycle"
      )
      private$smm.tcycle <- tcycle
      # check and set discounts
      abortifnot(is.numeric(discount.cost),
        message = "Discount rate must be numeric",
        class = "invalid_discount"
      )
      private$smm.discost <- discount.cost
      abortifnot(is.numeric(discount.utility),
        message = "Discount rate must be numeric",
        class = "invalid_discount"
      )
      private$smm.disutil <- discount.utility
      # set the initial probability matrix (equal chance from each state)
      Pt <- matrix(
        data = 0.0,
        nrow = self$order(), ncol = self$order(),
        dimnames = list(
          source = self$get_statenames(), target = self$get_statenames()
        )
      )
      for (e in self$edges()) {
        is <- self$vertex_index(e$source())
        it <- self$vertex_index(e$target())
        Pt[is, it] <- 1.0
      }
      Pt <- Pt / rowSums(Pt)
      self$set_probabilities(Pt)
      # reset the model to its ground state
      self$reset()
      # return a new SemiMarkovModel object
      return(invisible(self))
    },

    #' @description Sets transition probabilities.
    #' @param Pt Per-cycle transition probability matrix. The row and
    #' column labels must be the state names and each row must sum to one.
    #' Non-zero probabilities for undefined transitions are not allowed. At
    #' most one \code{NA} may appear in each row. If an NA is present in a row,
    #' it is replaced by 1 minus the sum of the defined probabilities.
    #' @return Updated \code{SemiMarkovModel} object
    set_probabilities = function(Pt) {
      # check form of Pt
      nstates <- self$order()
      sn <- self$get_statenames()
      abortifnot(
        is.matrix(Pt),
        nrow(Pt) == nstates && ncol(Pt) == nstates,
        setequal(sn, dimnames(Pt)[[1L]]),
        setequal(sn, dimnames(Pt)[[2L]]),
        message = paste(
          "'Pt' must be a matrix",
          "of size", nstates, "x", nstates, ".",
          "Each row and column must have a unique state name."
        ),
        class = "invalid_Pt"
      )
      # check NAs and replace them
      nNA <- rowSums(is.na(Pt))
      abortif(any(nNA > 1L),
        message = "No more than one NA per row is allowed",
        class = "invalid_Pt"
      )
      sumP <- rowSums(Pt, na.rm = TRUE)
      for (r in seq_len(nstates)) {
        if (nNA[[r]] > 0L) {
          Pt[[r, which(is.na(Pt[r, ]))]] <- 1.0 - sumP[[r]]
        }
      }
      # check that all cells are a probability
      abortif(
        any(Pt < 0.0),
        any(Pt > 1.0),
        message = "All elements of 'Pt' must be probabilities",
        class = "invalid_Pt"
      )
      # reorder rows and columns to match internal vertex order, allowing for
      # the possibility of a 1 x 1 model
      Pt <- Pt[sn, sn, drop = FALSE]
      # check that Pt[i,j]==0 if edge i->j is not in graph
      M <- self$digraph_adjacency_matrix(boolean = TRUE)
      AA <- (Pt != 0.0)
      abortif(sum(M | AA) > sum(M),
        message = "All non-zero Pt cells must correspond to transitions",
        class = "invalid_Pt"
      )
      # check that all rows of Pt sum to 1
      sumP <- rowSums(Pt)
      abortif(any(abs(sumP - 1.0) > sqrt(.Machine$double.eps)),
        message = "All rows of Pt must sum to 1",
        class = "invalid_Pt"
      )
      # set the class variable
      private$smm.Pt <- Pt
      # return updated model
      return(invisible(self))
    },

    #' @description Per-cycle transition probability matrix for the model.
    #' @return A square matrix of size equal to the number of states. If all
    #' states are labelled, the dimnames take the names of the states.
    transition_probabilities = function() {
      return(private$smm.Pt)
    },

    #' @description Return the per-cycle transition costs for the model.
    #' @return A square matrix of size equal to the number of states. If all
    #' states are labelled, the dimnames take the names of the states.
    transition_cost = function() {
      # get the state names
      state_names <- self$get_statenames()
      # construct the matrix
      o <- self$order()
      Ic <- matrix(
        data = 0.0,
        nrow = o, ncol = o,
        dimnames = list(source = state_names, target = state_names)
      )
      # populate the cells with costs
      for (ie in self$edge_along()) {
        e <- self$edge_at(ie)
        is <- self$vertex_index(e$source())
        it <- self$vertex_index(e$target())
        Ic[[is, it]] <- e$cost()
      }
      return(Ic)
    },

    #' @description Returns a character list of state names.
    #' @return List of the names of each state.
    get_statenames = function() {
      iv <- self$vertex_along()
      statenames <- self$vertex_label(iv)
      return(statenames)
    },

    #' @description Resets the model counters.
    #' @details Resets the state populations, next cycle number and elapsed time
    #' of the model. By default the model is returned to its ground state (zero
    #' people in the all states; next cycle is labelled
    #' zero; elapsed time (years) is zero). Any or all of these can be set via
    #' this function. \code{icycle} is simply an integer counter label for each
    #' cycle, \code{elapsed} sets the elapsed time in years from the index time
    #' from which discounting is assumed to apply.
    #' @param populations A named vector of populations for
    #' the start of the state. The names should be the state names.
    #' Due to the R implementation of matrix algebra, \code{populations}
    #' must be a numeric type and is not restricted to being an integer. If
    #' NULL, the population of all states is set to zero.
    #' @param icycle Cycle number at which to start/restart.
    #' @param elapsed Elapsed time since the index (reference) time used for
    #' discounting as an R \code{difftime} object.
    #' @return Updated \code{SemiMarkovModel} object.
    reset = function(populations = NULL, icycle = 0L,
                     elapsed = as.difftime(0.0, units = "days")) {
      # get the state names
      sn <- self$get_statenames()
      # check and set the populations
      if (is.null(populations)) {
        private$smm.pop <- vector(mode = "numeric", length = self$order())
        names(private$smm.pop) <- sn
      } else {
        abortifnot(
          length(populations) == self$order(),
          setequal(sn, names(populations)),
          is.numeric(populations),
          message = paste(
            "'populations' must be a numeric vector with one named element",
            "per state"
          ),
          class = "invalid_populations"
        )
        # set the populations
        for (s in sn) {
          private$smm.pop[s] <- populations[s]
        }
      }
      # check and set the cycle number
      abortifnot(
        is.integer(icycle),
        icycle >= 0L,
        message = "'icycle' must be an integer >= 0",
        class = "invalid_icycle"
      )
      private$smm.icycle <- icycle
      # check and update the elapsed time
      abortifnot(inherits(elapsed, what = "difftime"),
        message = "Argument 'elapsed' must be of class 'difftime'.",
        class = "invalid_elapsed"
      )
      private$smm.elapsed <- elapsed
      # return updated object
      return(invisible(self))
    },

    #' @description Gets the occupancy of each state
    #' @return A numeric vector of populations, named with state names.
    get_populations = function() {
      return(private$smm.pop)
    },

    #' @description Gets the current elapsed time.
    #' @details The elapsed time is defined as the difference between the
    #' current time in the model and an index time used as the reference
    #' time for applying discounting. By default the elapsed time starts at
    #' zero. It can be set directly by calling \code{reset}. It is incremented
    #' after each call to \code{cycle} by the cycle duration to the time at the
    #' end of the cycle (even if half cycle correction is used). Thus, via the
    #' \code{reset} and \code{cycle} methods, the time of each cycle relative
    #' to the discounting index and its duration can be controlled arbitrarily.
    #' @return Elapsed time as an R \code{difftime} object.
    get_elapsed = function() {
      return(private$smm.elapsed)
    },

    #' @description Tabulation of states
    #' @details Creates a data frame summary of each state in the model.
    #' @return A data frame with the following columns:
    #' \describe{
    #' \item{Name}{State name}
    #' \item{Cost}{Annual cost of occupying the state}
    #' \item{Utility}{Incremental utility associated with being in the state.}
    #' }
    tabulate_states = function() {
      v <- self$vertexes()
      df <- data.frame(
        Name = vapply(X = v, FUN.VALUE = "x", FUN = function(x) x$name()),
        Cost = vapply(X = v, FUN.VALUE = 1.0, FUN = function(x) x$cost()),
        Utility = vapply(X = v, FUN.VALUE = 1.0, FUN = function(x) {
          x$utility()
        })
      )
      return(df)
    },

    #' @description Applies one cycle of the model.
    #' @param hcc.pop Determines the state populations returned by this
    #' function and for calculating incremental utility, and the time at which
    #' the utility discount is applied. If FALSE, the end of cycle populations
    #' and time apply; if TRUE the mid-cycle populations and time apply. The
    #' mid-cycle populations are taken as the mean of the start and end
    #' populations and the discount time as the mid-point. The value of this
    #' parameter does not affect the state populations or elapsed time passed
    #' to the next cycle or available via
    #' \code{get_populations}; those are always the end cycle values.
    #' @param hcc.cost Determines the state occupancy costs returned by this
    #' function and the time at which the cost discount is applied to the
    #' occupancy costs and the entry costs. If FALSE, the end of cycle
    #' populations and time apply; if TRUE the mid-cycle populations and time
    #' apply, as per \code{hcc.pop}. The value of this parameter does not affect
    #' the state populations or elapsed time passed to the next cycle or
    #' available via \code{get_populations}; those are always the end cycle
    #' values.
    #' @return Calculated values, one row per state, as a data frame with the
    #' following columns:
    #' \describe{
    #' \item{\code{State}}{Name of the state.}
    #' \item{\code{Cycle}}{The cycle number.}
    #' \item{\code{Time}}{Clock time in years of the end of the cycle.}
    #' \item{\code{Population}}{Populations of the states; see \code{hcc.pop}.}
    #' \item{\code{OccCost}}{Cost of the population occupying the state for
    #' the cycle. Discounting and half cycle correction is applied, if those
    #' options are set. The costs are normalized by the model population. The
    #' cycle costs are derived from the annual occupancy costs of the
    #' \code{MarkovState}s.}
    #' \item{\code{EntryCost}}{Cost of the transitions \emph{into} the state
    #' during the cycle. Discounting is applied, if the option is set.
    #' The result is normalized by the model population. The cycle costs
    #' are derived from \code{Transition} costs.}
    #' \item{\code{Cost}}{Total cost, normalized by model population.}
    #' \item{\code{QALY}}{Quality adjusted life years gained by occupancy of
    #' the states during the cycle. Half cycle correction and discounting are
    #' applied, if these options are set. Normalized by the model population.}
    #' }
    cycle = function(hcc.pop = TRUE, hcc.cost = TRUE) {
      # check and set half cycle correction parameters
      abortifnot(
        is.logical(hcc.pop),
        is.logical(hcc.cost),
        message = "Arguments 'hcc.pop' and 'hcc.cost' must be logical.",
        class = "invalid_hcc"
      )
      # get the transition costs (matrix), incremental state occupancy
      # costs and per-cycle incremental state utilities (vectors)
      m_transition_costs <- self$transition_cost()
      nstates <- self$order()
      v_state_costs <- vector(mode = "numeric", length = nstates)
      v_state_utilities <- vector(mode = "numeric", length = nstates)
      for (iv in self$vertex_along()) {
        v <- self$vertex_at(iv)
        v_state_costs[[iv]] <- v$cost()
        v_state_utilities[[iv]] <- v$utility()
      }
      # cycle population and calculate values
      m_c <- private$cycle_pop_with_values(
        hcc.pop, hcc.cost, m_transition_costs, v_state_utilities, v_state_costs
      )
      # create return data frame
      rc <- data.frame(
        State = rownames(m_c),
        Cycle = rep(private$smm.icycle, times = nstates),
        Time = rep(
          as.numeric(private$smm.elapsed, units = "days") / 365.25,
          times = nstates
        ),
        Population = m_c[, "PopU"],
        OccCost = m_c[, "OccCost"],
        EntryCost = m_c[, "EntryCost"],
        Cost = m_c[, "Cost"],
        QALY = m_c[, "QALY"]
      )
      return(rc)
    },

    #' @description Applies multiple cycles of the model.
    #' @details The starting populations are redistributed through the
    #' transition probabilities and the state occupancy costs are
    #' calculated, using function \code{cycle}. The end populations are
    #' then fed back into the model for a further cycle and the
    #' process is repeated. For each cycle, the state populations and
    #' the aggregated occupancy costs are saved in one row of the
    #' returned data frame, with the cycle number. If the cycle count
    #' for the model is zero when called, the first cycle reported
    #' will be cycle zero, i.e. the distribution of patients to starting
    #' states.
    #' @param ncycles Number of cycles to run; default is 2.
    #' @param hcc.pop Determines the state populations returned by this
    #' function and for calculating incremental utility, and the time at which
    #' the utility discount is applied. If FALSE, the end of cycle populations
    #' and time apply; if TRUE the mid-cycle populations and time apply. The
    #' mid-cycle populations are taken as the mean of the start and end
    #' populations and the discount time as the mid-point. The value of this
    #' parameter does not affect the state populations or elapsed time passed
    #' to the next cycle or available via
    #' \code{get_populations}; those are always the end cycle values.
    #' @param hcc.cost Determines the state occupancy costs returned by this
    #' function and the time at which the cost discount is applied to the
    #' occupancy costs and the entry costs. If FALSE, the end of cycle
    #' populations and time apply; if TRUE the mid-cycle populations and time
    #' apply, as per \code{hcc.pop}. The value of this parameter does not affect
    #' the state populations or elapsed time passed to the next cycle or
    #' available via \code{get_populations}; those are always the end cycle
    #' values.
    #' @return Data frame with cycle results, with the following columns:
    #' \describe{
    #' \item{\code{Cycle}}{The cycle number.}
    #' \item{\code{Years}}{Elapsed time at end of cycle, years}
    #' \item{\code{Cost}}{Cost associated with occupancy and transitions between
    #' states during the cycle.}
    #' \item{\code{QALY}}{Quality adjusted life years associated with occupancy
    #' of the states in the cycle.}
    #' \item{\code{<name>}}{Population of state \code{<name>} at the end of
    #' the cycle.}
    #' }
    cycles = function(ncycles = 2L, hcc.pop = TRUE, hcc.cost = TRUE) {
      # show zero?
      nzero <- as.integer(private$smm.icycle == 0L)
      # list of state names
      statenames <- self$get_statenames()
      # construct output matrix
      rm <- matrix(
        nrow = ncycles + nzero, ncol = 4L + length(statenames),
        dimnames = list(NULL, c("Cycle", "Years", "Cost", "QALY", statenames))
      )
      # add zero
      if (nzero > 0L) {
        rm[1L, ] <- c(0L, 0.0, 0.0, 0.0, private$smm.pop)
      }
      # get the transition costs (matrix), incremental state occupancy
      # costs and per-cycle incremental state utilities (vectors)
      m_transition_costs <- self$transition_cost()
      nstates <- self$order()
      v_state_costs <- vector(mode = "numeric", length = nstates)
      v_state_utilities <- vector(mode = "numeric", length = nstates)
      for (iv in self$vertex_along()) {
        v <- self$vertex_at(iv)
        v_state_costs[[iv]] <- v$cost()
        v_state_utilities[[iv]] <- v$utility()
      }
      # cycle the model
      for (i in (1L + nzero) : (ncycles + nzero)) {
        # cycle population and calculate values
        m_c <- private$cycle_pop_with_values(
          hcc.pop, hcc.cost,
          m_transition_costs, v_state_utilities, v_state_costs
        )
        # add result to the return matrix (one row per state)
        rm[i, ] <- c(
          Cycle = private$smm.icycle,
          Years = as.numeric(private$smm.elapsed, units = "days") / 365.25,
          Cost = sum(m_c[, "Cost"]),
          QALY = sum(m_c[, "QALY"]),
          m_c[, "PopU"]
        )
      }
      # convert to data frame (note: it is more efficient to return as a matrix)
      DF <- as.data.frame(rm)
      DF$Cycle <- as.integer(DF$Cycle)
      # return summary data frame
      return(DF)
    },

    #' @description Find all the model variables in the Markov model.
    #' @details Returns variables of type \code{ModVar} that have been
    #' specified as values associated with transition rates and costs and
    #' the state occupancy costs and utilities.
    #' @return A list of \code{ModVar}s.
    modvars = function() {
      # create list
      mv <- list()
      # find the ModVars in the transitions
      for (e in self$edges()) {
        if (inherits(e, what = "Transition")) {
          mv <- c(mv, e$modvars())
        }
      }
      # find the ModVars in the states
      for (v in self$vertexes()){
        if (inherits(v, what = "MarkovState")) {
          mv <- c(mv, v$modvars())
        }
      }
      # return a unique list
      return(unique(mv))
    },

    #' @description Tabulate the model variables in the Markov model.
    #' @param expressions A logical that defines whether expression model
    #' variables should be included in the tabulation.
    #' @return Data frame with one row per model variable, as follows:
    #' \describe{
    #' \item{\code{Description}}{As given at initialization.}
    #' \item{\code{Units}}{Units of the variable.}
    #' \item{\code{Distribution}}{Either the uncertainty distribution, if
    #' it is a regular model variable, or the expression used to create it,
    #' if it is an \code{ExprModVar}.}
    #' \item{\code{Mean}}{Mean; calculated from means of operands if
    #' an expression.}
    #' \item{\code{E}}{Expectation; estimated from random sample if expression,
    #' mean otherwise.}
    #' \item{\code{SD}}{Standard deviation; estimated from random sample if
    #' expression, exact value otherwise.}
    #' \item{\code{Q2.5}}{p=0.025 quantile; estimated from random sample if
    #' expression, exact value otherwise.}
    #' \item{\code{Q97.5}}{p=0.975 quantile; estimated from random sample if
    #' expression, exact value otherwise.}
    #' \item{\code{Est}}{TRUE if the quantiles and SD have been estimated by
    #' random sampling.}
    #' }
    modvar_table = function(expressions = TRUE) {
      # create list of model variables in this decision tree, excluding
      # expressions if not wanted
      mvlist <- self$modvars()
      if (!expressions) {
        mvlist <- mvlist[vapply(mvlist, FUN.VALUE = TRUE, FUN = function(v) {
          !v$is_expression()
        })]
      }
      # create a data frame of model variables
      DF <- data.frame(
        Description = vapply(mvlist, FUN.VALUE = "x", FUN = function(x) {
          x$description()
        }),
        Units = vapply(mvlist, FUN.VALUE = "x", FUN = function(x) {
          x$units()
        }),
        Distribution = vapply(mvlist, FUN.VALUE = "x", FUN = function(x) {
          x$distribution()
        }),
        Mean = vapply(mvlist, FUN.VALUE = 1.0, FUN = function(x) {
          x$mean()
        }),
        E = vapply(mvlist, FUN.VALUE = 1.0, FUN = function(x) {
          rv <- ifelse(x$is_expression(), x$mu_hat(), x$mean())
          return(rv)
        }),
        SD = vapply(mvlist, FUN.VALUE = 1.0, FUN = function(x) {
          rv <- ifelse(x$is_expression(), x$sigma_hat(), x$SD())
          return(rv)
        }),
        Q2.5 = vapply(mvlist, FUN.VALUE = 1.0, FUN = function(x) {
          rv <- ifelse(
            x$is_expression(),
            x$q_hat(probs = 0.025),
            x$quantile(probs = 0.025)
          )
          return(rv)
        }),
        Q97.5 = vapply(mvlist, FUN.VALUE = 1.0, FUN = function(x) {
          rv <- ifelse(
            x$is_expression(),
            x$q_hat(probs = 0.975),
            x$quantile(probs = 0.975)
          )
          return(rv)
        }),
        Est = vapply(mvlist, FUN.VALUE = TRUE, FUN = function(exp) {
          exp$is_expression()
        })
      )
      # Return the table
      return(DF)
    }
  )
)
