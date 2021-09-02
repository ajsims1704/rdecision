#' @title A semi-Markov model for cohort simulation
#' 
#' @description An R6 class representing a semi-Markov model for 
#' cohort simulation.
#' 
#' @details A class to represent a continuous time semi-Markov chain, modelled
#' using cohort simulation. As interpeted in \pkg{rdecision}, semi Markov models
#' may include tunnel states and transitions are defined by per-cycle
#' probabilities. Although used widely in health economic modelling, the
#' differences between semi-Markov models and Markov processes introduce
#' some caveats for modellers:
#' \itemize{
#' \item{If there are tunnel states, the result will depend on cycle length.}
#' \item{Transitions are specified by their conditional probability, which
#' is a \emph{per-cycle} probability of making a jump; if the cycle length
#' changes, the probabilities should change, too.}
#' \item{Probabilities and rates cannot be linked by the Kolmogorov forward
#' equation, where the per-cycle probabilities are given by the matrix 
#' exponential of the transition rate matrix, because this equation breaks down
#' if there are tunnel states. In creating semi-Markov models, it is the 
#' modeller's task to estimate probabilities from published data on 
#' event rates.}
#' \item{The cycle time cannot be changed during the simulation.}
#' }
#'  
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
#'
#' @section Transition rates and probabilities:
#' 
#' \subsection{Why semi-Markov?}{
#' Beck and Pauker (1983) and later Sonnenberg and Beck (1993) proposed the
#' use of Markov processes to model the health economics of medical 
#' interventions. Further, they introduced the additional concept of tunnel 
#' states, to which patients who transition remain for exactly one cycle. This
#' breaks the principle of memorylessness required by the definition of a
#' Markov process, and thus the underlying mathematical formalism, first
#' developed by Kolmogorov, is not applicable. For example, ensuring that all
#' patients leave a tunnel state requires its transition rate to be infinite.
#' Hence, such models are usually labelled as semi-Markov processes.
#' }
#' 
#' \subsection{Rates and probabilities}{
#' Miller and Homan (1994) and Fleurence & Hollenbeak (2007) provide advice
#' on estimating probabilities from rates. Jones (2017) and Welton (2005) 
#' describe methods for estimating probabilities in multi-state, 
#' multi-transition models, although those methods may not apply to 
#' semi-Markov models with tunnel states. In particular note that the
#' "simple" equation, \eqn{p = 1-e^{-rt}} (Briggs 2006) applies only in a 
#' two-state, one transition model.
#' }
#' 
#' \subsection{Uncertainty in rates}{
#' In semi-Markov models, the conditional probabilities of the transitions 
#' from each state are usually modelled by a Dirichlet distribution. In 
#' \pkg{rdecision}, create \code{ModVar}s create a Dirichlet distribution for 
#' each state; create model variables for each
#' conditional probability (\eqn{\rho_{ij}}) linked to an applicable Dirichlet
#' distribution; and finally create expression model variables for each rate
#' \eqn{g_{ij}}.
#' }
#' 
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
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export 
#' 
SemiMarkovModel <- R6::R6Class(
  classname = "SemiMarkovModel",
  lock_class = TRUE,
  inherit = Digraph,
  private = list(
    smm.tcycle = NULL,
    smm.Pt = NULL,
    smm.discost = NULL,
    smm.disutil = NULL,
    smm.pop = NULL,
    smm.icycle = NULL,
    smm.elapsed = NULL
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
    #'   \item Each state must have at least one outgoing transition (which
    #'   can be a self-loop);
    #'   \item For each state the sum of outgoing conditional transition 
    #'   probabilities must be one. For convenience, one outgoing transition 
    #'   probability from each state may be set to NA when the 
    #'   \code{Transition}s are defined. Typically, probabilities for self 
    #'   loops would be set to NA). Transition probabilities in \eqn{Pt} 
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
    #' @param discount.cost Annual discount rate for future costs.
    #' @param discount.utility Annual discount rate for future incremental
    #' utility.
    #' @return A \code{SemiMarkovModel} object. The population of the first
    #' state is set to 1000.
    initialize = function(V, E, tcycle=as.difftime(365.25, units="days"), 
                          discount.cost=0, discount.utility=0) {
      # initialize the base class(es)
      super$initialize(V,E)
      # check minimum number of nodes and edges
      if (self$order() < 1 || self$size() < 1) {
        rlang::abort(
          "The model must have at least 1 node and 1 edge", 
          class = "invalid_graph"
        )
      }
      # check that all nodes inherit from MarkovState
      S <- which(
        sapply(V, function(v){inherits(v,what="MarkovState")}),arr.ind=TRUE
      )
      if (!setequal(seq_along(V),S)) {
        rlang::abort(
          "Each node must be a 'MarkovState'.", class="invalid_state"
        )
      }
      # check that all edges inherit from Transition
      T <- which(
        sapply(E, function(e){inherits(e,what="Transition")}),arr.ind=TRUE
      )
      if (!setequal(seq_along(E),T)) {
        rlang::abort(
          "Each edge must be a 'Transition'.", class="invalid_transition"
        )
      }
      # check that the underlying graph is connected
      if (!self$is_weakly_connected()) {
        rlang::abort("The underlying graph of {V,E} must be connected",
                     class = "invalid_graph")
      }
      # check that there are no multiple edges
      A <- self$digraph_adjacency_matrix()
      if (any(A > 1)) {
        rlang::abort(
          "The digraph must not have multiple edges",
          class = "multiple_edges"
        )
      }
      # check that the node names are unique
      if (length(unique(self$get_statenames()))!=self$order()) {
        rlang::abort(
          "State labels must be unique",
          class = "invalid_state_names"
        )
      }
      # check that the cycle time is an interval
      if (class(tcycle) != "difftime") {
        rlang::abort(
          "Argument 'tcycle' must be of class 'difftime'.",
          class = "invalid_tcycle"
        )
      }
      private$smm.tcycle <- tcycle
      # check and set discounts
      if (!is.numeric(discount.cost)) {
        rlang::abort(
          "Discount rate must be numeric", class="invalid_discount"
        )
      }
      private$smm.discost <- discount.cost
      if (!is.numeric(discount.utility)) {
        rlang::abort(
          "Discount rate must be numeric", class="invalid_discount"
        )
      }
      private$smm.disutil <- discount.utility
      # reset the model to its ground state
      self$reset()
      # return a new SemiMarkovModel object
      return(invisible(self))
    },
    
    #' @description Sets transition probabilities.
    #' @param Pt Per-cycle transition probability matrix. The row and 
    #' column labels must be the state names and each row must sum to one.
    #' Non-zero probabilities for undefined transitions are not allowed. 
    #' @return Updated \code{SemiMarkovModel} object
    set_probabilities = function(Pt) {
      # check Pt
      if (missing(Pt)) {
        rlang::abort("Pt is missing, without default", class="invalid_Pt")
      }
      if (!is.matrix(Pt)) {
        rlang::abort("'Pt' must be a matrix", class="invalid_Pt")
      }
      if ((nrow(Pt)!=self$order() || ncol(Pt)!=self$order())) {
        rlang::abort(
          paste("'Pt' must have size", self$order(), "by", self$order()),
          class = "invalid_Pt")
      }
      if (!setequal(self$get_statenames(), dimnames(Pt)[[1]])) {
        rlang::abort(
          "Each row of 'Pt' must have a state name",
          class = "invalid_Pt")
      }  
      if (!setequal(self$get_statenames(), dimnames(Pt)[[2]])) {
        rlang::abort(
          "Each column of 'Pt' must have a state name",
          class = "invalid_Pt")
      }  
      if (any(is.na(Pt))) {
        rlang::abort(
          "All elements of 'Pt' must be defined",
          class = "invalid_Pt"
        )
      }
      if (any(Pt<0) | any(Pt>1)) {
        rlang::abort(
          "All elements of 'Pt' must be probabilities",
          class = "invalid_Pt"
        )
      }
      # check that Pt[i,j]==0 if edge i->j is not in graph
      M <- matrix(
        data = FALSE, 
        nrow = self$order(), ncol = self$order(),
        dimnames = list(source=dimnames(Pt)[[1]], target=dimnames(Pt)[[2]])
      )
      for (ie in 1:self$size()) {
        e <- private$E[[ie]]
        is <- e$source()$name()
        it <- e$target()$name()
        M[is,it] <- TRUE
      }
      AA <- (Pt != 0)    
      if (sum(M | AA) > sum(M)) {
        rlang::abort(
          "All non-zero elements of Pt must correspond to allowed transitions",
          class = "invalid_Pt"
        )
      }
      # check that all rows of Pt sum to 1
      sumP <- rowSums(Pt)
      if (any(abs(sumP-1)>sqrt(.Machine$double.eps))) {
        rlang::abort(
          "All rows of Pt must sum to 1",
          class = "invalid_Pt"
        )
      }
      # set the class variable
      private$smm.Pt <- Pt
      # return updated model
      return(invisible(self))
    },
    
    #' @description Per-cycle transition probability matrix for the model.
    #' @return A square matrix of size equal to the number of states. If all
    #' states are labelled, the dimnames take the names of the states.
    transition_probability = function() {
      return(private$smm.Pt)
    },

    #' @description Return the per-cycle transition costs for the model.
    #' @return A square matrix of size equal to the number of states. If all
    #' states are labelled, the dimnames take the names of the states.
    transition_cost = function() {
      # get the state names
      state.names <- sapply(private$V, function(v) {v$label()})
      # construct the matrix
      Ic <- matrix(
        data = 0, 
        nrow = self$order(), ncol = self$order(),
        dimnames = list(source=state.names, target=state.names)
      )
      # populate the cells with costs
      for (ie in 1:self$size()) {
        e <- private$E[[ie]]
        is <- self$vertex_index(e$source())
        it <- self$vertex_index(e$target())
        Ic[is,it] <- e$cost()
      }
      return(Ic)
    },

    #' @description Returns a character list of state names.
    #' @return List of the names of each state.
    get_statenames = function() {
      statenames <- sapply(private$V, function(x) {return(x$label())})
      return(statenames)
    },
    
    #' @description Resets the model counters. 
    #' @details Resets the state populations, next cycle number and elapsed time
    #' of the model. By default the model is returned to its ground state (1000
    #' people in the first state and zero in the others; next cycle is labelled
    #' zero; elapsed time (years) is zero). Any or all of these can be set via 
    #' this function. \code{icycle} is simply an integer counter label for each 
    #' cycle, \code{elapsed} sets the elapsed time in years from the index time
    #' from which discounting is assumed to apply.
    #' @param populations A named vector of populations for
    #' the start of the state. The names should be the state names. 
    #' Due to the R implementation of matrix algebra, \code{populations} 
    #' must be a numeric type and is not restricted to being an integer. If 
    #' NULL, the population of the first state is set to 1000 and the others
    #' to zero.
    #' @param icycle Cycle number at which to start/restart. 
    #' @param elapsed Elapsed time since the index (reference) time used for
    #' discounting as an R \code{difftime} object.
    #' @return Updated \code{SemiMarkovModel} object.
    reset = function(populations=NULL, icycle=as.integer(0), 
                     elapsed=as.difftime(0, units="days")) {
      # check that population is valid
      if (is.null(populations)) {
        private$smm.pop <- vector(mode="numeric", length=self$order())
        names(private$smm.pop) <- self$get_statenames()
        private$smm.pop[1] <- 1000
      } else {
        if (length(populations) != self$order()) {
          rlang::abort(
            "Argument 'populations' must have one element per state", 
            class="incorrect_state_count"
          )
        }
        # check the state names are correct
        if (!setequal(self$get_statenames(), names(populations))) {
          rlang::abort(
            "Each element of 'populations' must have a state name",
            class="unmatched_states")
        }  
        # check that all populations are of type numeric
        sapply(populations, function(x) {
          if (is.numeric(x)==F){
            rlang::abort(
              "Each element of 'populations' must be of type numeric",
              class="non-numeric_state_population")
          }
        })
        # set the populations
        for (s in self$get_statenames()) {
          private$smm.pop[s] <- populations[s]
        }
      }
      # check and set the cycle number
      if (!is.integer(icycle)) {
        rlang::abort(
          "'icycle' must be an integer",
          class = "invalid_icycle"
        )
      }
      if (icycle < 0) {
        rlang::abort(
          "'icycle' must be >= 0",
          class = "invalid_icycle"
        )
        
      }
      private$smm.icycle <- icycle
      # check and update the elapsed time
      if (class(elapsed) != "difftime") {
        rlang::abort(
          "Argument 'elapsed' must be of class 'difftime'.",
          class = "invalid_elapsed"
        )
      }
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
      DF <- data.frame(
        Name = sapply(private$V, function(x) {return(x$name())}),
        Cost = sapply(private$V, function(x) {return(x$cost())}),
        Utility = sapply(private$V, function(x) {return(x$utility())})
      )
      return(DF)
    },
    
    #' @description Applies one cycle of the model.
    #' @param hcc.pop Boolean; whether to apply half cycle correction to the
    #' population and QALY. If TRUE, the correction is only applied to the 
    #' outputs of 
    #' functions \code{cycle} and \code{cycles}; the state population passed to
    #' the next cycle is the end cycle population, obtainable 
    #' with \code{get_populations}.
    #' @param hcc.cost Boolean; whether to apply half cycle correction to the
    #' costs. If true, the occupancy costs are computed using the population
    #' at half cycle; if false they are applied at the end of the cycle. 
    #' Applicable only if \code{hcc.pop} is TRUE.
    #' @return Calculated values, one row per state, as a data frame with the
    #' following columns:
    #' \describe{
    #' \item{\code{State}}{Name of the state.}
    #' \item{\code{Cycle}}{The cycle number.}
    #' \item{\code{Time}}{Clock time, years.}
    #' \item{\code{Population}}{Population of the state at the end of 
    #' the cycle, or at mid-cycle if half-cycle correction is applied.}
    #' \item{\code{OccCost}}{Cost of the population occupying the state for 
    #' the cycle. Discount is applied, if the options are set. The costs are
    #' normalized by the model population. The cycle costs are derived from the
    #' annual occupancy costs of the \code{MarkovState}s. Applied to the end
    #' population, i.e. unaffected by half cycle correction, as per 
    #' Briggs \emph{et al}.}
    #' \item{\code{EntryCost}}{Cost of the transitions \emph{into} the state
    #' during the cycle. Discounting is applied, if the option is set. 
    #' The result is normalized by the model population. The cycle costs
    #' are derived from \code{Transition} costs.}
    #' \item{\code{Cost}}{Total cost, normalized by model population.}
    #' \item{\code{QALY}}{Quality adjusted life years gained by occupancy of 
    #' the states during the cycle. Half cycle correction and discounting are 
    #' applied, if these options are set. Normalized by the model population.}
    #' }
    cycle = function(hcc.pop=TRUE, hcc.cost=TRUE) {
      # check and set half cycle correction
      if (!is.logical(hcc.pop)) {
        rlang::abort(
          "Argument 'hcc.pop' must be logical.",
          class = "invalid_hcc"
        )
      }
      if (!is.logical(hcc.cost)) {
        rlang::abort(
          "Argument 'hcc.cost' must be logical.",
          class = "invalid_hcc"
        )
      }
      if (hcc.cost & !hcc.pop) {
        rlang::abort(
          "hcc.cost applies only if hcc.pop is TRUE",
          class = "invalid_hcc"
        )
      }
      # calculate cycle duration in years (dty), elapsed time at end cycle and 
      # discount factors (dfc, dfu)
      dty <- as.numeric(private$smm.tcycle, units="days")/365.25
      elapsed <- as.numeric(private$smm.elapsed, units="days")/365.25 + dty
      dfc <- 1/(1+private$smm.discost)^elapsed
      dfu <- 1/(1+private$smm.disutil)^elapsed
      # transition costs, calculated as number of transitions between each
      # pair of states, multiplied by transition cost, summed by the state
      # being entered (entry costs)
      P <- matrix(
        data=rep(private$smm.pop,times=self$order()),
        nrow = self$order(), ncol=self$order(),
        byrow = FALSE
      )
      TC <- P*self$transition_probability()*self$transition_cost()
      entry.costs <- colSums(TC)*dfc
      # Apply the transition probabilities to get the end state populations,
      pop.end <- private$smm.pop %*% self$transition_probability()
      pop.end <- drop(pop.end)
      # calculate annual costs of state occupancy
      state.costs <- sapply(private$V, function(x) {return(x$cost())})
      state.costs <- state.costs * dty
      # calculate QALYs gained from state occupancy
      state.utilities <- sapply(private$V, FUN=function(v){v$utility()})
      state.utilities <- state.utilities*dfu
      # half cycle correction (affects reporting only)
      if (hcc.pop) {
        pop <- (private$smm.pop + pop.end)/2
        elapsed <- as.numeric(private$smm.elapsed, units="days")/365.25 + dty/2
      } else {
        pop <- pop.end
        elapsed <- as.numeric(private$smm.elapsed, units="days")/365.25 + dty
      }
      qaly <- pop*state.utilities*dty
      if (hcc.cost) {
        occupancy.costs <- pop*state.costs*dfc
      } else {
        occupancy.costs <- pop.end*state.costs*dfc
      }
      # update the populations
      private$smm.pop <- pop.end
      # update cycle number
      private$smm.icycle <- private$smm.icycle + 1
      # update elapsed time
      private$smm.elapsed <- private$smm.elapsed + private$smm.tcycle
      # return calculated values, per state
      RC <- data.frame(
        State = self$get_statenames(),
        Cycle = rep(private$smm.icycle, times=length(self$order())),
        Time = rep(elapsed, times=length(self$order())),
        Population = pop,
        OccCost = occupancy.costs/sum(self$order()),
        EntryCost = entry.costs/sum(self$order()),
        Cost = (occupancy.costs+entry.costs)/sum(private$smm.pop),
        QALY = qaly / sum(private$smm.pop),
        stringsAsFactors = F
      )
      return(RC)
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
    #' @param hcc.pop Boolean; whether to apply half cycle correction to the
    #' population and QALY. If TRUE, the correction is only applied to the 
    #' outputs of functions \code{cycle} and \code{cycles}; the state 
    #' population passed to
    #' the next cycle is the end cycle population, obtainable 
    #' with \code{get_populations}.
    #' @param hcc.cost Boolean; whether to apply half cycle correction to the
    #' costs. If true, the occupancy costs are computed using the population
    #' at half cycle; if false they are applied at the end of the cycle.
    #' Applicable only if \code{hcc.pop} is TRUE.
    #' @return Data frame with cycle results.
    #' following columns:
    #' \describe{
    #' \item{\code{Cycle}}{The cycle number.}
    #' \item{\code{Time}}{Elapsed time at end of cycle, years}
    #' \item{\code{<name>}}{Population of state \code{<name>} at the end of
    #' the cycle.}
    #' \item{\code{Cost}}{Cost associated with occupancy and transitions between
    #' states during the cycle.}
    #' \item{\code{QALY}}{Quality adjusted life years associated with occupancy
    #' of the states in the cycle.}
    #' } 
    cycles = function(ncycles=2, hcc.pop=TRUE, hcc.cost=TRUE) {
      # show zero?
      if (private$smm.icycle==0) {
        nzero <- 1
      } else {
        nzero <- 0
      }
      # data frame for cycle summaries
      DF <- data.frame(
        Cycle = integer(length=ncycles+nzero),
        Cost = numeric(length=ncycles+nzero)
      )
      statenames <- self$get_statenames()
      POP <- matrix(data=NA, nrow=ncycles+nzero, ncol=length(statenames))
      colnames(POP) <- statenames
      DF <- cbind(DF, POP)
      # add zero
      if (nzero > 0) {
        DF[1,"Cycle"] <- 0
        DF["Years"] <- 0
        DF[1, names(private$smm.pop)] <- private$smm.pop
        DF[1,"Cost"] <- 0
        DF[1,"QALY"] <- 0
      }
      # run the model
      for (i in (1+nzero):(ncycles+nzero)) {
        # single cycle
        DF.cycle <- self$cycle(hcc.pop, hcc.cost)
        # set the cycle number
        DF[i, "Cycle"] <- DF.cycle$Cycle[1]
        DF[i, "Years"] <- DF.cycle$Time[1]
        # collect state populations and cycle sums into a single frame
        DF[i, names(private$smm.pop)] <- DF.cycle$Population
        # add normalized sum of costs for all states 
        DF[i,"Cost"] <- sum(DF.cycle$Cost)
        # add normalized sum of QALYs gained
        DF[i,"QALY"] <- sum(DF.cycle$QALY)
      }
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
      for (e in private$E) {
        if (inherits(e, what=c("Transition"))) {
          mv <- c(mv, e$modvars())
        }
      }
      # find the ModVars in the states
      for (v in private$V){
        if (inherits(v, what=c("MarkovState"))) {
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
    modvar_table = function(expressions=TRUE) {
      # create list of model variables in this decision tree, excluding
      # expressions if not wanted
      mvlist <- self$modvars()
      if (!expressions) {
        mvlist <- mvlist[sapply(mvlist,FUN=function(v){!v$is_expression()})]
      }
      # create a data frame of model variables
      DF <- data.frame(
        Description = sapply(mvlist, FUN=function(x){
          rv <- x$description()
          return(rv)
        }),
        Units = sapply(mvlist, FUN=function(x){
          rv <- x$units()
          return(rv)
        }),
        Distribution = sapply(mvlist, FUN=function(x){
          rv <- x$distribution()
          return(rv)
        }),
        Mean = sapply(mvlist, FUN=function(x){
          rv <- x$mean()
          return(rv)
        }),
        E = sapply(mvlist, FUN=function(x){
          rv <- ifelse(x$is_expression(), x$mu_hat(), x$mean())
          return(rv)
        }),
        SD = sapply(mvlist, FUN=function(x){
          rv <- ifelse(x$is_expression(), x$sigma_hat(), x$SD())
          return(rv)
        }),
        Q2.5 = sapply(mvlist, FUN=function(x){
          rv <- ifelse(
            x$is_expression(), 
            x$q_hat(probs=c(0.025)), 
            x$quantile(probs=c(0.025))
          )
          return(rv)
        }),
        Q97.5 = sapply(mvlist, FUN=function(x){
          rv <- ifelse(
            x$is_expression(), 
            x$q_hat(probs=c(0.975)), 
            x$quantile(probs=c(0.975))
          )
          return(rv)
        }),
        Est = sapply(mvlist, FUN=function(exp){
          rv <- ifelse(exp$is_expression(),TRUE,FALSE)
          return(rv)
        })
      )
      # Return the table
      return(DF)
    }

  )
)
