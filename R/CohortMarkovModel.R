#' @title \verb{CohortMarkovModel}
#' 
#' @description An R6 class for a Markov model with cohort simulation.
#' 
#' @details A class to represent a Markov model, using cohort simulation. In 
#' graph theory terms, a Markov model is a directed multidigraph permitting 
#' loops (a loop multidigraph), optionally labelled, or a \dfn{quiver}. It is a
#' multidigraph because there are potentially two edges between each pair of
#' nodes {A,B} representing the transition probabilities from A to B and 
#' \emph{vice versa}. It is a directed graph because the transition
#' rates refer to transitions in one direction. Each edge can be optionally
#' labelled. It permits loops (edges whose source and target are the same node)
#' to represent patients that remain in the same state between cycles.
#'
#' @section Probabilities and rates:
#' To calculate per-cycle probabilities from rates, Briggs (2002) and
#' Sonnenberg & Beck (1993) use the expression \eqn{p = 1-\exp(-rt)}, where 
#' \eqn{r} is an instantaneous rate
#' and \eqn{t} is a time interval of interest. This is derived by assuming that 
#' if there is a population of \eqn{N} patients the rate of events is
#' proportional to the number of patients, i.e. that events occur independently:
#' \eqn{\frac{dN}{dt} \propto N}. The number of patients, \eqn{N(t)} who have
#' not experienced an event at time \eqn{t} is therefore given by the solution
#' to this differential equation, i.e. \eqn{N(t) = N e^{-r t}}, where \eqn{r}
#' is the rate, or the reciprocal of the time constant. The expected number of 
#' events \eqn{\hat{K}} which occur in interval \eqn{t} from a starting 
#' population of \eqn{N} is \eqn{\hat{K} = N - N e^{-r t}}, or 
#' \eqn{\hat{K} = Np}, where \eqn{p = 1-\exp(-r t)}, the probability with 
#' which events arise during interval \eqn{t}. If instead a per-interval 
#' probability is known, the rate is derived from the inverse relationship 
#' \eqn{r = -\ln(1-p)/t}. Miller & Homan (1994) and Fleurence & Hollenbeak 
#' (2007) discuss the subject in more detail.
#' 
#' @section Probabilities and rates in states with multiple transitions:
#' Consider a model with a starting state A, which has a self-loop, and three 
#' absorbing states, B, C and D, with allowed transitions from A to B, A to C 
#' and A to D only. Assume the transition rates (in units of per patient per
#' year) are \eqn{r_{AB}=2}, \eqn{r_{AC}=0.5} and \eqn{r_{AD}=0.1}. Rates
#' are additive, and the total rate of patients leaving state A is 2.6. Thus,
#' the probability of leaving state A in one year is 
#' \eqn{p_A = 1-e^{-2.6}=0.9257},
#' and the probability of leaving in one month is \eqn{1-e^{-2.6/12}=0.1948}; 
#' i.e. 93\% of patients will leave state A within 1 year, and 19\% will leave
#' within one month. However, the individual transition probabilities per cycle
#' \eqn{p_{AB}}, \eqn{p_{AC}} and \eqn{p_{AD}} must be calculated from the
#' total per cycle probability (\eqn{p_A}) and the conditional probability for
#' each transition. For example in one year, \eqn{p_{AB} = p_A * 2/2.6 = 0.712},
#' \eqn{p_{AC} = p_A * 0.5/2.6 = 0.178}, \eqn{p_{AD} = p_A * 0.1/2.6 = 0.036}, 
#' with \eqn{p_{AA} = 1-0.9257 = 0.074}. Applying the inverse relationship 
#' described in the previous section to calculate individual probabilities 
#' from individual rates is incorrect  (i.e. \eqn{p_{AB} \ne 1 - e^{-r_{AB}t}}).
#' 
#' @references{
#'   Briggs A, Claxton K, Sculpher M. Decision modelling for health economic 
#'   evaluation. Oxford, UK: Oxford University Press; 2006.
#'   
#'   Fleurence RL and Hollenbeak CS. Rates and probabilities in economic 
#'   modelling. \emph{PharmacoEconomics}, 2007;\strong{25}:3--6. 
#'   
#'   Miller DK and Homan SM. Determining transition probabilities: confusion
#'   and suggestions. \emph{Medical Decision Making} 1994;\strong{14}:52-58. 
#'   
#'   Sonnenberg FA, Beck JR. Markov models in medical decision making: a
#'   practical guide. \emph{Medical Decision Making}, 1993:\strong{13}:322. 
#' }
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export 
#' 
CohortMarkovModel <- R6::R6Class(
  classname = "CohortMarkovModel",
  lock_class = TRUE,
  inherit = Digraph,
  private = list(
    cmm.discost = NULL,
    cmm.disutil = NULL,
    cmm.pop = NULL,
    cmm.icycle = NULL,
    cmm.elapsed = NULL
  ),
  public = list(
    
    #' @description Creates a Markov model for cohort simulation.
    #' @details A Markov model must meet the following conditions:
    #' \enumerate{
    #'   \item It must have at least one node and at least one edge.
    #'   \item All nodes must be of class \code{MarkovState};
    #'   \item All edges must be of class \code{MarkovTransition};
    #'   \item The nodes and edges must form a digraph whose underlying
    #'   graph is connected;
    #'   \item Each state must have at least one outgoing transition and 
    #'   have exactly one outgoing edge whose hazard rate is NULL. This is to
    #'   ensure that the multinomial distribution of the probability of leaving
    #'   each node is not over or under-constrained. Rates can be changed
    #'   between cycles, and this constraint is checked at the point of cycling,
    #'   not at the point of model creation.
    #'   \item No two edges may share the same source and target nodes (i.e. 
    #'   the digraph may not have multiple edges). This is to ensure that there
    #'   are no more transitions than cells in the transition matrix.
    #' }
    #' @param V A list of nodes (\code{MarkovState}s).
    #' @param E A list of edges (\code{MarkovTransition}s).
    #' @param discount.cost Annual discount rate for future costs.
    #' @param discount.utility Annual discount rate for future incremental
    #' utility.
    #' @return A \code{CohortMarkovModel} object. The population of the first
    #' state is set to 1000.
    initialize = function(V, E, discount.cost=0, discount.utility=0) {
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
      # check that all edges inherit from MarkovTransition
      T <- which(
        sapply(E, function(e){inherits(e,what="MarkovTransition")}),arr.ind=TRUE
      )
      if (!setequal(seq_along(E),T)) {
        rlang::abort(
          "Each edge must be a 'MarkovTransition'.", class="invalid_transition"
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
      # check and set discounts
      if (!is.numeric(discount.cost)) {
        rlang.abort(
          "Discount rate must be numeric", class="invalid_discount"
        )
      }
      private$cmm.discost <- discount.cost
      if (!is.numeric(discount.utility)) {
        rlang.abort(
          "Discount rate must be numeric", class="invalid_discount"
        )
      }
      private$cmm.disutil <- discount.utility
      # reset the model to its ground state
      self$reset()
      # return a new CohortMarkovModel object
      return(invisible(self))
    },
    
    #' @description Return the per-cycle transition matrix for the model.
    #' @param tcycle Cycle length, expressed as an R \code{difftime} object.
    #' @details Checks that each state has at least one outgoing transition and
    #' that exactly one outgoing transition rate whose rate is NULL. 
    #' @return A square matrix of size equal to the number of states. If all
    #' states are labelled, the dimnames take the names of the states.
    transition_probability = function(tcycle) {
      # check that the cycle time is an interval
      if (class(tcycle) != "difftime") {
        rlang::abort(
          "Argument 'tcycle' must be of class 'difftime'.",
          class = "invalid_cycle_length"
        )
      }
      # check that each non-absorbing state has exactly one outgoing
      # transition whose rate is NULL
      lv <- sapply(1:self$order(), function(iv) {
        n.out <- 0
        n.null <- 0
        v <- private$V[[iv]]
        for (ie in 1:self$size()) {
          e <- private$E[[ie]]
          if (identical(e$source(),v)) {
            n.out <- n.out + 1
            if (is.na(e$rate())) {
              n.null <- n.null + 1
            }
          }
        }
        return(n.out>=1 && n.null==1)
      })
      if (!all(lv)) {
        rlang::abort(
          "Each state must have one outgoing NULL rate transition",
          class = "invalid_rate")
      }
      # get the state names
      state.names <- sapply(private$V, function(v) {v$label()})
      # construct the matrix
      Ip <- matrix(
        data = 0, 
        nrow = self$order(), ncol = self$order(),
        dimnames = list(source=state.names, target=state.names)
      )
      # populate the cells with rates
      for (ie in 1:self$size()) {
        e <- private$E[[ie]]
        is <- self$vertex_index(e$source())
        it <- self$vertex_index(e$target())
        Ip[is,it] <- e$rate()
      }
      # compute the sum of outgoing defined rates for each state
      sumr <- rowSums(Ip, na.rm=TRUE)
      # convert total outgoing rates to total per-cycle probabilities, by state
      t <- as.numeric(tcycle, units="days")/365.25
      sump <- 1 - exp(-sumr*t)
      # convert rates to per-cycle probabilities
      for (is in 1:nrow(Ip)) {
        if (sumr[is] > 0) {
          for (it in 1:nrow(Ip)) {
            Ip[is,it] <- (Ip[is,it] / sumr[is]) * sump[is] 
          }
        }
      }
      # replace NAs with values to ensure all rows sum to unity
      for (iv in 1:nrow(Ip)) {
        p.out <- sum(Ip[iv,], na.rm=TRUE)
        if (p.out > 1) {
          label <- private$V[[iv]]$label()
          rlang::abort(
            paste("P(transition) from state", label, "exceeds 1"),
            class = "invalid_transitions"
          )
        }
        Ip[iv,which(is.na(Ip[iv,]))] <- 1 - p.out
      }
      return(Ip)
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
    #' @return Updated \code{CohortMarkovModel} object.
    reset = function(populations=NULL, icycle=as.integer(0), 
                     elapsed=as.difftime(0, units="days")) {
      # check that population is valid
      if (is.null(populations)) {
        private$cmm.pop <- vector(mode="numeric", length=self$order())
        names(private$cmm.pop) <- self$get_statenames()
        private$cmm.pop[1] <- 1000
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
          private$cmm.pop[s] <- populations[s]
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
      private$cmm.icycle <- icycle
      # check and update the elapsed time
      if (class(elapsed) != "difftime") {
        rlang::abort(
          "Argument 'elapsed' must be of class 'difftime'.",
          class = "invalid_elapsed"
        )
      }
      private$cmm.elapsed <- elapsed
      # return updated object
      return(invisible(self))
    },
    
    #' @description Gets the occupancy of each state
    #' @return A numeric vector of populations, named with state names.
    get_populations = function() {
      return(private$cmm.pop)
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
      return(private$cmm.elapsed)  
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
    #' @param tcycle Cycle length, expressed as an R \code{difftime} object; 
    #' default 1 year.
    #' @param hcc.pop Boolean; whether to apply half cycle correction to the
    #' population and QALY. If TRUE, the correction is only applied to the 
    #' outputs of 
    #' functions \code{cycle} and \code{cycles}; the state population passed to
    #' the next cycle is the end cycle population, obtainable 
    #' with \code{get_populations}.
    #' @param hcc.cost Boolean; whether to apply half cycle correction to the
    #' costs. If true, the occupancy costs are computed using the population
    #' at half cycle; if false they are applied at the end of the cycle. 
    #' Applicable only if hcc.pop is TRUE.
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
    #' population, i.e. unaffacted by half cycle correction, as per 
    #' Briggs \emph{et al}.}
    #' \item{\code{EntryCost}}{Cost of the transitions \emph{into} the state
    #' during the cycle. Discounting is applied, if the option is set. 
    #' The result is normalized by the model population. The cycle costs
    #' are derived from \code{MarkovTransition} costs.}
    #' \item{\code{Cost}}{Total cost, normalized by model population.}
    #' \item{\code{QALY}}{Quality adjusted life years gained by occupancy of 
    #' the states during the cycle. Half cycle correction and discounting are 
    #' applied, if these options are set. Normalized by the model population.}
    #' }
    cycle = function(tcycle=as.difftime(365.25, units="days"), hcc.pop=TRUE,
                     hcc.cost=TRUE) {
      # check that the cycle time is an interval
      if (class(tcycle) != "difftime") {
        rlang::abort(
          "Argument 'tcycle' must be of class 'difftime'.",
          class = "invalid_cycle_length"
        )
      }
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
      dty <- as.numeric(tcycle, units="days")/365.25
      elapsed <- as.numeric(private$cmm.elapsed, units="days")/365.25 + dty
      dfc <- 1/(1+private$cmm.discost)^elapsed
      dfu <- 1/(1+private$cmm.disutil)^elapsed
      # transition costs, calculated as number of transitions between each
      # pair of states, multiplied by transition cost, summed by the state
      # being entered (entry costs)
      P <- matrix(
        data=rep(private$cmm.pop,times=self$order()),
        nrow = self$order(), ncol=self$order(),
        byrow = FALSE
      )
      TC <- P*self$transition_probability(tcycle)*self$transition_cost()
      entry.costs <- colSums(TC)*dfc
      # Apply the transition probabilities to get the end state populations,
      pop.end <- private$cmm.pop %*% self$transition_probability(tcycle)
      pop.end <- drop(pop.end)
      # calculate annual costs of state occupancy
      state.costs <- sapply(private$V, function(x) {return(x$cost())})
      state.costs <- state.costs * dty
      # calculate QALYs gained from state occupancy
      state.utilities <- sapply(private$V, FUN=function(v){v$utility()})
      state.utilities <- state.utilities*dfu
      # half cycle correction (affects reporting only)
      if (hcc.pop) {
        pop <- (private$cmm.pop + pop.end)/2
        elapsed <- as.numeric(private$cmm.elapsed, units="days")/365.25 + dty/2
      } else {
        pop <- pop.end
        elapsed <- as.numeric(private$cmm.elapsed, units="days")/365.25 + dty
      }
      qaly <- pop*state.utilities*dty
      if (hcc.cost) {
        occupancy.costs <- pop*state.costs*dfc
      } else {
        occupancy.costs <- pop.end*state.costs*dfc
      }
      # update the populations
      private$cmm.pop <- pop.end
      # update cycle number
      private$cmm.icycle <- private$cmm.icycle + 1
      # update elapsed time
      private$cmm.elapsed <- private$cmm.elapsed + tcycle
      # return calculated values, per state
      RC <- data.frame(
        State = self$get_statenames(),
        Cycle = rep(private$cmm.icycle, times=length(self$order())),
        Time = rep(elapsed, times=length(self$order())),
        Population = pop,
        OccCost = occupancy.costs/sum(self$order()),
        EntryCost = entry.costs/sum(self$order()),
        Cost = (occupancy.costs+entry.costs)/sum(private$cmm.pop),
        QALY = qaly / sum(private$cmm.pop),
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
    #' @param tcycle Cycle length, expressed as an R \code{difftime} object; 
    #' default 1 year.
    #' @param hcc.pop Boolean; whether to apply half cycle correction to the
    #' population and QALY. If TRUE, the correction is only applied to the 
    #' outputs of functions \code{cycle} and \code{cycles}; the state 
    #' population passed to
    #' the next cycle is the end cycle population, obtainable 
    #' with \code{get_populations}.
    #' @param hcc.cost Boolean; whether to apply half cycle correction to the
    #' costs. If true, the occupancy costs are computed using the population
    #' at half cycle; if false they are applied at the end of the cycle.
    #' Applicable only if hcc.pop is TRUE.
    #' @return Data frame with cycle results.
    #' following columns:
    #' \describe{
    #' \item{\code{Cycle}}{The cycle number.}
    #' \item{\code{Time}}{Elapsed time at end of cycle, years}
    #' \item{\code{<name>}}{Population of state \code{<name} at the end of
    #' the cycle.}
    #' \item{\code{Cost}}{Cost associated with occupancy and transitions between
    #' states during the cycle.}
    #' \item{\code{QALY}}{Quality adjusted life years associated with occupancy
    #' of the states in the cycle.}
    #' } 
    cycles = function(ncycles=2, tcycle=as.difftime(365.25, units="days"), 
                      hcc.pop=TRUE, hcc.cost=TRUE) {
      # show zero?
      if (private$cmm.icycle==0) {
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
        DF[1, names(private$cmm.pop)] <- private$cmm.pop
        DF[1,"Cost"] <- 0
        DF[1,"QALY"] <- 0
      }
      # run the model
      for (i in (1+nzero):(ncycles+nzero)) {
        # single cycle
        DF.cycle <- self$cycle(tcycle, hcc.pop, hcc.cost)
        # set the cycle number
        DF[i, "Cycle"] <- DF.cycle$Cycle[1]
        DF[i, "Years"] <- DF.cycle$Time[1]
        # collect state populations and cycle sums into a single frame
        DF[i, names(private$cmm.pop)] <- DF.cycle$Population
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
        if (inherits(e, what=c("MarkovTransition"))) {
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
