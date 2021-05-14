#' @title \verb{CohortMarkovModel}
#' 
#' @description An R6 class for a Markov model with cohort simulation.
#' 
#' @details A class to represent a Markov model, using cohort simulation. In 
#' graph theory terms, a Markov model is a directed multidigraph permitting 
#' loops (a loop multidigraph), optionally labelled, or \dfn{quiver}. It is a
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
#' \eqn{r = -\ln(1-p)/t}.
#' 
#' @references{
#'   Briggs A, Claxton K, Sculpher M. Decision modelling for health economic 
#'   evaluation. Oxford, UK: Oxford University Press; 2006.
#'   
#'   Sonnenberg FA, Beck JR. Markov models in medical decision making: a
#'   practical guide. \emph{Med Decis Making}, 1993:\strong{13}:322. 
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
    cmm.tcycle = NULL,
    cmm.hcc = NULL,
    cmm.discost = NULL,
    cmm.disutil = NULL,
    cmm.pop = NULL,
    cmm.icycle = NULL
  ),
  public = list(
    
    #' @description Creates a Markov model for cohort simulation.
    #' @details A Markov model must meet the following conditions:
    #' \enumerate{
    #'   \item It must have at least one node and one edge.
    #'   \item All nodes must be of class \code{MarkovState};
    #'   \item All edges must be of class \code{MarkovTransition};
    #'   \item The nodes and edges must form a digraph whose underlying
    #'   graph is connected;
    #'   \item Each non-absorbing state must have one outgoing transition
    #'    whose hazard rate is NULL.
    #' }
    #' @param V A list of nodes (\code{MarkovState}s).
    #' @param E A list of edges (\code{MarkovTransition}s).
    #' @param tcycle Cycle length, expressed as an R \code{difftime} object; 
    #' default 1 year.
    #' @param hcc Boolean; whether to apply half cycle correction.
    #' @param discount.cost Annual discount rate for future costs.
    #' @param discount.utility Annual discount rate for future incremental
    #' utility.
    #' @return A \code{CohortMarkovModel} object. The population of the first
    #' state is set to 1000.
    initialize = function(V,E,tcycle=as.difftime(365.25, units="days"),
                          hcc=TRUE, discount.cost=0, discount.utility=0) {
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
      # check that the cycle time is an interval
      if (class(tcycle) != "difftime") {
        rlang::abort(
          "Argument 'tcycle' must be of class 'difftime'.",
          class = "invalid_cycle_length"
        )
      }
      private$cmm.tcycle <- tcycle
      # check that each non-absorbing state has exactly one outgoing
      # transition whose rate is NULL, by creating the transition matrix
      self$transition_probability()
      # check and set half cycle correction
      if (!is.logical(hcc)) {
        rlang::abort(
          "Argument 'hcc' must be logical.",
          class = "invalid_hcc"
        )
      }
      private$cmm.hcc <- hcc
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
      # create a population vector and initialize first state to 1000
      private$cmm.pop <- vector(mode="numeric", length=self$order())
      names(private$cmm.pop) <- self$get_statenames()
      private$cmm.pop[1] <- 1000
      # set the cycle number
      private$cmm.icycle <- 0
      # force creation of transition probabilities and costs matrices
      self$transition_probability()
      self$transition_cost()
      # return a new CohortMarkovModel object
      return(invisible(self))
    },
    
    #' @description Return the per-cycle transition matrix for the model.
    #' @details Checks that each non-absorbing state has exactly one outgoing
    #' transition rate whose rate is NULL. 
    #' @returns A square matrix of size equal to the number of states. If all
    #' states are labelled, the dimnames take the names of the states.
    transition_probability = function() {
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
        return(ifelse(n.out==0, TRUE, n.null==1))
      })
      if (!all(lv)) {
        rlang::abort(
          "Each non-absorbing state must have one NULL rate transition",
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
      # convert rates to per-cycle probabilities
      for (is in 1:nrow(Ip)) {
        for (it in 1:nrow(Ip)) {
          t <- as.numeric(private$cmm.tcycle, units="days")/365.25
          Ip[is,it] <- 1 - exp(-Ip[is,it]*t)
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
    #' @returns A square matrix of size equal to the number of states. If all
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
    
    #' @description Sets the occupancy of each state. 
    #' @details Sets the cycle count to zero.
    #' @param populations A named vector of populations for
    #' the start of the state. The names should be the state names. 
    #' Due to the R implementation of matrix algebra, \code{populations} 
    #' must be a numeric type and is not restricted to being an integer.
    #' @return Updated \code{CohortMarkovModel} object.
    set_populations = function(populations) {
      # check that prevalence is valid
      if (length(populations) != self$order()) {
        rlang::abort("Argument 'populations' must have one element per state", 
                     class="incorrect_state_count")
      }
      # check the state names are correct
      if (!setequal(self$get_statenames(), names(populations))) {
        rlang::abort("Each element of 'populations' must have a state name",
                     class="unmatched_states")
      }  
      # check that all populations are of type numeric
      sapply(populations, function(x) {
        if (is.numeric(x)==F){
          rlang::abort("Each element of 'populations' must be of type numeric",
                       class="non-numeric_state_population")
        }
      })
      # set the populations
      for (s in self$get_statenames()) {
        private$cmm.pop[s] <- populations[s]
      }
      # reset the cycle number (assumed restart if new population)
      private$cmm.icycle <- 0
      # return updated object
      return(invisible(self))
    },
    
    #' @description Gets the occupancy of each state
    #' @returns A numeric vector of populations, named with state names.
    get_populations = function() {
      return(private$cmm.pop)
    },
 
    #' @description Tabulation of states
    #' @details Creates a data frame summary of each state in the model.
    #' @returns A data frame with the following columns:
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
    #' @returns Calculated values, one row per state, as a data frame with the
    #' following columns:
    #' \describe{
    #' \item{\code{State}}{Name of the state.}
    #' \item{\code{Cycle}}{The cycle number.}
    #' \item{\code{Time}}{Clock time, years.}
    #' \item{\code{Population}}{Population of the state at the end of
    #' the cycle.}
    #' \item{\code{OccCost}}{Cost of the population occupying the state for 
    #' the cycle. Half-cycle correction and discount are applied, if the
    #' options are set. the costs are normalized by the model population. The
    #' cycle costs are derived from the annual occupancy costs of the
    #' \code{MarkovState}s.}
    #' \item{code{EntryCost}}{Cost of the transitions \emph{into} the state
    #' during the cycle. Discounting is applied, if the option is set. 
    #' The result is normalized by the model population. The cycle costs
    #' are derived from \code{MarkovTransition} costs.}
    #' \item{\code{Cost}}{Total cost, normalized by model population.}
    #' \item{QALY}{Quality adjusted life years gained by occupancy of the states
    #' during the cycle. Half cycle correction and discounting are applied,
    #' if these options are set. Normalized by the model population.}
    #' }
    cycle = function() {
      # calculate cycle duration (dty), clock time (ty) and discount 
      # factors (dfc, dfu)
      dty <- as.numeric(private$cmm.tcycle, units="days")/365.25
      if (private$cmm.hcc) {
        ty <- (private$cmm.icycle+0.5)*dty
      } else {
        ty <- (private$cmm.icycle+1)*dty
      }
      dfc <- 1/(1+private$cmm.discost)^ty
      dfu <- 1/(1+private$cmm.disutil)^ty
      # transition costs, calculated as number of transitions between each
      # pair of states, multiplied by transition cost, summed by the state
      # being entered (entry costs)
      P <- matrix(
        data=rep(private$cmm.pop,times=self$order()),
        nrow = self$order(), ncol=self$order(),
        byrow = FALSE
      )
      TC <- P*self$transition_probability()*self$transition_cost()
      entry.costs <- colSums(TC)*dfc
      # Apply the transition probabilities to get the end state populations,
      # with half-cycle correction, if required
      pop.end <- private$cmm.pop %*% self$transition_probability()
      pop.end <- drop(pop.end)
      if (private$cmm.hcc) {
        pop.occ <- (private$cmm.pop + pop.end)/2
      } else {
        pop.occ <- pop.end
      }
      private$cmm.pop <- pop.end
      # calculate annual costs of state occupancy
      state.costs <- sapply(private$V, function(x) {return(x$cost())})
      state.costs <- state.costs * dty
      occupancy.costs <- pop.occ*state.costs*dfc
      # calculate QALYs gained from state occupancy
      state.utilities <- sapply(private$V, FUN=function(v){v$utility()})
      state.utilities <- state.utilities*dfu
      qaly <- pop.occ*state.utilities*dty
      # update cycle number
      private$cmm.icycle <- private$cmm.icycle + 1
      # return calculated values, per state
      RC <- data.frame(
        State = self$get_statenames(),
        Cycle = rep(private$cmm.icycle, times=length(self$order())),
        Time = rep(ty, times=length(self$order())),
        Population = private$cmm.pop,
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
    #' @returns Data frame with cycle results.
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
    cycles = function(ncycles=2) {
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
        DF.cycle <- self$cycle()
        # set the cycle number
        DF[i, "Cycle"] <- DF.cycle$Cycle[1]
        DF[i, "Years"] <- DF.cycle$Time[1]
        # collect state populations and cycle sums into a single frame
        DF[i, names(private$cmm.pop)] <- private$cmm.pop
        # add normalized sum of costs for all states 
        DF[i,"Cost"] <- sum(DF.cycle$Cost)
        # add normalized sum of QALYs gained
        DF[i,"QALY"] <- sum(DF.cycle$QALY)
      }
      # return summary data frame
      return(DF)
    }
    
  
  )
)
