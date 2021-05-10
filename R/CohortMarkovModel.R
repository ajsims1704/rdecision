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
    cmm.Ip = NULL,
    cmm.Ic = NULL,
    cmm.hcc = NULL,
    cmm.discount = NULL,
    cmm.pop = NULL,
    cmm.icycle = NULL
  ),
  public = list(
    
    #' @description Creates a Markov model for cohort simulation.
    #' @details A Markov model must meet the following conditions:
    #' \enumerate{
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
    #' @param discount Annual discount rate, as a percentage.
    #' @return A \code{CohortMarkovModel} object.
    initialize = function(V,E,tcycle=as.difftime(365.25, units="days"),
                          hcc=TRUE, discount=0) {
      # initialize the base class(es)
      super$initialize(V,E)
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
      # check that the cycle time is an interval
      if (class(tcycle) != "difftime") {
        rlang::abort(
          "Argument 'tcycle' must be of class 'difftime'.",
          class = "invalid_cycle_length"
        )
      }
      private$cmm.tcycle <- tcycle
      # check and set half cycle correction
      if (!is.logical(hcc)) {
        rlang::abort(
          "Argument 'hcc' must be logical.",
          class = "invalid_hcc"
        )
      }
      private$cmm.hcc <- hcc
      # check and set discount
      if (!is.numeric(discount)) {
        rlang.abort(
          "Discount rate must be numeric", class="invalid_discount"
        )
      }
      private$cmm.discount <- discount/100
      # create a population vector
      private$cmm.pop <- vector(mode="numeric", length=self$order())
      names(private$cmm.pop) <- self$get_statenames()
      # set the cycle number
      private$cmm.icycle <- 0
      # force creation of transition probabilities and costs matrices
      self$transition_probability()
      self$transition_cost()
      # return a new CohortMarkovModel object
      return(invisible(self))
    },
    
    #' @description Return the per-cycle transition matrix for the model.
    #' @returns A square matrix of size equal to the number of states. If all
    #' states are labelled, the dimnames take the names of the states.
    transition_probability = function() {
      # if the matrix is not null, create it. This assumes the graph is 
      # immutable (no edges or vertexes added or removed since its creation)
      if (is.null(private$Ip)) {
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
        # save the matrix as a class private variable
        private$cmm.Ip <- Ip
      }
      return(private$cmm.Ip)
    },

    #' @description Return the per-cycle transition costs for the model.
    #' @returns A square matrix of size equal to the number of states. If all
    #' states are labelled, the dimnames take the names of the states.
    transition_cost = function() {
      # if the matrix is not null, create it. This assumes the graph is 
      # immutable (no edges or vertexes added or removed since its creation)
      if (is.null(private$cmm.Ic)) {
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
        # save the matrix as a class private variable
        private$cmm.Ic <- Ic
      }
      return(private$cmm.Ic)
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
    #' Due to R's implementation of matrix algebra, \code{populations} 
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
    
    #' @description Applies one cycle of the model
    #' @return Calculated values, per state.
    cycle = function() {
      # transition costs, calculated as number of transitions between each
      # pair of states, multiplied by transition cost, summed by the state
      # being entered (entry costs)
      P <- matrix(
        data=rep(private$cmm.pop,times=self$order()),
        nrow = self$order(), ncol=self$order(),
        byrow = FALSE
      )
      TC <- P*private$cmm.Ip*private$cmm.Ic
      entry.costs <- colSums(TC)
      # Apply the transition probabilities to get the end state populations
      pop.end <- private$cmm.pop %*% self$transition_probability()
      pop.end <- drop(pop.end)
      # half-cycle correction
      if (private$cmm.hcc) {
        pop.occ <- (private$cmm.pop + pop.end)/2
      } else {
        pop.occ <- pop.end
      }
      private$cmm.pop <- pop.end
      # calculate annual costs of state occupancy
      state.costs <- sapply(private$V, function(x) {return(x$cost())})
      dty <- as.numeric(private$cmm.tcycle, units="days")/365.25
      state.costs <- state.costs * dty
      occupancy.costs <- pop.occ*state.costs
      # apply discounting
      ty <- (private$cmm.icycle)*dty
      denom <- (1+private$cmm.discount)^ty
      entry.costs <- entry.costs / (1+private$cmm.discount)^ty
      occupancy.costs <- occupancy.costs / (1+private$cmm.discount)^ty
      # update cycle number
      private$cmm.icycle <- private$cmm.icycle + 1
      # return calculated values, per state
      RC <- data.frame(
        Cycle = rep(private$cmm.icycle, times=length(private$cmm.pop)),
        Population = private$cmm.pop,
        NormCost = (occupancy.costs+entry.costs)/sum(private$cmm.pop),
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
    #' @return Data frame with cycle results.
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
        DF[1, names(private$cmm.pop)] <- private$cmm.pop
        DF[1,"Cost"] <- 0
      }
      # run the model
      for (i in (1+nzero):(ncycles+nzero)) {
        # single cycle
        DF.cycle <- self$cycle()
        # set the cycle number
        DF[i, 'Cycle'] <- min(DF.cycle$Cycle)
        # collect state populations and cycle sums into a single frame
        DF[i, names(private$cmm.pop)] <- private$cmm.pop
        # add sum of costs for all states 
        DF[i,'Cost'] <- sum(DF.cycle$NormCost)
      }
      # return summary data frame
      return(DF)
    }
    
  
  )
)
