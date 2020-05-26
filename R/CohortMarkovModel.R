#' @title 
#' CohortMarkovModel
#' 
#' @description
#' An R6 class for a Markov model with cohort simulation.
#' 
#' @details 
#' A class to represent a complete Markov model.
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export 
#' 
CohortMarkovModel <- R6::R6Class(
  classname = "CohortMarkovModel",
  private = list(
    states = list(),
    Ip = NULL,
    nCyclesPerYear = as.numeric(NA),
    populations = vector(mode="numeric", length=0),
    icycle = "integer",
    discount = 0
  ),
  public = list(
    
    #' @description
    #' Creates a Markov model.
    #' @param states a list of objects of type MarkovState 
    #' @param Ip Matrix of \emph{annual} rates of transition between states,
    #' represented by a TransitionMatrix object.
    #' @param discount The annual discount rate (percentage).
    #' @param nCyclesPerYear The number of cycles per year.
    #' @return A MarkovModel object.
    initialize = function(states, Ip, discount=0, nCyclesPerYear=1) {
      # set the cycle number to be zero
      private$icycle <- 0
      # check and set the states
      sapply(states, function(s) {
        # check that the states are of type MarkovState
        if (!inherits(s, what="MarkovState")){
          rlang::abort("Each element in 'states' must be of class 'MarkovState'",
                       class="not_markov_state")
        }
        # check that temporary states have cycleLimit=1
        if (s$has_cycle_limit() & s$get_cycle_limit() != 1) {
          rlang::abort("Temporary states in a cohort model must have cycle limit of 1",
                       class="unsupported_cycle_limit")
        }
      })
      private$states <- states 
      # check that the transition matrix is a TransitionMatrix object, with the
      # same number and names of states as the state list.
      if (!inherits(Ip, what='TransitionMatrix')) {
        rlang::abort("Argument Ip must be of type TransitionMatrix",
                     class="not_transition_matrix")
      }
      if (!setequal(self$get_statenames(), Ip$dimnames()[["from"]])) {
        rlang::abort("Transition matrix 'Ip' must have the same state names as 'states'",
                     class="unmatched_states")
      }
      private$Ip <- Ip
      # set the number of cycles per year
      private$nCyclesPerYear <- nCyclesPerYear
      # set the discount rate
      self$setDiscount(discount)
    },

    #' @description
    #' Sets the occupancy of each state. 
    #' Calling this resets the cycle count for the model to zero.
    #' @param populations A named vector of populations for
    #' the start of the state. The names should be the state names. 
    #' Due to R's implementation of matrix algebra, \code{populations} 
    #' must be a numeric type and is not restricted to being an integer.
    #' @return Updated MarkovModel object.
    set_populations = function(populations) {
      # check that prevalence is valid
      if (length(populations) != length(private$states)) {
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
      # re-order the population vector to match the transition matrix
      colnames <- private$Ip$dimnames()[["from"]]
      private$populations <- populations[order(match(names(populations), colnames))]
      # reset the cycle number (assumed restart if new population)
      private$icycle <- 0
      # return updated object
      return(invisible(self))
    },

    #' @description
    #' Applies one cycle of the model, starting at zero.
    #' @return Calculated values, per state.
    cycle = function() {
      # check that populations have been set
      if (length(private$populations)==0) {
        rlang::abort("State populations must be initialized",
                     "missing_state_populations")
      }
      # Apply the transition probabilities to the population
      if (private$icycle > 0) {
        pop.end <- private$populations %*% private$Ip$value()
        private$populations <- drop(pop.end)
      }
      # calculate entry costs
      state.costs <- sapply(private$states, function(x) {return(x$getEntryCost())})
      entry.costs <- private$populations*state.costs
      # calculate annual costs of state occupancy
      if (private$icycle > 0) {
        state.costs <- sapply(private$states, function(x) {return(x$getAnnualCost())})
      }
      else {
        state.costs <- sapply(private$states, function(x) {return(0)})
      }
      state.costs <- state.costs / private$nCyclesPerYear
      occupancy.costs <- private$populations*state.costs
      # apply discounting
      y <- (private$icycle)/(private$nCyclesPerYear)
      denom <- (1+private$discount)^y
      entry.costs <- entry.costs / (1+private$discount)^y
      occupancy.costs <- occupancy.costs / (1+private$discount)^y
      # return calculated values, per state
      RC <- data.frame(
        Cycle = rep(private$icycle, times=length(private$populations)),
        Population = private$populations,
        Normalized.Cycle.Cost = (occupancy.costs+entry.costs)/sum(private$populations),
        stringsAsFactors = F
      )
      # update cycle number
      private$icycle <- private$icycle + 1
      return(RC)
    },
    
    #' @description
    #' Applies \code{nCycles} cycles of the model. The starting
    #' populations are redistributed through the
    #' transition probabilities and the state occupancy costs are
    #' calculated, using function \code{cycle}. The end populations are
    #' then fed back into the model for a further cycle and the
    #' process is repeated. For each cycle, the state populations and
    #' the aggregated occupancy costs are saved in one row of the
    #' returned dataframe, with the cycle number. If the cycle count
    #' for the model is zero when called, the first cycle evaluated
    #' will be cycle zero, i.e. the distribution of patients to starting
    #' states, which will include calculation of state entry costs.
    #' @param nCycles Number of cycles to run; default is 2.
    #' @return Data frame with cycle results.
    cycles = function(nCycles=2, roundpop=FALSE) {
      # check that populations have been set
      if (length(private$populations)==0) {
        stop("State populations must be initialized")
      }
      # data frame for cycle summaries
      DF <- data.frame(
        Cycle = integer(length=nCycles),
        'Cost' = numeric(length=nCycles)
      )
      statenames <- self$get_statenames()
      POP <- matrix(data=NA, nrow=nCycles, ncol=length(statenames))
      colnames(POP) <- statenames
      DF <- cbind(DF, POP)
      # run the model
      for (i in 1:nCycles) {
        # single cycle
        DF.cycle <- self$cycle()
        # set the cycle number
        DF[i, 'Cycle'] <- min(DF.cycle$Cycle)
        # collect state populations and cycle sums into a single frame
        DF[i, names(private$populations)] <- private$populations
        # add sum of costs for all states 
        DF[i,'Cost'] <- sum(DF.cycle$Normalized.Cycle.Cost)
      }
      # return summary data frame
      return(DF)
    },
    
    #' @description
    #' Returns a character list of state names.
    #' @return List of the names of each state.
    get_statenames = function() {
      statenames <- sapply(private$states, function(x) {return(x$get_name())})
      return(statenames)
    },

    #' @description
    #' Set annual discount rate
    #' @param r Annual discount rate, expressed as a percentage.
    #' @return Updated MarkovModel object.
    setDiscount = function(r) {
      if (!is.numeric(r)) {
        stop("Discount rate must be numeric")
      }
      private$discount <- r/100
    },
  
    #' Sets the annual state transition rates. 
    #' @param Ip A numeric square matrix of dimension equal to the number of
    #' states, with row and column names equal to Markov state names. Transition 
    #' rates are from row state to column state. Exactly one element of each
    #' row should be NA; this element is computed from the others to ensure that
    #' the sum of 'from' probabilities is unity/ 
    #' @return Updated MarkovModel object
    # set_transitions = function(Ip) {
    #   # check that the transition matrix is of the correct dimension
    #   if (nrow(Ip) != length(private$states)) {
    #     stop("Ip must have ", length(private$states), " rows", call.=FALSE)
    #   }
    #   if (ncol(Ip) != length(private$states)) {
    #     stop("Ip must have ", length(private$states), " columns", call.=FALSE)
    #   }
    #   # check that the transition matrix is labelled with state names
    #   statenames <- self$get_statenames()
    #   if (!setequal(statenames, rownames(Ip))) {
    #     stop("Ip must have state names as row names", call.=FALSE)
    #   }
    #   if (!setequal(statenames, colnames(Ip))) {
    #     stop("Ip must have state names as column names", call.=FALSE)
    #   }
    #   # check that there is one NA element per row
    #   nna <- apply(Ip, MARGIN=1, FUN=function(row){
    #     return(sum(is.na(row)))
    #   })
    #   if (!all.equal(nna, rep(1,times=ncol(Ip)))) {
    #     stop("Each row of Ip must have exactly one NA", call.=FALSE)
    #   }
    #   # set Ip
    #   private$Ip <- Ip
    #   # reorder Ip to match state names
    #   private$Ip <- private$Ip[order(match(rownames(private$Ip), statenames)), 
    #                            order(match(colnames(private$Ip), statenames))]
    #   # calculate per-cycle transitions and correct leading diagonals
    #   for (row in length(private$states)) {
    #     sigma <- sum(private$Ip[row,], na.rm=TRUE)
    #     if (sigma > 1) {
    #       stop("Non-missing elements of rows of Ip must sum to <= 1", call.=FALSE)
    #     }
    #     col <- which(row, arr.ind=TRUE)
    #     private$Ip[row,col] <- 1-sigma
    #   }
      #for (s in private$states) {
      #  if (!s$hasCycleLimit()) {
      #    # calculate per-cycle transition rates
      #    private$Ip[s$getName(),] <- 1-exp(log(1-private$Ip[s$getName(),])/private$nCyclesPerYear) 
      #    # adjust leading diagonals to ensure unity transition rates
      #    #offdiag <- colnames(private$Ip)[colnames(private$Ip) != s$getName()]
      #    sigma <- sum(private$Ip[s$getName(), offdiag], na.rm=TRUE)
      #    if (sigma > 1) {
      #      stop("Rows of Ip for normal states must sum to <= 1", call.=FALSE)
      #    }
      #    private$Ip[s$getName(),s$getName()] <- 1-sigma
      #  }
      #  else {
      #    # leave transitions in tunnel states alone, but check them
      #    sigma <- sum(private$Ip[s$getName(),])
      #    if (isFALSE(all.equal(sigma,1))) {
      #      stop('Rows of Ip for tunnel state ', s$getName(), 
      #           ' must sum to 1 (specified as ', sigma, ')')
      #    }
      #  }
      #}
#      return(invisible(self))
#    },
    
    #' @description
    #' Creates a state summary data frame suitable for printing with kable etc.
    #' @return A dataframe with details of all states.
    tabulate_states = function() {
      DF <- data.frame(
        Name = sapply(private$states, function(x) {return(x$get_name())}),
        hasCycleLimit = sapply(private$states, function(x) {return(x$hasCycleLimit())}),
        cycleLimit = sapply(private$states, function(x) {return(x$getCycleLimit())}),
        'Entry Cost' = sapply(private$states, function(x) {return(x$getEntryCost())}),
        'Annual Cost' = sapply(private$states, function(x) {return(x$getAnnualCost())})
      )
      return(DF)
    }
    
#    #' @description
#    #' Creates a table of the annual transition probabilities.
#    #' @return A data frame of annual transition probabilities.
#    transitionSummary = function() {
#      return(as.data.frame(private$Ip))
#    }
  )
)