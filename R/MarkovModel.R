#' @title 
#' MarkovModel
#' 
#' @description
#' An R6 class for a Markov model
#' 
#' @details 
#' A class to represent a complete Markov model.
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims5@nhs.net}
#' @export 
#' 
MarkovModel <- R6::R6Class(
  classname = "MarkovModel",
  private = list(
    states = "list",
    Ip = "matrix",
    nCyclesPerYear = "numeric",
    populations = "vector",
    icycle = "integer",
    discount = 0
  ),
  public = list(
    
    #' @description
    #' Creates a Markov model.
    #' @param states a list of objects of type MarkovState 
    #' @param Ip Matrix of \emph{annual} rates of transition between states. The
    #'        dimensions are (nStates,nStates) with each entry being the
    #'        annual rate of transitions from the row state to the
    #'        column state. Values are in the range [0,1]. Transitions
    #'        to self (i.e. leading diagonal) should be set to zero, and will
    #'        be adjusted so that the sum of transitions from each row are
    #'        zero. The matrix should have row names and column names which are the
    #'        state names.
    #' @param discount The annual discount rate (percentage).
    #' @param nCyclesPerYear The number of cycles per year.
    #' @return A MarkovModel object.
    initialize = function(states, Ip, discount=0, nCyclesPerYear=1) {
      # set the cycle number to be zero
      private$icycle <- 0
      # check that all states are of type MarkovState
      sapply(states, function(x) {
        if (inherits(x, what="MarkovState")==F){
          stop("Each element in `states` must be of class `MarkovState`")
        }
      })
      # set the states
      private$states <- states 
      # set the number of cycles per year
      private$nCyclesPerYear <- nCyclesPerYear
      # set the transition probabilities
      self$setTransitions(Ip)
      # set the discount rate
      self$setDiscount(discount)
    },

    #' @description
    #' Applies one cycle of the model, starting at zero.
    #' @return Calculated values, per state.
    cycle = function() {
      # check that populations have been set
      if (length(private$populations)==0) {
        stop("State populations must be initialized")
      }
      # check that temporary states only have cycleLimit=1
      for (s in private$states) {
        if (s$hasCycleLimit() & s$getCycleLimit() != 1) {
          stop("All temporary states in cohort solver must have cycleLimit of 1",
               " (", s$getName(), " has ", s$getCycleLimit(), ')')
        }
      }
      # Apply the transition probabilities to the population
      if (private$icycle > 0) {
        pop.end <- private$populations %*% private$Ip
        private$populations <- pop.end[1,]
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
    cycles = function(nCycles=2) {
      # check that populations have been set
      if (length(private$populations)==0) {
        stop("State populations must be initialized")
      }
      # data frame for cycle summaries
      DF <- data.frame(
        Cycle = integer(length=nCycles),
        'Cost' = numeric(length=nCycles)
      )
      statenames <- self$getStatenames()
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
    getStatenames = function() {
      statenames <- sapply(private$states, function(x) {return(x$getName())})
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

    #' @description
    #' Sets the occupancy of each state. Takes argument
    #' Calling this resets the cycle count for the model to zero.
    #' @param populations A named vector of populations for
    #' the start of the state. The names should be the state names. 
    #' Due to R's implementation of matrix algebra, \code{populations} 
    #' must be a numeric type and is not restricted to being an integer.
    #' @return Updated MarkovModel object.
    setPopulations = function(populations) {
      # check that prevalence is valid
      if (length(populations) != length(private$states)) {
        stop("`populations` must have length ", length(private$states))
      }
      # check the state names are correct
      statenames <- self$getStatenames()
      if (!setequal(statenames, names(populations))) {
        stop("`populations` must be vector named with state names")
      }  
      # check that all populations are of type numeric
      sapply(populations, function(x) {
        if (is.numeric(x)==F){
          stop("Each element in `populations` must be of type numeric")
        }
      })
      # re-order the population to match the transition matrix
      private$populations <- populations[order(match(names(populations), statenames))]
      # reset the cycle number (assumed restart if new population)
      private$icycle <- 0
    },
    
    #' Sets the annual state transition rates. 
    #' @param Ip A numeric square matrix of dimension equal to the number of
    #' states, with row and column names equal to Markov state names. Transition 
    #' rates are from row state to column state.
    #' @return Updated MarkovModel object
    setTransitions = function(Ip) {
      # check that the transition matrix is of the correct dimension
      if (nrow(Ip) != length(private$states)) {
        stop("Ip must have ", length(private$states), " rows")
      }
      if (ncol(Ip) != length(private$states)) {
        stop("Ip must have ", length(private$states), " columns")
      }
      # check that the transition matrix is labelled with state names
      statenames <- self$getStatenames()
      if (!setequal(statenames, rownames(Ip))) {
        stop("Ip must have state names as row names")
      }
      if (!setequal(statenames, colnames(Ip))) {
        stop("Ip must have state names as column names")
      }
      # set Ip
      private$Ip <- Ip
      # reorder Ip to match state names
      private$Ip <- private$Ip[order(match(rownames(private$Ip), statenames)), 
                               order(match(colnames(private$Ip), statenames))]
      # calculate per-cycle transitions and correct leading diagonals
      for (s in private$states) {
        if (!s$hasCycleLimit()) {
          # calculate per-cycle transition rates
          private$Ip[s$getName(),] <- 1-exp(log(1-private$Ip[s$getName(),])/private$nCyclesPerYear) 
          # adjust leading diagonals to ensure unity transition rates
          offdiag <- colnames(private$Ip)[colnames(private$Ip) != s$getName()]
          sigma <- sum(private$Ip[s$getName(), offdiag])
          if (sigma > 1) {
            stop('Rows of Ip for normal states must sum to <= 1')
          }
          private$Ip[s$getName(),s$getName()] <- 1-sigma
        }
        else {
          # leave transitions in tunnel states alone, but check them
          sigma <- sum(private$Ip[s$getName(),])
          if (isFALSE(all.equal(sigma,1))) {
            stop('Rows of Ip for tunnel state ', s$getName(), 
                 ' must sum to 1 (specified as ', sigma, ')')
          }
        }
      }
      return(invisible(self))
    },
    
    #' @description
    #' Creates a state summary data frame suitable for printing with kable etc.
    #' @return A dataframe with details of all states.
    stateSummary = function() {
      DF <- data.frame(
        Name = self$getStatenames(),
        hasCycleLimit = sapply(private$states, function(x) {return(x$hasCycleLimit())}),
        cycleLimit = sapply(private$states, function(x) {return(x$getCycleLimit())}),
        'Entry Cost' = sapply(private$states, function(x) {return(x$getEntryCost())}),
        'Annual Cost' = sapply(private$states, function(x) {return(x$getAnnualCost())})
      )
      return(DF)
    },
    
    #' @description
    #' Creates a table of the annual transition probabilities.
    #' @return A data frame of annual transition probabilities
    transitionSummary = function() {
      return(as.data.frame(private$Ip))
    }
  )
)