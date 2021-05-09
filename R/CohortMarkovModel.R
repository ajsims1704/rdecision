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
    cmm.Ip = NULL
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
    #' @return A \code{CohortMarkovModel} object.
    initialize = function(V,E,tcycle=as.difftime(365.25, units="days")) {
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
      # return a new CohortMarkovModel object
      return(invisible(self))
    },
    
    #' @description Return the per-cycle transition matrix for the model.
    #' @returns A square matrix of size equal to the number of states. If all
    #' states are labelled, the dimnames take the names of the states.
    transition_matrix = function() {
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
      # re-order the population vector to match the transition matrix
      colnames <- private$Ip$dimnames()[["from"]]
      private$populations <- populations[order(match(names(populations), colnames))]
      # reset the cycle number (assumed restart if new population)
      private$icycle <- 0
      # return updated object
      return(invisible(self))
    }
    
    
  )
)
