#' @title \verb{CohortMarkovModel}
#' 
#' @description An R6 class for a Markov model with cohort simulation.
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
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export 
#' 
CohortMarkovModel <- R6::R6Class(
  classname = "CohortMarkovModel",
  lock_class = TRUE,
  inherit = Digraph,
  private = list(
    Ip = NULL
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
    #'    whose hazard rate is NULL;
    #' }
    #' @param V A list of nodes (\code{MarkovState}s).
    #' @param E A list of edges (\code{MarkovTransition}s).
    #' @return A \code{CohortMarkovModel} object.
    initialize = function(V,E) {
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
        
        # replace NAs with values to ensure all rows sum to unity
        for (iv in 1:nrow(Ip)) {
          p.out <- sum(Ip[iv,], na.rm=TRUE)
          if (p.out > 1) {
            rlang::abort(
              paste("P(transition) from state", state.name[iv],"exceeds 1"),
              class = "invalid_transitions"
            )
          }
          Ip[iv,which(is.na(Ip[iv,]))] <- 1 - p.out
        }
        # save the matrix as a class private variable
        private$Ip <- Ip
      }
      return(private$Ip)
    }
    
  )
)
