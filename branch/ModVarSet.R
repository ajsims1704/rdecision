#' @title A collection of model variables
#' @description An R6 class for a collection of variables used in a model.
#' @details A collection class for model variables. In practical modelling it 
#' can be used to store a set of model variables that represent a scenario, or 
#' a set of variables associated with a Markov model or decision tree. It is
#' called a set because its elements are unordered. 
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export 
#' 
ModVarSet <- R6::R6Class(
  classname = "ModVarSet",
  lock_class = TRUE,
  private = list(
    .description = NULL,
    .set = NULL
  ),
  public = list(
    
    #' @description Create an object of type \code{ModVarSet}.
    #' @param description A character string description of the variable set
    #' and its role in the model. This description will be used in a
    #' tabulation of the elements in the set (model variables).
    #' @param mvlist A list of \code{ModVar}s. If it is missing or an empty 
    #' list the set is empty.
    #' @return A new \verb{ModVarSet} object.
    initialize = function(description, mvlist=NULL) {
      # test and set description
      if (!is.character(description)) {
        rlang::abort("Argument 'description' must be a string", 
                     class="description_not_string")
      }
      private$.description <- description
      # test and set the list of model variables
      if (missing(mvlist)) {
        private$.set <- list()
      } else {
        if (!is.list(mvlist)) {
          rlang::abort("mvlist must be a list", class="non-list_modvars")
        }
        sapply(mvlist, FUN=function(v) {
          if (!inherits(v, what="ModVar")) {
            rlang::abort(
              "Each mvlist must be a ModVar", 
              class="non-ModVar_element"
            )
          }
        })
        if (length(unique(mvlist)) != length(mvlist)) {
          rlang::abort(
            "Each mvlist element must be unique", 
            class="repeated_modvars")
        }
        private$.set <- mvlist
      }
      # return new object
      return(invisible(self))
    }  
    
  )
)

    