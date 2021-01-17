#' @title
#' ModVarMap
#' 
#' @description
#' An R6 class for a map of model variables to be included in a
#' health economic model.
#' 
#' @details 
#' A health economic model comprises a structure (decision tree, Markov model)  
#' represented by a set of nodes and a set of edges which link the nodes. Also
#' required is a set of variables which define the costs, utilities and
#' probabilities used in the model. In `rdecision` each variable can be
#' represented by a ModVar object (in fact one of its subclasses, 
#' e.g. GammaModVar), and ModVarMap is a container (map) for ModVar objects,
#' passed into the model with the nodes and edges. Each variable is
#' referenced using a unique key (character string). ModVar elements
#' may be scalar numeric variables.
#'
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export 
#' 
ModVarMap <- R6::R6Class(
  classname = "ModVarMap",
  private = list(
    .description = NULL,
    .map = NULL
  ),
  public = list(
    
    #' @description 
    #' Create an object of type `ModVarMap`
    #' @param description A character string description of the variable
    #' and its role in the model. This description will be used in a
    #' tabulation of the variables linked to a model.
    #' @return A new ModVarMap object.
    initialize = function(description) {
      # test and set description
      # check there is a label
      if (rlang::is_missing(description)) {
        rlang::abort("Argument `description` must not be missing", class="missing_description")
      }
      if (!is.character(description)) {
        rlang::abort("Argument 'description' must be a string", 
                     class="description_not_string")
      }
      private$.description <- description
      # create the internal named list
      private$.map <- list()
      return(invisible(self))
    },
    
    #' @description 
    #' Add a ModVar to the map (i.e. add a variable to the model).
    #' @param k The key to use for the variable (must be unique).
    #' @param v The ModVar (or subclass) to associate with this key.
    #' v may also be a numeric scalar, for non PSA models.
    #' @return An updated object
    add = function(k, v) {
      # check that k is not missing
      if (rlang::is_missing(k)) {
        rlang::abort("Argument `k` must not be missing", class="missing_key")
      }
      # check that k is a character string
      if (!is.character(k)) {
        rlang::abort("Argument 'k' must be a string", 
                     class="key_not_string")
      }
      # check that k is not already in the list
      if (k %in% names(private$.map)) {
        rlang::abort("Key 'k' is already in the list", 
                     class="key_exists")
      }
      # check that v is not missing
      if (rlang::is_missing(v)) {
        rlang::abort("Argument `v` must not be missing", class="missing_value")
      }
      # check that v inherits from ModVar
      if (!inherits(v, what=c("numeric", "ModVar"))){
        rlang::abort("Argument 'v' must be of type 'numeric' or 'ModVar'.",
                     class = "incorrect_type")
      }
      # add v to the map
      private$.map[[k]] <- v
      # return updated map
      return(invisible(self))
    }
  )
)

    