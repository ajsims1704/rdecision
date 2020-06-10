#' @title 
#' Digraph
#' 
#' @description
#' An R6 class to represent a digraph (a directed graph).
#' 
#' @details 
#' Encapulates, and provides methods for computation and checking of directed
#' graphs (digraphs). Inherits from class Graph.
#' 
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
Digraph <- R6::R6Class(
  classname = "Digraph",
  inherit = Graph,
  private = list(
    A = NULL
  ),
  public = list(
    
    #' @description 
    #' Create a new Digraph object from sets of nodes and edges. 
    #' @param V A list of Nodes.
    #' @param A A list of Arrows.
    #' @return A Digraph object.
    initialize = function(V, A) {
      # check and set arrows
      if (!is.list(A)) {
        rlang::abort("A must be a list", class="non-list_arrows")
      }
      sapply(A, FUN=function(a) {
        if (!inherits(a, what="Arrow")) {
          rlang::abort("Each A must be an Arrow", class="non-Arrow_edge")
        }
      })
      private$A <- A
      # initialize the base Graph class (also checks V)
      super$initialize(V, A)
      # return new Digraph object
      return(invisible(self))
    },

    #' @description 
    #' Compute the adjacency matrix for the digraph. Each cell contains the
    #' number of edges from the row vertex to the column vertex, with the 
    #' convention of self loops being counted once, unless 'binary' is TRUE
    #' when cells are either 0 (not adjacent) or 1 (adjacent).
    #' @param binary If TRUE, the adjacency matrix is logical, each cell is
    #' {0,1}.
    #' @return A square numeric matrix with the number of rows and columns
    #' equal to the order of the graph. The rows and columns are in the
    #' same order as V. If all the nodes have labels the
    #' dimnames of the matrix are the labels of the nodes. 
    adjacency_matrix = function(binary=FALSE) {
      # check argument
      if (!is.logical(binary)) {
        rlang::abort("Argument 'binary' must be 'logical'.", class="non-logical_binary")
      }
      # create matrix
      L <- sapply(private$V,function(v){v$get_label()})
      n <- self$order()
      if (all(nchar(L)>0)) {
        A <- matrix(rep(0,times=n*n), nrow=n, dimnames=list(out.node=L,in.node=L))
      } else {
        A <- matrix(rep(0,times=n*n), nrow=n)
      }
      # populate it
      sapply(private$E, function(e) {
        W <- e$endpoints()
        iv1 <- self$element_index(W[[1]])
        iv2 <- self$element_index(W[[2]])
        A[iv1,iv2] <<- A[iv1,iv2]+1
      })
      # convert to binary, if required
      if (binary) {
        A <- apply(A, MARGIN=c(1,2), FUN=function(c){ifelse(c>1,1,c)})
      }
      return(A)
    },
    
    #' @description
    #' Find the direct successors of a node. 
    #' @return A list of nodes or an empty list if the specified
    #' node has no successors.
    direct_successors = function(v) {
      successors <- list()
      if (self$has_vertex(v)) {
        AA <- self$adjacency_matrix(binary=TRUE)
        iv <- self$element_index(v)
        iw <- which(AA[iv,]>0,arr.ind=TRUE)
        successors <- private$V[iw]
      } else {
        rlang::abort("Argument 'v' is not in graph", class="not_in_graph")
      }
      return(successors)
    },
      
    #' @description
    #' Find the direct predecessors of a node. 
    #' @return A list of nodes or an empty list if the specified
    #' node has no predecessors.
    direct_predecessors = function(v) {
      pred <- list()
      if (self$has_vertex(v)) {
        AA <- self$adjacency_matrix(binary=TRUE)
        iv <- self$element_index(v)
        iw <- which(AA[,iv]>0,arr.ind=TRUE)
        pred <- private$V[iw]
      } else {
        rlang::abort("Argument 'v' is not in graph", class="not_in_graph")
      }
      return(pred)
    },
    
    #' @description 
    #' Establish whether the graph is weakly connected, i.e. whether it is
    #' a connected undirected graph. 
    #' @return TRUE if graph is connected, assuming the edges are undirected.
    weakly_connected = function() {
      
    },

    #' @description 
    #' Non-recursive depth-first search. Starts with a specified node and
    #' finds all the nodes reachable from it.
    #' @return List of reachable nodes, including self.
    DFS = function(v) {
      # check argument
      if (!self$has_vertex(v)) {
        rlang::abort("Argument 'v' is not in graph", class="not_in_graph")
      }
      # List of discovered nodes, and a stack
      D <- list()
      S <- list()
      # S.push(v)
      S <- c(S,v)
      # while S is not empty do
      while (length(S)>0) {
        # v = S.pop()
        v <- S[[length(S)]]
        S[[length(S)]] <- NULL
        # if v is not labelled as discovered then
        if (!any(sapply(D,function(n){return(n$is_same_node(v))}))) {
          # label v as discovered
          D <- c(D,v)
          # for all edges from v to w in G.adjacentEdges(v) do
          for (w in self$direct_successors(v)) {
            # S.push(w)
            S <- c(S,w)
          }
        }
      }
      # return discovered nodes
      return(D)
    }  
        
  ) 
)
