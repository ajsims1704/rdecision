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
    #' convention of self loops being counted once, unless 'boolean' is TRUE
    #' when cells are either FALSE (not adjacent) or TRUE (adjacent).
    #' @param boolean If TRUE, the adjacency matrix is logical, each cell is
    #' {FALSE,TRUE}.
    #' @return A square numeric matrix with the number of rows and columns
    #' equal to the order of the graph. The rows and columns are in the
    #' same order as V. If all the nodes have labels the
    #' dimnames of the matrix are the labels of the nodes. 
    adjacency_matrix = function(boolean=FALSE) {
      # check argument
      if (!is.logical(boolean)) {
        rlang::abort("Argument 'boolean' must be 'logical'.", class="non-logical_boolean")
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
      # convert to logical, if required
      if (boolean) {
        A <- apply(A, MARGIN=c(1,2), FUN=function(c){ifelse(c>=1,TRUE,FALSE)})
      }
      return(A)
    },
    
    #' @description 
    #' Attempt to topologically sort the vertexes in the directed graph using
    #' Kahn's algorithm (https://doi.org/10.1145%2F368996.369025).
    #' @return A list of vertexes, topologically sorted. If the digraph has
    #' cycles, the returned ordered list will not contain all the vertexes
    #' in the graph, but no error will be raised.
    topological_sort = function() {
      # get the adjacency matrix (note: only vertex indexes are needed)
      AA <- self$adjacency_matrix()
      # L is an empty list that will contain the sorted vertexes
      L <- list()
      # S is a set of all vertexes with no incoming edge
      S <- list()
      for (n in 1:ncol(AA)) {
        if (sum(AA[,n])==0) {
          S <- c(S,n)
        }
      }
      # while S is not empty
      while(length(S)>0) {
        # remove a vertex n from S
        n <- S[[length(S)]]
        S[[length(S)]] <- NULL
        # add n to tail of L
        L <- c(L, n)
        # for each node m with an edge e from n to m
        for (m in which(AA[n,]>0,arr.ind=TRUE)) {
          # remove edge e from graph
          AA[n,m] <- 0
          # if m has no other incoming edges, insert m into S
          if (sum(AA[,m])==0) {
            S <- c(S,m)
          }
        }
      }
      # return list of nodes indexed by L
      LL <- sapply(L, function(l) {
        return(private$V[[l]])
      })
      return(LL)
    },
    
    #' @description 
    #' Checks for the presence of a cycle in the graph by attempting to do 
    #' a topological sort. If the sort does not contain all vertexes, the
    #' digraph contains at least one cycle.
    #' This method overrides 'is_acyclic' in Graph.
    #' @return TRUE if no cycles detected.
    is_acyclic = function() {
      L <- self$topological_sort()
      return(length(L)==length(private$V))
    },    

    #' @description 
    #' Compute whether the digraph's underlying graph is a tree (connected and
    #' acyclic).
    #' @return TRUE if the underlying graph is a tree; FALSE if not.
    is_tree = function() {
      return(super$is_connected() && super$is_acyclic())
    },
    
    #' @description 
    #' Compute whether the digraph's underlying graph is a tree (connected and
    #' acyclic). Synonymous with 'is_graph'.
    #' @return TRUE if the underlying graph is a tree; FALSE if not.
    is_polytree = function() {
      return(self$is_tree())
    },
    
    #' @description
    #' Find the direct successors of a node. 
    #' @return A list of nodes or an empty list if the specified
    #' node has no successors.
    direct_successors = function(v) {
      successors <- list()
      if (self$has_vertex(v)) {
        AA <- self$adjacency_matrix(boolean=TRUE)
        iv <- self$element_index(v)
        iw <- which(AA[iv,],arr.ind=TRUE)
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
        AA <- self$adjacency_matrix(boolean=TRUE)
        iv <- self$element_index(v)
        iw <- which(AA[,iv],arr.ind=TRUE)
        pred <- private$V[iw]
      } else {
        rlang::abort("Argument 'v' is not in graph", class="not_in_graph")
      }
      return(pred)
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
    },
    
    #' @description 
    #' Find all directed paths from source node 's' to target node 't'. In this
    #' definition, 'path' is a simple path, i.e. all vertexes are unique.s
    #' Uses a recursive depth-first search algorithm.
    #' @return A list of ordered node lists. 
    paths = function(s,t) {
      # check arguments
      if (!self$has_vertex(s)) {
        rlang::abort("Argument 's' is not in graph", class="not_in_graph")
      }
      s <- self$element_index(s)
      if (!self$has_vertex(t)) {
        rlang::abort("Argument 't' is not in graph", class="not_in_graph")
      }
      t <- self$element_index(t)
      # AA is the adjacency matrix
      AA <- self$adjacency_matrix(boolean=TRUE)
      # D is list of discovered nodes
      D <- list()
      # P is current path
      P <- list()
      # PL is list of paths
      PL <- list()
      # recurse
      dfs <- function(v) {
        D <<- c(D,v)
        P <<- c(P,v)
        if (v==t) {
          PL[[length(PL)+1]] <<- P
        } else {
          for (w in which(AA[v,],arr.ind=TRUE)) {
            if (!(w %in% D)) {
              dfs(w)
            }
          }
        }
        P[[length(P)]] <<- NULL
        D[[length(D)]] <<- NULL
      }
      dfs(s)
      return(PL)
    }
  ) 
)

# dfs <- function(v) {
#   D <<- c(D,v)
#   P <<- c(P,v)
#   if (identical(v,t)) {
#     PL[[length(PL)+1]] <<- P
#   } else {
#     for (w in self$direct_successors(v)) {
#       if (!any(sapply(D,function(d){identical(d,w)}))) {
#         dfs(w)
#       }
#     }
#   }
#   P[[length(P)]] <<- NULL
#   D[[length(D)]] <<- NULL
# }