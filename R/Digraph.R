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
    B = NULL     # incidence matrix
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
      # initialize the base Graph class (also checks V)
      super$initialize(V, A)
      # compute and save the incidence matrix
      private$B <- self$incidence_matrix()
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
    #' same order as V. If the nodes have defined and unique labels the
    #' dimnames of the matrix are the labels of the nodes. 
    adjacency_matrix = function(boolean=FALSE) {
      # check argument
      if (!is.logical(boolean)) {
        rlang::abort("Argument 'boolean' must be 'logical'.", class="non-logical_boolean")
      }
      # create matrix
      L <- sapply(private$V,function(v){v$label()})
      n <- self$order()
      if (length(unique(L))==length(L) && all(nchar(L)>0)) {
        A <- matrix(rep(0,times=n*n), nrow=n, dimnames=list(out.node=L,in.node=L))
      } else {
        A <- matrix(rep(0,times=n*n), nrow=n)
      }
      # populate it
      sapply(private$E, function(e) {
        s <- self$element_index(e$source())
        t <- self$element_index(e$target())
        A[s,t] <<- A[s,t]+1
      })
      # convert to logical, if required
      if (boolean) {
        A <- apply(A, MARGIN=c(1,2), FUN=function(c){ifelse(c>=1,TRUE,FALSE)})
      }
      return(A)
    },
    
    #' @description 
    #' Compute the incidence matrix for the graph. Each row is a vertex and
    #' each column is an edge. Edges leaving a vertex have value -1 and edges
    #' entering have value +1. if all vertexes have defined and unique labels and all
    #' edges have defined and unique labels, the dimnames of the matrix are the labels of
    #' the vertexes and edges.
    #' @return The incidence matrix.
    incidence_matrix = function() {
      # create matrix
      LV <- sapply(private$V,function(v){v$label()})
      LE <- sapply(private$E,function(e){e$label()})
      nv <- self$order()
      ne <- self$size()
      if ((length(unique(LV))==length(LV)) && (length(unique(LE))==length(LE)) &&
          all(nchar(LV)>0) && all(nchar(LE)>0)) {
        B <- matrix(rep(0,times=nv*ne), nrow=nv, dimnames=list(vertex=LV,edge=LE))
      } else {
        B <- matrix(rep(0,times=nv*ne), nrow=nv)
      }
      # populate it
      sapply(private$E, function(e) {
        s <- self$element_index(e$source())
        t <- self$element_index(e$target())
        e <- self$element_index(e)
        B[s,e] <<- -1
        B[t,e] <<- 1
      })
      # save a local copy
      private$B <- B
      # return matrix
      return(B)
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
      L <- Stack$new()
      # S is a stack of all vertexes with no incoming edge
      S <- Stack$new()
      for (n in 1:ncol(AA)) {
        if (sum(AA[,n])==0) {
          S$push(n)
        }
      }
      # while S is not empty
      while(S$size()>0) {
        # remove a vertex n from S
        n <- S$pop()
        # add n to tail of L
        L$push(n)
        # for each node m with an edge e from n to m
        for (m in which(AA[n,]>0,arr.ind=TRUE)) {
          # remove edge e from graph
          AA[n,m] <- 0
          # if m has no other incoming edges, insert m into S
          if (sum(AA[,m])==0) {
            S$push(m)
          }
        }
      }
      # return list of nodes indexed by L
      LL <- sapply(L$as_list(), function(l) {private$V[[l]]})
      return(LL)
    },

    #' @description 
    #' Test whether the graph is connected. For digraphs this will
    #' always return FALSE because "connected" is not defined. Function
    #' \code{weakly_connected} calculates whether the underlying
    #' graph is connected.
    #' @return TRUE if connected, FALSE if not.
    is_connected = function() {
      return(FALSE)
    },

    #' @description 
    #' Test whether the digraph is weakly connected, i.e. if the
    #' underlying graph is connected.
    #' @return TRUE if connected, FALSE if not.
    is_weakly_connected = function() {
      connected <- FALSE
      if (self$order()==0) {
        connected <- FALSE
      } else if (self$order()==1) {
        connected <- TRUE
      } else {
        # get the adjacency matrix
        A <- super$adjacency_matrix(boolean=TRUE)
        # D marks nodes as discovered
        D <- vector(mode="logical", length=self$order())
        # S is a stack of nodes being processed
        S <- Stack$new()
        # start with first vertex
        S$push(1)
        # while S is not empty, do
        while (S$size()>0) {
          s <- S$pop()
          # if s is not labelled as discovered then
          if (!D[s]) {
            # label s as discovered
            D[s] <- TRUE
            # for all edges from s to n
            for (n in which(A[s,], arr.ind=TRUE)) {
              S$push(n)
            }
          }
        }
        if (all(D)) {
          connected <- TRUE
        }
      }
      return(connected)
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
      return(self$is_weakly_connected() && super$is_acyclic())
    },
    
    #' @description 
    #' Compute whether the digraph's underlying graph is a tree (connected and
    #' acyclic). Synonymous with 'is_graph'.
    #' @return TRUE if the underlying graph is a tree; FALSE if not.
    is_polytree = function() {
      return(self$is_tree())
    },

    #' @description    
    #' Check whether the digraph is an arborescence (a tree with a
    #' single root and unique paths from the root).
    #' @return TRUE if the digraph is an arborescence; FALSE if not.
    is_arborescence = function() {
      # there must be one and only one root vertex
      u <- which(apply(private$B, MARGIN=1, function(r){!any(r>0)}),arr.ind=TRUE)
      if (length(u) != 1) {
        return(FALSE)
      }
      # there must be exactly one path from the root to all other vertexes
      np <- sapply(setdiff(seq(length(private$V)),u), function(v) {
        P <- self$paths(private$V[[u]], private$V[[v]])
        return(length(P))
      })
      if (any(np != 1)) {
        return(FALSE)
      }
      # otherwise return TRUE
      return(TRUE)
    },

    #' @description
    #' Find the direct successors of a node. 
    #' @param v The index vertex.
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
    #' @param v The index vertex.
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
    #' Find all directed paths from source node 's' to target node 't'. In this
    #' definition, 'path' is a simple path, i.e. all vertexes are unique.
    #' Uses a recursive depth-first search algorithm.
    #' @param s Source node.
    #' @param t Target node.
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
      # D marks nodes as discovered
      D <- vector(length=self$order())
      # P is current path
      P <- Stack$new()
      # PL is list of paths
      PL <- list()
      # recurse
      dfs <- function(v) {
        D[v] <<- TRUE
        P$push(v)
        if (v==t) {
          PL[[length(PL)+1]] <<- P$as_list()
        } else {
          for (w in which(AA[v,],arr.ind=TRUE)) {
            if (!D[w]) {
              dfs(w)
            }
          }
        }
        P$pop()
        D[v] <<- FALSE
      }
      dfs(s)
      # convert vertex indices into vertices before returning
      VPL <- lapply(PL, function(p){
        vp <- lapply(p, function(v) {private$V[[v]]})
      })
      return(VPL)
    },
    
    #' @description 
    #' Construct the sequence of edges which joins the specified
    #' sequence of vertexes in this graph.
    #' @param P A list of Nodes
    #' @return A list of Edges
    walk = function(P) {
      # check vertexes and get index of each
      p <- sapply(P, function(n) {
        # reject if vertex is not in the graph
        if (!self$has_vertex(n)) {
          rlang::abort("At least one Node is not in graph", class="not_in_graph")
        }
        return(self$element_index(n))
      })
      # loop through the ordered vertexes
      if (length(P) > 1) {
        pairs <- data.frame(
          s = p[1:(length(P)-1)],
          t = p[2:length(P)]
        )
        W <- apply(pairs, MARGIN=1, function(r) {
          # look for edges leaving s and entering t
          e <- which((private$B[r[1],]==-1) & (private$B[r[2],]==1), arr.ind=TRUE)
          if (length(e)>=1) {
            return(e[1])
          } else {
            rlang::abort("There must be an edge between adjacent nodes in 'P'",
                         class="missing_edge")
          }
        })
      } else {
        W <- list()
      }
      # convert edge indices into edges before returning
      E <- lapply(W, function(e){private$E[[e]]})
      # return the walk 
      return(E)
    }
  ) 
)

