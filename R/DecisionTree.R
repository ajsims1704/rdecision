#' @title \verb{DecisionTree} class
#' 
#' @description 
#' An R6 class to represent a decision tree
#' 
#' @details 
#' A class to represent a decision tree. An object contains a tree of
#' decision nodes, chance nodes and leaf nodes, connected by edges
#' (either actions or reactions) and which satisfies the following
#' conditions:
#' \enumerate{
#' \item{Nodes and edges must form a tree with a single root and
#' there must be a unique path from the root to each node.
#' In graph theory terminology, the directed graph formed by the nodes
#' and edges must be an \dfn{arborescence}.}
#' \item{Each node must inherit from one of \verb{DecisionNode},
#' \code{ChanceNode} or \verb{LeafNode}. Formally the set of vertices
#' must be a disjoint union of sets of decision nodes, chance nodes
#' and leaf nodes.}
#' \item{All and only leaf nodes must have no children.}
#' \item{Each edge must inherit from either \verb{Action} or
#' \code{Reaction}.}
#' \item{All and only edges that have source endpoints joined to 
#' decision nodes must inherit from \verb{Action}.}
#' \item{All and only edges that have source endpoints joined to 
#' chance nodes must inherit from \verb{Reaction}.}
#' \item{The sum of probabilities of each set of reaction edges 
#' with a common source endpoint must be 1.}
#' \item{Each \verb{DecisionNode} must have a label, and the labels of all
#' \code{DecisionNodes} must be unique within the model.}
#' \item{Each \verb{Action} must have a label, and the labels of  
#' \code{Action}s that share a common source endpoint must be unique.}
#' }
#' @references{
#'   Briggs A, Claxton K, Sculpher M. Decision modelling for health economic 
#'   evaluation. Oxford, UK: Oxford University Press; 2006.
#' 
#'   Briggs AH, Weinstein MC, Fenwick EAL, Karnon J, Sculpher MJ, Paltiel AD.
#'   Model Parameter Estimation and Uncertainty: A Report of the 
#'   \acronym{ISPOR-SMDM} Modeling Good Research Practices Task Force-6. 
#'   \emph{Value in Health} 2012;\bold{15}:835–42, 
#'   \doi{10.1016/j.jval.2012.04.014}.
#'
#'   Kamiński B, Jakubczyk M, Szufel P. A framework for sensitivity analysis of
#'   decision trees. \emph{Central European Journal of Operational Research}
#'   2018;\bold{26}:135–59, \doi{10.1007/s10100-017-0479-6}.
#' } 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
DecisionTree <- R6::R6Class(
  classname = "DecisionTree",
  lock_class = TRUE,
  inherit = Arborescence,
  private = list(
  ),
  public = list(
    
    #' @description 
    #' Create a new decision tree. The tree must consist of a set of
    #' nodes and a set of edges which satisfy the conditions given
    #' in the details section of this class.
    #' @param V A list of nodes.
    #' @param E A list of edges.
    #' @return A \verb{DecisionTree} object
    initialize = function(V,E) {
      # initialize the base class(es); checks that {V,E} form an arborescence
      super$initialize(V,E)
      # check the V is a disjoint union {D,C,L} nodes
      D <- which(
        sapply(V, function(v){inherits(v,what="DecisionNode")}),arr.ind=TRUE
      )
      C <- which(
        sapply(V, function(v){inherits(v,what="ChanceNode")}),arr.ind=TRUE
      )
      L <- which(
        sapply(V, function(v){inherits(v,what="LeafNode")}),arr.ind=TRUE
      )
      W <- union(D,union(C,L))
      if (!setequal(seq_along(V),W)) {
        rlang::abort(
          "Each node must be a 'DecisionNode', 'ChanceNode' or 'LeafNode'.",
          class="incorrect_node_type")
      }
      # all and only leaf nodes must have no children
      P <- which(sapply(V,function(v) {self$is_parent(v)}),arr.ind=TRUE)
      if (!setequal(P,union(D,C))) {
        rlang::abort("All and only leaf nodes must have no children",
                     class = "leaf_non-child_sets_unequal")
      }  
      # each edge must inherit from action or reaction
      lv <- sapply(E,function(e){inherits(e, what=c("Action","Reaction"))})
      if (!all(lv)) {
        rlang::abort("Each edge must inherit from Action or Reaction", 
                     class="incorrect_edge_type")
      }
      # DecisionNode labels must be unique and all their Action labels
      # must be unique
      D.lab <- sapply(D,function(d){
        v <- private$V[[d]]
        K <- self$direct_successors(v)
        choices <- sapply(K, function(k) {
          w <- self$walk(list(v,k))
          e <- w[[1]]
          return(e$label())
        })
        if (length(choices) != length(unique(choices))) {
          rlang::abort(
            "Labels of actions with a common source node must be unique",
            class="non_unique_labels"
          )
        }
        return(v$label())
      })
      if (length(D.lab) != length(unique(D.lab))) {
        rlang::abort(
          "Labels of DecisionNodes must be unique", 
          class = "non_unique_labels"
        )
      }
      # return a new DecisionTree object
      return(invisible(self))
    },
    
    #' @description 
    #' Find the decision nodes in the tree.
    #' @param what A character string defining what to return. Must be one
    #' of "node", "label" or "index".
    #' @return A list of \verb{DecisionNode} objects (for what="node"); a list
    #' of character strings (for what="label"); or a list of integer indexes of 
    #' the decision nodes (for what="index").
    decision_nodes = function(what="node") {
      # find the vertex IDs
      id <- which(
        sapply(private$V, function(v){inherits(v,what="DecisionNode")}),
        arr.ind=TRUE
      )
      if (what=="node") {
        rc <- private$V[id]
      } else if (what=="label") {
        rc <- sapply(id, function(iv) {private$V[[iv]]$label()})
      } else if (what=="index") {
        rc <- id
      } else {
        rlang::abort(
          "Argument 'what' must be one of 'node', 'label' or 'index'.",
          class="unknown_what_value"
        )
      }
      return(rc)      
    },

    #' @description 
    #' Find the chance nodes in the tree.
    #' @return A list of \verb{ChanceNode} objects.
    chance_nodes = function() {
      ic <- which(sapply(private$V, function(v){inherits(v,what="ChanceNode")}),
                  arr.ind=TRUE)
      return(private$V[ic])      
    },
    
    #' @description 
    #' Find the leaf nodes in the tree.
    #' @param what One of "node" (returns Node objects), "label" (returns the
    #' leaf node labels) or "index" (returns the vertex index of the leaf
    #' nodes).
    #' @return A list of \verb{LeafNode} objects (for what="node"); a list
    #' of character strings (for what="label"); or a list of integer indexes of 
    #' the decision nodes (for what="index").
    leaf_nodes = function(what="node") {
      il <- which(
        vapply(X=private$V, FUN.VALUE=TRUE, FUN=function(v){
          inherits(v,what="LeafNode")
        }),
        arr.ind = TRUE
      )
      if (what=="node") {
        rc <- private$V[il]
      } else if (what=="label") {
        rc <- sapply(il, function(iv) {private$V[[iv]]$label()})
      } else if (what=="index") {
        rc <- il
      } else {
        rlang::abort(
          "Argument 'what' must be one of 'node', 'label' or 'index'.",
          class="unknown_what_value"
        )
      }
      return(rc)      
    },

    #' @description 
    #' Return the edges that have the specified decision node as their source.
    #' @param d A decision node.
    #' @return A list of \verb{Action} edges.
    actions = function(d) {
      # check argument
      if (missing(d)) {
        rlang::abort(
          "Node 'd' must be defined", 
          class="decision_node_not_defined"
        )
      }
      if (!self$has_vertex(d)) {
        rlang::abort(
          "Node 'd' is not in the Decision Tree", 
          class="not_in_tree"
        )
      }
      if (!inherits(d, what="DecisionNode")) {
        rlang::abort(
          "Node 'd' is not a Decision Node", 
          class="not_decision_node"
        )
      }
      id <- self$vertex_index(d)
      # find the edges with d as their source 
      B <- self$digraph_incidence_matrix()
      ie <- which(B[id,]==-1,arr.ind=TRUE)
      return(private$E[ie])
    },
    
    #' @description 
    #' Find all the model variables of type \verb{ModVar} that have been 
    #' specified as values associated with the nodes and edges of the tree.
    #' @return A list of \verb{ModVar}s.
    modvars = function() {
      # create list
      mv <- list()
      # find the ModVars in Actions and Reactions
      for (e in private$E) {
        if (inherits(e, what=c("Action", "Reaction"))) {
          mv <- c(mv, e$modvars())
        }
      }
      # find the modvars in leaf nodes
      for (v in private$V){
        if (inherits(v, what=c("LeafNode"))) {
          mv <- c(mv, v$modvars())
        }
      }
      # return a unique list
      return(unique(mv))
    },
    
    #' @description 
    #' Tabulate the model variables.
    #' @param expressions A logical that defines whether expression model
    #' variables should be included in the tabulation. 
    #' @return Data frame with one row per model variable, as follows:
    #' \describe{
    #' \item{Description}{As given at initialization.}
    #' \item{Units}{Units of the variable.}
    #' \item{Distribution}{Either the uncertainty distribution, if
    #' it is a regular model variable, or the expression used to create it,
    #' if it is an \verb{ExprModVar}.}
    #' \item{Mean}{Mean; calculated from means of operands if
    #' an expression.}
    #' \item{E}{Expectation; estimated from random sample if expression, 
    #' mean otherwise.}
    #' \item{SD}{Standard deviation; estimated from random sample if
    #' expression, exact value otherwise.}
    #' \item{Q2.5}{p=0.025 quantile; estimated from random sample if
    #' expression, exact value otherwise.}
    #' \item{Q97.5}{p=0.975 quantile; estimated from random sample if
    #' expression, exact value otherwise.}
    #' \item{Est}{TRUE if the quantiles and SD have been estimated by 
    #' random sampling.}
    #' }
    modvar_table = function(expressions=TRUE) {
      # create list of model variables in this decision tree, excluding
      # expressions if not wanted
      mvlist <- self$modvars()
      if (!expressions) {
        mvlist <- mvlist[sapply(mvlist,FUN=function(v){!v$is_expression()})]
      }
      # create a data frame of model variables
      DF <- data.frame(
        Description = sapply(mvlist, FUN=function(x){
          rv <- x$description()
          return(rv)
        }),
        Units = sapply(mvlist, FUN=function(x){
          rv <- x$units()
          return(rv)
        }),
        Distribution = sapply(mvlist, FUN=function(x){
          rv <- x$distribution()
          return(rv)
        }),
        Mean = sapply(mvlist, FUN=function(x){
          rv <- x$mean()
          return(rv)
        }),
        E = sapply(mvlist, FUN=function(x){
          rv <- ifelse(x$is_expression(), x$mu_hat(), x$mean())
          return(rv)
        }),
        SD = sapply(mvlist, FUN=function(x){
          rv <- ifelse(x$is_expression(), x$sigma_hat(), x$SD())
          return(rv)
        }),
        Q2.5 = sapply(mvlist, FUN=function(x){
          rv <- ifelse(
            x$is_expression(), 
            x$q_hat(probs=c(0.025)), 
            x$quantile(probs=c(0.025))
          )
          return(rv)
        }),
        Q97.5 = sapply(mvlist, FUN=function(x){
          rv <- ifelse(
            x$is_expression(), 
            x$q_hat(probs=c(0.975)), 
            x$quantile(probs=c(0.975))
          )
          return(rv)
        }),
        Est = sapply(mvlist, FUN=function(exp){
          rv <- ifelse(exp$is_expression(),TRUE,FALSE)
          return(rv)
        })
      )
      # Return the table
      return(DF)
    },
    
    #' @description Draw the decision tree to the current graphics output. Uses
    #' the algorithm of Walker (1989) to distribute the nodes compactly (see
    #' the \link{Arborescence} class help for details).
    #' @param border If TRUE draw a light grey border around the plot area.
    #' @return No return value.
    draw = function(border=FALSE) {
      # margin widths (in tree space)
      lmargin <- 4
      rmargin <- 2
      tmargin <- 4
      bmargin <- 2
      # node size (in tree space); the radius of a chance node. All node
      # shapes are scaled to have same area (pi*node.size^2).
      node.size <- 0.75
      # fontsize for labels
      fontsize <- 8
      # fraction of edge that slopes after leading parent (range 0,1)
      fs <- 0.20
      # find the aspect ratio (width/height) of the current figure area
      fig.size <- dev.size("cm")
      fig.asp <- fig.size[1]/fig.size[2]
      # find the (x,y) coordinates of nodes using Walker's algorithm
      LevelSeparation <- 1
      XY <- self$postree(
        RootOrientation="EAST", 
        LevelSeparation=LevelSeparation
      )
      # adjust level separation to create a tree with an aspect ratio
      # that approximates the figure aspect ratio
      xmin <- min(XY[,"x"])
      xmax <- max(XY[,"x"])
      ymin <- min(XY[,"y"])
      ymax <- max(XY[,"y"])
      tree.asp <- (xmax-xmin)/(ymax-ymin)
      LevelSeparation <- fig.asp/tree.asp
      # find the (x,y) coordinates of the nodes with adjusted level separation
      XY <- self$postree(
        RootOrientation="EAST", 
        LevelSeparation=LevelSeparation
      )
      # find the extent of the new tree
      xmin <- min(XY[,"x"])
      xmax <- max(XY[,"x"])
      ymin <- min(XY[,"y"])
      ymax <- max(XY[,"y"])
      # width and height of the diagram in tree space
      tw <- (xmax-xmin)+(lmargin+rmargin)
      th <- (ymax-ymin)+(bmargin+tmargin)
      # calculate scale factor
      scale <- max(tw/fig.size[1], th/fig.size[2])
      # find centre of drawing in tree space
      cx <- tw/2 + (xmin-lmargin)
      cy <- th/2 + (ymin-bmargin)
      # centre of the figure space
      cx.f <- fig.size[1]/2
      cy.f <- fig.size[2]/2
      # functions to transform coordinates and distances in tree space 
      # to grid space (cm)
      gx <- function(xtree) {
        xcm <- cx.f + (xtree-cx)/scale
        return(xcm)
      }
      gy <- function(ytree) {
        ycm <- cy.f + (ytree-cy)/scale
        return(ycm)
      }
      gd <- function(dtree) {
        dcm <- dtree/scale
        return(dcm)
      }
      # start new page for drawing
      grid::grid.newpage()
      # viewport rectangle (border)
      if (border) {
        grid::grid.rect(
          x=grid::unit(0.5,"npc"), 
          y=grid::unit(0.5,"npc"), 
          width = grid::unit(1,"npc"), 
          height = grid::unit(1,"npc"),
          gp = grid::gpar(col="lightgray")
        )
      }
      # draw the edges as articulated lines between node centres
      sapply(private$E, FUN=function(e) {
        # find source and target nodes
        n.source <- self$vertex_index(e$source())
        n.target <- self$vertex_index(e$target())
        x.source <- XY$x[XY$n==n.source]
        y.source <- XY$y[XY$n==n.source]
        x.target <- XY$x[XY$n==n.target]
        y.target <- XY$y[XY$n==n.target]
        grid::grid.move.to(
          x = grid::unit(gx(x.source),"cm"),
          y = grid::unit(gy(y.source),"cm")
        )
        x.joint <- (x.target-x.source)*fs + x.source
        y.joint <- y.target
        grid::grid.line.to(
          x = grid::unit(gx(x.joint),"cm"),
          y = grid::unit(gy(y.joint),"cm")
        )
        grid::grid.line.to(
          x = grid::unit(gx(x.target),"cm"),
          y = grid::unit(gy(y.target),"cm")
        )
        # add label
        grid::grid.text(
          label = e$label(),
          x = grid::unit(gx(x.joint),"cm")+grid::unit(0.2,"char"),
          y = grid::unit(gy(y.joint),"cm")+grid::unit(0.4,"char"),
          just = c("left", "bottom"),
          gp = grid::gpar(fontsize=fontsize)
        )
      })
      # draw the nodes
      sapply(private$V, function(v) {
        # find the node from its index
        i <- which(XY$n==self$vertex_index(v))
        # switch type
        if (inherits(v, what="DecisionNode")) {
          a <- sqrt(pi/4)*node.size
          grid::grid.rect(
            x = gx(XY[i,"x"]),
            y = gy(XY[i,"y"]),
            width = gd(a*2),
            height = gd(a*2),
            default.units = "cm",
            gp = grid::gpar(col="black", fill="lightgray")
          )
          # add label
          grid::grid.text(
            label = v$label(),
            x = grid::unit(gx(XY[i,"x"]),"cm"),
            y = grid::unit(gy(XY[i,"y"]+a),"cm")+grid::unit(0.4,"char"),
            just = c("right", "bottom"),
            gp = grid::gpar(fontsize=fontsize)
          )
        } else if (inherits(v, what="ChanceNode")) {
          a <- node.size
          grid::grid.circle(
            x = gx(XY[i,"x"]),
            y = gy(XY[i,"y"]),
            r = gd(a),
            default.units = "cm",
            gp = grid::gpar(col="black", fill="lightgray")
          )
          # add label
          grid::grid.text(
            label = v$label(),
            x = grid::unit(gx(XY[i,"x"]),"cm"),
            y = grid::unit(gy(XY[i,"y"]+a),"cm")+grid::unit(0.4,"char"),
            just = c("right", "bottom"),
            gp = grid::gpar(fontsize=fontsize)
          )
        } else if (inherits(v, what="LeafNode")) {
          a <- 2*sqrt(pi/sqrt(3))*node.size
          grid::grid.polygon(
            x = c(
              gx(XY[i,"x"]-a/sqrt(3)),
              gx(XY[i,"x"]+sqrt(3)*a/6),
              gx(XY[i,"x"]+sqrt(3)*a/6)
            ),
            y = c(
              gy(XY[i,"y"]+0),
              gy(XY[i,"y"]+a/2),
              gy(XY[i,"y"]-a/2)
            ),
            default.units = "cm",
            gp = grid::gpar(fill="lightgray", col="black")
          )
          # add label
          grid::grid.text(
            label = v$label(),
            x = grid::unit(gx(XY[i,"x"]),"cm"),
            y = grid::unit(gy(XY[i,"y"]+a/3),"cm")+grid::unit(0.4,"char"),
            just = c("right", "bottom"),
            gp = grid::gpar(fontsize=fontsize)
          )
        }
      })
      # return updated DecisionTree (unchanged)
      return(invisible(self))
    },
    
    #' @description Tests whether a strategy (a unanimous prescription of an
    #' action in each decision node, specified as a list of nodes) is a valid
    #' strategy for this decision tree.
    #' @param strategy A list of Action edges.
    #' @return TRUE if the strategy is valid for this tree. Returns
    #' FALSE if the list of Action edges are not a valid strategy.
    is_strategy = function(strategy) {
      # check that the argument is a list of Action edges
      isA <- vapply(X=strategy, FUN.VALUE=TRUE, FUN=function(e) {
        inherits(e,what="Action")
      })
      if (!all(isA)) {
        return(FALSE)
      }
      # there must be as many Actions as Decision nodes
      iD <- self$decision_nodes("index")
      if (length(strategy) != length(iD)) {
        return(FALSE)
      }
      # the set of source nodes must be the same as the set of decision nodes
      iS <- vapply(X=strategy, FUN.VALUE=1, FUN=function(e){
        i <- self$vertex_index(e$source())
        return(self$vertex_index(e$source()))
      })
      if (!setequal(iS,iD)) {
        return(FALSE)
      }
      # return validity
      return(TRUE)
    },

    #' @description Find all potential strategies for the decision tree. A
    #' strategy is a unanimous prescription of the actions at each decision 
    #' node. If there are decision nodes that are descendants of other nodes
    #' in the tree, the strategies returned will not necessarily be unique.
    #' @param what A character string defining what to return. Must be one
    #' of "label" or "index".
    #' @param select A single strategy (given as a list of action edges, with
    #' one action edge per decision node). If provided, only that strategy
    #' is selected from the returned table. Intended for tabulating a
    #' single strategy into a readable form.
    #' @return A data frame where each row is a potential strategy 
    #' and each column is a Decision Node. Values are either the index of each
    #' action edge, or their label. The row names are the edge labels of each
    #' strategy, concatenated with underscores.
    strategy_table = function(what="index", select=NULL) {
      # check arguments
      if ((what != "index") & (what != "label")) {
        rlang::abort("Argument 'what' must be one of 'index' or 'label'",
                     class="unknown_what_value")
      }
      if (!is.null(select)) {
        if (!self$is_strategy(select)) {
          rlang::abort(
            "'s' must be a valid strategy for this decision tree",
            class = "invalid_strategy"
          )
        }
      }
      # build a table with indexes of the action edges
      f <- list()
      for (d in self$decision_nodes("node")) {
        a <- vapply(X=self$actions(d), FUN.VALUE=1, FUN=function(a){
          self$edge_index(a)
        })
        f[[d$label()]] <- a
      }
      TTI <- expand.grid(f, KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE)
      # select a single strategy, if required
      if (!is.null(select)) {
        lv <- apply(X=TTI, MARGIN=1, FUN=function(row) {
          # indexes of action edges from the table
          st <- row
          # indexes of action edges in 'select' argument
          ss <- vapply(X=select, FUN.VALUE=1, FUN=function(e){
            self$edge_index(e)
          })
          # test whether the table row is the same as 'select'
          return(setequal(st,ss))
        })
        TTI <- TTI[lv,,drop=FALSE]
      }
      # build a table with edge labels in place of edge indexes
      TTL <- TTI
      dn <- self$decision_nodes("label")
      for (d in dn) {
        TTL[,d] <- vapply(X=TTI[,d], FUN.VALUE="x", FUN=function(ie){
          e <- private$E[[ie]]$label()
          return(e)
        })
      }
      # build row names
      rn <- apply(TTL, 1, paste, collapse="_")
      rownames(TTI) <- rn
      rownames(TTL) <- rn
      # return object
      TT <- TTI
      if (what == "label") {
        TT = TTL
      }
      # return the table
      return(TT)
    },

    #' @description Find all paths walked in each possible strategy. A strategy 
    #' is a unanimous prescription of an action in each decision node. Some 
    #' paths can be walked in more than one strategy, if there exist paths
    #' that do not pass a decision node.
    #' @return A data frame, where each row is a path walked in a strategy. The
    #' structure is similar to that returned by \code{strategy_table} but 
    #' includes an extra column, 'Leaf' which gives the leaf node index of each
    #' path, and there is one row for each path in each strategy.
    strategy_paths = function() {
      # find possible strategies 
      S <- self$strategy_table()
      # find the set of action edges in the tree
      eA <- which(
        vapply(X=private$E, FUN.VALUE=TRUE, FUN=function(e){
          return(inherits(e,what="Action"))
        }),
        arr.ind=TRUE
      )
      # iterate all paths and create data frame with action nodes visited and
      # terminating leaf node
      dn <- self$decision_nodes("label")
      P <- do.call("rbind", lapply(self$root_to_leaf_paths(), function(p){
        # find the set of action edges in this walk
        eW <- vapply(X = self$walk(p), FUN.VALUE=1, FUN=function(e){
          return(self$edge_index(e))
        })
        eW <- intersect(eA,eW)
        # iterate the strategies and find those consistent with the path
        lv <- apply(S, MARGIN=1, FUN=function(row){
          # the set of action edges which define the strategy
          eS <- row
          eS <- setdiff(eW,eS)
          return(length(eS)==0)
        })        
        # Append to the strategy data frame
        SL <- cbind(
          S[lv,,drop=FALSE], 
          Leaf=self$vertex_index(p[[length(p)]])
        )
        return(SL)
      }))
      # return the paths
      return(P)
    },

    #' @description 
    #' Evaluate the components of pay-off associated with a set of walks in the
    #' decision tree. For each walk, probability, cost, benefit and utility are
    #' calculated. 
    #' @details There is minimal checking of the argument because this function 
    #' is intended to be called repeatedly during tree evaluation, including
    #' PSA. The argument P is expected to be obtained from 
    #' \code{root_to_leaf_paths}.
    #' @param W A list of root-to-leaf walks. Each walk must start with the
    #' root node and end with a leaf node. Normally this is all the root to leaf
    #' paths in a tree.
    #' @return A matrix (pay-off table) with one row per path and columns
    #' organized as follows:
    #' \describe{
    #' \item{Leaf}{The unique identifier of the path, taken to be the index 
    #' of the terminal (leaf) node.}
    #' \item{Probability}{The probability of traversing the pathway. }
    #' \item{Path.Cost}{The cost of traversing the pathway.}
    #' \item{Path.Benefit}{The benefit derived from traversing the pathway.}
    #' \item{Path.Utility}{The utility associated with the outcome (leaf node).}
    #' \item{Path.QALY}{The QALYs associated with the outcome (leaf node).}
    #' \item{Cost}{Path.Cost \eqn{*} probability of traversing the pathway.}
    #' \item{Benefit}{Path.Benefit \eqn{*} probability of traversing the 
    #' pathway.}
    #' \item{Utility}{Path.Utility \eqn{*} probability of traversing the
    #' pathway.}
    #' \item{QALY}{Path.QALY \eqn{*} probability of traversing the
    #' pathway.}
    #' }
    evaluate_walks = function(W) {
      # create a return matrix
      PAYOFF <- matrix(
        data = NA,
        nrow = length(W),
        ncol = 10,
        dimnames = list(
          NULL, 
          c("Leaf", "Probability", "Path.Cost", "Path.Benefit", "Path.Utility",
            "Path.QALY", "Cost", "Benefit", "Utility", "QALY")
        )
      )
      # evaluate each path
      for (i in 1:length(W)) {
        # get walk
        walk <- W[[i]]
        # terminal node
        last_step <- walk[[length(walk)]] 
        leaf <- last_step$target()
        # check
        if (!inherits(leaf, what="LeafNode")) {
          rlang::abort("Walk must end on a LeafNode", class = "not_to_leaf")
        }
        # set the ID as the index of the leaf node
        PAYOFF[i,"Leaf"] <- self$vertex_index(leaf)
        # walk the path and accumulate p, cost and benefit
        pr <- 1
        cost <- 0
        benefit <- 0
        for (e in walk) {
          # probability 
          pr <- pr * e$p()
          # cost
          cost <- cost + e$cost()
          # benefit
          benefit <- benefit + e$benefit()
        }
        PAYOFF[i,"Probability"] <- pr
        PAYOFF[i,"Path.Cost"] <- cost
        PAYOFF[i,"Path.Benefit"] <- benefit
        # utility of the leaf node at end of the path
        PAYOFF[i, "Path.Utility"] <- leaf$utility()
        PAYOFF[i, "Path.QALY"] <- leaf$QALY()
      }
      # add expected cost and utility     
      PAYOFF[,"Cost"] <- PAYOFF[,"Probability"]*PAYOFF[,"Path.Cost"]
      PAYOFF[,"Benefit"] <- PAYOFF[,"Probability"]*PAYOFF[,"Path.Benefit"]
      PAYOFF[,"Utility"] <- PAYOFF[,"Probability"]*PAYOFF[,"Path.Utility"]
      PAYOFF[,"QALY"] <- PAYOFF[,"Probability"]*PAYOFF[,"Path.QALY"]
      # return the payoff table      
      return(PAYOFF)
    },

    #' @description 
    #' Evaluate each strategy. Starting with the root, the function
    #' works though all possible paths to leaf nodes and computes the 
    #' probability, cost, benefit and utility of each, then aggregates 
    #' by strategy.   
    #' @param setvars One of "expected" (evaluate with each model variable at
    #' its mean value), "random" (sample each variable from its uncertainty 
    #' distribution and evaluate the model), "q2.5", "q50", "q97.5" (set each
    #' model variable to its 2.5\%, 50\% or 97.5\% quantile, respectively, and
    #' evaluate the model), "current" (leave each model variable at its current
    #' value prior to calling the function and evaluate the model).
    #' @param N Number of replicates. Intended for use with PSA 
    #' (\code{modvars="random"}); use with \code{modvars="expected"}
    #' will be repetitive and uninformative. 
    #' @param by One of {"path", "strategy", "run"}. If "path", the table has
    #' one row per path walked per strategy, per run, and includes the label of
    #' the terminating leaf node to identify each path. If "strategy" (the 
    #' default), the table is aggregated by strategy, i.e. there is one row per
    #' strategy per run. If "run", the table has one row per run and uses
    #' concatenated strategy names and one (cost, benefit, utility, QALY) column
    #' for each strategy. 
    #' @return A data frame whose columns depend on \code{by}; see "Details".
    #' @details 
    #' For \code{by="path"} the columns of the data frame are: 
    #' \tabular{lllllllll}{
    #' Leaf \tab The label of terminating leaf node \cr
    #' <label of 1st decision node> \tab label of action leaving the node \cr
    #' <label of 2nd decision node (etc.)> \tab label of action \cr
    #' Probability \tab Probability of traversing the path \cr
    #' Cost \tab Cost of traversing the path \cr
    #' Benefit \tab Benefit of traversing the path \cr
    #' Utility \tab Utility of traversing the path \cr
    #' QALY \tab QALY of traversing the path \cr
    #' Run \tab Run number \cr
    #' }
    #' For \code{by="strategy"} the columns are:
    #' \tabular{llllllll}{
    #' <label of first decision node> \tab label of action leaving the node \cr
    #' <label of second decision node (etc) \tab label of action \cr
    #' Run \tab Run number \cr
    #' Probability \tab \eqn{\Sigma p_i} for the run (1) \cr
    #' Cost \tab Aggregate cost of the strategy \cr
    #' Benefit \tab Aggregate benefit of the strategy \cr
    #' Utility \tab Aggregate utility of the strategy \cr
    #' QALY \tab Aggregate QALY of the strategy \cr
    #' }
    #' For \code{by="run"} the columns are:
    #' \tabular{llllll}{
    #' Run \tab Run number \cr
    #' Probability.<S> \tab Probability for strategy S \cr
    #' Cost.<S> \tab Cost for strategy S\cr
    #' Benefit.<S> \tab Benefit for strategy S\cr
    #' Utility.<S> \tab Benefit for strategy S\cr
    #' QALY.<S> \tab QALY for strategy S\cr
    #' }
    #' where <S> is string composed of the action labels in strategy S
    #' concatenated with an underscore and there will be one probability etc.,
    #' column for each strategy.
   evaluate = function(setvars="expected", N=1, by="strategy") {
      # check arguments
      if (!is.character(setvars)) {
        rlang::abort(
          "'setvars' must be a character", 
          class="setvars_not_character"
        )
      }
      valids <- c("expected","random","q2.5","q50","q97.5","current")
      if (!(setvars %in% valids)) {
        rlang::abort(
          paste(
            "'setvars' must be one of", 
            paste(valids, collapse=" "), 
            collapse=" "
          ), 
          class="setvars_invalid"
        )
      }
      if (!is.numeric(N)) {
        rlang::abort("'N' must be numeric", class="N_not_numeric")
      }
      if (!is.character(by)) {
        rlang::abort("'by' must be character", class="by_not_character")
      }
      if (!(by %in% c("path", "strategy", "run"))) {
        rlang::abort(
          "'by' must be one of {path|strategy|run}.",
          class = "by_invalid"
        )
      }
      # find the root-to-leaf paths
      P <- self$root_to_leaf_paths()
      # find the walk for each root-to-leaf path
      W <- lapply(P, FUN=function(p){self$walk(p)})
      # create template matrix for vapply
      TM <- cbind(self$evaluate_walks(W),Run=rep(NA,length(W)))
      # create list of modvars
      MV <- self$modvars()
      # N tree evaluations, stacked as a 3d array
      RES <- vapply(X=1:N, FUN.VALUE=TM, FUN=function(n){
        # set the ModVar values (to chosen option)
        for (v in MV) {
          v$set(setvars)
        }
        # evaluate the tree
        M <- self$evaluate_walks(W)
        M <- cbind(M,"Run"=rep(n,length(W)))
        # return matrix to be appended
        return(M)
      })
      # reform 3d array into concatenated 2d form
      RES <- do.call("rbind", lapply(1:N,function(n){RES[,,n]}))
      # find all paths walked for each strategy
      SW <- self$strategy_paths()
      # merge with results to get one row per path walked per strategy
      RES <- merge(SW, RES, all.x=TRUE, by="Leaf")
      # Replace the edge indexes of the strategy with their edge labels
      dn <- self$decision_nodes("label")
      for (d in dn) {
        RES[,d] <- vapply(X=RES[,d], FUN.VALUE="x", FUN=function(ie){
          e <- private$E[[ie]]$label()
          return(e)
        })
      }
      # exclude unnecessary columns
      RES$Path.Cost <- NULL
      RES$Path.Utility <- NULL
      RES$Path.QALY <- NULL
      RES$Path.Benefit <- NULL
      # modify output as required
      if (by == "path") {
        # replace Leaf node indexes by their label
        RES$Leaf <- vapply(X=RES$Leaf, FUN.VALUE="x", FUN=function(iv){
          l <- private$V[[iv]]$label()
          return(l)
        })
        PAYOFF <- RES
      } else if (by == "strategy") {
        # aggregate by strategy
        dn <- self$decision_nodes("label")
        f <- as.formula(
          paste(
            "cbind(Probability, Cost, Benefit, Utility, QALY)",
            paste(paste(dn, collapse="+"), "Run", sep = "+"),
            sep = "~"
          ) 
        )
        PAYOFF <- aggregate(f, data=RES, FUN=sum)
      } else if (by == "run") {
        # aggregate by strategy
        dn <- self$decision_nodes("label")
        f <- as.formula(
          paste(
            "cbind(Probability, Cost, Benefit, Utility, QALY)",
            paste(paste(dn, collapse="+"), "Run", sep = "+"),
            sep = "~"
          ) 
        )
        STR <- aggregate(f, data=RES, FUN=sum)
        # reshape to wide format
        if (length(dn)==1) {
          STR$Strategy <- STR[,dn[[1]]]
        } else {
          STR$Strategy <- apply(STR[,dn], MARGIN=1, FUN=paste, collapse="_")
        }
        STR[,dn] <- NULL
        PAYOFF <- reshape(
          STR, idvar="Run", timevar="Strategy", direction="wide"
        )
      }
      # return the data frame     
      return(PAYOFF)
    },

    #' @description
    #' Create a "tornado" diagram to compare two strategies for traversing
    #' the decision tree. A strategy is a unanimous prescription of the actions
    #' at each decision node.
    #' @param index The index strategy (option) to be evaluated.
    #' @param ref The reference strategy (option) with which the index strategy
    #' will be compared.
    #' @param outcome One of \verb{"cost"} or \verb{"ICER"}. For \verb{"cost"}
    #' (e.g. in cost
    #' consequence analysis), the x axis is cost saved (cost of reference minus
    #' cost of index), on the presumption that the new technology will be cost
    #' saving at the point estimate. For \verb{"ICER"} the x axis is
    #' \eqn{\Delta C/\Delta E} and is expected to be positive at the point 
    #' estimate (i.e. in the NE or SW quadrants of the cost-effectiveness 
    #' plane), where \eqn{\Delta C} is cost of index minus cost of reference, 
    #' and \eqn{\Delta E} is utility of index minus utility of reference.
    #' @param exclude A list of descriptions of model variables to be excluded
    #' from the tornado.
    #' @param draw TRUE if the graph is to be drawn; otherwise return the
    #' data frame silently.
    #' @return A data frame with one row per input model variable and columns
    #' for: minimum value of the variable, maximum value of the variable,
    #' minimum value of the outcome and maximum value of the outcome. NULL
    #' if there are no \verb{ModVar}s.
    #' @details The extreme values of each input variable are the upper and 
    #' lower 95\% confidence limits of the uncertainty distributions of each 
    #' variable. This ensures that the range of each input is defensible 
    #' (Briggs 2012).
    tornado = function(index, ref, outcome="cost", exclude=NULL, draw=TRUE) {
      # find all input modvars, excluding expressions and those stated
      mvlist <- self$modvars()
      lv <- vapply(X=mvlist, FUN.VALUE=TRUE, FUN=function(v) {
        return(!v$is_expression())
      })
      mvlist <- mvlist[lv]
      if (length(mvlist)==0) {
        return(NULL)
      }
      # check the parameters
      if (missing(index) || missing(ref)) {
        rlang::abort(
          "'index' and 'ref' must be defined",
          class = "missing_strategy"
        )
      }
      if (!self$is_strategy(index) || !self$is_strategy(ref)) {
        rlang::abort(
          "'index' and 'ref' must be valid strategies for the decision tree",
          class = "invalid_strategy"
        )
      }
      if (!(outcome %in% c("cost", "ICER"))) {
        rlang::abort(
          "'outcome' must be one of {cost|ICER}",
          class = "invalid_outcome"
        )
      }
      if (!is.null(exclude)) {
        if (!is.list(exclude)) {
          rlang::abort(
            "'exclude must be a list of model variable descriptions",
            class = "exclude_not_list"
          )
        }
        mvd <- vapply(mvlist, FUN.VALUE="x", FUN=function(v){v$description()})
        isc <- vapply(X=exclude, FUN.VALUE=TRUE, FUN=function(d) {
          rv <- (is.character(d) && (d %in% mvd))
          return(rv)
        })
        if (!all(isc)) {
          rlang::abort(
            "'exclude' must be a character list of model variable descriptions",
            class = "exclude_element_not_modvar"
          )
        }
      }
      if (!is.logical(draw)) {
        rlang::abort(
          "'draw' must be boolean",
          class = "invalid_draw"
        )
      }
      if (!is.null(exclude)) {
        lv <- vapply(X=mvlist, FUN.VALUE=TRUE, FUN=function(v) {
          return(v$description() %in% exclude)
        })
        mvlist <- mvlist[!lv]
      }
      # create data frame with limits of CIs
      TO <- data.frame(
        Description = vapply(
          X = mvlist,
          FUN.VALUE = "x",
          FUN=function(v){v$description()
        }),
        Units = vapply(
          X = mvlist,
          FUN.VALUE = "x",
          FUN=function(v){v$units()
          }),
        LL = vapply(
          X = mvlist,
          FUN.VALUE = 1.5,
          FUN = function(v){v$quantile(c(0.025))}
        ),
        UL = vapply(
          X = mvlist,
          FUN.VALUE = 1.5,
          FUN = function(v){v$quantile(c(0.975))}
        ),
        stringsAsFactors = FALSE
      )
      # find all modvars in the model, not only those in the tornado
      all.mv <- self$modvars()
      # build strategy tables for index and ref
      st.index <- self$strategy_table("label", select=index)
      st.ref <- self$strategy_table("label", select=ref)
      # find univariate outcome limits
      template <- vector(mode="numeric", length=3)
      names(template) <- c("outcome.min","outcome.mean","outcome.max")
      O <- vapply(X=mvlist, FUN.VALUE=template, FUN=function(this.mv) {
        # result template
        res <- template
        # set all modvars to their mean (including those excluded)
        for (v in all.mv) {
          v$set("expected")
        }
        # function to evaluate outcome for levels of this mv
        ce <- function(what) {
          # set this modvar to its minimum
          this.mv$set(what)
          # evaluate the tree
          RES <- self$evaluate(setvars="current")
          # get outcome for index strategy
          ORES <- merge(st.index, RES, all.x=TRUE)
          index.cost <- ORES$Cost[1]
          index.utility <- ORES$Utility[1]
          index.QALY <- ORES$QALY[1]
          # get outcome for reference strategy
          ORES <- merge(st.ref, RES, all.x=TRUE)
          ref.cost <- ORES$Cost[1]
          ref.utility <- ORES$Utility[1]
          ref.QALY <- ORES$QALY[1]
          # outcome
          if (outcome == "cost") {
            rv <- ref.cost - index.cost
          } else {
            rv <- (index.cost-ref.cost)/(index.QALY-ref.QALY)
          } 
          return(rv)          
        }
        res["outcome.min"] <- ce("q2.5")
        res["outcome.mean"] <- ce("expected")
        res["outcome.max"] <- ce("q97.5")
        # return row of results
        return(res)
      })
      # transpose the outcome
      O <- t(O)
      # append to the data frame
      TO <- cbind(TO,O)
      
      # re-order it with least variation first
      TO$range <- abs(TO$outcome.max - TO$outcome.min)
      TO <- TO[order(TO$range, decreasing=FALSE),]
      TO$range <- NULL
      
      # plot the graph, if required
      if (draw) {
        # save current graphical parameters and ensure they are restored on exit
        oldpar <- par(no.readonly = TRUE)
        on.exit(par(oldpar))
        # controllable parameters
        cex = 0.75
        # x axis label
        xlab <- ifelse(outcome=="cost", "Mean cost saving", "Mean ICER")
        # make labels (description + units)
        TO$Label <- paste(TO$Description, TO$Units, sep=", ")
        # set up the outer margins; bar labels are in the outer margin,
        dw <- vapply(X=TO$Label, FUN.VAL=0.5, FUN=function(s){
          return(strwidth(paste0(s,"MM"), cex=cex, units="inches"))
        })
        par(omi=c(0,max(dw),0,0))
        # create sufficient space in the inner margin to write the variable
        # limits to the left and right of each bar
        lw <- vapply(X=TO$LL, FUN.VAL=0.5, FUN=function(v){
          return(strwidth(signif(v,3), cex=cex, units="inches"))
        })
        uw <- vapply(X=TO$UL, FUN.VAL=0.5, FUN=function(v){
          return(strwidth(signif(v,3), cex=cex, units="inches"))
        })
        par(mar = c(4.1,4.1,1.1,1.1))
        mai = par("mai")
        par(mai = c(mai[1],max(max(lw),max(uw)),mai[3],max(max(lw),max(uw))))
        mgp <- par("mgp")
        mgp <- mgp*cex
        # create the plot frame
        plot(
          x = NULL,
          y = NULL,
          xlim = c(
            min(min(TO$outcome.min),min(TO$outcome.max)),
            max(max(TO$outcome.min),max(TO$outcome.max))
          ),
          ylim = c(0.5,nrow(TO)+0.5),
          xlab = xlab,
          ylab = "",
          yaxt = "n",
          cex.axis = cex,
          cex.lab = cex,
          cex.main = cex,
          cex.sub = cex,
          mgp = mgp,
          xpd = TRUE,
          frame.plot = FALSE
        )
        # label the y axis
        axis(
          side = 2,
          at = 1:nrow(TO),
          labels = TO$Label,
          lty = 0,
          lwd.ticks = 0,
          las = 2,
          cex.axis = cex,
          mgp = mgp,
          outer = TRUE
        )
        # add bars and limits
        for (i in 1:nrow(TO)) {
          xleft = min(TO[i,"outcome.min"], TO[i,"outcome.max"])
          xright = max(TO[i,"outcome.min"], TO[i,"outcome.max"])
          rect(
            xleft,
            xright,
            ybottom = i - 0.25,
            ytop = i + 0.25,
            border = "black",
            col = "lightgray",
            xpd = TRUE
          )
          LL <- TO[i,"LL"]
          UL <- TO[i,"UL"]
          if (TO[i,"outcome.max"] > TO[i,"outcome.min"]) {
            labels = c(signif(LL,3), signif(UL,3))
          } else {
            labels = c(signif(UL,3), signif(LL,3))
          }
          text(
            x = c(xleft, xright),
            y = c(i, i),
            labels = labels,
            pos = c(2, 4),
            offset = 0.25,
            cex = cex, 
            xpd = TRUE
          )
        }
        # add mean (base case)
        abline(v = TO[1,"outcome.mean"], lty = "dashed")
        # remove label column
        TO$Label <- NULL
        # restore graphics state
        par(oldpar)
      }
      # re-order it with greatest variation first
      TO$range <- abs(TO$outcome.max - TO$outcome.min)
      TO <- TO[order(TO$range, decreasing=TRUE),]
      TO$range <- NULL
      # remove mean column
      TO$outcome.mean <- NULL
      # return tornado data frame
      return(TO)
    },
 
    #' @description Find the threshold value of a model variable at which
    #' either the cost difference or ICER, for an index strategy compared with  
    #' a reference strategy, is zero. 
    #' @param index The index strategy (option) to be evaluated.
    #' @param ref The reference strategy (option) with which the index strategy
    #' will be compared.
    #' @param outcome One of \verb{"cost"} or \verb{"ICER"}. For \verb{"cost"}
    #' (e.g. in cost consequence analysis), the x axis is cost saved (cost of
    #' reference minus cost of index), on the presumption that the new 
    #' technology will be cost
    #' saving at the point estimate. For \verb{"ICER"} the x axis is
    #' \eqn{\Delta C/\Delta E} and is expected to be positive at the point 
    #' estimate (i.e. in the NE or SW quadrants of the cost-effectiveness 
    #' plane), where \eqn{\Delta C} is cost of index minus cost of reference, 
    #' and \eqn{\Delta E} is utility of index minus utility of reference.
    #' @param mvd The description of the model variable for which the threshold
    #' is to be found.
    #' @param a The lower bound of the range to search for the root (numeric).
    #' @param b The lower bound of the range to search for the root (numeric).
    #' @param tol The tolerance to which the threshold should be 
    #' calculated (numeric).
    #' @param nmax Maximum number if iterations allowed to reach convergence.
    #' @returns Value of the model variable of interest at the threshold.
    #' @details Uses a rudimentary bisection method method to find the root
    #' (i.e. threshold value when the outcome is zero). In PSA terms, the 
    #' algorithm finds the value of the specified model variable for which
    #' 50% of runs are cost saving and 50% are cost incurring. 
    threshold = function(index, ref, outcome="cost", mvd, a, b, tol, 
                         nmax=1000) {
      # find all input modvars, excluding expressions
      mvlist <- self$modvars()
      lv <- vapply(X=mvlist, FUN.VALUE=TRUE, FUN=function(v) {
        return(!v$is_expression())
      })
      mvlist <- mvlist[lv]
      # check the parameters
      if (missing(index) || missing(ref)) {
        rlang::abort(
          "'index' and 'ref' must be defined",
          class = "missing_strategy"
        )
      }
      if (!self$is_strategy(index) || !self$is_strategy(ref)) {
        rlang::abort(
          "'index' and 'ref' must be valid strategies for the decision tree",
         class = "invalid_strategy"
        )
      }
      if (!(outcome %in% c("cost", "ICER"))) {
        rlang::abort(
          "'outcome' must be one of {cost|ICER}",
          class = "invalid_outcome"
        )
      }
      dsav <- NULL
      if (is.character(mvd)) {
        lv <- vapply(X=mvlist, FUN.VALUE=TRUE, FUN=function(v) {
          return(identical(v$description(),mvd))
        })
        if (sum(lv) != 1) {
          rlang::abort(
            "'mvd' must identify exactly one variable in the model",
            class = "invalid_mvd"
          )
        }
        dsav <- mvlist[[which(lv)]]
      } else {
        rlang::abort(
          "'mvd' must be a character string",
          class = "invalid_mvd"
        )
      }
      if (!is.numeric(a) || !is.numeric(b) || (b < a)) {
        rlang::abort(
          "'a' and 'b' must be numeric and 'b' > 'a'",
          class = "invalid_brackets"
        )
      }
      if (missing(tol) || !is.numeric(tol) || tol <= 0) {
        rlang::abort("'tol' must be numeric and >0", class = "invalid_tol")
      }
      # set all modvars to their mean
      for (v in mvlist) {
        v$set("expected")
      }
      # get names of index and ref strategies
      ST <- self$strategy_table(select=index)
      index.name <- rownames(ST)[1]
      ST <- self$strategy_table(select=ref)
      ref.name <- rownames(ST)[1]
      # construct a function to calculate the outcome at value x of the 
      # variable for which the threshold is needed
      f <- function(x) {
        # set the variable under investigation to be x
        dsav$set("value",x)
        # evaluate the tree
        RES <- self$evaluate(setvars="current", by="run")
        # get costs and QALYs of index and ref strategies
        ref.cost <- RES[1,paste("Cost", ref.name, sep=".")]
        index.cost <- RES[1,paste("Cost", index.name, sep=".")]
        ref.qaly <- RES[1,paste("QALY", ref.name, sep=".")]
        index.qaly <- RES[1,paste("QALY", index.name, sep=".")]
        # outcome
        if (outcome == "cost") {
          rv <- ref.cost - index.cost
        } else {
          rv <- (index.cost-ref.cost)/(index.qaly-ref.qaly)
        }
        return(rv)
      }
      # return variable
      threshold <- NA
      # check that sign of cost saving is different at the brackets
      if ((f(a)*f(b)) < 0) {
        n <- 0
        while (n < nmax) {
          # new midpoint
          c <- (a + b)/2
          if (((b-a)/2) < tol) {
            break
          }
          n <- n + 1
          if (sign(f(c))==sign(f(a))) {
            a <- c
          } else {
            b <- c
          }
        }
        if (n >= nmax) {
          rlang::abort("Failed to converge", class="convergence_failure")          
        } else {
          threshold <- c  
        }
      } else {
        rlang::abort(
          "[a,b] does not bracket the root", 
          class = "invalid_brackets"
        )
      }     
      # return the threshold
      return(threshold)
    }
    
  )
)


