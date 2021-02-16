#' @title 
#' DecisionTree
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
#' \item{Each node must inherit from one of \code{DecisionNode},
#' \code{ChanceNode} or \code{LeafNode}. Formally the set of vertices
#' must be a disjoint union of sets of decision nodes, chance nodes
#' and leaf nodes.}
#' \item{All and only leaf nodes must have no children.}
#' \item{Each edge must inherit from either \code{Action} or
#' \code{Reaction}.}
#' \item{All and only edges that have source endpoints joined to 
#' decision nodes must inherit from \code{Action}.}
#' \item{All and only edges that have source endpoints joined to 
#' chance nodes must inherit from \code{Reaction}.}
#' \item{The sum of probabilities of each set of reaction edges 
#' with a common source endpoint must be 1.}
#' \item{Each \code{DecisionNode} must have a label, and the labels of all
#' \code{DecisionNodes} must be unique within the model.}
#' \item{Each \code{Action} must have a label, and the labels of  
#' \code{Action}s that share a common source endpoint must be unique.}
#' }
#' 
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
DecisionTree <- R6::R6Class(
  classname = "DecisionTree",
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
    #' @return A DecisionTree object
    initialize = function(V,E) {
      # initialize the base class(es); checks that {V,E} form an arboresecence
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
      # Action (reaction) edges must emerge from DecisionNodes (ChanceNodes)
      eok <- sapply(E,function(e){
        rc <- TRUE
        if (inherits(e,what="Action")) {
          if (!inherits(e$source(), what="DecisionNode")) {
            rc <- FALSE
          }
        } else {
          if (!inherits(e$source(), what="ChanceNode")) {
            rc <- FALSE
          }
        }
        return(rc)
      })
      if (!all(eok)) {
        rlang::abort(
          "Actions must start at DecisionNodes; Reactions at ChanceNodes",
          class = "incorrect_edge_type"
        )
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
          class="non_unique_labels"
        )
      }
      # return a new DecisionTree object
      return(invisible(self))
    },
    
    #' @description 
    #' Find the decision nodes in the tree.
    #' @param what A character string defining what to return. Must be one
    #' of "node", "label" or "index".
    #' @return A list of \code{DecisionNode} objects (for what="node"); a list
    #' of character strings (for what="label"); or a list of integer indexes of 
    #' the decision nodes (for what="index").
    decision_nodes = function(what="node") {
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
    #' @return A list of \code{ChanceNode} objects.
    chance_nodes = function() {
      ic <- which(sapply(private$V, function(v){inherits(v,what="ChanceNode")}),
                  arr.ind=TRUE)
      return(private$V[ic])      
    },
    
    #' @description 
    #' Find the leaf nodes in the tree.
    #' @return A list of \code{LeafNode} objects.
    leaf_nodes = function() {
      il <- which(sapply(private$V, function(v){inherits(v,what="LeafNode")}),
                  arr.ind=TRUE)
      return(private$V[il])      
    },

    #' @description 
    #' Return the edges that have the specified decision node as their source.
    #' @param d A decision node.
    #' @return A list of Action edges.
    actions = function(d) {
      # check argument
      if (!self$has_element(d)) {
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
      id <- self$element_index(d)
      # find the edges with d as their source 
      B <- self$incidence_matrix()
      ie <- which(B[id,]==-1,arr.ind=TRUE)
      return(private$E[ie])
    },
    
    #' @description 
    #' Find all the model variables of type ModVar that have been specified
    #' as values associated with the nodes and edges of the tree.
    #' @return A list of \code{ModVar}s.
    modvars = function() {
      # create list
      mv <- list()
      # find the ModVars in Actions and Reactions
      sapply(private$E, function(e){
        if (inherits(e, what=c("Action", "Reaction"))) {
          mv <<- c(mv, e$modvars())
        }
      })
      # find the modvars in leaf nodes
      sapply(private$V, function(v){
        if (inherits(v, what=c("LeafNode"))) {
          mv <<- c(mv, v$modvars())
        }
      })
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
    #' if it is an ExprModVar.}
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
      # to grid space (npc)
      gx <- function(xtree) {
        xnpc <- cx.f + (xtree-cx)/scale
        return(xnpc)
      }
      gy <- function(ytree) {
        ynpc <- cy.f + (ytree-cy)/scale
        return(ynpc)
      }
      gd <- function(dtree) {
        dnpc <- dtree/scale
        return(dnpc)
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
        n.source <- self$element_index(e$source())
        n.target <- self$element_index(e$target())
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
        i <- which(XY$n==self$element_index(v))
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

    #' @description Find all the root to leaf paths traversable under 
    #' the specified strategy. A strategy is a unanimous prescription 
    #' of an action in each decision node. 
    #' @param strategy A list of Actions, with one action per decision node.
    #' @return A list of root to leaf paths.
    paths_in_strategy = function(strategy) {
      # get decision nodes
      D <- self$decision_nodes()
      # check argument
      if (length(strategy)!=length(D)) {
        rlang::abort(
          "Argument 'strategy' must have as many elements as DecisionNodes",
           class="incorrect_strategy_length"
        )
      }
      sapply(strategy, function(e) {
        if (!inherits(e,what="Action")) {
          rlang::abort(
            "Argument 'strategy' must only contain Action elements",
            class="incorrect_strategy_type")
        }
      })
      DS <- sapply(strategy, function(e){e$source()})
      iDS <- sapply(DS, function(v){self$element_index(v)})
      iD <- sapply(D, function(v){self$element_index(v)})
      if (!setequal(iD, iDS)) {
        rlang::abort(
          "Argument 'strategy' must have one Action per DecisionNode",
          class = "incorrect_strategy_prescription")
      }
      # all non-strategy action edges are forbidden
      eAction <- which(sapply(private$E, function(e){
        inherits(e,what="Action")}),arr.ind=TRUE)
      eAllowed <- sapply(strategy, function(e){self$element_index(e)})
      eForbidden <- setdiff(eAction, eAllowed)
      # find all root to leaf paths and filter out those traversing 
      # forbidden edges
      P <- self$root_to_leaf_paths()
      bAllowed <- sapply(P, function(p) {
        w <- self$walk(p)
        eWalk <- sapply(w, function(e){self$element_index(e)})
        return(length(intersect(eWalk,eForbidden))==0)
      })
      return(P[bAllowed])
    },

    #' @description Find all unique strategies for the decision tree. A
    #' strategy is a unanimous prescription of the actions at each decision 
    #' node. In trees where there are decision nodes that are descendants
    #' of other decision nodes, not all decision nodes are reachable in 
    #' each strategy. Equivalently, different strategies involve the
    #' traversal of an identical set of paths and are considered non-
    #' unique. Only unique strategies are returned.
    #' @param what A character string defining what to return. Must be one
    #' of "label" or "index".
    #' @return A table (data frame) where each row is a strategy traversed by
    #' a unique set of paths, and each column is a Decision Node. Values are
    #' either the index of each action edge, or their label.
    strategies = function(what="index") {
      if ((what != "index") & (what != "label")) {
        rlang::abort("Argument 'what' must be one of 'index' or 'label'",
                     class="unknown_what_value")
      }
      # names of columns to copy to identify each strategy
      dn <- self$decision_nodes("label")
      # build a table with indexes of the action edges
      f <- lapply(self$decision_nodes("node"), function(d) {
        a <- sapply(self$actions(d), function(a){self$element_index(a)})
        return(a)
      }) 
      names(f) <- dn
      TT <- expand.grid(f, KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE)
      # find the paths which are traversed by each strategy
      TT$paths <- apply(TT, MARGIN=1, function(row) {
        strategy <- private$E[row]
        P <- self$paths_in_strategy(strategy)      
        leaf <- sapply(P, function(p){self$element_index(p[[length(p)]])})
        return(paste(leaf,collapse="."))
      })
      # remove non-unique paths
      TT <- TT[!duplicated(TT$paths),]
      TT$paths <- NULL
      # convert indexes to labels if required
      if (what == "label") {
        TT <- apply(TT, MARGIN=c(1,2), function(ie) {
          private$E[[ie]]$label()
        })
      }
      # return the table
      return(TT)
    },

    #' @description 
    #' Evaluate the components of payoff associated with the paths in the
    #' decision tree. For each path, the strategy, probability, cost,
    #' benefit and utility are calculated.
    #' @param strategy A list of Actions, with one action per decision node.
    #' @return A data frame (payoff table) with one row per path and columns
    #' organized as follows:
    #' \describe{
    #' \item{<label of decision node>}{One column for each decision node
    #' in the mode. Each column is named with the label of the node. For each
    #' row (path) the value is the label of the Action edge taken from the
    #' decision node.}
    #' \item{Leaf}{The label of the leaf node on which the pathway ends; 
    #' normally the clinical outcome.}
    #' \item{Probability}{The probability of traversing the pathway. The total
    #' probability of each strategy should sum to unity.}
    #' \item{Path.Cost}{The cost of traversing the pathway.}
    #' \item{Path.Benefit}{The benefit derived from traversing the pathway.}
    #' \item{Path.Utility}{The utility associated with the outcome (leaf node).}
    #' \item{Cost}{Path.Cost \eqn{*} probability of traversing the pathway.}
    #' \item{Benefit}{Path.Benefit \eqn{*} probability of traversing the 
    #' pathway.}
    #' \item{Utility}{Path.Utility \eqn{*} probability of traversing the
    #' pathway.}
    #' }
    evaluate_strategy = function(strategy) {
      # check argument
      if (length(strategy)!=length(self$decision_nodes())) {
        rlang::abort(
          "Argument 'strategy' must have as many elements as DecisionNodes",
          class="incorrect_strategy_length"
        )
      }
      sapply(strategy, function(e) {
        if (!inherits(e,what="Action")) {
          rlang::abort("Argument 'strategy' must only contain Action elements",
                       class="incorrect_strategy_type")
        }
      })
      # find all root to leaf paths for the specified strategy
      P <- self$paths_in_strategy(strategy)
      # create a matrix of strategies
      dn <- sapply(strategy, function(e){e$source()$label()})
      DM <- matrix(
        rep(sapply(strategy,function(e){e$label()}),times=length(P)),
        nrow=length(P),
        ncol=length(dn),
        byrow=TRUE,
        dimnames=list(list(),dn)
      )
      # create a data frame
      PAYOFF <- data.frame(PID=seq_along(P), stringsAsFactors=FALSE)
      PAYOFF <- cbind(PAYOFF, DM, deparse.level=1, stringsAsFactors=FALSE)
      PAYOFF$Probability <- rep(as.numeric(NA), length(P))
      PAYOFF$Path.Cost <- rep(as.numeric(NA), length(P))
      PAYOFF$Path.Benefit <- rep(as.numeric(NA), length(P))
      PAYOFF$Path.Utility <- rep(as.numeric(NA), length(P))
      # evaluate each path
      for (i in seq_along(P)) {
        # get path
        path <- P[[i]]
        # leaf node
        leaf.label <- path[[length(path)]]$label()
        PAYOFF[PAYOFF$PID==i,"Leaf"] <- leaf.label
        # probability
        pr <- 1
        sapply(self$walk(path), function(e) {
          if (inherits(e, what="Reaction")) {
            pr <<- pr * e$p()
          }
        })
        PAYOFF[PAYOFF$PID==i,"Probability"] <- pr
        # cost
        cost <- 0
        sapply(self$walk(path), function(e) {
          cost <<- cost + e$cost()
        })
        PAYOFF[PAYOFF$PID==i,"Path.Cost"] <- cost
        # benefit
        benefit <- 0
        sapply(self$walk(path), function(e) {
          benefit <<- benefit + e$benefit()
        })
        PAYOFF[PAYOFF$PID==i,"Path.Benefit"] <- benefit
        # utility of the leaf node at end of the path
        PAYOFF[PAYOFF$PID==i, "Path.Utility"] <- path[[length(path)]]$utility()
      }
      # add expected cost and utility     
      PAYOFF$Cost <- PAYOFF$Probability*PAYOFF$Path.Cost
      PAYOFF$Benefit <- PAYOFF$Probability*PAYOFF$Path.Benefit
      PAYOFF$Utility <- PAYOFF$Probability*PAYOFF$Path.Utility
      # remove path ID
      PAYOFF$PID <- NULL
      # return the payoff table      
      return(PAYOFF)
    },
 
    #' @description 
    #' Create a "tornado" diagram to compare two strategies for traversing
    #' the decision tree. A strategy is a unanimous prescription of the actions
    #' at each decision node. 
    #' @param index The index strategy (option) to be evaluated.
    #' @param ref The reference strategy (option) with which the index strategy
    #' will be compared.
    #' @param outcome One of "cost" or "ICER". For "cost" (e.g. in cost 
    #' consequence analysis), the x axis is cost saved (cost of reference minus
    #' cost of index), on the presumption that the new technology will be cost
    #' saving at the point estimate. For "ICER" the x axis is
    #' $\Delta C/\Delta E$ and is expected to be positive at the point estimate
    #' (i.e. in the NE or SW quadrants of the cost-effectiveness plane).
    #' @param draw TRUE if the graph is to be drawn; otherwise return the
    #' data frame silently.
    #' @return A data frame with one row per input model variable and columns
    #' for: minimum value of the variable, maximum value of the variable,
    #' minimum value of the outcome and maximum value of the outcome. 
    #' @note The extreme values of each input variable are the upper and lower
    #' 95% confidence limits of the uncertainty distributions of each variable.
    #' This ensures that the range of each input is defensible (Briggs 2012).
    tornado = function(index, ref, outcome="cost", draw=TRUE) {
      # check the parameters
      
      # 
      
      # evaluate each strategy
      #NDX <- self$evaluate_strategy(index)
      #REF <- self$evaluate_strategy(ref)
      
    },

    #' @description 
    #' Evaluate each strategy. Starting with the root, the function
    #' works though all possible paths to leaf nodes and computes the 
    #' probability, cost, benefit and utility of each, then aggregates 
    #' by strategy.   
    #' @param expected If TRUE, evaluate each model variable as its mean value,
    #' otherwise sample each one from their uncertainty distribution.
    #' @param N Number of replicates. Intended for use with PSA (expected=F);
    #' use with expected=T will be repetitive and uninformative. 
    #' @return A data frame with one row per strategy per run and columns
    #' organized as follows:
    #' \describe{
    #' \item{Run}{The run number}
    #' \item{Strategy}{The strategy.}
    #' \item{Cost}{Aggregate cost of the strategy.}
    #' \item{Benefit}{Aggregate benefit of the strategy.}
    #' \item{Utility}{Aggregate utility of the strategy.}
    #' }
    evaluate = function(expected=TRUE, N=1) {
      # find unique strategies
      TT <- self$strategies()
      ## names of columns to copy to identify each strategy
      dn <- self$decision_nodes("label")
      # make repeated calls 
      DF <- do.call('rbind', sapply(1:N, FUN=function(n){
        # set the ModVar values (either mean or sampled)
        for (v in self$modvars()) {
          v$set(expected)
        }
        # evaluate each strategy
        ALL <- apply(TT, MARGIN=1, function(row) {
          strategy <- private$E[row]
          RES <- self$evaluate_strategy(strategy)
          f <- as.formula(
            paste(
              "cbind(Probability, Cost, Benefit, Utility)",
              paste(dn, collapse="+"),
              sep = "~"
            ) 
          )
          PAYOFF <- aggregate(f, data=RES, FUN=sum)
          PAYOFF <- cbind(Run=n, PAYOFF)
          return(PAYOFF)
        })
        return(ALL)
      }))
      return(DF)
    }

  )
)


