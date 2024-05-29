#' @title A decision tree
#' @description An R6 class to represent a decision tree model.
#' @details A class to represent a decision tree. An object contains a tree of
#' decision nodes, chance nodes and leaf nodes, connected by edges
#' (either actions or reactions). It inherits from class \code{Arborescence} and
#' satisfies the following conditions:
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
#'   Kaminski B, Jakubczyk M, Szufel P. A framework for sensitivity analysis of
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

  public = list(

    #' @description Create a new decision tree.
    #' @details The tree must consist of a set of nodes and a set of edges
    #' which satisfy the conditions given in the details section of this class.
    #' In addition, the decision nodes must not be labelled with any of the
    #' words used as the column headings listed in \code{evaluate}.
    #' @param V A list of nodes.
    #' @param E A list of edges.
    #' @return A \code{DecisionTree} object
    initialize = function(V, E) {
      # initialize the base class(es); checks that {V,E} form an arborescence
      super$initialize(V, E)
      D <- self$decision_nodes(what = "index")
      C <- self$chance_nodes(what = "index")
      L <- self$leaf_nodes(what = "index")
      W <- union(D, union(C, L))
      abortifnot(setequal(self$vertex_along(), W),
        message = "Each node must be a decision, chance or leaf node.",
        class = "incorrect_node_type"
      )
      # all and only leaf nodes must have no children
      vi <- self$vertex_along()
      P <- vi[vapply(vi, FUN.VALUE = TRUE, FUN = function(i) {
        v <- self$vertex_at(i)
        self$is_parent(v)
      })]
      abortifnot(setequal(P, union(D, C)),
        message = "All and only leaf nodes must have no children",
        class = "leaf_non-child_sets_unequal"
      )
      # each edge must inherit from action or reaction
      lv <- vapply(self$edge_along(), FUN.VALUE = TRUE, FUN = function(i) {
        e <- self$edge_at(i)
        inherits(e, what = c("Action", "Reaction"))
      })
      abortifnot(all(lv),
        message = "Each edge must inherit from Action or Reaction",
        class = "incorrect_edge_type"
      )
      # DecisionNode labels must be unique, the labels of the Actions from
      # each decision node must be unique, and the labels must not be reserved
      # words.
      d_lab <- vapply(D, FUN.VALUE = "x", FUN = function(d) {
        v <- self$vertex_at(d)
        K <- self$direct_successors(v)
        choices <- vapply(K, FUN.VALUE = "x", FUN = function(k) {
          w <- self$walk(list(v, k))
          e <- w[[1L]]
          return(e$label())
        })
        abortifnot(anyDuplicated(choices) == 0L,
          message = "Labels of actions with common source node must be unique",
          class = "invalid_labels"
        )
        return(v$label())
      })
      reserved <- c(
        "Run", "Leaf", "Probability", "Cost", "Benefit", "Utility", "QALY"
      )
      abortifnot(
        anyDuplicated(d_lab) == 0L,
        all(is.na(match(x = d_lab, table = reserved))),
        message = "Labels of DecisionNodes must be unique, unreserved words",
        class = "invalid_labels"
      )
      # return a new DecisionTree object
      return(invisible(self))
    },

    #' @description Find the decision nodes in the tree.
    #' @param what A character string defining what to return. Must be one
    #' of "node", "label" or "index".
    #' @return A list of \code{DecisionNode} objects (for what = "node"); a list
    #' of character strings (for what = "label"), or an integer vector with
    #' indexes of the decision nodes (for what = "index").
    decision_nodes = function(what = "node") {
      # check arguments
      abortifnot(
        what %in% c("node", "index", "label"),
        message = "Argument 'what' must be one of 'node', 'label' or 'index'.",
        class = "unknown_what_value"
      )
      # indexes of decision nodes
      n <- which(
        vapply(
          X = self$vertexes(), FUN.VALUE = TRUE, FUN = inherits,
          what = "DecisionNode"
        )
      )
      if (what == "node") {
        n <- self$vertex_at(n, as_list = TRUE)
      } else if (what == "label") {
        n <- self$vertex_label(n)
      }
      return(n)
    },

    #' @description Find the chance nodes in the tree.
    #' @param what A character string defining what to return. Must be one
    #' of "node", "label" or "index".
    #' @return A list of \code{ChanceNode} objects (for what = "node"); a list
    #' of character strings (for what = "label"), or an integer vector with
    #' indexes of the decision nodes (for what = "index").
    chance_nodes = function(what = "node") {
      # check arguments
      abortifnot(
        what %in% c("node", "index", "label"),
        message = "Argument 'what' must be one of 'node', 'label' or 'index'.",
        class = "unknown_what_value"
      )
      # indexes of chance nodes
      n <- which(
        vapply(
          self$vertexes(), FUN.VALUE = TRUE, FUN = inherits,
          what = "ChanceNode"
        )
      )
      if (what == "node") {
        n <- self$vertex_at(n, as_list = TRUE)
      } else if (what == "label") {
        n <- self$vertex_label(n)
      }
      return(n)
    },

    #' @description Find the leaf nodes in the tree.
    #' @param what One of "node" (returns Node objects), "label" (returns the
    #' leaf node labels) or "index" (returns the vertex indexes of the leaf
    #' nodes).
    #' @return A list of \code{LeafNode} objects (for what = "node"); a list
    #' of character strings (for what = "label"); or an integer vector of
    #' leaf node indexes (for what = "index").
    leaf_nodes = function(what = "node") {
      # check arguments
      abortifnot(
        what %in% c("node", "index", "label"),
        message = "Argument 'what' must be one of 'node', 'label' or 'index'.",
        class = "unknown_what_value"
      )
      # indexes of leaf nodes
      n <- which(
        vapply(
          X = self$vertexes(), FUN.VALUE = TRUE, FUN = inherits,
          what = "LeafNode"
        )
      )
      if (what == "node") {
        n <- self$vertex_at(n, as_list = TRUE)
      } else if (what == "label") {
        n <- self$vertex_label(n)
      }
      return(n)
    },

    #' @description Find the edges that have the specified decision node as
    #' their source.
    #' @param d A decision node.
    #' @return A list of \code{Action} edges.
    actions = function(d) {
      # check argument
      abortif(missing(d),
        message = "Node 'd' must be defined",
        class = "decision_node_not_defined"
      )
      abortifnot(self$has_vertex(d),
        message = "Node 'd' is not in the Decision Tree",
        class = "not_in_tree"
      )
      abortifnot(inherits(d, what = "DecisionNode"),
        message = "Node 'd' is not a Decision Node",
        class = "not_decision_node"
      )
      id <- self$vertex_index(d)
      # find the edges with d as their source
      B <- self$digraph_incidence_matrix()
      ie <- which(B[id, ] == -1L)
      return(self$edge_at(ie, as_list = TRUE))
    },

    #' @description Find all the model variables of type \code{ModVar}.
    #' @details Find \code{ModVar}s that have been specified as values
    #' associated with the nodes and edges of the tree.
    #' @return A list of \code{ModVar}s.
    modvars = function() {
      # create list
      mv <- list()
      # find the ModVars in Actions and Reactions
      for (e in self$edges()) {
        if (inherits(e, what = c("Action", "Reaction"))) {
          mv <- c(mv, e$modvars())
        }
      }
      # find the modvars in leaf nodes
      for (v in private$V){
        if (inherits(v, what = "LeafNode")) {
          mv <- c(mv, v$modvars())
        }
      }
      # return a unique list
      return(unique(mv))
    },

    #' @description Tabulate the model variables.
    #' @param expressions A logical that defines whether expression model
    #' variables should be included in the tabulation.
    #' @return Data frame with one row per model variable, as follows:
    #' \describe{
    #' \item{\code{Description}}{As given at initialization.}
    #' \item{\code{Units}}{Units of the variable.}
    #' \item{\code{Distribution}}{Either the uncertainty distribution, if
    #' it is a regular model variable, or the expression used to create it,
    #' if it is an \code{ExprModVar}.}
    #' \item{\code{Mean}}{Mean; calculated from means of operands if
    #' an expression.}
    #' \item{\code{E}}{Expectation; estimated from random sample if expression,
    #' mean otherwise.}
    #' \item{\code{SD}}{Standard deviation; estimated from random sample if
    #' expression, exact value otherwise.}
    #' \item{\code{Q2.5}}{p=0.025 quantile; estimated from random sample if
    #' expression, exact value otherwise.}
    #' \item{\code{Q97.5}}{p=0.975 quantile; estimated from random sample if
    #' expression, exact value otherwise.}
    #' \item{\code{Est}}{TRUE if the quantiles and SD have been estimated by
    #' random sampling.}
    #' }
    modvar_table = function(expressions = TRUE) {
      # create list of model variables in this decision tree, excluding
      # expressions if not wanted
      mvlist <- self$modvars()
      if (!expressions) {
        mvlist <- mvlist[vapply(mvlist, FUN.VALUE = TRUE, FUN = function(v) {
          !v$is_expression()
        })]
      }
      # create a data frame of model variables
      DF <- data.frame(
        Description = vapply(mvlist, FUN.VALUE = "x", FUN = function(x) {
          rv <- x$description()
          return(rv)
        }),
        Units = vapply(mvlist, FUN.VALUE = "x", FUN = function(x) {
          rv <- x$units()
          return(rv)
        }),
        Distribution = vapply(mvlist, FUN.VALUE = "x", FUN = function(x) {
          rv <- x$distribution()
          return(rv)
        }),
        Mean = vapply(mvlist, FUN.VALUE = 1.0, FUN = function(x) {
          rv <- x$mean()
          return(rv)
        }),
        E = vapply(mvlist, FUN.VALUE = 1.0, FUN = function(x) {
          rv <- ifelse(x$is_expression(), x$mu_hat(), x$mean())
          return(rv)
        }),
        SD = vapply(mvlist, FUN.VALUE = 1.0, FUN = function(x) {
          rv <- ifelse(x$is_expression(), x$sigma_hat(), x$SD())
          return(rv)
        }),
        Q2.5 = vapply(mvlist, FUN.VALUE = 1.0, FUN = function(x) {
          rv <- ifelse(
            x$is_expression(),
            x$q_hat(probs = 0.025),
            x$quantile(probs = 0.025)
          )
          return(rv)
        }),
        Q97.5 = vapply(mvlist, FUN.VALUE = 1.0, FUN = function(x) {
          rv <- ifelse(
            x$is_expression(),
            x$q_hat(probs = 0.975),
            x$quantile(probs = 0.975)
          )
          return(rv)
        }),
        Est = vapply(mvlist, FUN.VALUE = TRUE, FUN = function(exp) {
          rv <- exp$is_expression()
          return(rv)
        })
      )
      # Return the table
      return(DF)
    },

    #' @description Draw the decision tree to the current graphics output.
    #' @details Uses the algorithm of Walker (1989) to distribute the nodes
    #' compactly (see the \link{Arborescence} class help for details).
    #' @param border If TRUE draw a light grey border around the plot area.
    #' @param fontsize Font size for labels in point. Symbols for nodes scale
    #' with font size.
    #' @return No return value.
    draw = function(border = FALSE, fontsize = 8.0) {
      # start new page for drawing
      grid::grid.newpage()
      # fraction of edge that slopes after leading parent (range 0,1)
      fs <- 0.20
      # create a root viewport with default gpar settings
      grid::pushViewport(grid::viewport(gp = grid::gpar(fontsize = fontsize)))
      # draw border around figure region, if required
      if (border) {
        grid::grid.rect(gp = grid::gpar(col = "lightgrey"))
      }
      # create a viewport for the outer margins
      grid::pushViewport(
        grid::viewport(
          width = grid::unit(1.0, "npc") - 2.0 * grid::stringWidth("M"),
          height = grid::unit(1.0, "npc") - 2.0 * grid::stringWidth("M"),
          clip = "on"
        )
      )
      # find the (x,y) coordinates of nodes using Walker's algorithm and the
      # unadjusted aspect ratio of the tree
      XY <- self$postree(RootOrientation = "EAST", LevelSeparation = 1.0)
      xmin <- min(XY[, "x"])
      xmax <- max(XY[, "x"])
      ymin <- min(XY[, "y"])
      ymax <- max(XY[, "y"])
      tree_asp <- (xmax - xmin) / (ymax - ymin)
      # find the label length of the leftmost node
      inode <- XY[[which.min(XY[, "x"]), "n"]]
      vnode <- self$vertex_at(inode)
      llabel <- grid::stringWidth(vnode$label())
      # calculate margins for the viewport which will contain the node centres,
      # leaving space for the node shapes and the leftmost label
      lmargin <- max(llabel, grid::unit(1.5, "char"))
      rmargin <- grid::unit(1.5, "char")
      bmargin <- grid::unit(2.0, "char")
      tmargin <- grid::unit(2.5, "char")
      # compute the level separation for the tree which will optimise the use
      # of space in the inner viewport
      vpgw <- grid::unit(1.0, "npc") - lmargin - rmargin
      vpgh <- grid::unit(1.0, "npc") - bmargin - tmargin
      vpg_asp <- grid::convertUnit(vpgw, unitTo = "cm", valueOnly = TRUE) /
        grid::convertUnit(vpgh, unitTo = "cm", valueOnly = TRUE)
      # lay out the tree again, with the revised aspect ratio
      XY <- self$postree(
        RootOrientation = "EAST", LevelSeparation = vpg_asp / tree_asp
      )
      xmin <- min(XY[, "x"])
      xmax <- max(XY[, "x"])
      ymin <- min(XY[, "y"])
      ymax <- max(XY[, "y"])
      # create a viewport for the inner margins (containing the node centres),
      # with the postree coordinate system
      grid::pushViewport(
        grid::viewport(
          x = lmargin, y = bmargin, width = vpgw, height = vpgh,
          just = c("left", "bottom"),
          xscale = c(xmin, xmax), yscale = c(ymin, ymax),
          clip = "inherit"
        )
      )
      # function to draw an action or reaction
      draw_edge <- function(xs, ys, xt, yt, fs = 0.2, label = "") {
        # draw the articulated line
        grid::grid.move.to(x = xs, y = ys, default.units = "native")
        xj <- (xt - xs) * fs + xs
        yj <- yt
        grid::grid.line.to(x = xj, yj, default.units = "native")
        grid::grid.line.to(x = xt, y = yt, default.units = "native")
        # add label above or below
        grid::pushViewport(viewport(
          x = xj, y = yj, default.units = "native", just = c("left", "bottom")
        ))
        if (yt < ys) {
          grid::grid.text(
            label = label,
            x = grid::unit(0.2, "char"), y = grid::unit(0.4, "char"),
            default.units = "char", just = c("left", "bottom")
          )
        } else {
          grid::grid.text(
            label = label,
            x = grid::unit(0.2, "char"), y = -grid::unit(0.4, "char"),
            default.units = "char", just = c("left", "top")
          )
        }
        grid::popViewport()
      }
      # function to draw a decision node and label
      decision_node <- function(x, y, label) {
        a <- grid::unit(sqrt(pi / 4.0), "char")
        dy <- grid::unit(0.4, "char")
        # create a viewport with origin at the centre of the object
        grid::pushViewport(viewport(
          x = x, y = y, default.units = "native", just = c("left", "bottom")
        ))
        grid::grid.rect(
          x = 0.0, y = 0.0, width = a * 2.0, height = a * 2.0,
          default.units = "char",
          just = c("centre", "centre"),
          gp = grid::gpar(col = "black", fill = "lightgray")
        )
        grid::grid.text(
          label = label, x = 0.0, y = a  + dy, default.units = "char",
          just = c("right", "bottom")
        )
        grid::popViewport()
      }
      # function to draw a chance node and label
      chance_node <- function(x, y, label) {
        a <- grid::unit(1.0, "char")
        dy <- grid::unit(0.4, "char")
        # create a viewport with origin at the centre of the object
        grid::pushViewport(viewport(
          x = x, y = y, default.units = "native", just = c("left", "bottom")
        ))
        grid::grid.circle(
          x = 0.0, y = 0.0, r = a, default.units = "char",
          gp = grid::gpar(col = "black", fill = "lightgray")
        )
        grid::grid.text(
          label = label, x = 0.0, y = a  + dy, default.units = "char",
          just = c("right", "bottom")
        )
        grid::popViewport()
      }
      # function to draw leaf node and label (in char space to keep aspect)
      leaf_node <- function(x, y, label) {
        a <- grid::unit(1.5 * sqrt(pi / sqrt(3.0)), "char")
        dy <- grid::unit(0.4, "char")
        # create a viewport with origin at the centre of the object
        grid::pushViewport(viewport(
          x = x, y = y, default.units = "native", just = c("left", "bottom")
        ))
        grid::grid.polygon(
          x = c(-a / sqrt(3.0), sqrt(3.0) * a / 6.0, sqrt(3.0) * a / 6.0),
          y = c(0.0, -a / 2.0, a / 2.0),
          default.units = "char",
          gp = grid::gpar(fill = "lightgray", col = "black")
        )
        grid::grid.text(
          label = label, x = 0.0, y = a / 3.0 + dy, default.units = "char",
          just = c("right", "bottom")
        )
        grid::popViewport()
      }
      # draw the edges as articulated lines between node centres
      for (ie in self$edge_along()) {
        e <- self$edge_at(ie)
        # find source and target nodes
        ns <- self$vertex_index(e$source())
        nt <- self$vertex_index(e$target())
        xs <- XY[[which(XY[, "n"] == ns), "x"]]
        ys <- XY[[which(XY[, "n"] == ns), "y"]]
        xt <- XY[[which(XY[, "n"] == nt), "x"]]
        yt <- XY[[which(XY[, "n"] == nt), "y"]]
        draw_edge(
          xs = xs, ys = ys, xt = xt, yt = yt, fs = fs, label = e$label()
        )
      }
      # draw the nodes
      for (iv in self$vertex_along()) {
        v <- self$vertex_at(iv)
        # find the node from its index
        i <- which(XY[, "n"] == self$vertex_index(v))
        # switch type
        if (inherits(v, what = "DecisionNode")) {
          decision_node(XY[[i, "x"]], XY[[i, "y"]], v$label())
        } else if (inherits(v, what = "ChanceNode")) {
          chance_node(XY[[i, "x"]], XY[[i, "y"]], v$label())
        } else if (inherits(v, what = "LeafNode")) {
          leaf_node(XY[[i, "x"]], XY[[i, "y"]], v$label())
        }
      }
      # return updated DecisionTree (unchanged)
      return(invisible(self))
    },

    #' @description Tests whether an object is a valid strategy.
    #' @details A strategy is a unanimous prescription of an action taken at
    #' each decision node, coded as a list of action edges. This checks
    #' whether the strategy is valid for this decision tree.
    #' @param strategy A list of Action edges.
    #' @return TRUE if the strategy is valid for this tree. Returns
    #' FALSE if the list of Action edges are not a valid strategy.
    is_strategy = function(strategy) {
      # find the set of source nodes for the action edges in the strategy
      iS <- vapply(X = strategy, FUN.VALUE = 1L, FUN = self$arrow_source)
      # find the list of Decision nodes
      iD <- self$decision_nodes("index")
      # the set of source nodes of the action edges must be the same as the set
      # of decision nodes
      return((length(iS) == length(iD)) && setequal(iS, iD))
    },

    #' @description Find all potential strategies for the decision tree.
    #' @details A strategy is a unanimous prescription of the actions at each
    #' decision node. If there are decision nodes that are descendants of other
    #' nodes in the tree, the strategies returned will not necessarily
    #' be unique.
    #' @param what A character string defining what to return. Must be one
    #' of "label" or "index".
    #' @param select A single strategy (given as a list of action edges, with
    #' one action edge per decision node). If provided, only that strategy
    #' is selected from the returned table. Intended for tabulating a
    #' single strategy into a readable form.
    #' @return A data frame where each row is a potential strategy
    #' and each column is a decision node, ordered lexicographically. Values
    #' are either the index of each action edge, or their label. The row names
    #' are the edge labels of each strategy, concatenated with underscores.
    strategy_table = function(what = "index", select = NULL) {
      # check arguments
      abortifnot(what %in% c("index", "label"),
        message = "Argument 'what' must be one of 'index' or 'label'",
        class = "unknown_what_value"
      )
      if (!is.null(select)) {
        abortifnot(self$is_strategy(select),
          message = "'select' must be a valid strategy for this decision tree",
          class = "invalid_strategy"
        )
      }
      # build a list of action edges emerging from each decision node
      ae <- lapply(self$decision_nodes(), FUN = self$actions)
      # convert action edge objects to indexes
      aei <- lapply(ae, vapply, self$edge_index, FUN.VALUE = 1L)
      # add the decision node label to each list element
      names(aei) <- self$decision_nodes(what = "label")
      # order the decision nodes lexicograpically
      aei <- aei[order(names(aei))]
      # find all possible strategies, by taking each combination of one action
      # edge from each decision node
      tti <- expand.grid(aei, KEEP.OUT.ATTRS = FALSE)
      # select a single strategy, if required
      if (!is.null(select)) {
        # indexes of action edges in 'select' argument
        ss <- vapply(X = select, FUN.VALUE = 1L, FUN = self$edge_index)
        # test whether each table row has the same action edges as 'select'
        lv <- apply(X = tti, MARGIN = 1L, FUN = setequal, y = ss)
        tti <- tti[lv, , drop = FALSE]
      }
      # build a table with labels in place of indexes
      ttl <- tti
      for (d in colnames(tti)) {
        ttl[, d] <- vapply(X = tti[, d], FUN.VALUE = "x", FUN = self$edge_label)
      }
      # build row names
      rn <- apply(ttl, 1L, paste, collapse = "_")
      rownames(tti) <- rn
      rownames(ttl) <- rn
      # return object
      tt <- if (what == "label") ttl else tti
      return(tt)
    },

    #' @description Find all paths walked in each possible strategy.
    #' @details A strategy is a unanimous prescription of an action in each
    #' decision node. Some paths can be walked in more than one strategy, if
    #' there exist paths that do not pass a decision node.
    #' @return A data frame, where each row is a path walked in a strategy. The
    #' structure is similar to that returned by \code{strategy_table} but
    #' includes an extra column, \code{Leaf} which gives the leaf node index of
    #' each path, and there is one row for each path in each strategy.
    strategy_paths = function() {
      # find possible strategies
      st <- self$strategy_table()
      rownames(st) <- NULL
      # find the set of action edges in the tree
      ea <- which(
        vapply(
          X = self$edges(), FUN.VALUE = TRUE, FUN = inherits, what = "Action"
        )
      )
      # iterate all paths and create data frame with action nodes visited and
      # terminating leaf node
      dn <- self$decision_nodes("label")
      p <- do.call("rbind", lapply(self$root_to_leaf_paths(), function(p) {
        # find the set of action edges in this walk
        ew <- vapply(X = self$walk(p), FUN.VALUE = 1L, FUN = self$edge_index)
        ew <- intersect(ea, ew)
        # iterate the strategies and find those consistent with the path
        lv <- apply(st, MARGIN = 1L, FUN = function(es) {
          # the set of action edges which define the strategy
          ae <- setdiff(ew, es)
          return(length(ae) == 0L)
        })
        # Append to the strategy data frame
        sl <- cbind(
          st[lv, , drop = FALSE],
          Leaf = self$vertex_index(p[[length(p)]])
        )
        return(sl)
      }))
      # return the paths
      return(p)
    },

    #' @description Properties of all actions and reactions as a matrix.
    #' @details Gets the properties (probability, cost, benefit) of each
    #' action and reaction in the decision tree in matrix form. If there are
    #' reactions from chance nodes whose conditional probability of
    #' traversal set to NA, the missing values are replaced by one minus
    #' the sum of the conditional probabilities of the other reaction edges
    #' from that node (provided there is no more than one with NA). If the
    #' method is called at evaluation, the replacement of NAs happens after
    #' sampling from model variables.
    #' @return A numeric matrix with one row per edge, and with four columns:
    #' the index of the edge, the conditional probability of traversing the
    #' edge, the cost of traversing the edge and the benefit associated with
    #' traversing the edge. The column names are \code{index},
    #' \code{probability}, \code{cost}, \code{benefit} and the row names are
    #' the labels of the edges.
    edge_properties = function() {
      # create a matrix with information from each edge
      ep <- matrix(data = NA, nrow = self$size(), ncol = 5L)
      colnames(ep) <- c("index", "src", "probability", "cost", "benefit")
      rn <- vector(mode = "character", length = self$size())
      r <- 1L
      for (ie in self$edge_along()) {
        e <- self$edge_at(ie)
        ep[[r, "index"]] <- ie
        ep[[r, "src"]] <- self$arrow_source(e)
        ep[[r, "probability"]] <- e$p()
        ep[[r, "cost"]] <- e$cost()
        ep[[r, "benefit"]] <- e$benefit()
        rn[[r]] <- e$label()
        r <- r + 1L
      }
      rownames(ep) <- rn
      # replace probabilities that have been set to NA, checking that there is
      # at most one NA among the Reactions from each chance node; it is safe to
      # check this by source because Actions have non-modifiable p = 1, so will
      # have no NAs.
      if (anyNA(ep[, "probability"])) {
        # count probability NAs by source node and raise error if there are > 1
        sumna <- aggregate(
          x = as.formula(cbind(nna = is.na(probability)) ~ src),
          data = ep,
          FUN = sum
        )
        abortif(
          any(sumna[, "nna"] > 1L),
          message = "At most 1 Reaction from a chance node may have p = NA.",
          class = "invalid_probability_sum"
        )
        # find the source nodes having one missing reaction probability
        missp <- with(data = sumna, src[nna == 1L])
        # find sum of probabilities for source nodes with missing p
        sump <- aggregate(probability ~ src, data = ep, FUN = sum)
        sump <- with(data = sump, sump[src %in% missp, ])
        # replace NAs in ep with balance of probabilities
        index_na <- which(is.na(ep[, "probability"]))
        src_na <- ep[index_na, "src"]
        ep[, "probability"] <- replace(
          x = ep[, "probability"],
          list = index_na,
          values = with(data = sump, 1.0 - probability[match(src_na, src)])
        )
      }
      # remove 'src' column
      ep <- ep[, colnames(ep) != "src"]
      return(ep)
    },

    #' @description Evaluate the components of pay-off associated with a set of
    #' walks in the decision tree.
    #' @details For each walk, probability, cost, benefit and utility are
    #' calculated. There is minimal checking of the argument because this
    #' function is intended to be called repeatedly during tree evaluation,
    #' including PSA.
    #' @param W A list of root-to-leaf walks. A walk is a sequence of edges
    #' (actions and reactions), stored as a list. Each walk must start with an
    #' edge whose source is the root node and end with an edge whose target is
    #' a leaf node. The list of walks is normally the walks associated with all
    #' the root to leaf paths in a tree.
    #' @param Wi As \var{W} but with edge indices instead of Edge objects. One
    #' of \var{W} and \var{Wi} must be NULL. It is more efficient to provide
    #' \var{Wi} during PSA, where the paths do not change between cycles, to
    #' avoid repeated conversion of edges to indices.
    #' @return A pay-off table, represented as a matrix of numeric values
    #' with response columns as follows:
    #' \describe{
    #' \item{\code{Probability}}{The probability of traversing the pathway. }
    #' \item{\code{Path.Cost}}{The cost of traversing the pathway.}
    #' \item{\code{Path.Benefit}}{The benefit derived from traversing the
    #'       pathway.}
    #' \item{\code{Path.Utility}}{The utility associated with the outcome (leaf
    #'       node).}
    #' \item{\code{Path.QALY}}{The QALYs associated with the outcome (leaf
    #'       node).}
    #' \item{\code{Cost}}{\code{Path.Cost} \eqn{*} probability of traversing the
    #'       pathway.}
    #' \item{\code{Benefit}}{\code{Path.Benefit} \eqn{*} probability of
    #' traversing the pathway.}
    #' \item{\code{Utility}}{\code{Path.Utility} \eqn{*} probability of
    #' traversing the pathway.}
    #' \item{\code{QALY}}{\code{Path.QALY} \eqn{*} probability of traversing the
    #' pathway.}
    #' }
    #' The matrix has one row per path, with the row label equal to the
    #' character representation of the index of the leaf node at the end of
    #' the path.
    evaluate_walks = function(W = NULL, Wi = NULL) {
      # convert edge nodes into edge indices for each walk
      if (is.null(Wi)) {
        Wi <- lapply(W, self$edge_index)
      }
      # create a return matrix
      cnames <- c(
        "Probability",
        "Path.Cost", "Path.Benefit", "Path.Utility", "Path.QALY",
        "Cost", "Benefit", "Utility", "QALY"
      )
      rnames <- rep(NA_character_, times = length(Wi))
      payoff <- matrix(
        data = NA_real_,
        nrow = length(Wi),
        ncol = length(cnames),
        dimnames = list(Leaf = rnames, Response = cnames)
      )
      # get the edge properties for the tree
      ep <- self$edge_properties()
      # evaluate each walk
      for (i in seq_along(Wi)) {
        walk <- Wi[[i]]
        # terminal node
        stem <- self$edge_at(walk[[length(walk)]])
        leaf <- stem$target()
        # check
        abortifnot(
          inherits(leaf, what = "LeafNode"),
          message = "Walk must end on a leaf",
          class = "not_to_leaf"
        )
        # set the row name as the index of the leaf node
        ileaf <- self$vertex_index(leaf)
        rnames[[i]] <- as.character(ileaf)
        # find the rows of the edge properties that are in the walk
        wrows <- ep[, "index"] %in% walk
        # compute probability, cost and benefit
        pr <- prod(ep[wrows, "probability"])
        cost <- sum(ep[wrows, "cost"])
        benefit <- sum(ep[wrows, "benefit"])
        # # walk the path and accumulate p, cost and benefit
        payoff[[i, "Probability"]] <- pr
        payoff[[i, "Path.Cost"]] <- cost
        payoff[i, "Path.Benefit"] <- benefit
        # utility of the leaf node at end of the path
        payoff[[i, "Path.Utility"]] <- leaf$utility()
        payoff[[i, "Path.QALY"]] <- leaf$QALY()
      }
      # add expected cost and utility
      payoff[, "Cost"] <- payoff[, "Probability"] * payoff[, "Path.Cost"]
      payoff[, "Benefit"] <- payoff[, "Probability"] * payoff[, "Path.Benefit"]
      payoff[, "Utility"] <- payoff[, "Probability"] * payoff[, "Path.Utility"]
      payoff[, "QALY"] <- payoff[, "Probability"] * payoff[, "Path.QALY"]
      # set the row names
      rownames(payoff) <- rnames
      # return the payoff table
      return(payoff)
    },

    #' @description Evaluate each strategy.
    #' @details Starting with the root, the function works though all possible
    #' paths to leaf nodes and computes the probability, cost, benefit and
    #' utility of each, optionally aggregated by strategy or run.
    #' The columns of the returned data frame are:
    #' \describe{
    #' \item{\code{by = "path"}}{
    #'   \describe{
    #'     \item{\code{Run}}{Run number}
    #'     \item{\code{<label of first decision node>}}{label of action leaving
    #'           the node}
    #'     \item{\code{<label of second decision node (etc.)>}}{label of action
    #'       leaving the node}
    #'     \item{\code{Leaf}}{The label of terminating leaf node}
    #'     \item{\code{Probability}}{Probability of traversing the path}
    #'     \item{\code{Cost}}{Cost of traversing the path}
    #'     \item{\code{Benefit}}{Benefit of traversing the path}
    #'     \item{\code{Utility}}{Utility of traversing the path}
    #'     \item{\code{QALY}}{QALY of traversing the path}
    #'   }
    #' }
    #'
    #' \item{\code{by = "strategy"}}{
    #'   \describe{
    #'     \item{\code{Run}}{Run number}
    #'     \item{\code{<label of first decision node>}}{label of action leaving
    #'           the node}
    #'     \item{\code{<label of second decision node (etc)}}{label of action}
    #'     \item{\code{Probability}}{\eqn{\Sigma p_i} for the run (1)}
    #'     \item{\code{Cost}}{Aggregate cost of the strategy}
    #'     \item{\code{Benefit}}{Aggregate benefit of the strategy}
    #'     \item{\code{Utility}}{Aggregate utility of the strategy}
    #'     \item{\code{QALY}}{Aggregate QALY of the strategy}
    #'   }
    #' }
    #'
    #' \item{\code{by = "run"}}{
    #'   \describe{
    #'     \item{\code{Run}}{Run number}
    #'     \item{\code{Probability.<S>}}{Probability for strategy S}
    #'     \item{\code{Cost.<S>}}{Cost for strategy S}
    #'     \item{\code{Benefit.<S>}}{Benefit for strategy S}
    #'     \item{\code{Utility.<S>}}{Benefit for strategy S}
    #'     \item{\code{QALY.<S>}}{QALY for strategy S}
    #'   }
    #'   where <S> is a label associated with strategy \code{S}. Each strategy
    #'   label is
    #'   a list of the labels of the action edges that are traversed in the
    #'   strategy, concatenated with underscores. The ordering of each label
    #'   part follows the lexicographical order of the decision node labels
    #'   concatenated with underscores. For example, if there are three
    #'   decision nodes labelled d1, d2 and d3, each strategy label will be of
    #'   the form a1i_a2i_a3i where a1i is the label of one action edge
    #'   emanating from decision node d1, etc. There will be one probability,
    #'   cost, benefit, utility and QALY column for each strategy.
    #'   }
    #' }
    #' @param setvars One of "expected" (evaluate with each model variable at
    #' its mean value), "random" (sample each variable from its uncertainty
    #' distribution and evaluate the model), "q2.5", "q50", "q97.5" (set each
    #' model variable to its 2.5\%, 50\% or 97.5\% quantile, respectively, and
    #' evaluate the model) or "current" (leave each model variable at its
    #' current value prior to calling the function and evaluate the model).
    #' @param N Number of replicates. Intended for use with PSA
    #' (\code{modvars = "random"}); use with \code{modvars} = "expected"
    #' will be repetitive and uninformative.
    #' @param by One of \{"path", "strategy", "run"\}. If "path", the table has
    #' one row per path walked per strategy, per run, and includes the label of
    #' the terminating leaf node to identify each path. If "strategy" (the
    #' default), the table is aggregated by strategy, i.e., there is one row per
    #' strategy per run. If "run", the table has one row per run and uses
    #' concatenated strategy names (as above) and one (cost, benefit, utility,
    #' QALY) as row names.
    #' @return A data frame whose columns depend on \code{by}; see "Details".
    evaluate = function(setvars = "expected", N = 1L, by = "strategy") {
      # check arguments
      abortifnot(is.character(setvars),
        message = "'setvars' must be a character",
        class = "setvars_not_character"
      )
      valids <- c("expected", "random", "q2.5", "q50", "q97.5", "current")
      abortifnot(setvars %in% valids,
        message = paste(
          "'setvars' must be one of",
          paste(valids, collapse = " "),
          collapse = " "
        ),
        class = "setvars_invalid"
      )
      abortifnot(is.numeric(N),
        message = "'N' must be numeric",
        class = "N_not_numeric"
      )
      abortifnot(is.character(by),
        message = "'by' must be character",
        class = "by_not_character"
      )
      abortifnot(by %in% c("path", "strategy", "run"),
        message = "'by' must be one of {path|strategy|run}.",
        class = "by_invalid"
      )
      
      ####
      
      # find the root-to-leaf paths
      P <- self$root_to_leaf_paths()
      # find the walk for each root-to-leaf path
      Wi <- lapply(P, FUN = self$walk, what = "index")
      # create template matrix for the result
      TM <- self$evaluate_walks(Wi = Wi)
      # create list of modvars
      MV <- self$modvars()
      # N tree evaluations, stored as stacked 2d matrices, with one matrix per
      # run, keeping only the necessary columns
      keep <- c("Probability", "Cost", "Benefit", "Utility", "QALY")
      nleaf <- nrow(TM)
      RES <- matrix(
        data = NA_real_,
        ncol = length(keep),
        nrow = nleaf * N,
        dimnames = list(NULL, Response = keep)
      )
      for (i in seq_len(N)) {
        # set the ModVar values (to chosen option)
        for (v in MV) {
          v$set(setvars)
        }
        # evaluate the tree
        M <- self$evaluate_walks(Wi = Wi)
        # add it to the array
        rstart <- (i - 1L) * nleaf + 1L
        rend <- rstart + nleaf - 1L
        RES[rstart : rend, ] <- M[, keep]
      }
      # form array into a data frame with correct column types
      payoff <- as.data.frame(RES)
      payoff[, "Run"] <- rep(seq_len(N), each = nrow(TM))
      payoff[, "Leaf"] <- rep(as.integer(rownames(TM)), times = N)
      # find all paths walked for each strategy
      sw <- self$strategy_paths()
      # merge with results to get one row per path walked per strategy
      payoff <- merge(sw, payoff, all.x = TRUE, by = "Leaf")
      # check that the sum of probabilities for each strategy, for each run,
      # is unity, by aggregating the payoff table by strategy and run
      f <- paste(c(self$decision_nodes("label"), "Run"), collapse = "+")
      f <- paste("Probability", f, sep = "~")
      psum <- aggregate(as.formula(f), data = payoff, FUN = sum)
      abortifnot(
        all.equal(psum[, "Probability"], rep(1.0, times = nrow(psum))),
        message = "Probabilities for each strategy must sum to 1 for each run",
        class = "invalid_probability_sum"
      )
      # Replace the edge indexes of the strategy with their edge labels
      dn <- self$decision_nodes("label")
      dn <- sort(dn, na.last = TRUE)
      for (d in dn) {
        payoff[, d] <- self$edge_label(payoff[, d])
      }
      # modify output as required
      if (by == "path") {
        # replace Leaf node indexes by their label and reorder
        payoff[, "Leaf"] <- self$vertex_label(payoff[, "Leaf"])
        ofact <- c("Run", dn, "Leaf")
        oresp <- c("Probability", "Cost", "Benefit", "Utility", "QALY")
        payoff <- payoff[do.call(order, payoff[, ofact]), c(ofact, oresp)]
      } else {
        # aggregate by strategy
        f <- as.formula(
          paste(
            "cbind(Probability, Cost, Benefit, Utility, QALY)",
            paste(paste(dn, collapse = "+"), "Run", sep = "+"),
            sep = "~"
          )
        )
        payoff <- aggregate(f, data = payoff, FUN = sum)
        ofact <- c("Run", dn)
        oresp <- c("Probability", "Cost", "Benefit", "Utility", "QALY")
        payoff <- payoff[do.call(order, payoff[, ofact]), c(ofact, oresp)]
        # further aggregate strategies by run if required
        if (by == "run") {
          # reshape to wide format
          if (length(dn) == 1L) {
            payoff$Strategy <- payoff[, dn[[1L]]]
          } else {
            payoff$Strategy <- apply(
              payoff[, dn], MARGIN = 1L, FUN = paste, collapse = "_"
            )
          }
          payoff[, dn] <- NULL
          payoff <- reshape(
            payoff, idvar = "Run", timevar = "Strategy", direction = "wide"
          )
        }
      }
      # return the data frame
      row.names(payoff) <- NULL
      return(payoff)
    },

    #' @description Create a "tornado" diagram.
    #' @details Used to compare two strategies for traversing the decision tree.
    #' A strategy is a unanimous prescription of the actions at each
    #' decision node. The extreme values of each input variable are the upper
    #' and lower 95\% confidence limits of the uncertainty distributions of each
    #' variable. This ensures that the range of each input is defensible
    #' (Briggs 2012).
    #' @param index The index strategy (option) to be evaluated.
    #' @param ref The reference strategy (option) with which the index strategy
    #' will be compared.
    #' @param outcome One of \code{"saving"} or \code{"ICER"}. For
    #' \code{"saving"} (e.g. in cost consequence analysis), the x axis is cost
    #' saved (cost of reference minus
    #' cost of index), on the presumption that the new technology will be cost
    #' saving at the point estimate. For \code{"ICER"} the x axis is
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
    #' if there are no \code{ModVar}s.
    tornado = function(
      index, ref, outcome = "saving", exclude = NULL, draw = TRUE
    ) {
      # find all input modvars, excluding expressions and those stated
      mvlist <- self$modvars()
      lv <- vapply(X = mvlist, FUN.VALUE = TRUE, FUN = function(v) {
        return(!v$is_expression())
      })
      mvlist <- mvlist[lv]
      if (length(mvlist) == 0L) {
        if (draw) {
          plot.new()
        }
        return(NULL)
      }
      # check the parameters
      abortif(missing(index) || missing(ref),
        message = "'index' and 'ref' must be defined",
        class = "missing_strategy"
      )
      abortifnot(self$is_strategy(index) && self$is_strategy(ref),
        message = "'index' and 'ref' must be valid strategies",
        class = "invalid_strategy"
      )
      abortifnot(outcome %in% c("saving", "ICER"),
        message = "'outcome' must be one of {saving|ICER}",
        class = "invalid_outcome"
      )
      if (!is.null(exclude)) {
        abortifnot(is.list(exclude),
          message = "'exclude' must be a list of model variable descriptions",
          class = "exclude_not_list"
        )
        mvd <- vapply(
          mvlist,
          FUN.VALUE = "x",
          FUN = function(v) v$description()
        )
        isc <- vapply(X = exclude, FUN.VALUE = TRUE, FUN = function(d) {
          rv <- (is.character(d) && (d %in% mvd))
          return(rv)
        })
        abortifnot(all(isc),
          message = "'exclude' must be a list of model variable descriptions",
          class = "exclude_element_not_modvar"
        )
      }
      abortifnot(is.logical(draw),
        message = "'draw' must be boolean",
        class = "invalid_draw"
      )
      if (!is.null(exclude)) {
        lv <- vapply(X = mvlist, FUN.VALUE = TRUE, FUN = function(v) {
          return(v$description() %in% exclude)
        })
        mvlist <- mvlist[!lv]
      }
      # create data frame with limits of CIs
      TO <- data.frame(
        Description = vapply(
          X = mvlist,
          FUN.VALUE = "x",
          FUN = function(v) v$description()
        ),
        Units = vapply(
          X = mvlist,
          FUN.VALUE = "x",
          FUN = function(v) v$units()
        ),
        LL = vapply(
          X = mvlist,
          FUN.VALUE = 1.5,
          FUN = function(v) v$quantile(0.025)
        ),
        UL = vapply(
          X = mvlist,
          FUN.VALUE = 1.5,
          FUN = function(v) v$quantile(0.975)
        ),
        stringsAsFactors = FALSE
      )
      # find all modvars in the model, not only those in the tornado
      all.mv <- self$modvars()
      # build strategy tables for index and ref
      st.index <- self$strategy_table("label", select = index)
      st.ref <- self$strategy_table("label", select = ref)
      # find univariate outcome limits
      template <- vector(mode = "numeric", length = 3L)
      names(template) <- c("outcome.min", "outcome.mean", "outcome.max")
      O <- vapply(X = mvlist, FUN.VALUE = template, FUN = function(this.mv) {
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
          RES <- self$evaluate(setvars = "current")
          # get outcome for index strategy
          ORES <- merge(st.index, RES, all.x = TRUE)
          index.cost <- ORES$Cost[[1L]]
          index.utility <- ORES$Utility[[1L]]
          index.QALY <- ORES$QALY[[1L]]
          # get outcome for reference strategy
          ORES <- merge(st.ref, RES, all.x = TRUE)
          ref.cost <- ORES$Cost[[1L]]
          ref.utility <- ORES$Utility[[1L]]
          ref.QALY <- ORES$QALY[[1L]]
          # outcome
          if (outcome == "saving") {
            rv <- ref.cost - index.cost
          } else {
            rv <- (index.cost - ref.cost) / (index.QALY - ref.QALY)
          }
          return(rv)
        }
        res[["outcome.min"]] <- ce("q2.5")
        res[["outcome.mean"]] <- ce("expected")
        res[["outcome.max"]] <- ce("q97.5")
        # return row of results
        return(res)
      })
      # transpose the outcome
      O <- t(O)
      # append to the data frame
      TO <- cbind(TO, O)

      # re-order it with least variation first
      TO[, "range"] <- abs(TO[, "outcome.max"] - TO[, "outcome.min"])
      TO <- TO[order(TO[, "range"], decreasing = FALSE), ]
      TO[, "range"] <- NULL

      # plot the graph, if required
      if (draw) {
        # x axis label
        xlab <- ifelse(outcome == "saving", "Mean cost saving", "Mean ICER")
        # make labels (description + units)
        TO[, "Label"] <- paste(TO[, "Description"], TO[, "Units"], sep = ", ")
        # width and height of the plot in inches
        dsize <- dev.size(unit = "in")
        figw <- dsize[[1L]]
        figh <- dsize[[2L]]
        # width of the left outer margin as a proportion of figw
        louter <- 0.4
        # size of the inner margins as lines of text (0.2 inches per line)
        binner <- 4.1
        linner <- 2.1
        tinner <- 1.1
        rinner <- linner
        # create the plot frame
        withr::with_par(
          new = list(
            omi = c(0.0, figw * louter, 0.0, 0.0),
            mar = c(binner, linner, tinner, rinner),  # lines of text
            cex = 0.75
          ),
          code = {
            # set up the plot axes
            plot(
              x = NULL,
              y = NULL,
              xlim = c(
                min(min(TO$outcome.min), min(TO$outcome.max)),
                max(max(TO$outcome.min), max(TO$outcome.max))
              ),
              ylim = c(0.5, nrow(TO) + 0.5),
              xlab = xlab,
              ylab = "",
              yaxt = "n",
              frame.plot = FALSE
            )
            # find the longest label and scale text size accordingly
            lw <- max(strwidth(s = TO[, "Label"], unit = "in"))
            lw <- lw + strwidth("MM", unit = "in")
            cex_axis <- min((louter * figw) / lw, 1.0)
            # label the y axis
            axis(
              side = 2L,
              at = seq_len(nrow(TO)),
              labels = TO$Label,
              lty = 0L,
              tick = FALSE,
              las = 2L,
              hadj = 1.0,
              cex.axis = cex_axis,
              outer = TRUE
            )
            # function to return a limit value (vectorized)
            limtxt <- function(x) {
              txt <- signif(x, 3L)
            }
            # find longest limit labels and adjust label text size
            lmin <- max(strwidth(s = limtxt(TO[, "outcome.min"]), unit = "in"))
            lmax <- max(strwidth(s = limtxt(TO[, "outcome.max"]), unit = "in"))
            lw <- max(lmin, lmax)
            cex_limit <- min(linner * 0.2 / lw, 1.0)
            # add bars and limits
            for (i in seq_len(nrow(TO))) {
              xleft <- min(TO[i, "outcome.min"], TO[i, "outcome.max"])
              xright <- max(TO[i, "outcome.min"], TO[i, "outcome.max"])
              rect(
                xleft,
                xright,
                ybottom = i - 0.25,
                ytop = i + 0.25,
                border = "black",
                col = "lightgray",
                xpd = TRUE
              )
              LL <- TO[i, "LL"]
              UL <- TO[i, "UL"]
              if (TO[i, "outcome.max"] > TO[i, "outcome.min"]) {
                labels <- limtxt(c(LL, UL))
              } else {
                labels <- limtxt(c(UL, LL))
              }
              text(
                x = c(xleft, xright),
                y = c(i, i),
                labels = labels,
                pos = c(2.0, 4.0),
                offset = 0.25,
                cex = cex_limit,
                xpd = TRUE
              )
            }
            # add mean (base case)
            abline(v = TO[[1L, "outcome.mean"]], lty = "dashed")
            # remove label column
            TO[, "Label"] <- NULL
          }
        )
      }
      # re-order it with greatest variation first
      TO$range <- abs(TO[, "outcome.max"] - TO[, "outcome.min"])
      TO <- TO[order(TO[, "range"], decreasing = TRUE), ]
      TO[, "range"] <- NULL
      # remove mean column
      TO[, "outcome.mean"] <- NULL
      # return tornado data frame
      return(TO)
    },

    #' @description Find the threshold value of a model variable at which
    #' the cost difference is zero or the ICER is equal to a threshold, for an
    #' index strategy compared with a reference strategy.
    #' @details Uses a rudimentary bisection method method to find the root.
    #' In PSA terms, the algorithm finds the value of the specified model
    #' variable for which 50\% of runs are cost saving (or above the ICER
    #' threshold) and 50\% are cost incurring (below the ICER threshold).
    #' @param index The index strategy (option) to be evaluated.
    #' @param ref The reference strategy (option) with which the index strategy
    #' will be compared.
    #' @param outcome One of \code{"saving"} or \code{"ICER"}. For
    #' \code{"saving"} (e.g., in cost consequence analysis), the value of
    #' \code{mvd}
    #' is found at which cost saved is zero (cost saved is cost of reference
    #' minus cost of index, on the presumption that the new technology will be
    #' cost saving at the point estimate). For \code{"ICER"} the value of
    #' \code{mvd}
    #' is found for which the incremental cost effectiveness ratio (ICER) is
    #' equal to the threshold \code{lambda}. ICER is calculated as
    #' \eqn{\Delta C/\Delta E}, which will normally be positive
    #' at the point estimate (i.e. in the NE or SW quadrants of the
    #' cost-effectiveness plane), where \eqn{\Delta C} is cost of index minus
    #' cost of reference and \eqn{\Delta E} is utility of index minus utility
    #' of reference.
    #' @param mvd The description of the model variable for which the threshold
    #' is to be found.
    #' @param a The lower bound of the range of values of \code{mvd} to search
    #' for the root (numeric).
    #' @param b The upper bound of the range of values of \code{mvd} to search
    #' for the root (numeric).
    #' @param tol The tolerance to which the threshold should be
    #' calculated (numeric).
    #' @param lambda The ICER threshold (threshold ratio) for outcome="ICER".
    #' @param nmax Maximum number if iterations allowed to reach convergence.
    #' @return Value of the model variable of interest at the threshold.
    threshold = function(
      index, ref, outcome, mvd, a, b, tol, lambda = NULL, nmax = 1000L
    ) {
      # find all input modvars, excluding expressions
      mvlist <- self$modvars()
      lv <- vapply(X = mvlist, FUN.VALUE = TRUE, FUN = function(v) {
        return(!v$is_expression())
      })
      mvlist <- mvlist[lv]
      # check the parameters
      abortif(missing(index) || missing(ref),
        message = "'index' and 'ref' must be defined",
        class = "missing_strategy"
      )
      abortifnot(self$is_strategy(index) && self$is_strategy(ref),
        message = "'index' and 'ref' must be valid strategies",
        class = "invalid_strategy"
      )
      abortif(missing(outcome),
        message = "'outcome' must not be missing",
        class = "invalid_outcome"
      )
      abortifnot(outcome %in% c("saving", "ICER"),
        message  = "'outcome' must be one of {saving|ICER}",
        class = "invalid_outcome"
      )
      if (outcome == "ICER") {
        abortif(missing(lambda) || !is.numeric(lambda) || lambda <= 0.0,
          message = "'lambda' must be numeric and > 0",
          class = "invalid_lambda"
        )
      }
      abortifnot(is.character(mvd),
        message = "'mvd' must be a character string",
        class = "invalid_mvd"
      )
      lv <- vapply(X = mvlist, FUN.VALUE = TRUE, FUN = function(v) {
        return(identical(v$description(), mvd))
      })
      abortifnot(sum(lv) == 1L,
        message = "'mvd' must identify exactly one variable in the model",
        class = "invalid_mvd"
      )
      dsav <- mvlist[[which(lv)]]
      abortifnot(is.numeric(a) && is.numeric(b) && (b > a),
        message = "'a' and 'b' must be numeric and 'b' > 'a'",
        class = "invalid_brackets"
      )
      abortif(missing(tol) || !is.numeric(tol) || tol <= 0.0,
        message = "'tol' must be numeric and > 0",
        class = "invalid_tol"
      )
      # set all modvars to their mean
      for (v in mvlist) {
        v$set("expected")
      }
      # get names of index and ref strategies
      ST <- self$strategy_table(select = index)
      index.name <- rownames(ST)[[1L]]
      ST <- self$strategy_table(select = ref)
      ref.name <- rownames(ST)[[1L]]
      # construct a function to calculate the outcome at value x of the
      # variable for which the threshold is needed
      f <- function(x) {
        # set the variable under investigation to be x
        dsav$set("value", x)
        # evaluate the tree
        RES <- self$evaluate(setvars = "current", by = "run")
        # get costs and QALYs of index and ref strategies
        ref.cost <- RES[[1L, paste("Cost", ref.name, sep = ".")]]
        index.cost <- RES[[1L, paste("Cost", index.name, sep = ".")]]
        ref.qaly <- RES[[1L, paste("QALY", ref.name, sep = ".")]]
        index.qaly <- RES[[1L, paste("QALY", index.name, sep = ".")]]
        # outcome
        if (outcome == "saving") {
          saving <- ref.cost - index.cost
          rv <- (saving - 0.0)
        } else {
          icer <- (index.cost - ref.cost) / (index.qaly - ref.qaly)
          rv <- icer - lambda
        }
        return(rv)
      }
      # return variable
      threshold <- NA
      # check that sign of cost saving is different at the brackets
      abortifnot(f(a) * f(b) < 0.0,
        message = "[a, b] does not bracket the root",
        class = "invalid_brackets"
      )
      n <- 0L
      while (n < nmax) {
        # new midpoint
        c <- (a + b) / 2.0
        if (((b - a) / 2.0) < tol) {
          break
        }
        n <- n + 1L
        if (sign(f(c)) == sign(f(a))) {
          a <- c
        } else {
          b <- c
        }
      }
      abortif(n >= nmax,
        message = "Failed to converge",
        class = "convergence_failure"
      )
      threshold <- c
      # return the threshold
      return(threshold)
    }
  )
)
