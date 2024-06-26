% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LeafNode.R
\docType{class}
\name{LeafNode}
\alias{LeafNode}
\title{A leaf node in a decision tree}
\description{
An R6 class representing a leaf (terminal) node in a decision
tree.
}
\details{
Represents a terminal state in a tree, and is associated with an
incremental utility. Inherits from class \code{Node}.
}
\author{
Andrew J. Sims \email{andrew.sims@newcastle.ac.uk}
}
\section{Super class}{
\code{\link[rdecision:Node]{rdecision::Node}} -> \code{LeafNode}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-LeafNode-new}{\code{LeafNode$new()}}
\item \href{#method-LeafNode-modvars}{\code{LeafNode$modvars()}}
\item \href{#method-LeafNode-set_utility}{\code{LeafNode$set_utility()}}
\item \href{#method-LeafNode-set_interval}{\code{LeafNode$set_interval()}}
\item \href{#method-LeafNode-utility}{\code{LeafNode$utility()}}
\item \href{#method-LeafNode-interval}{\code{LeafNode$interval()}}
\item \href{#method-LeafNode-QALY}{\code{LeafNode$QALY()}}
\item \href{#method-LeafNode-clone}{\code{LeafNode$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="rdecision" data-topic="Node" data-id="label"><a href='../../rdecision/html/Node.html#method-Node-label'><code>rdecision::Node$label()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rdecision" data-topic="Node" data-id="type"><a href='../../rdecision/html/Node.html#method-Node-type'><code>rdecision::Node$type()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LeafNode-new"></a>}}
\if{latex}{\out{\hypertarget{method-LeafNode-new}{}}}
\subsection{Method \code{new()}}{
Create a new \code{LeafNode} object; synonymous with a clinical outcome.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LeafNode$new(
  label,
  utility = 1,
  interval = as.difftime(365.25, units = "days")
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{label}}{Character string; a label for the state; must be
defined because it is used in tabulations. The label is automatically
converted to a syntactically valid (in R) name to ensure it can be used
as a column name in a data frame.}

\item{\code{utility}}{The incremental utility that a user associates with
being in the health state for the interval.
Intended for use with cost benefit analysis. Can be \code{numeric} or
a type of \code{ModVar}. If the type is \code{numeric}, the allowed
range is \code{-Inf} to 1; if it is of type \code{ModVar}, it is
unchecked.}

\item{\code{interval}}{The time interval over which the \code{utility}
parameter applies, expressed as an R \code{difftime} object; default
1 year.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{LeafNode} object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LeafNode-modvars"></a>}}
\if{latex}{\out{\hypertarget{method-LeafNode-modvars}{}}}
\subsection{Method \code{modvars()}}{
Find all the model variables of type \code{ModVar} that have
been specified as values associated with this \code{LeafNode}. Includes
operands of these \code{ModVar}s, if they are expressions.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LeafNode$modvars()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A list of \code{ModVar}s.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LeafNode-set_utility"></a>}}
\if{latex}{\out{\hypertarget{method-LeafNode-set_utility}{}}}
\subsection{Method \code{set_utility()}}{
Set the incremental utility associated with the node.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LeafNode$set_utility(utility)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{utility}}{The incremental utility that a user associates with
being in the health state for the interval.
Intended for use with cost benefit analysis. Can be \code{numeric} or
a type of \code{ModVar}. If the type is \code{numeric}, the allowed
range is \code{-Inf} to 1, not NA; if it is of type \code{ModVar}, it is
unchecked.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Updated \code{Leaf} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LeafNode-set_interval"></a>}}
\if{latex}{\out{\hypertarget{method-LeafNode-set_interval}{}}}
\subsection{Method \code{set_interval()}}{
Set the time interval associated with the node.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LeafNode$set_interval(interval)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{interval}}{The time interval over which the \code{utility}
parameter applies, expressed as an R \code{difftime} object; default
1 year, not NA.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Updated \code{Leaf} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LeafNode-utility"></a>}}
\if{latex}{\out{\hypertarget{method-LeafNode-utility}{}}}
\subsection{Method \code{utility()}}{
Return the incremental utility associated with being in the
state for the interval.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LeafNode$utility()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Incremental utility (numeric value).
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LeafNode-interval"></a>}}
\if{latex}{\out{\hypertarget{method-LeafNode-interval}{}}}
\subsection{Method \code{interval()}}{
Return the interval associated with being in the state.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LeafNode$interval()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Interval (as a \code{difftime}).
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LeafNode-QALY"></a>}}
\if{latex}{\out{\hypertarget{method-LeafNode-QALY}{}}}
\subsection{Method \code{QALY()}}{
Return the quality adjusted life years associated with
being in the state.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LeafNode$QALY()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\acronym{QALY}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LeafNode-clone"></a>}}
\if{latex}{\out{\hypertarget{method-LeafNode-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LeafNode$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
