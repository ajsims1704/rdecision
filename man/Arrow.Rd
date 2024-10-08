% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Arrow.R
\docType{class}
\name{Arrow}
\alias{Arrow}
\title{A directed edge in a digraph}
\description{
An R6 class representing an directed edge in a digraph.
}
\details{
An arrow is the formal term for an edge between pairs of nodes in a
directed graph. Inherits from class \code{Edge}.
}
\author{
Andrew J. Sims \email{andrew.sims@newcastle.ac.uk}
}
\section{Super class}{
\code{\link[rdecision:Edge]{rdecision::Edge}} -> \code{Arrow}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Arrow-new}{\code{Arrow$new()}}
\item \href{#method-Arrow-source}{\code{Arrow$source()}}
\item \href{#method-Arrow-target}{\code{Arrow$target()}}
\item \href{#method-Arrow-clone}{\code{Arrow$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="rdecision" data-topic="Edge" data-id="endpoints"><a href='../../rdecision/html/Edge.html#method-Edge-endpoints'><code>rdecision::Edge$endpoints()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rdecision" data-topic="Edge" data-id="is_same_edge"><a href='../../rdecision/html/Edge.html#method-Edge-is_same_edge'><code>rdecision::Edge$is_same_edge()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rdecision" data-topic="Edge" data-id="label"><a href='../../rdecision/html/Edge.html#method-Edge-label'><code>rdecision::Edge$label()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rdecision" data-topic="Edge" data-id="modvars"><a href='../../rdecision/html/Edge.html#method-Edge-modvars'><code>rdecision::Edge$modvars()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Arrow-new"></a>}}
\if{latex}{\out{\hypertarget{method-Arrow-new}{}}}
\subsection{Method \code{new()}}{
Create an object of type \code{Arrow}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Arrow$new(source_node, target_node, label = "")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{source_node}}{Node from which the arrow leaves.}

\item{\code{target_node}}{Node to which the arrow points.}

\item{\code{label}}{Character string containing the arrow label.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{Arrow} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Arrow-source"></a>}}
\if{latex}{\out{\hypertarget{method-Arrow-source}{}}}
\subsection{Method \code{source()}}{
Access source node.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Arrow$source()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Node from which the arrow leads.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Arrow-target"></a>}}
\if{latex}{\out{\hypertarget{method-Arrow-target}{}}}
\subsection{Method \code{target()}}{
Access target node.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Arrow$target()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Node to which the arrow points.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Arrow-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Arrow-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Arrow$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
