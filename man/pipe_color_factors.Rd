% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{pipe_color_factors}
\alias{pipe_color_factors}
\title{Color factors (background color)}
\usage{
pipe_color_factors(
  graf,
  field = "frequency",
  lo = "#FCFDBF",
  hi = "#9F189F",
  mid = "#D5438E",
  fixed = NULL,
  pal = 1
)
}
\arguments{
\item{graf}{A mapfile representing a causal map.
A mapfile is a tidygraph, which consists of a table of edges linked to a table of nodes,
with an optional additional table of statements.
In this package, nodes are called `factors` and edges are called `links.`}

\item{field}{A numerical or character field in the factor table.
If it is character, the other parameters are ignored and a color-blind friendly qualitative palette is applied.}

\item{lo}{Optionally, a color specification for the low end of the color range. Default is `green`.}

\item{hi}{Optionally, a color specification for the high end of the color range. Default is `blue`.}

\item{mid}{Optionally, a color specification for the middle of the color range. Default is `gray`.}

\item{fixed}{Optionally, a color specification which will be applied everywhere and overrides `field`.}
}
\value{
A mapfile with a new or overwritten column `color.background`in the factor table.
}
\description{
Color factors (background color)
}
