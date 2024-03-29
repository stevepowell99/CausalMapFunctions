% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{pipe_find_links}
\alias{pipe_find_links}
\title{Find links}
\usage{
pipe_find_links(
  graf,
  field = NULL,
  value,
  operator = "contains",
  remove_isolated = T,
  highlight_only = F
)
}
\arguments{
\item{field}{Field (column, variable) to search}

\item{value}{Value to search for}

\item{operator}{c('contains','notcontains','=','notequals','greater','less','starts','ends').
How to search.}
}
\value{
A mapfile containing only matching links. Factors are not removed, so this function may return maps with isolated factors,
i.e. factors with no links.
}
\description{
Find links
}
