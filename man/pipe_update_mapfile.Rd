% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{pipe_update_mapfile}
\alias{pipe_update_mapfile}
\title{Update mapfile}
\usage{
pipe_update_mapfile(
  tables = NULL,
  factors = NULL,
  links = NULL,
  statements = NULL,
  sources = NULL,
  questions = NULL,
  settings = NULL
)
}
\arguments{
\item{tables}{A list of data frames which includes factors, links, statements, sources, questions, settings.
Each element of the list will be used to replace corresponding NULL input parameters.}

\item{factors}{A data frame representing factors. If NULL, it will be replaced by tables$factors or standard_factors().}

\item{links}{A data frame representing links. If NULL, it will be replaced by tables$links or standard_links().}

\item{statements}{A data frame representing statements. If NULL, it will be replaced by tables$statements or standard_statements().}

\item{sources}{A data frame representing sources. If NULL, it will be replaced by tables$sources or standard_sources().}

\item{questions}{A data frame representing questions. If NULL, it will be replaced by tables$questions or standard_questions().}

\item{settings}{A data frame representing settings. If NULL, it will be replaced by tables$settings or standard_settings().}
}
\value{
A list of updated data frames for factors, links, statements, sources, questions, settings.
}
\description{
This function takes a mapfile, which is a list of data frames (factors, links, statements, sources, questions, settings)
and updates it with provided new data or, if not provided, with standard data. The standard data is defined by
respective standard_* functions.
}
\examples{
# Example usage (please replace with actual usage)
updated_data = pipe_update_mapfile(tables = list_of_dataframes)

}
