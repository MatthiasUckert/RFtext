% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/position_count.R
\name{position_count}
\alias{position_count}
\title{Get Position of Terms in Text}
\usage{
position_count(.table_terms, .table_text, ..., .workers = 1, .pre_check = 3)
}
\arguments{
\item{.table_terms}{A Dataframe with at least 2 columns (tid: Term ID, term: Term) \cr
Term column must be a list column}

\item{.table_text}{A Dataframe with at least 2 columns (doc_id: A Document Identifier, text: Text) \cr
Text column mus be tokenized (one token per row)}

\item{...}{columns to be passed to the function:\cr
Any included column is treated as a splitting value.}

\item{.workers}{Number of workers (default = 1)}

\item{.pre_check}{Number of ngram pre-check}
}
\value{
A Dataframe
}
\description{
Get Position of Terms in Text
}
\examples{
table_terms <- tibble::tibble(
  tid = 1:2,
  term = c("a b", "b")
) \%>\% prepare_table_terms()

table_text <- tibble::tibble(
  doc_id = 1:2,
  text = rep("A A A B A C B A C A B A B AB B A", 2)
) \%>\% prepare_table_text() \%>\%
  dplyr::rename(text = token)

position_count(table_terms, table_text)
}
