% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/position_count.R
\name{helper_validate_tables}
\alias{helper_validate_tables}
\title{Helper Function: Check Input for position_count()}
\usage{
helper_validate_tables(.table_terms, .table_text)
}
\arguments{
\item{.table_terms}{A Dataframe with at least 2 columns (tid: Term ID, term: Term) \cr
Term column must be a list column}

\item{.table_text}{A Dataframe with at least 2 columns (doc_id: A Document Identifier, text: Text) \cr
Text column mus be tokenized (one token per row)}
}
\value{
Checks if Input is correct
}
\description{
Helper Function: Check Input for position_count()
}
