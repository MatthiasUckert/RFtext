#' Replace Words in Text/Tokens
#'
#' @param .string character vector; strings to search in
#' @param .table_replace
#' A Table with 2 columns (column names are not important) \cr
#' column 1: pattern word \cr
#' column 2: replace word \cr
#' @param .tokenized
#' If input is already tokenized a more efficient algorithm is used. Gives a considerable speed advantage in large datasets
#' @param .regex
#' If TRUE word boundaries are added, if FALSE blanks are added
#'
#' @return A character vector
#' @export
#'
#' @examples
#' tab_replace <- tibble::tribble(
#' ~ from, ~ to,
#' "A", "A1",
#' "B", "B1"
#' )
#'
#' replace_words("A B", tab_replace)
#' replace_words("A B,", tab_replace, .regex = TRUE)
#' replace_words("A B", tab_replace, .tokenized = TRUE)
#' replace_words(c("A", "B"), tab_replace, .tokenized = TRUE)
#'
replace_words <- function(.string, .table_replace, .tokenized = FALSE, .regex = FALSE) {
  if (!.tokenized) {
    if (!.regex) {
      s <- paste0(" ", trimws(.string), " ")
      t1 <- paste0(" ", trimws(.table_replace[[1]]), " ")
      t2 <- paste0(" ", trimws(.table_replace[[2]]), " ")
      trimws(stringi::stri_replace_all_fixed(s, t1, t2, vectorize_all = FALSE))
    } else {
      s <- trimws(.string)
      t1 <- paste0("\\b", trimws(.table_replace[[1]]), "\\b")
      t2 <- trimws(.table_replace[[2]])
      trimws(stringi::stri_replace_all_regex(s, t1, t2, vectorize_all = FALSE))
    }
  } else {
    s <- .string
    t1 <- .table_replace[[1]]
    t2 <- .table_replace[[2]]

    qdapTools::lookup(s, t1, t2, missing = NULL)
  }
}


