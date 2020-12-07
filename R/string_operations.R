#' Wrapper for gsub to align arguments with stringi package
#'
#' @param str character vector; strings to search in
#' @param pattern character vector with replacements for matched patterns
#' @param replacement character vector; search patterns
#'
#' @return a character vector
gsub_regex <- function(str, pattern, replacement) {
  gsub(pattern = pattern, x = str, replacement = replacement)
}

#' Wrapper for gsub to align arguments with stringi package
#'
#' @param str character vector; strings to search in
#' @param pattern character vector with replacements for matched patterns
#' @param replacement character vector; search patterns
#'
#' @return a character vector
gsub_fixed <- function(str, pattern, replacement) {
  gsub(pattern = pattern, x = str, replacement = replacement, fixed = TRUE)
}

#' Perform multiple string operations
#'
#' @param .string character vector; strings to search in
#' @param .table_string Ordererd dataframe from RFtext package
#'
#' @return a character vector
#' @export
#'
#' @examples
string_operations <- function(.string, .table_string) {
  s <- .string
  t <- .table_string
  for (i in 1:nrow(t)) {
    if (is.na(t$search[i])) {
      s <- t$call[[i]](s)
    } else {
      s <- t$call[[i]](s, t$search[i], t$replace[i])
    }
  }
  return(s)
}

#' Table containing arguments for string_operations()
#'
#' @format A Table with 5 columns:
#' \describe{
#'   \item{name}{name of the operation}
#'   \item{search}{search pattern}
#'   \item{replace}{replace of the operation}
#'   \item{example}{an example of the operation}
#'   \item{call}{function call}
#' }
"table_string"
