#' Wrapper around pdftools::pdf_info
#'
#' @param .path_in full path to the pdf
#'
#' @return A Dataframe
#' @export
pdf_info <- function(.path_in) {
  ichr_doc_id = gsub(".pdf$", "", basename(.path_in))

  safe_pdf_info  <- purrr::safely(pdftools::pdf_info)
  lst <- suppressMessages(safe_pdf_info(.path_in))
  res <- purrr::compact(lst$result)
  err <- paste(purrr::compact(lst$error), collapse = "|")

  if (length(res) > 0) {
    purrr::flatten_dfc(res) %>%
      dplyr::mutate(
        created = lst$created,
        modified = lst$modified,
        doc_id = ichr_doc_id,
        error = FALSE
      ) %>%
      dplyr::select(doc_id, dplyr::everything())
  } else {
    tibble::tibble(
      doc_id = ichr_doc_id,
      error = TRUE,
      error_msg = err
    )
  }

}
