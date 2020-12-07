#' Find a sequence in another sequence
#'
#' @param .seq_find The sequence you want to find in another sequence
#' @param .seq_base The sequence to be searched
#'
#' @return An integer vector (vector has length zero if sequence is not found)
#' @export
#'
#' @examples
#' # Find an integer sequence
#' find_seq_in_seq(2:10, c(1:20, 1:20))
#'
#' # Find a character sequence
#' find_seq_in_seq(c("C", "D"), LETTERS)
#'
#' # No sequence found
#' find_seq_in_seq(c("R", "D"), LETTERS)
#'
find_seq_in_seq <- function(.seq_find, .seq_base) {
  w <- seq_along(.seq_base)
  for (i in seq_along(.seq_find)) {
    w <- w[.seq_base[w + i - 1L] == .seq_find[i]]
    if (length(w) == 0) return(integer(0))
  }
  w <- w[!is.na(w)]
  return(w)
}


#' Get Position of Terms in Text
#'
#' @param .table_terms
#' A Dataframe with at least 2 columns (tid: Term ID, term: Term) \cr
#' Term column must be a list column
#' @param .table_text
#' A Dataframe with at least 2 columns (doc_id: A Document Identifier, text: Text) \cr
#' Text column mus be tokenized (one token per row)
#' @param .col_split_id
#' Additionally an ID column that shows breaks in the Text Dataframe (e.g. an sentence ID)
#' @param .workers
#' Number of workers (default = 1)
#' @param .pre_check
#' Number of ngram pre-check
#'
#' @return
#' A Dataframe
#' @export
#'
#' @examples
position_count <- function(.table_terms, .table_text, .col_split_id = NULL, .workers = 1, .pre_check = 3) {
  helper_validate_tables(.table_terms, .table_text)

  i_lst_terms <- split(.table_terms, .table_terms[["tid"]])

  if (.pre_check > 0) {
    i_chr_tid <- helper_pre_check(
      .table_terms  = .table_terms,
      .table_text   = .table_text,
      .col_split_id = {{ .col_split_id }},
      .pre_check    = .pre_check
    )
    i_lst_terms <- i_lst_terms[names(i_lst_terms) %in% i_chr_tid]
  }


  p <- progressr::progressor(steps = length(i_lst_terms))
  future::plan("multiprocess", workers = .workers)
  i_tab_pos <- furrr::future_map_dfr(
    .x = i_lst_terms,
    .f = ~ {p(); helper_position_count(.x, .table_text, {{ .col_split_id }})},
    .options = furrr::furrr_options(seed = TRUE)
  ) %>% dplyr::arrange(dplyr::desc(ngram)) %>%
    dplyr::mutate(id = dplyr::row_number())
  future::plan("default")

  i_tab_dup <- i_tab_pos %>%
    dplyr::mutate(idx = purrr::map2(start, stop, ~.x:.y)) %>%
    tidyr::unnest(idx) %>%
    dplyr::mutate(dup = duplicated(idx)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(dup = any(dup), .groups = "drop")

  dplyr::left_join(i_tab_pos, i_tab_dup, by = "id") %>% dplyr::select(-id)

}


#' Helper Function: Check Input for position_count()
#'
#' @param .table_terms
#' A Dataframe with at least 2 columns (tid: Term ID, term: Term) \cr
#' Term column must be a list column
#' @param .table_text
#' A Dataframe with at least 2 columns (doc_id: A Document Identifier, text: Text) \cr
#' Text column mus be tokenized (one token per row)
#'
#' @return Checks if Input is correct
#'
#' @examples
helper_validate_tables <- function(.table_terms, .table_text) {

  # Check if Inputs are Dataframes ------------------------------------------
  if (!is.data.frame(.table_terms)) {
    stop("'.table_terms' MUST be a dataframe", call. = FALSE)
  }

  if (!is.data.frame(.table_text)) {
    stop("'.table_text' MUST be a dataframe", call. = FALSE)
  }


  # Check Columns in Dataframe ----------------------------------------------
  if (!all(c("tid", "term") %in% colnames(.table_terms))) {
    stop("'.table_terms' MUST contain the columns 'tid' (Term ID) and 'term'", call. = FALSE)
  }

  if (!all(c("doc_id", "text") %in% colnames(.table_text))) {
    stop("'.table_text' MUST contain the columns 'doc_id' and 'text'", call. = FALSE)
  }


  i_lst_terms <- .table_terms[["term"]]
  # Check if term column is a list containing single vectors ----------------
  if (!class(i_lst_terms) == "list") {
    stop("'term' column in '.table_terms' MUST be a list column", call. = FALSE)
  }


  # Check if term column only contains characters ---------------------------
  if (!all(purrr::map_lgl(i_lst_terms, ~ class(.x) == "character"))) {
    stop("'term' column in '.table_terms' MUST only contain character vectors", call. = FALSE)
  }


  # Check Tokenizations -----------------------------------------------------
  if (any(grepl(" ", unlist(i_lst_terms)))) {
    warning("'term' column in '.table_terms' contains space characters (consider calling prepare_table_terms())", call. = FALSE)
  }

  if (any(grepl(" ", unlist(.table_text[["text"]])))) {
    warning("'text' column in '.table_text' contains space characters (consider calling prepare_table_text())", call. = FALSE)
  }




}

#' Helper Function: Checks if Ngrams in Term list appear in text
#'
#' @param .table_terms
#' A Dataframe with at least 2 columns (tid: Term ID, term: Term) \cr
#' Term column must be a list column
#' @param .table_text
#' A Dataframe with at least 2 columns (doc_id: A Document Identifier, text: Text) \cr
#' Text column mus be tokenized (one token per row)
#' @param .col_split_id
#' Additionally an ID column that shows breaks in the Text Dataframe (e.g. an sentence ID)
#' @param .pre_check
#' Number of ngram pre-check
#'
#' @return A charcter vector with valid tids
#'
#' @examples
helper_pre_check <- function(.table_terms, .table_text, .col_split_id = NULL, .pre_check = 3) {

  i_check_text <- .table_text %>%
    dplyr::group_by(doc_id, {{ .col_split_id }}) %>%
    dplyr::summarise(text = paste(text, collapse = " "), .groups = "drop") %>%
    tidytext::unnest_tokens(text, text, token = "skip_ngrams", n = .pre_check)

  i_check_terms <- .table_terms %>%
    dplyr::mutate(term = purrr::map_chr(term, ~ paste(.x, collapse = " "))) %>%
    tidytext::unnest_tokens(term, term, token = "skip_ngrams", n = .pre_check)

  i_check_terms %>%
    dplyr::mutate(check = term %in% i_check_text$text) %>%
    dplyr::group_by(tid) %>%
    dplyr::summarise(check = all(check), .groups = "drop") %>%
    dplyr::filter(check) %>%
    dplyr::pull(tid) %>%
    as.character()
}


#' Helper Function: Single Term Position Count
#'
#' @param .row_terms
#' A Dataframe Row with at least 2 columns (tid: Term ID, term: Term) \cr
#' Term column must be a list column
#' @param .table_text
#' A Dataframe with at least 2 columns (doc_id: A Document Identifier, text: Text) \cr
#' Text column mus be tokenized (one token per row)
#' @param .col_split_id
#' Additionally an ID column that shows breaks in the Text Dataframe (e.g. an sentence ID)
#'
#' @return A dataframe
#'
#' @examples
helper_position_count <- function(.row_terms, .table_text, .col_split_id = NULL) {
  i_chr_term <- unlist(.row_terms[["term"]])

  if (length(i_chr_term) == 1) {
    i_tab_pos <- tibble::tibble(
      start = which(.table_text[["text"]] == i_chr_term)
    )
  } else {
    i_tab_pos <- tibble::tibble(
      start = find_seq_in_seq(i_chr_term, .table_text[["text"]])
    )
  }

  i_tab_pos <- i_tab_pos %>%
    dplyr::mutate(
      tid = .row_terms[["tid"]],
      stop = start + length(i_chr_term) - 1L,
      ngram = as.integer(length(i_chr_term)),
      term = paste(i_chr_term, collapse = " ")
    ) %>%
    dplyr::select(tid, ngram, term, start, stop)

  if (!is.null(.col_split_id)) {
    i_check_id <- paste0(
      dplyr::pull(.table_text, doc_id),
      dplyr::pull(.table_text, {{ .col_split_id }})
    )
  } else {
    i_check_id <- dplyr::pull(.table_text, doc_id)
  }


  i_lgl_check <- i_check_id[i_tab_pos[["start"]]] == i_check_id[i_tab_pos[["stop"]]]
  i_tab_pos <- i_tab_pos %>%
    dplyr::filter(i_lgl_check) %>%
    dplyr::left_join(
      y  = dplyr::mutate(i_tab_text, merging_id = dplyr::row_number()),
      by = c("start" = "merging_id")) %>%
    dplyr::select(doc_id, {{ .col_split_id }}, dplyr::everything())

  return(i_tab_pos)
}
