#' Prepare Term List Table for position_count()
#'
#' @param .table_terms
#' A Dataframe with at least 2 columns (tid: Term ID, term: Term) \cr
#' @return A Dataframe with a term list column
#' @export
#' @importFrom rlang .data
#' @examples
prepare_table_terms <- function(.table_terms) {
  # Check if Inputs are Dataframes ------------------------------------------
  if (!is.data.frame(.table_terms)) {
    stop("'.table_terms' MUST be a dataframe", call. = FALSE)
  }

  # Check Columns in Dataframe ----------------------------------------------
  if (!all(c("tid", "term") %in% colnames(.table_terms))) {
    stop("'.table_terms' MUST contain the columns 'tid' (Term ID) and 'term'", call. = FALSE)
  }

  dplyr::mutate(.table_terms, term = stringi::stri_split_fixed(.data$term, " "))
}



#' Prepare Term List Table for position_count()
#'
#' @param .table_text
#' A Dataframe with at least 2 columns (doc_id: A Document Identifier, text: Text) \cr
#' @param .use_udpipe
#' Use UDPipe for tokenization (if FALSE the default, other parameters are not important)
#' @param .lan UDPipe language (see ?udpipe_download_model)
#' @param .dir_mod UDPipe model directory
#' @param .tagger one of c("default", "none")
#' @param .parser one of c("none", "default")
#' @param .return_raw return the UDPipe raw output
#' @return A tokenized Dataframe
#' @export
#' @importFrom rlang .data
#' @examples
prepare_table_text <- function(.table_text, .use_udpipe = FALSE,
                               .lan = "english-ewt", .dir_mod = getwd(),
                               .tagger = c("default", "none"),
                               .parser = c("none", "default"),
                               .return_raw = FALSE) {
  # Check if Inputs are Dataframes ------------------------------------------
  if (!is.data.frame(.table_text)) {
    stop("'.table_terms' MUST be a dataframe", call. = FALSE)
  }

  # Check Columns in Dataframe ----------------------------------------------
  if (!all(c("doc_id", "text") %in% colnames(.table_text))) {
    stop("'.table_text' MUST contain the columns 'doc_id' and 'text'", call. = FALSE)
  }

  if (!.use_udpipe) {
    tab_token <- .table_text %>%
      tidytext::unnest_tokens(
        .data$text, .data$text,
        token = stringi::stri_split_regex, pattern = "\n\n", to_lower = FALSE
        ) %>%
      dplyr::group_by(.data$doc_id) %>%
      dplyr::mutate(par_id = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      tidytext::unnest_tokens(
        .data$text, .data$text, token = "sentences", to_lower = FALSE
        ) %>%
      dplyr::group_by(.data$doc_id) %>%
      dplyr::mutate(sen_id = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      tidytext::unnest_tokens(.data$oken, .data$text) %>%
      dplyr::group_by(.data$doc_id) %>%
      dplyr::mutate(tok_id = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$doc_id, .data$par_id, .data$sen_id, .data$tok_id, .data$token)
  } else {
    .parser <- match.arg(.parser)
    .tagger <- match.arg(.tagger)
    tab_mod <- udpipe::udpipe_download_model(.lan, .dir_mod)
    mod <- udpipe::udpipe_load_model(tab_mod$file_model)

    tab_token <- .table_text %>%
      udpipe::udpipe(mod, parser = .parser, tagger = .tagger) %>%
      tibble::as_tibble()

    if (!.return_raw) {
      tab_token <- tab_token %>%
        dplyr::filter(!.data$upos %in% c("PART", "PUNCT"))
      dplyr::select(.data$doc_id,
                    par_id = .data$paragraph_id, sen_id = .data$sentence_id,
                    tok_id = .data$term_id, .data$token, .data$lemma
      )
    }
  }
}
