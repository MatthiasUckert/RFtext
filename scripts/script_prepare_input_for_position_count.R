library(tidyverse)

.table_terms <- tab_term_list <- tribble(
  ~ tid, ~ term,
  1, "property plant and equipment",
  2, "property",
  3, "plant",
  4, "tangible asset",
  5, "tangible fixed asset",
  6, "asset"
)

.table_text <- tab_text <- tibble::tibble(
  doc_id = 1:2,
  text = c(
    "property plant and equipment are asset's and not intangible asset but asset they are tangible and fixed asset.",
    "AAA plant and equipment are asset and not intangible asset but asset they are tangible and fixed asset."
    )
)


prepare_table_terms <- function(.table_terms) {
  # Check if Inputs are Dataframes ------------------------------------------
  if (!is.data.frame(.table_terms)) {
    stop("'.table_terms' MUST be a dataframe", call. = FALSE)
  }

  # Check Columns in Dataframe ----------------------------------------------
  if (!all(c("tid", "term") %in% colnames(.table_terms))) {
    stop("'.table_terms' MUST contain the columns 'tid' (Term ID) and 'term'", call. = FALSE)
  }

  dplyr::mutate(.table_terms, term = stringi::stri_split_fixed(term, " "))
}

prepare_table_text <- function(.table_text, .use_udpipe,
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
      tidytext::unnest_tokens(text, text, token = stringi::stri_split_regex, pattern = "\n\n", to_lower = FALSE) %>%
      dplyr::group_by(doc_id) %>%
      dplyr::mutate(par_id = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      tidytext::unnest_tokens(text, text, token = "sentences", to_lower = FALSE) %>%
      dplyr::group_by(doc_id) %>%
      dplyr::mutate(sen_id = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      tidytext::unnest_tokens(token, text) %>%
      dplyr::group_by(doc_id) %>%
      dplyr::mutate(tok_id = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::select(doc_id, par_id, sen_id, tok_id, token)
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
        dplyr::filter(!upos %in% c("PART", "PUNCT"))
      dplyr::select(doc_id,
        par_id = paragraph_id, sen_id = sentence_id,
        tok_id = term_id, token, lemma
      )
    }
  }
}


prepare_table_terms(tab_term_list)
prepare_table_text(tab_text)


.string <- c(paste(LETTERS, collapse = " "),paste(LETTERS, collapse = " "))
.string <- rep(LETTERS, 100000)

.table_replace <- tibble::tribble(
  ~ search, ~ replace,
  "A", "1",
  "C", "2",
  "E", "3",
  "F", "5"
)

replace_in_string <- function(.string, .table_replace, .tokenized = FALSE) {
  if (!.tokenized) {
    s <- paste0(" ", trimws(.string), " ")
    t1 <- paste0(" ", trimws(.table_replace[[1]]), " ")
    t2 <- paste0(" ", trimws(.table_replace[[2]]), " ")

    trimws(stringi::stri_replace_all_fixed(s, t1, t2, vectorize_all = FALSE))
  } else {
    s <- .string
    t1 <- .table_replace[[1]]
    t2 <- .table_replace[[2]]

    qdapTools::lookup(s, t1, t2, missing = NULL)
  }
}

benchmark <- bench::mark(
  replace_in_string(.string, .table_replace, FALSE),
  replace_in_string(.string, .table_replace, TRUE)
) %>% select(-result)


replace_in_string2 <- function(.string, .table_replace) {
  s <- paste0(" ", trimws(.string), " ")
  t1 <- paste0(" ", trimws(.table_replace[[1]]), " ")
  t2 <- paste0(" ", trimws(.table_replace[[2]]), " ")

  for (i in 1:length(t1)) {
    s <- stringi::stri_replace_all_fixed(s, t1[i], t2[i])
  }

  trimws(s)

}

replace_in_string3 <- function(.string, .table_replace) {
  s <- paste0(" ", trimws(.string), " ")
  t1 <- paste0(" ", trimws(.table_replace[[1]]), " ")
  t2 <- paste0(" ", trimws(.table_replace[[2]]), " ")

  for (i in 1:length(t1)) {
    s <- gsub(t1[i], t2[i], s, fixed = TRUE)
  }

  trimws(s)

}

benchmark <- bench::mark(
  replace_in_string1(.string, .table_replace),
  replace_in_string2(.string, .table_replace),
  replace_in_string3(.string, .table_replace)
)
View(benchmark)
