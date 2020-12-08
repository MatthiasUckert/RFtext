library(tidyverse); library(bench)
devtools::load_all(".")


.table_terms <- tibble::tribble(
  ~ tid, ~ term,
  1, c("A", "B", "C"),
  2, c("D", "E", "F"),
  3, c("A", "B")
)

.table_text <- tibble::tribble(
  ~ doc_id, ~ sen_id, ~ text,
  1, 1, "A",
  1, 1, "B",
  1, 1, "C",
  1, 1, "D",
  1, 1, "E",
  1, 1, "F",
  1, 2, "D",
  1, 1, "E",
  1, 1, "F",
)

terms <- .table_terms
lst_text10 <- purrr::map(1:10, ~ .table_text)
tab_text10 <- dplyr::bind_rows(lst_text10)

lst_text100 <- purrr::map(1:100, ~ .table_text)
tab_text100 <- dplyr::bind_rows(lst_text100)

lst_text1000 <- purrr::map(1:1000, ~ .table_text)
tab_text1000 <- dplyr::bind_rows(lst_text1000)

tab_bench <- bench::mark(
  t10a = position_count(terms, tab_text10, sen_id),
  t10m = map_dfr(lst_text10, ~ position_count(terms, .x, sen_id)),

  t100a = position_count(terms, tab_text100, sen_id),

  t1000a = position_count(terms, tab_text1000, sen_id),
  check = FALSE,
  relative = TRUE

)

tab_bench <- bench::mark(
  t10a = position_count(terms, tab_text10, sen_id),

  t100a = position_count(terms, tab_text100, sen_id),

  t1000a = position_count(terms, tab_text1000, sen_id),
  check = FALSE,
  relative = TRUE,
  iterations = 50

)

autoplot(tab_bench)
