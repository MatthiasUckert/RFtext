# Dataset -----------------------------------------------------------------
table_string1 <- tibble::tribble(
  ~name, ~search, ~replace, ~example, ~call,
  "asciify", NA_character_, NA_character_, "ÄÖÜ", function(.x) stringi::stri_trans_general(.x, "latin-ascii"),
  "to lower", NA_character_, NA_character_, "AAA", base::tolower,
  "to upper", NA_character_, NA_character_, "aaa", base::toupper,
  "to title", NA_character_, NA_character_, "hello there", stringr::str_to_title,
  "to sentence", NA_character_, NA_character_, "hello there", stringr::str_to_sentence,
  "trim", NA_character_, NA_character_, "  hello  ", base::trimws
)

table_string1 <- table_string1 %>%
  dplyr::mutate(
    example = purrr::map_chr(
      .x = 1:nrow(table_string1),
      .f = ~ paste0(
        table_string1$example[.x], " -> ",
        table_string1$call[[.x]](table_string1$example[.x])
      )
    )
  )

table_string2 <- tibble::tribble(
  ~name, ~search, ~replace, ~example, ~call,
  "remove control characters (non-space)", "([[:cntrl:]])+", "", "A\x01A", stringi::stri_replace_all_regex,
  "remove control characters (space)", "([[:cntrl:]])+", " ", "A\x01A", stringi::stri_replace_all_regex,

  "remove space characters (non-space)", "([[:space:]]|[[:blank:]])+", "", "A   A", stringi::stri_replace_all_regex,
  "remove space characters (space)", "([[:space:]]|[[:blank:]])+", " ", "A   A", stringi::stri_replace_all_regex,

  "standardize quotes", "[\x22\xB4\x60]+", "'", "A´s A`s", stringi::stri_replace_all_regex,

  "remove hyphens (non-space)", "(\\s+)?[\x2D\x96-\x97]+(\\s+)?+", "", "cash-flow", stringi::stri_replace_all_regex,
  "remove hyphens (space)", "(\\s+)?[\x2D\x96-\x97]+(\\s+)?+", " ", "cash-flow", stringi::stri_replace_all_regex,
  "standardize hyphens (non-space)", "(\\s+)?[\x2D\x96-\x97]+(\\s+)?+", "-", "cash  -  flow", stringi::stri_replace_all_regex,
  "standardize hyphens (space)", "(\\s+)?[\x2D\x96-\x97]+(\\s+)?+", " - ", "cash  -  flow", stringi::stri_replace_all_regex,

  "remove slash (non-space)", "(\\s+)?/+(\\s+)?+", "", "cash/flow", stringi::stri_replace_all_regex,
  "remove slash (space)", "(\\s+)?/+(\\s+)?+", " ", "cash/flow", stringi::stri_replace_all_regex,
  "standardize slash (non-space)", "(\\s+)?/+(\\s+)?+", "/", "cash  /  flow", stringi::stri_replace_all_regex,
  "standardize slash (space)", "(\\s+)?/+(\\s+)?+", " / ", "cash  /  flow", stringi::stri_replace_all_regex,

  "remove back slash (non-space)", "(\\s+)?\\\\+(\\s+)?+", "", "cash\\flow", stringi::stri_replace_all_regex,
  "remove back slash (space)", "(\\s+)?\\\\+(\\s+)?+", " ", "cash\\flow", stringi::stri_replace_all_regex,
  "standardize back slash (non-space)", "(\\s+)?\\\\+(\\s+)?+", "\\\\", "cash  \\  flow", stringi::stri_replace_all_regex,
  "standardize back slash (space)", "(\\s+)?\\\\+(\\s+)?+", " \\\\ ", "cash  \\  flow", stringi::stri_replace_all_regex,

  "standardize brackets", "(\\s+)?([\\(|\\[])+(\\s+)?(.+)(\\s+)?([\\)\\]])(\\s+)?", " $2$4$6 ", "( test ]", stringi::stri_replace_all_regex,
)

table_string2 <- tibble::tribble(
  ~name, ~search, ~replace, ~example, ~call,
  "remove control characters (non-space)", "([[:cntrl:]])+", "", "A\x01A", gsub_regex,
  "remove control characters (space)", "([[:cntrl:]])+", " ", "A\x01A", gsub_regex,

  "remove space characters (non-space)", "([[:space:]]|[[:blank:]])+", "", "A   A", gsub_regex,
  "remove space characters (space)", "([[:space:]]|[[:blank:]])+", " ", "A   A", gsub_regex,

  "standardize quotes", "[\x22\xB4\x60]+", "'", "A´s A`s", gsub_regex,

  "remove hyphens (non-space)", "(\\s+)?[\x2D\x96-\x97]+(\\s+)?", "", "cash-flow", gsub_regex,
  "remove hyphens (space)", "(\\s+)?[\x2D\x96-\x97]+(\\s+)?", " ", "cash-flow", gsub_regex,
  "standardize hyphens (non-space)", "(\\s+)?[\x2D\x96-\x97]+(\\s+)?", "-", "cash  -  flow", gsub_regex,
  "standardize hyphens (space)", "(\\s+)?[\x2D\x96-\x97]+(\\s+)?", " - ", "cash  -  flow", gsub_regex,

  "remove slash (non-space)", "(\\s+)?/+(\\s+)?", "", "cash/flow", gsub_regex,
  "remove slash (space)", "(\\s+)?/+(\\s+)?", " ", "cash/flow", gsub_regex,
  "standardize slash (non-space)", "(\\s+)?/+(\\s+)?", "/", "cash  /  flow", gsub_regex,
  "standardize slash (space)", "(\\s+)?/+(\\s+)?", " / ", "cash  /  flow", gsub_regex,

  "remove back slash (non-space)", "(\\s+)?\\\\+(\\s+)?", "", "cash\\flow", gsub_regex,
  "remove back slash (space)", "(\\s+)?\\\\+(\\s+)?", " ", "cash\\flow", gsub_regex,
  "standardize back slash (non-space)", "(\\s+)?\\\\+(\\s+)?", "\\\\", "cash  \\  flow", gsub_regex,
  "standardize back slash (space)", "(\\s+)?\\\\+(\\s+)?", " \\\\ ", "cash  \\  flow", gsub_regex,

  "standardize round bracket left", "( ", " (", " ( test", gsub_fixed,
  "standardize round bracket right", " )", ") ", "test )", gsub_fixed,
  "standardize square bracket left", "[ ", " [", " [ test", gsub_fixed,
  "standardize square bracket right", " ]", "] ", "test ]", gsub_fixed,

  "standardize plus", "+", " and ", "A+B", gsub_fixed,
  "standardize ampersand", "&", " and ", "A&B", gsub_fixed,

  "standardize comma", "(\\s+)?(?<=[a-zA-Z\\s]),", ", ", "Assets,200,000", stringi::stri_replace_all_regex,
  "standardize colon", ":", ": ", "Test:A", gsub_fixed,
  "standardize semicolon", ";", "; ", "Test;A", gsub_fixed,

  "standardize currencies", "(\\p{Sc})(\\s+)?([\\d,\\.]+)", "$1$3", "€  200,000, $  200.000", stringi::stri_replace_all_regex,

  "remove 's", "'s", "", "Auditor's", gsub_fixed,

  "remove punctuation (all)", "[[:punct:]]", " ", "(Test, A: B=C)", gsub_regex,
  "remove punctuation (beginning)", "^[[:punct:]]", " ", "(Test, A: B=C)", gsub_regex,
  "remove punctuation (end)", "[[:punct:]]$", " ", "(Test, A: B=C)", gsub_regex,

  "escape regex", "([[:punct:]])", "\\\\$1", "(Test.)", stringi::stri_replace_all_regex
)

table_string2 <- table_string2 %>%
  dplyr::mutate(
    example = purrr::map_chr(
      .x = 1:nrow(table_string2),
      .f = ~ paste0(
        table_string2$example[.x], " <=> ",
        table_string2$call[[.x]](table_string2$example[.x], table_string2$search[.x], table_string2$replace[.x])
      )
    )
  )


table_string <- dplyr::bind_rows(table_string1, table_string2)
usethis::use_data(table_string, overwrite = TRUE)


