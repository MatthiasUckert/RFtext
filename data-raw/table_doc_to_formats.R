## code to prepare `table_doc_to_formats` dataset goes here
library(tidyverse)

cmd_tool <- system.file(
  "cmdtools/docto/docto.exe",
  package = "RFtext"
)

table_doc_to_formats <- dplyr::bind_rows(
  tibble::tibble(tmp = system(paste(cmd_tool, "-H XL"), intern = TRUE)),
  tibble::tibble(tmp = system(paste(cmd_tool, "-H WD"), intern = TRUE)),
  tibble::tibble(tmp = system(paste(cmd_tool, "-H PP"), intern = TRUE))
) %>%
  filter(grepl("^xl|^wd|^pp", tmp)) %>%
  separate(tmp, c("format", "file_ext"), sep = "=") %>%
  mutate(
    document_type = case_when(
      grepl("^xl", format) ~ "excel",
      grepl("^wd", format) ~ "word",
      grepl("^pp", format) ~ "powerpoint"
    )
  )


usethis::use_data(table_doc_to_formats, overwrite = TRUE)
