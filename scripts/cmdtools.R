list.files(system.file("extdata/pdf", package = "RFtext"))
RFtex::pdf_to_txt()

lfc <- function(.dirs, reg = "*", rec = FALSE) {
  fils <- unlist(purrr::map(.dirs, ~ list.files(.x, reg, F, T, rec)))
  names(fils) <- stringi::stri_replace_all_fixed(
    str = basename(fils),
    pattern = paste0(".", tools::file_ext(fils)),
    replacement = ""
  )
  return(fils)
}


.dir_pdf <- system.file("extdata/pdf", package = "RFtext")
.dir_dest <- "E:/test_pdf"

.path_in = "E:/R/R_projects/RPDatastreamPulls/0_data/pull_2020_12_28/pull_2020_12_28_codes/ARE.xls"
.path_out = "E:/R/R_projects/RPDatastreamPulls/0_data/pull_2020_12_28/pull_2020_12_28_codes/ARE.xlsx"



pdf_to_txt <- function(.path_in = NULL, .path_out = NULL) {
  cmd_tool <- system.file(
    "cmdtools/xpdf-tools-win-4.02/bin64/pdftotext.exe",
    package = "RFtext"
  )

  if (!dir.exists(dirname(.path_out))) {
    dir.create(dirname(.path_out), recursive = TRUE)
  }

  path_in <- paste0("\"", .path_in, "\"")
  path_out <- paste0("\"", .path_out, "\"")

  try(
    expr = system(paste(cmd_tool, path_in, path_out), ignore.stderr = TRUE),
    silent = TRUE
  )

}

.path_in <- "E:/Uckert/R_Projects/RFtext/inst/extdata/pdf/encryption_nocopy.pdf"
.path_out <- "E:/test_pdf/encryption_nocopy.pdf"

pdf_decrypt <- function(.path_in, .path_out) {
  cmd_tool <- system.file(
    "cmdtools/qpdf-10.0.4/bin/qpdf.exe",
    package = "RFtext"
  )

  if (!dir.exists(dirname(.path_out))) {
    dir.create(dirname(.path_out), recursive = TRUE)
  }

  path_in <- paste0("\"", .path_in, "\"")
  path_out <- paste0("\"", .path_out, "\"")

  .catch <- try(
    expr = system(paste(cmd_tool, "--decrypt", path_in, path_out), ignore.stderr = TRUE),
    silent = TRUE
  )

}

system(paste(cmd_tool, "--help"))


pdf_info <- function(.path_in) {
  lst <- pdftools::pdf_info(.path_in)
  purrr::flatten_dfc(lst) %>%
    dplyr::mutate(
      created = lst$created,
      modified = lst$modified
    )
}

.path_in <- "E:/test_pdf/63452296.pdf"
.dir_out <- "E:/test_pdf/63452296"

.path_img <- "E:/test_pdf/test/63452296-0020.ppm"
img <- magick::image_read(.path_img)
magick::image_write(img, "E:/test_pdf/test/63452296-0020.jpg", format = "jpeg")

path_pdfs <- RFgen::lfc("E:/test_pdf/pdf")
.path_in <- path_pdfs[names(path_pdfs) == "62859605"]
.dir_out <- dir_out <- "E:/test_pdf/img"



path_pdfs <- RFgen::lfc("E:/test_pdf/pdf")
dir_out <- "E:/test_pdf/img"


plan("multiprocess", workers = 20)
future_walk(path_pdfs, ~ pdf_images(.x, dir_out, TRUE, 1000, ".fst"),
            .options = furrr_options(seed = TRUE, scheduling = Inf))
plan("default")
