#' List Files as a Named Character
#'
#' @param .dirs Paths to the directories
#' @param reg RegEx to find files (defaults to '*' all files)
#' @param rec Should the directories be searched recursively?
#'
#' @return A named character
#' @export
lfc <- function(.dirs, reg = "*", rec = FALSE) {
  fils <- unlist(purrr::map(.dirs, ~ list.files(.x, reg, F, T, rec)))
  names(fils) <- stringi::stri_replace_all_fixed(
    str = basename(fils),
    pattern = paste0(".", tools::file_ext(fils)),
    replacement = ""
  )
  return(fils)
}

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

#' Rotate PDF Pages
#'
#' @param .path_in full path to the pdf
#' @param .path_out full path to the new pdf file
#' @param .angle either a single value (-270, -180, -90, 90, 180, 270) or a integer vector of the same length as pages
#' @param .pages an integer vector
#'
#' @return a pdf
#' @export
pdf_rotate <- function(.path_in, .path_out, .angle, .pages) {
  cmd_tool <- system.file(
    "cmdtools/qpdf-10.0.4/bin/qpdf.exe",
    package = "RFtext"
  )

  if (length(.angle) > 1 & length(.angle) != length(.pages)) {
    stop("angle must be length 1 or must have the same length as pages", call. = FALSE)
  }

  if (length(.angle) == 1) {
    angle <- ifelse(.angle < 0, as.character(.angle), paste0("+", .angle))
    pages <- paste(.pages, collapse = ",")
    rotation <- glue::glue("--rotate={angle}:{pages}")

  } else {
    rotation <- paste(
      purrr::map2_chr(.angle, .pages, ~ glue::glue("--rotate={.x}:{.y}")),
      collapse = " "
    ) %>% as.character()
  }
  system(paste(cmd_tool, rotation, .path_in, .path_out))

}


#' Wrapper around pdftools::pdf_convert
#'
#' @param .path_in file path or raw vector with pdf data
#' @param .dir_out directory to store images
#' @param .format string with output format such as "png" or "jpeg". Must be equal to one of poppler_config()$supported_image_formats.
#' @param .pages vector with one-based page numbers to render. NULL means all pages.
#' @param .dpi resolution (dots per inch) to render
#' @param .antialias enable antialiasing. Must be "text" or "draw" or TRUE (both) or FALSE (neither).
#' @param .opw owner password
#' @param .upw user password
#' @param .verbose print some progress info to stdout
#' @param .pad how many leading zeros
#'
#' @return Images
#' @export
pdf_convert <- function(.path_in, .dir_out, .format = "png", .pages = NULL,
                        .dpi = 72, .antialias = TRUE, .opw = "", .upw = "",
                        .verbose = TRUE, .pad = 4) {
  if (!dir.exists(.dir_out)) dir.create(.dir_out, recursive = TRUE)

  base_file <- gsub("\\.pdf", "", basename(.path_in))

  if (is.null(.pages)) {
    base_files <- paste0(
      base_file, "-", stringr::str_pad(1:(qpdf::pdf_length(.path_in)), .pad, pad = 0), ".", .format
    )
  } else {
    base_files <- paste0(
      base_file, "-", stringr::str_pad(.pages, .pad, pad = 0), .format
    )
  }

  pdftools::pdf_convert(
    pdf = .path_in,
    format = .format,
    pages = .pages,
    filenames = file.path(.dir_out, base_files),
    dpi = .dpi,
    antialias = .antialias,
    opw = .opw,
    upw = .upw,
    verbose = .verbose
  )

}


#' Convert PDF to txt with xpdftools
#'
#' @param .path_in full path to the pdf
#' @param .path_out full path to the new txt file
#'
#' @return A text file
#' @export
#'
#' @examples
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
    expr = system(paste(cmd_tool, path_in, path_out), wait = FALSE),
    silent = TRUE
  )

}


#' Batch Convert PDF to Text
#'
#' @param .dir_in full path to the directory with the pdfs
#' @param .dir_out full path to the directory to store txt files
#' @param .paths_in if .dir_in = NULL, vector with file paths of the pdf
#' @param .paths_out if .dir_out = NULL, vector with file paths of the pdf
#' @param .inst max instances of pdftotex.exe to run
#'
#' @return text files
#' @export
pdf_to_txt_batch <- function(.dir_in = NULL, .dir_out = NULL, .paths_in = NULL,
                             .paths_out = NULL, .inst = 20) {
  if (!is.null(.dir_in) & !is.null(.dir_out)) {
    paths_in <- lfc(.dir_in, "\\.pdf$", rec = TRUE)
    paths_out <- stringi::stri_replace_first_fixed(
      str = paths_in,
      pattern = paste0("/", basename(.dir_in), "/"),
      replacement = paste0("/", basename(.dir_out), "/")
    )
    paths_out <- gsub("\\.pdf$", ".txt", paths_out)
  } else {
    paths_in <- .paths_in
    paths_out <- .paths_out
  }

  dir_names <- unique(dirname(paths_out))
  for (i in 1:length(dir_names)) {
    if (!dir.exists(dir_names[i])) dir.create(dir_names[i], recursive = TRUE)
  }

  check_inst <- function() {
    sum(stringi::stri_count_fixed(system("tasklist", intern = T), "PDFTOT"))
  }

  for (j in 1:length(paths_in)) {
    pdf_to_txt(paths_in[j], paths_out[j])

    if (j %% .inst == 0) {
      while (check_inst() > .inst / 4) {
        Sys.sleep(1)
      }
    }
  }
}


#' Decryp PDFs
#'
#' @param .path_in full path to the pdf
#' @param .path_out full path to the new pdf file
#'
#' @return A pdf
#' @export
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
