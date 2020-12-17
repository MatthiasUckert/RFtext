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

  if (length(angle) == 1) {
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
