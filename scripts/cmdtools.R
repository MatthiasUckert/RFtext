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
pdf_images <- function(.path_in, .dir_out, .convert = TRUE, .resize = 1000, .save = c(".rds", ".csv", ".fst")) {
  cmd_tool <- system.file(
    "cmdtools/xpdf-tools-win-4.02/bin64/pdfimages.exe",
    package = "RFtext"
  )



  chr_doc_id <- gsub("\\.pdf$", "", basename(.path_in))
  dir_img <- file.path(.dir_out, chr_doc_id, "imgs")
  if (!dir.exists(dir_img)) dir.create(dir_img, recursive = TRUE)
  root_dir <- file.path(dir_img, chr_doc_id)

  path_in <- paste0("\"", .path_in, "\"")
  info <- system(paste(cmd_tool, "-j -list", path_in, root_dir), intern = TRUE, wait = TRUE)

  if (length(info) == 0) return(NULL)

  info_adj <- info[!startsWith(info, "Syntax Error")]
  info_err <- info[startsWith(info, "Syntax Error")]



  if (length(info_err) > 0) {
    write(info_err, file.path(.dir_out, chr_doc_id, paste0(chr_doc_id, "error.txt")))
  }

  info1 <- tibble::tibble(raw = info_adj) %>%
    dplyr::mutate(raw = gsub(" \\w+\\=", " ", raw)) %>%
    tidyr::separate(
      col = raw,
      into = c("path", "page", "width0", "height0", "hdpi", "vdpi", "colorspace", "bpc"),
      sep = ": | ",
      convert = TRUE
      ) %>%
    dplyr::mutate(
      ext0 = paste0(".", tools::file_ext(path)),
      img_id = stringi::stri_replace_all_fixed(basename(path), ext0, ""),
      path_new = stringi::stri_replace_all_fixed(path, ext0, ".jpg"),
      width1 = width0, height1 = height0, ext1 = ext0,
      doc_id = chr_doc_id
      ) %>%
    dplyr::select(doc_id, img_id, ext0, ext1, width0, height0, width1, height1, hdpi,
                  vdpi, colorspace, bpc, path, path_new)

  if (.convert) {
    int_convert <- which(info1$ext0 != ".jpg")
    purrr::walk2(
      .x = info1[["path"]][int_convert],
      .y = info1[["path_new"]][int_convert],
      .f = ~ magick::image_write(magick::image_read(.x), .y, format = "jpeg")
    )
    invisible(file.remove(info1[["path"]][int_convert]))
    info1[["path"]] <- info1[["path_new"]]
    info1[["ext1"]] <- ".jpg"
  }

  info1 <- dplyr::select(info1, -path_new)

  if (.resize > 0) {
    info1 <- info1 %>%
      dplyr::mutate(
        pix_max = pmax.int(width0, height0),
        width1 = dplyr::if_else(pix_max > .resize, as.integer(width0 / (pix_max / .resize)), width0),
        height1 = dplyr::if_else(pix_max > .resize, as.integer(height0 / (pix_max / .resize)), height0),
        geom = paste0(width1, "x", height1)
      )

    int_rs <- which(info1[["pix_max"]] > .resize)
    purrr::walk2(
      .x = info1[["geom"]][int_rs],
      .y = info1[["path"]][int_rs],
      .f = ~ .y %>%
      magick::image_read() %>%
      magick::image_scale(geometry = .x) %>%
      magick::image_write(path = .y)
    )
    info1 <- dplyr::select(info1, -pix_max, -geom)
  }

  path_save <- file.path(.dir_out, chr_doc_id, paste0(chr_doc_id, .save))
  if (.save == ".rds") {
    readr::write_rds(info1, path_save)
  } else if (.save == ".csv") {
    readr::write_delim(info1, path_save, ";", "")
  } else {
    fst::write_fst(info1, path_save)
  }
}


path_pdfs <- RFgen::lfc("E:/test_pdf/pdf")
dir_out <- "E:/test_pdf/img"


plan("multiprocess", workers = 20)
future_walk(path_pdfs, ~ pdf_images(.x, dir_out, TRUE, 1000, ".fst"),
            .options = furrr_options(seed = TRUE, scheduling = Inf))
plan("default")
