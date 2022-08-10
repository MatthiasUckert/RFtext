#' Extract PDF Images
#'
#' @param .path_in full path to the pdf
#' @param .dir_out directory
#' @param .convert convert images to .jpg (default = TRUE)
#' @param .resize resize large images (default 1000px for the longer dimension)
#' @param .remove_dup remove duplicated picture
#' @param .save output format of the info file
#' @param .zip zip images?
#'
#' @return
#' @export
pdf_images_old <- function(.path_in, .dir_out, .convert = TRUE, .resize = 1000,
                           .remove_dup = FALSE, .save = c(".rds", ".csv", ".fst"),
                           .zip = FALSE
) {
  cmd_tool <- system.file(
    "cmdtools/xpdf-tools-win-4.02/bin64/pdfimages.exe",
    package = "RFtext"
  )

  chr_doc_id <- gsub("\\.pdf$", "", basename(.path_in))
  dir_img <- file.path(.dir_out, chr_doc_id, paste0(chr_doc_id, "_imgs"))
  if (!dir.exists(dir_img)) dir.create(dir_img, recursive = TRUE)
  root_dir <- file.path(dir_img, chr_doc_id)

  path_in <- paste0("\"", .path_in, "\"")
  info <- system(paste(cmd_tool, "-j -list", path_in, root_dir), intern = TRUE, wait = TRUE)



  if (length(info) == 0) {
    return(NULL)
  }

  info_adj <- info[!startsWith(info, "Syntax Error")]
  info_err <- info[startsWith(info, "Syntax Error")]

  if (length(info_err) > 0) {
    write(info_err, file.path(.dir_out, chr_doc_id, paste0(chr_doc_id, "_errors.txt")))
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
    dplyr::select(
      doc_id, img_id, ext0, ext1, width0, height0, width1, height1, hdpi,
      vdpi, colorspace, bpc, path, path_new
    )

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

  info1 <- info1 %>%
    dplyr::mutate(
      pix = purrr::map_chr(
        .x = path,
        .f = ~ .x %>%
          magick::image_read() %>%
          magick::image_resize("25x25") %>%
          as.raster() %>%
          paste(., collapse = "")
      )
    ) %>%
    dplyr::group_by(pix) %>%
    dplyr::mutate(
      dup = dplyr::n() > 1,
      dup = dplyr::if_else(dup, dplyr::first(img_id), NA_character_),
      dup = dplyr::if_else(img_id == dplyr::first(dup), NA_character_, dup)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-pix)

  if (.remove_dup) {
    fil_rem <- info1 %>%
      dplyr::filter(!is.na(dup)) %>%
      dplyr::pull(path)

    if (length(fil_rem) > 0) {
      invisible(file.remove(fil_rem))
    }
  }

  if (.zip) {
    zip::zipr(
      zipfile = file.path(.dir_out, chr_doc_id, paste0(chr_doc_id, "_imgs.zip")),
      files = lfc(dir_img)
    )
    unlink(dir_img, recursive = TRUE, force = TRUE)
  }

  info1 <- dplyr::select(info1, -path)


  path_save <- file.path(.dir_out, chr_doc_id, paste0(chr_doc_id, "_infos", .save))
  if (.save == ".rds") {
    readr::write_rds(info1, path_save)
  } else if (.save == ".csv") {
    readr::write_delim(info1, path_save, ";", "")
  } else {
    fst::write_fst(info1, path_save)
  }
}

