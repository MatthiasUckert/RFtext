.path_in <- "L:/pi_documents/documents_raw/acrobat_pdf_document_text/150441821.pdf"
.dir_out <- "E:/Uckert/R_Projects/RPpi/2_output/08_extract_pdf_imgs/tmp"
.time_outs <- 600
.convert = TRUE
.max_imgs = 10
.resize = 1000
.remove_dup = TRUE
.zip = TRUE
.save = ".csv"
.max_imgs = 1000
#
#
# pdf_images(.path_in, .dir_out, TRUE, 1000, TRUE, ".rds", TRUE, 60, 10000)

#' Extract PDF Images
#'
#' @param .path_in full path to the pdf
#' @param .dir_out directory
#' @param .convert convert images to .jpg (default = TRUE)
#' @param .resize resize large images (default 1000px for the longer dimension)
#' @param .remove_dup remove duplicated picture
#' @param .save output format of the info file
#' @param .zip zip images?
#' @param .time_outs Number of Seconds to wait for each process
#' @param .max_imgs Maximal Number of Images to process
#'
#' @return Images and Info Table
#' @export
pdf_images <- function(.path_in, .dir_out, .convert = TRUE, .resize = Inf,
                       .remove_dup = FALSE, .save = c(".rds", ".csv", ".fst"),
                       .zip = FALSE, .time_outs = Inf, .max_imgs = Inf
) {


  # Get Dirs and DocIDs -----------------------------------------------------
  chr_doc_id <- gsub("\\.pdf$", "", basename(.path_in)) # Get Doc ID
  dir_main   <- file.path(.dir_out, chr_doc_id)
  dir_img    <- file.path(dir_main, paste0(chr_doc_id, "_imgs"))
  dir_root   <- file.path(dir_img, chr_doc_id) # Add doc_id prefix


  # Logging Table -----------------------------------------------------------
  tab_log <- tibble::tibble(
    doc_id = character(),
    start = Sys.time(), stop  = Sys.time(),
    process = character(), error = character(),
    .rows = 0
  )


  # Extract Images ----------------------------------------------------------
  t_start <- Sys.time()
  tab_info <- try(
    R.utils::withTimeout(
      expr = pdf_images_extract(.path_in, .dir_out),
      timeout = .time_outs,
      onTimeout = "error"
    )
  )
  t_stop <- Sys.time()

  # Check for Extraction Timout Error
  if (inherits(tab_info, "try-error")) {
    p <- "extraction"
    e <- "error-timeout"
    tab_log <- pdf_images_log_table(tab_log, t_start, t_stop, p, e, dir_main, chr_doc_id)
    unlink(dir_img, recursive = TRUE, force = TRUE)
    return(NULL)
  } else {
    p <- "extraction"
    e <- NA_character_
    tab_log <- pdf_images_log_table(tab_log, t_start, t_stop, p, e, dir_main, chr_doc_id)
  }

  # Check for no images
  if (nrow(tab_info) == 0) {
    p <- "extraction"
    e <- "error-no_images"
    tab_log <- pdf_images_log_table(tab_log, t_start, t_stop, p, e, dir_main, chr_doc_id)
    unlink(dir_img, recursive = TRUE, force = TRUE)
    return(NULL)
  }


  # Check for Sytax Errors
  if (file.exists(file.path(dir_main, paste0(chr_doc_id, "_error-syntax.txt")))) {
    p <- "extraction"
    e <- "syntax-error"
    tab_log <- pdf_images_log_table(tab_log, t_start, t_stop, p, e, dir_main, chr_doc_id)
  }

  # Check for Max Images
  if (nrow(tab_info) > .max_imgs) {
    p <- "extraction"
    e <- "error-max_images"
    tab_log <- pdf_images_log_table(tab_log, t_start, t_stop, p, e, dir_main, chr_doc_id)
    unlink(dir_img, recursive = TRUE, force = TRUE)
    return(NULL)
  }

  # Convert Images ----------------------------------------------------------
  if (.convert) {
    t_start <- Sys.time()
    tab_info <- try(
      R.utils::withTimeout(
        expr = pdf_images_convert(tab_info),
        timeout = .time_outs,
        onTimeout = "error"
      )
    )
    t_stop <- Sys.time()

    # Check for Extraction Timout Error
    if (inherits(tab_info, "try-error")) {
      p <- "conversion"
      e <- "error-timeout"
      tab_log <- pdf_images_log_table(tab_log, t_start, t_stop, p, e, dir_main, chr_doc_id)
      unlink(dir_img, recursive = TRUE, force = TRUE)
      return(NULL)
    } else {
      p <- "conversion"
      e <- NA_character_
      tab_log <- pdf_images_log_table(tab_log, t_start, t_stop, p, e, dir_main, chr_doc_id)
    }
  }


  # Resize Images -----------------------------------------------------------
  if (!is.infinite(.resize)) {
    t_start <- Sys.time()
    tab_info <- try(
      R.utils::withTimeout(
        expr = pdf_images_resize(tab_info, .resize),
        timeout = .time_outs,
        onTimeout = "error"
      )
    )
    t_stop <- Sys.time()

    if (inherits(tab_info, "try-error")) {
      p <- "resize"
      e <- "error-timeout"
      tab_log <- pdf_images_log_table(tab_log, t_start, t_stop, p, e, dir_main, chr_doc_id)
      unlink(dir_img, recursive = TRUE, force = TRUE)
      return(NULL)
    } else {
      p <- "resize"
      e <- NA_character_
      tab_log <- pdf_images_log_table(tab_log, t_start, t_stop, p, e, dir_main, chr_doc_id)
    }
  }


  # Remove Duplicates -------------------------------------------------------
  if (.remove_dup) {
    t_start <- Sys.time()
    tab_info <- try(
      R.utils::withTimeout(
        expr = pdf_images_duplicates(tab_info),
        timeout = .time_outs,
        onTimeout = "error"
      )
    )
    t_stop <- Sys.time()

    if (inherits(tab_info, "try-error")) {
      p <- "duplicates"
      e <- "error-timeout"
      tab_log <- pdf_images_log_table(tab_log, t_start, t_stop, p, e, dir_main, chr_doc_id)
      unlink(dir_img, recursive = TRUE, force = TRUE)
      return(NULL)
    } else {
      p <- "duplicates"
      e <- NA_character_
      tab_log <- pdf_images_log_table(tab_log, t_start, t_stop, p, e, dir_main, chr_doc_id)
    }
  }


  # Zip Images --------------------------------------------------------------
  if (.zip) {
    t_start <- Sys.time()
    pdf_images_zip(tab_info)
    t_stop <- Sys.time()
    p <- "zip"
    e <- NA_character_
    tab_log <- pdf_images_log_table(tab_log, t_start, t_stop, p, e, dir_main, chr_doc_id)
  }


  # Save Info ---------------------------------------------------------------
  pdf_images_save(tab_info, chr_doc_id, .save)
  return(tab_info)
}


#' Extract Images from PDF
#'
#' @param .path_in Full Path to the PDF
#' @param .dir_out Full Path to the directory to save the images
#'
#' @return Images and Dataframe with Infos
#' @export
pdf_images_extract <- function(.path_in, .dir_out) {
  path <- ext0 <- doc_id <- img_id <- width0 <- height0 <- hdpi <- vdpi <-
    colorspace <- bpc <- NULL

  cmd_tool <- system.file(
    "cmdtools/xpdf-tools-win-4.02/bin64/pdfimages.exe",
    package = "RFtext"
  )

  # Set up File Structure
  path_in    <- paste0("\"", .path_in, "\"") # Adjust for whitespaces for CMD command
  chr_doc_id <- gsub("\\.pdf$", "", basename(.path_in)) # Get Doc ID
  dir_main   <- file.path(.dir_out, chr_doc_id)
  dir_img    <- file.path(dir_main, paste0(chr_doc_id, "_imgs"))
  dir_root   <- file.path(dir_img, chr_doc_id) # Add doc_id prefix
  if (!dir.exists(dir_img)) dir.create(dir_img, recursive = TRUE)


  # Extract Images
  chr_info <- system(paste(cmd_tool, "-j -list", path_in, dir_root), intern = TRUE, wait = TRUE)

  # Look for Conversion Errors
  chr_info_adj <- chr_info[!startsWith(chr_info, "Syntax Error")]
  chr_info_err <- chr_info[startsWith(chr_info, "Syntax Error")]


  # Write Error Files
  if (length(chr_info_err) > 0) {
    write(
      x = paste0("syntax error: ", chr_info_adj),
      file = file.path(dir_main, paste0(chr_doc_id, "_error-syntax.txt"))
    )
  }

  if (length(chr_info_adj) > 0) {
    # Tidy Dataframe
    tab_info <- tibble::tibble(raw = chr_info_adj) %>%
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
        doc_id = chr_doc_id
      ) %>%
      dplyr::select(
        doc_id, img_id, ext0, width0, height0, hdpi, vdpi, colorspace, bpc, path
      )
  } else {
    tab_info <- tibble::tibble(
      doc_id = character(),
      img_id = character(),
      ext0  = character(),
      width0 = integer(),
      height0 = integer(),
      hdpi = numeric(),
      vdpi = numeric(),
      colorspace = character(),
      bpc = integer(),
      path = character()
    )
  }

  return(tab_info)

}

#' Convert Images to jpg
#'
#' @param .tab_info A Dataframe generated by pdf_images_extract()
#'
#' @return Converted Images and a Dataframe with Infos
pdf_images_convert <- function(.tab_info) {
  ext1 <- ext0 <- NULL

  tab_info <- .tab_info
  int_convert <- which(tab_info$ext0 != ".jpg")
  chr_path <- tab_info$path
  chr_path_new <- stringi::stri_replace_all_fixed(chr_path, tab_info$ext0, ".jpg")
  purrr::walk2(
    .x = chr_path[int_convert],
    .y = chr_path_new[int_convert],
    .f = ~ {
      .x %>%
        magick::image_read() %>%
        magick::image_write(.y, format = "jpeg")
      invisible(file.remove(.x))
    }
  )
  tab_info <- tab_info %>%
    dplyr::mutate(
      path = chr_path_new,
      ext1 = ".jpg"
    ) %>% dplyr::relocate(ext1, .after = ext0)

  return(tab_info)

}

#' Resize Images
#'
#' @param .tab_info A Dataframe generated by pdf_images_extract()
#' @param .resize Resized Images and a Dataframe with Infos
#'
#' @return
pdf_images_resize <- function(.tab_info, .resize = Inf) {
  width0 <- height0 <- pix_max <- width1 <- height1 <- geom <- NULL

  tab_info <- .tab_info

  tab_info <- tab_info %>%
    dplyr::mutate(
      pix_max = pmax.int(width0, height0),
      width1 = dplyr::if_else(
        pix_max > .resize,
        as.integer(width0 / (pix_max / .resize)),
        width0
      ),
      height1 = dplyr::if_else(
        pix_max > .resize,
        as.integer(height0 / (pix_max / .resize)),
        height0
      ),
      geom = paste0(width1, "x", height1)
    )

  int_resize <- which(tab_info[["pix_max"]] > .resize)
  purrr::walk2(
    .x = tab_info[["geom"]][int_resize],
    .y = tab_info[["path"]][int_resize],
    .f = ~ .y %>%
      magick::image_read() %>%
      magick::image_scale(geometry = .x) %>%
      magick::image_write(path = .y)
  )
  tab_info <- tab_info %>%
    dplyr::select(-pix_max, -geom) %>%
    dplyr::relocate(width1, .after = height0) %>%
    dplyr::relocate(height1, .after = width1)
  return(tab_info)
}

#' Deduplicate Images
#'
#' @param .tab_info A Dataframe generated by pdf_images_extract()
#'
#' @return Deleted Images and a Dataframe with Infos
pdf_images_duplicates <- function(.tab_info) {
  path <- pix <- dup <- img_id <- NULL

  tab_info <- .tab_info

  tab_info <- tab_info %>%
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

  fil_rem <- tab_info %>%
    dplyr::filter(!is.na(dup)) %>%
    dplyr::pull(path)

  if (length(fil_rem) > 0) {
    invisible(file.remove(fil_rem))
  }

  return(tab_info)
}

#' Zip Image Folder
#'
#' @param .tab_info A Dataframe generated by pdf_images_extract()
#'
#' @return zipped Images
pdf_images_zip <- function(.tab_info) {
  file_img <- .tab_info[["path"]]
  dir_img <- unique(dirname(file_img))
  file_zip <- paste0(dir_img, ".zip")

  zip::zipr(file_zip, lfc(dir_img))
  unlink(dir_img, recursive = TRUE, force = TRUE)
}

#' Save Image Info file
#'
#' @param .tab_info A Dataframe generated by pdf_images_extract()
#' @param .doc_id Document ID of the PDF
#' @param .save c(".rds", ".csv", ".fst")
#'
#' @return Saved File
#' @export
#'
#' @examples
pdf_images_save <- function(.tab_info, .doc_id, .save = c(".rds", ".csv", ".fst")) {
  dir_img   <- dirname(unique(dirname(.tab_info[["path"]])))
  path_save <- file.path(dir_img, paste0(.doc_id, "_infos", .save))
  if (.save == ".rds") {
    readr::write_rds(.tab_info, path_save)
  } else if (.save == ".csv") {
    readr::write_delim(.tab_info, path_save, ";", "")
  } else {
    fst::write_fst(.tab_info, path_save)
  }
}

#' Write Logging Table
#'
#' @param .tab_log Initial logging table
#' @param .start Start time
#' @param .stop Stop time
#' @param .process Process name
#' @param .error error name
#' @param .dir_main Main directory
#' @param .doc_id Document ID of the PDF
#'
#' @return A Dataframe
pdf_images_log_table <- function(.tab_log, .start, .stop, .process, .error, .dir_main, .doc_id) {
  tab_log <- .tab_log %>%
    dplyr::add_row(
      doc_id = .doc_id,
      start = .start,
      stop  = .stop,
      process = .process,
      error = .error
    ) %>% dplyr::distinct()

  readr::write_delim(tab_log, file.path(.dir_main, paste0(.doc_id, "_log.csv")), ";", "")
  return(tab_log)

}
