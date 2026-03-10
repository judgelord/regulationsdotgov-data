#!/usr/bin/env Rscript

library(readxl)
library(pdftools)
library(here)
library(tools)

files_root <- here("data", "files")
md_root <- here("data", "md")

get_files <- function(dir_path, exclude_ext = c("png", "jpg", "jpeg", "bmp", "gif", "tif")) {
  files <- list.files(dir_path, full.names = TRUE, recursive = TRUE)
  files[!file_ext(files) %in% exclude_ext]
}

all_files <- get_files(files_root)

for (file in all_files) {
  file_ext <- tolower(file_ext(file))
  file_name <- file_path_sans_ext(basename(file))
  relative_dir <- sub(paste0("^", files_root, "/?"), "", dirname(file))
  out_dir <- file.path(md_root, relative_dir)
  out_path_base <- file.path(out_dir, file_name)

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  if (file_ext %in% c("xls", "xlsx", "xlsm")) {
    sheets <- excel_sheets(file)
    for (sheet in sheets) {
      df <- read_excel(file, sheet = sheet)
      text_length <- nchar(paste(capture.output(print(df)), collapse = " "))
      if (text_length >= 100) {
        write.csv(df, paste0(out_path_base, "_", sheet, ".csv"), row.names = FALSE)
      }
    }

  } else if (file_ext %in% c("doc", "docx", "ppt", "pptx", "wpd", "htm", "html", "rtf", "txt")) {
    md_path <- paste0(out_path_base, ".md")
    command <- sprintf('pandoc "%s" -t markdown -o "%s"', file, md_path)
    system(command)

  } else if (file_ext == "pdf") {
    content <- pdf_text(file)
    if (nchar(paste(content, collapse = " ")) >= 100) {
      writeLines(c("```", content, "```"), paste0(out_path_base, ".md"))
    }

  } else {
    content <- readLines(file, warn = FALSE)
    if (nchar(paste(content, collapse = " ")) >= 100) {
      writeLines(c("```", content, "```"), paste0(out_path_base, ".md"))
    }
  }
}

warnings()
