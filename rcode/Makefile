%.md: %.Rmd
	R -e 'doc<-rmarkdown::md_document();doc[["post_processor"]]=function (metadata, input_file, output_file, clean, verbose) { output_str <- readLines(output_file, warn = FALSE, encoding = "UTF-8"); image_relative <- function(img_src, src) {in_file <- utils::URLdecode(src); if (grepl("^[.][.]", in_file)) return(img_src); if (length(in_file) && file.exists(in_file)) { img_src <- sub(src,paste0("data:image/png;base64,",base64enc::base64encode(in_file)), img_src, fixed = TRUE) };  img_src  } ;  output_str <- rmarkdown:::process_images(output_str, image_relative); writeLines(output_str, output_file, useBytes = TRUE); output_file}; rmarkdown::render("$<", output_format=doc)'


