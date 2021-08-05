
#' extract_md_sections2
#'
#' Extract text when there are nested parentheses
#' @param string string to extract md sections from

extract_md_sections2 <- function(string){
  pattern = r"(\[(?!\s*\@).*?\]\{#.+?\})"
  nested_pattern = "\\[.*\\[(?!.*\\@)"
  end = "\\]\\{\\#\\w+\\}"
  clean_end = "[\\]\\{\\#\\}]*"

  section_list <- list()

  while(grepl(pattern, string, perl = TRUE)){

  extracted_text <- stringr::str_match(string, pattern)

  if(grepl(nested_pattern, extracted_text, perl = TRUE)){
   extracted_text  <- gsub(nested_pattern, "", extracted_text, perl = TRUE)
   extracted_text  <- paste0("[",extracted_text)
  }
  section_content <- gsub(end, "", extracted_text, perl = TRUE)
  section_content <- substr(section_content,2,nchar(section_content))
  section <- stringr::str_extract(extracted_text, end)
  section <- gsub(clean_end,"",section, perl = TRUE)
  out <- data.frame(tag = section, section = section_content)

  string <- gsub(extracted_text, section_content, string, fixed = TRUE)

  section_list  <- append(section_list, list(out))

  }

    do.call(rbind, section_list)

}
