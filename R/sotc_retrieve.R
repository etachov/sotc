
#' State of the City Speeches
#' 
#' This function loads State of the City speeches from Github one paragraph per row. 
#' 
#' @param meta Include or exclude speech metadata. Excluded by default.
#' @return A data.frame with speech paragraphs in rows
#' @export
sotc_retrieve <- function(meta = FALSE) {
  
  sofc_meta <- read.csv("https://raw.githubusercontent.com/etachov/state_of_the_city/master/sotc_metadata.csv")
  
  # filter down to speeches that have been collected from 2017
  sofc_meta <- dplyr::filter(sofc_meta, collected == "Yes")

  # use the id variable to create a list of file paths
  file_paths <- paste0("https://raw.githubusercontent.com/etachov/state_of_the_city/master/text/", sofc_meta$id, ".txt")
  
  
  # and a function to read the text from github and return a data frame with the text and line numbers
  speech_txt <- function(file_path) {
    
    # file_path <- file_paths[1]
    
    df <- tibble::data_frame(text = readr::read_lines(file_path))
    
    df <- dplyr::mutate(df, 
                        line = row_number(),
                        id = gsub("https://raw.githubusercontent.com/etachov/state_of_the_city/master/text/|\\.txt", "", file_path))
    
    return(df)
    
  }
  
  # map this function over the list of file paths and return a single data.frame
  text_raw <- purrr::map_df(file_paths, speech_txt) 
  
  if(meta == FALSE) {
    return(text_raw)
  } else {
    return(dplyr::left_join(text_raw, sofc_meta))
  }
  
}




