pacman::p_load(tidyverse)

check_pep725_species <- function(species = NULL,
                                 list = FALSE){
  
  # grab species info from the data selection page
  # this does not require a login and will be used
  # to check the validity of the selected species number
  # or name
  data_selection <- httr::GET("http://www.pep725.eu/data_download/data_selection.php")
  
  # extract the species numbers and names from the page
  number <- xml2::read_html(data_selection) %>%
    rvest::html_nodes("form select") %>%
    rvest::html_children() %>%
    rvest::html_attr("value") %>%
    as.numeric()
  
  name <- xml2::read_html(data_selection) %>%
    rvest::html_nodes("form select") %>%
    rvest::html_children() %>%
    rvest::html_text() %>%
    as.character() %>%
    tolower()
  
  # combine the data in a species info data frame
  species_info <- data.frame(number, name)
  
  # provide verbose output listing all
  # species names and numbers
  if(list){
    if (is.null(species)){
      return(species_info)
    } else {
      print(species_info,
            row.names = FALSE)
    }
  }
  
  # if the input is a character vector grep for results
  # this will work on partial matches as well
  if(is.character(species)){
    numbers <- species_info$number[grep(paste(tolower(species),collapse = "|"),
                                        species_info$name)]
    if(length(numbers)==0){
      stop("Species (number) not listed in PEP725 database!")
    } else {
      return(numbers)
    }
  }
  
  # if the input is a vector of numbers check if they are in
  # the list (and properly numeric)
  if (is.numeric(species) & all(species %in% species_info$number)){
    return(species)
  } else {
    stop("Species (number) not listed in PEP725 database!")
  }
}


pr_dl_pep725 <- function(
    credentials = "C:/Users/LENOVO/Desktop/lab/credentials.txt",
    species = 115,
    path = "C:/Users/LENOVO/Desktop/lab/PEP",
    internal = FALSE
){
  
  # check the validity of the species, return list of
  # numbers to query or stop()
  species_numbers = check_pep725_species(species = species)
  
  # check if email / password or credential file is available
  if(any(!file.exists(credentials) & missing(credentials))){
    stop("Credentials file not given or does not exist, check path !")
  } else {
    credentials = as.vector(unlist(utils::read.table(
      credentials, stringsAsFactors = FALSE)))
    email = credentials[1]
    password = credentials[2]
  }
  
  # create login form credentials or generated them from a file
  # the latter is preferred
  login <- list(
    email = email,
    pwd = password,
    submit = "Login"
  )
  
  # login to set cookie (will not expire until end of session)
  httr::POST("http://www.pep725.eu/login.php",
             body = login,
             encode = "form")
  
  # download data for all species number(s), will merge
  # data if internal = TRUE otherwise return NULL
  all_pep_data = do.call("rbind",lapply(species_numbers, function(number){
    
    # select the species of interest and pull the table listing
    # all download files
    species_html = httr::POST(
      "http://www.pep725.eu/data_download/data_selection.php",
      body = list(
        plant = number,
        submit1 = "Submit"),
      encode = "form")
    
    # extract the links to download
    species_links = xml2::read_html(species_html) %>%
      rvest::html_nodes("td a") %>%
      rvest::html_attr("href")
    
    # loop over all files for all countries
    # of a particular species, download the (zipped) files
    # select the relevant data, and unzip to specified path
    # use merge_pep725() to merge the downloaded data
    # into a tidy data format
    do.call("rbind",lapply(species_links, function(link){
      
      # create a temporary file
      tmp = tempfile()
      
      # download all files for a specific species
      httr::GET(link, httr::write_disk(path = tmp,
                                       overwrite = TRUE))
      
      # only return data if internal processing is TRUE
      # (file.rename might not be consistent in behaviour
      # hence file.copy() / file.remove() )
      if (internal){
        pep_data = pr_merge_pep725(path = tmp)
        file.remove(tmp)
        return(pep_data)
      } else {
        file.copy(tmp, sprintf("%s/PEP725_%s.tar.gz",
                               path,
                               strsplit(link,"=")[[1]][2]),
                  overwrite = TRUE)
        file.remove(tmp)
      }
    }))
  }))
  
  # return results if internal flag is set
  # this suppresses file.remove() feedback
  # which
  if (internal){
    return(all_pep_data)
  }
}

species_file <- "C:/Users/LENOVO/Desktop/lab/species.txt"
species_names <- readLines(species_file)

# 遍历物种名列表，批量下载数据
for (species_name in species_names) {
  cat("Downloading data for species:", species_name, "\n")
  pr_dl_pep725(species = species_name)
}


# species_list = c("Fagus")
# for (i in species_list) {
#  pr_dl_pep725(species = i)
# }
