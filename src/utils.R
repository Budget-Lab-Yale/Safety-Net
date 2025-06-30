#--------------------------------
# utils.R 
#
# Miscellaneous helper functions
#--------------------------------

#----------------------------------------------------------------------------
# Read in and store API keys as global environment values
# 
# Parameters:
#   - keys_csv (str) : CSV file containing API keys in two columns, 'api' (name of API) and 'key'
#   - keep (chr) : list of API keys to return (default is to return all)
#
# Returns: Global environment value(s) [API name]_key
#----------------------------------------------------------------------------

parse_api_keys <- function(
    keys_csv,
    keep = NA) {
  keys <- read.csv(keys_csv) 
  for (keynum in seq(1,nrow(keys))) {
    if (any(is.na(keep), (!is.na(keep) & any(keys[keynum,'api'] %in% keep)))) {
      assign(paste0(keys[keynum,'api'],'_key'),keys[keynum,'key'], envir = .GlobalEnv)
    }
  }
}


#----------------------------------------------------------------------------
# TK: DOCUMENTATION
#----------------------------------------------------------------------------

ipums_pull <- function(
    collection,
    samples_csv,
    variables_csv,
    download_path = getwd(),
    description = as.character(Sys.time()),
    api_key = '',
    return_samples = FALSE,
    return_variables = FALSE,
    keep_files = FALSE) {
  
  #a. Check API key
  if(api_key == '') {
    if(Sys.getenv('IPUMS_API_KEY')!='') {
      api_key <- Sys.getenv('IPUMS_API_KEY')
    } else {
      stop('Missing IPUMS API key')
    }
  }
  
  #b. Pull samples/variables from CSV files
  if (return_samples == TRUE) {
    ipums_samples <<- read_csv(samples_csv, col_names = FALSE, show_col_types = FALSE) %>% deframe
  } else {
    ipums_samples <- read_csv(samples_csv, col_names = FALSE, show_col_types = FALSE) %>% deframe
  }
  if (return_variables == TRUE) {
    ipums_variables <<- read_csv(variables_csv, col_names = FALSE, show_col_types = FALSE) %>% deframe
  } else {
    ipums_variables <- read_csv(variables_csv, col_names = FALSE, show_col_types = FALSE) %>% deframe
  }  
  
  #c. Define extract
  ipums_extract_define <- define_extract_micro(collection,description,ipums_samples,ipums_variables)

  #d. Submit request
  ipums_extract_submit <- submit_extract(ipums_extract_define, api_key = api_key)
  
  #f. Wait and download files
  wait_for_extract(ipums_extract_submit, api_key = api_key)
  extract_path <- download_extract(ipums_extract_submit, download_dir = download_path, api_key = api_key)
  
  #g. Read in extract and delete files
  ipums_extract <- read_ipums_micro(extract_path)
  names(ipums_extract) <- tolower(names(ipums_extract))
  if (keep_files == FALSE) {
    unlink(extract_path)
    unlink(gsub(".xml",".dat.gz",extract_path))
  }
  
  #h. Return
  return(ipums_extract)
}
