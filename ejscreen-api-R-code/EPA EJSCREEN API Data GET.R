#---
#title: EPA EJSCREEN API Get Data Save JSON
#
#author: Ben Millam
#
#date: November 22, 2019
#
#description: takes HIFLD prison shape files, calculates geo coordinates, and queries EPA EJSCREEN API for the EPA's set of 'environmental justice' data values
#   for a point and a specified radius; saves the results as individual JSON text files and as one single text file (a JSON array, for convenience 
#   when reading all results at once into jsonlite package.).
#   
#references:
#   API url structure: https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx?namestr=&geometry={"spatialReference":{"wkid":4326},"x":-90.43382492108161,"y":32.848323133090894}&distance=1&unit=9035&areatype=&areaid=&f=pjson
#   error handling: http://mazamascience.com/WorkingWithData/?p=912
#   'sf' package and geospatial operations in R: https://geocompr.robinlovelace.net/
#
#notes: see first 'setup' section for user config variables; with these configured, user can run all code at once.
#---


############################################################################ - setup

################################### - user-config:
#   working_directory (chr)
#   prisons_shape_file (chr): HIFLD prison shape files via https://hifld-geoplatform.opendata.arcgis.com/datasets/2d6109d4127d458eaf0958e4c5296b67_0
#   
#   radius (int): radius around coordinate point; API appears to accept fractional distances too
#   unit (chr): "9036" specifies units as kilometers, "9035" for miles
#   endpoint (chr): the endpoint URL
#   max_tries (int): the number of retries to query the API if a request isn't successful.   
#
#   collect_all_records(logical): if TRUE, will collect all supplied records in prisons_shape_file, if FALSE, will only collect first 3 (for testing)
#
#   save_files(logical): TRUE #option to save text files, FALSE will query API and keep results in a data.frame without saving text files
#   overwrite(logical): if TRUE will overwrite existing files (otherwise throws error), TRUE is useful for testing
#   subdirectory_to_save_in(chr): subdirectory_to_save_in (of one level only) to save text files to, can comment out if none desired
#   extension(chr): I'm overengineering here! note: full filename is hardcoded based on HIFLD facility ID and date e.g. "10003650-ejscreen-data-queried-2019-11-22.txt"
# 
#   verbose = TRUE

working_directory <- "C:/hack-ca-local/epa-facilities"
prisons_shape_file <- "C:/hack-ca-local/epa-facilities/Prison_Boundaries_Shapefiles/Prison_Boundaries.shp"

radius <- "1"
unit <- "9036"
endpoint <- "https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx"
max_tries <- 3

collect_all_records <- FALSE

save_files <-  TRUE
overwrite <- FALSE
subdirectory_to_save_in <- "testing"
extension <- ".txt" #yeah, overengineering...

verbose <- TRUE

################################### - load-libraries
library(httr)
library(sf)
library(tidyverse)


############################################################################ - helper-functions

################################### - savetextfile
save_text_file <- function(charstring, filename, extension, overwrite) 
{
  # """
  # Saves a text file in UTF-8 encoding, prevents overwrite if desired.
  # 
  #
  # Args:
  #   charstring (coerced to chr): Content to save.
  #   filename (chr)
  #   extension (chr)
  #   overwrite (logical) if FALSE, will error if file already exists
  # 
  # Returns:
  #   NULL
  # """
  
  filename <- paste0(filename, extension)
  
  current_encoding <- getOption("encoding")
  options("encoding" = "UTF-8")
  
  if(!overwrite) {
    if (file.exists(filename)) {
      error_message <- paste("Overwrite error; this file already exists:", filename)
      stop(error_message)
    }
  }
  
  write(charstring, file = filename)
  
  options("encoding" = current_encoding)
  
  return(NULL)
}

################################### - enterdirectory
enter_directory <- function(dirname) 
{
  # """
  # Enters a directory, if doesn't exist => creates it first.
  # 
  #
  # Args:
  #   dirname (chr): The directory name.
  # 
  # Returns:
  #   NULL
  # """
  if (dir.exists(subdirectory_to_save_in)) {
    setwd(subdirectory_to_save_in)
  } else {
    dir.create(subdirectory_to_save_in)
    setwd(subdirectory_to_save_in)
  }
  
  return(NULL)
}

################################### - ejscreen-api-function
ejscreen_api_call <- function(params, endpoint, max_tries, prison_hifld_info) 
{
  # """
  # Gets EJSCREEN results for a single location point (long/lat coordinates) from https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx
  # 
  #
  # Args:
  #   params (list): A list of API expected parameters, passed to httr::GET().
  #   endpoint (chr): the API endpoing URL.
  #   max_tries (int): the number of retries to query the API if a request isn't successful.
  #   prison_hifld_info (named chr vector): info from the hifld dataset we'll pass into JSON response, hacky, so we have something to join on when needed, e.g. FACILITYID
  #
  # Returns:
  #   list(character(),logical(),character())
  #   [['results']] (chr) the API JSON
  #   [['successflag']] (logical) TRUE if expected JSON was retrieved
  #   [['error_message']] (chr) error message if not successful
  #   [['response']] (list response) response object from httr, useful for inspecting errors
  # """
  
  trynum <- 0
  
  response <- NULL
  response_status <- character()
  expected_content <- logical()
  
  while (trynum < max_tries) {
    
    response <- GET(url = endpoint, query = params)
    
    response_status <- response[['status_code']]
    expected_content <- grepl("RAW_E_PM25", x = content(response, as="text")) #check that an expected JSON key is present, we'll handle error catching outside of the api call function
    if (expected_content) {
      break
    } else {
      error_message <- paste("Expected JSON key RAW_E_PM25 not found in response content of attempt",trynum,"of",max_tries,". Response code",response_status,"content:",content(response, as="text"))
      cat(error_message)
      Sys.sleep(1)
      trynum <- trynum + 1
    }
  }
  
  JSON_content <- content(response, as="text")
  p_facid <- prison_hifld_info["FACILITYID"]
  p_name <- prison_hifld_info["NAME"]
  p_facid <- str_replace_all(p_facid,"\"","\\\\\"") #double quotes will break the JSON structure, let's escape them, more likely in name
  p_name <- str_replace_all(p_name,"\"","\\\\\"") #double quotes will break the JSON structure, let's escape them
  
  JSON_hack_parts <- list(substr(JSON_content,1,1),"\"FACILITYID_HIFLD\":\"",p_facid,"\",","\"NAME_HIFLD\":\"",p_name,"\",",substr(JSON_content,2,nchar(JSON_content)))
  JSON_content <- do.call(paste0,JSON_hack_parts)
  
  results <- list(character(),logical(),character(),character())
  names(results) <- c("results","successflag","error_message","response")
  
  if (expected_content) {
    results[['results']] <- JSON_content
    results[['successflag']] <- TRUE
    results[['error_message']] <- NA
    results[['response']] <- response #store the whole response, useful for errors/diagnostics
    return(results)
  } else {
    results[['results']] <- NA
    results[['successflag']] <- FALSE
    results[['error_message']] <- error_message
    results[['response']] <- response
    return(results)
  }
}

################################### - collectresults
collect_results <- function(prison_row, radius, unit, save_files, overwrite, extension, subdirectory_to_save_in, verbose, endpoint, max_tries) 
{
  # """
  # A function for apply(), relying on helper functions above, it structures call parameters, queries api,
  # and saves individual JSON text file, for a single set of coordinates.
  #
  # Args:
  #   prison_row (chr vector): The data.frame row, passed as a named vector and coerced to character.
  #   see user config section at top for all others
  #
  # Returns:
  #   list(character(),logical(),character(),character()), the results; returned from ejscreen_api_call(), see that function for details
  # """  
  
  #I couldn't find any API good citizen guidelines/rate limits, we'll sleep for 1 second
  Sys.sleep(1)
  
  #note
  endpoint <- endpoint
  
  #endpoint <- "https://asdlkajsdflkjaskldjfas.com" #endpoint for testing
  
  lat <- as.character(prison_row["latitude"]) #apply is passing the row as a named vector, likely already character but just in case
  long <- as.character(prison_row["longitude"])
  
  geo_query_string <- paste0('{"spatialReference":{"wkid":4326},"x":',long,',"y":',lat,'}') #reference: '{"spatialReference":{"wkid":4326},"x":-90.43382492108161,"y":32.848323133090894}'
  
  params <- list(
    namestr = "",
    distance = radius,
    areatype = "",
    unit = unit,
    f = "json", #format
    geometry = I(geo_query_string)#wrapping in I() to avoid character conversion eg ' ' into %20, this fixed a bug where API was returning error
    #note the hardcoded spatialReference in geo_query_string above, 4326 is standard for long/lat
  )
  
  #hacky, prison id info to add to JSON results...
  prison_id_hifld <- prison_row["FACILITYID"] #we can join on this when needed
  prison_name_hifld <- prison_row["NAME"] #for convenience
  
  prison_hifld_info <- c(prison_id_hifld,prison_name_hifld)
  names(prison_hifld_info) <- c("FACILITYID","NAME")
  
  tryCatch( 
    expr = {
      
      response <- ejscreen_api_call(params = params, endpoint = endpoint, max_tries = max_tries, prison_hifld_info = prison_hifld_info)
      
      if (nchar(subdirectory_to_save_in) > 1) { #did user specify a subdirectory_to_save_in?
        enter_directory(subdirectory_to_save_in)
      }
      
      datequeried <- Sys.Date()
      filename <- paste0(prison_row["FACILITYID"],'-ejscreen-data-queried-',datequeried) #save_text_file will append user config'd extension
      
      save_text_file(response[['results']], filename = filename, extension = extension, overwrite = overwrite)
      
      return(response) #list of 'results' char, 'successflag' logical, 'error_message', 'request_url'
      
    },
    warning = function(w) {
      if (verbose) {
        message(w)
      }
    },
    error = function(e) {
      if (verbose) {
        message(e)
      }
      results <- list(character(),logical(),character(),character())
      names(results) <- c("results","successflag","error_message","response")
      results[['results']] <- NA
      results[['successflag']] <- FALSE
      results[['error_message']] <- e
      results[['response']] <- NA
      
      return(results)
    },
    finally = {
      #are we in subdirectory_to_save_in?
      if (basename(getwd()) == subdirectory_to_save_in) {
        #move back up to our original working directory
        setwd('..')
      }
    }
  )
}

############################################################################ - read-in-prisons-and-collect-data
#read prisons shape file from config, returns class "sf" "data.frame", dfs with extra info for sf package
prisons <- st_read(prisons_shape_file, stringsAsFactors=FALSE) 

#calculate centroids of prison boundaries
prisons <- st_transform(prisons, crs = 32617) %>% #convert to utm for calculating centroids
  st_centroid() %>% #centroids from original multipolygons
  st_transform(crs = 4326) #back to 4326 for long/lat coordinates

#add columns for coordinates
prisons$longitude <- st_coordinates(prisons)[,1] #st_coordinates returns a matrix of long/lat
prisons$latitude <- st_coordinates(prisons)[,2]


#set number of records to collect
numrecords <- integer()
if (collect_all_records) {
  numrecords <- 1:nrow(prisons)
} else {
  numrecords <- 1:3
}

#collect results!
time_estimate = max(numrecords) * 2.1 / 60^2 #calculate seconds to hours; estimate is very rough, based on 2 sec system sleep time and arbitrary API ping estimate

message("Starting to collect ", max(numrecords)," records with an estimated time of ", time_estimate, " hours to complete.")
run_query <- apply(prisons[numrecords,],
                   MARGIN = 1,
                   
                   FUN = collect_results, #the function doin' the work
                   
                   radius = radius, #user config'd
                   unit = unit,
                   save_files = save_files,
                   overwrite = overwrite,
                   extension = extension,
                   subdirectory_to_save_in = subdirectory_to_save_in, 
                   verbose = verbose,
                   endpoint = endpoint,
                   max_tries = max_tries
                   )

#put results in a data.frame
run_query <- as.data.frame(do.call(rbind, run_query))

#check for errors
message("Data collection complete.")
Sys.sleep(2)
message("Now checking for errors...")
Sys.sleep(2)
successvector <- as.logical(run_query$successflag) #need convert df list column of logicals into logical vector 
message("There were ",sum(!successvector)," errors out of ",nrow(prisons[numrecords,])," records.") #error count
Sys.sleep(3)
message("The error rate was ",1 - mean(successvector),".") #error rate
Sys.sleep(2)
message("The 'run_query' has a success flag column indicating which records errored.") #error rate

#save all results to one file in a JSON array, later fromJSON will output a nice dataframe vs having to read individual files and rbind etc.
save_index <- !is.na(run_query['results']) #we don't want to write the error records to our one_big_string
one_big_string <- do.call(paste, c(list(run_query['results'][save_index,]), collapse = ",\n")) #paste and do.call, I don't understand, need list() a list to get desired single string...
one_big_string <- paste0('[\n',one_big_string,'\n]')

#save file  
if (nchar(subdirectory_to_save_in) > 0) {
  enter_directory(subdirectory_to_save_in)
}
datequeried <- Sys.Date()
filename <- paste0('ALL-FACILITIES-ONE-FILE-ejscreen-data-queried-',datequeried)
save_text_file(one_big_string, filename = filename, extension = extension, overwrite = overwrite)
#are we in subdirectory_to_save_in?
if (basename(getwd()) == subdirectory_to_save_in) {
        #move back up to our original working directory
        setwd('..')
}