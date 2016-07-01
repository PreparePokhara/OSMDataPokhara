require(OSMTimeLapseR)

#' Create a data.table with columns as specified, given osm file.
#' 
#' OSM file can be a .csv file, a .osm file or a .pbf file.
#' If .osm or .pbf file are input, then osmconvert must be the path to
#' the osmconvert command. A .csv file with an identical basename will be created.
#' @param osm_file The osm_file to process. Can be a .csv, .osm, or .pbf file.
#' @param osmconvert Path to the osmconvert command-line utility. See
#'          http://wiki.openstreetmap.org/wiki/osmconvert for installation.
#' @param columns Columns that osmconvert should output. Syntax from osmconvert.
#' @export
#' @return A data.table, with lat, lon, and timestamp columns.
read_OSM <- function(osm_file, osmconvert = 'osmconvert', columns = '@id @lon @lat @version @timestamp @changeset @uid @user highway amenity building name') {
  ## Verify that osm_file exists
  osm_file = normalizePath(osm_file)
  if(!file.exists(osm_file)) { stop("Could not find file: ", osm_file)}
  ## If input file is osm (xml) or pbf, make csv file in the same directory / with the same name
  if(tools::file_ext(osm_file) %in% c('osm', 'pbf')) {
    ## First, verify that osmconvert can be run from the command line
    cmd.fun = if (.Platform$OS.type == 'windows') shell else system
    tryCatch(cmd.fun(sprintf('%s -h', osmconvert), intern = TRUE, ignore.stdout = TRUE), 
             error = function(e) { stop("Could not find command: ", osmconvert )})
    ## Time to convert to csv. Pick the same basename and directory as the input file.
    osm_file_basename = tools::file_path_sans_ext(osm_file)
    cmd.fun(sprintf('%s %s --csv="%s" --csv-separator="," -o=%s.csv',
                    osmconvert, osm_file, columns, osm_file_basename))
    osm_file = sprintf("%s.csv", osm_file_basename)
    stopifnot(file.exists(osm_file)) # Verify that output happened correctly
  } else if (tools::file_ext(osm_file) != 'csv') {
    stop("File with that extension not support. Please report a bug if it should be.")
  }
  ## Finally, read the csv file, convert into a data.table, convert timestamp, and return
  dt = data.table(setNames(read.csv(osm_file, header = F, quote = ""), 
                           stringr::str_replace(unlist(strsplit(columns, " ")), "@", "")))
  dt$timestamp = ymd_hms(dt$timestamp) #dt[, timestamp := ymd_hms(timestamp)]
  dt
}
setwd("~/Programming/Projects/R/PokharaOSMStats/")

download.file(url = "https://s3.amazonaws.com/metro-extracts.mapzen.com/pokhara_nepal.osm.pbf", destfile = "pokhara.osm.pbf")
pokhara <- read_OSM("pokhara.osm.pbf")
pokhara$timestamp <- as.Date(pokhara$timestamp);
write.csv(subset(pokhara, select=c("lat", "lon","timestamp","uid")),"pokhara.osm.csv")
data_date <- as.Date("2016-05-25")

write_column_summary <- function(columnname){
  summaryDataFrame <- as.data.frame(table(subset(pokhara, select=c(columnname))))
  write.csv(summaryDataFrame, file = sprintf("data/csv/overall/%sSummary.csv",columnname) )
}

write_column_summary_month <- function(columnname){
  lastMonthData <- pokhara[pokhara$timestamp >= data_date - 30 & pokhara$timestamp <= data_date]
  summaryDataFrame <- as.data.frame(table(subset(lastMonthData, select=c(columnname))))
  summaryDataFrame <- summaryDataFrame [summaryDataFrame$Freq > 0,]
  write.csv(summaryDataFrame, file = sprintf("data/csv/last_month/%sSummary.csv",columnname) )
}


write_column_summary_week <- function(columnname){
  lastWeekData <- pokhara[pokhara$timestamp >= data_date - 7 & pokhara$timestamp <= data_date]
  summaryDataFrame <- as.data.frame(table(subset(lastWeekData, select=c(columnname))))
  summaryDataFrame <- summaryDataFrame [summaryDataFrame$Freq > 0,]
  write.csv(summaryDataFrame, file = sprintf("data/csv/last_week/%sSummary.csv",columnname) )
}


lapply(c("user","highway","building","amenity","version","timestamp"), write_column_summary)
lapply(c("user","highway","building","amenity","version","timestamp"), write_column_summary_month)
lapply(c("user","highway","building","amenity","version","timestamp"), write_column_summary_week)



# write_user_csv <- function(username){
#   userDataFrame <- pokhara[pokhara$user == username]
#   write.csv(userDataFrame, file= sprintf("data/csv/%s.csv",username))
# }
# 
# lapply(c("Nirab Pudasaini", "Ro Sun", "gaurab basnet", "Manoj Thapa","Mrnprabhat", "JasnaBudhathoki", "Parassrest", "Bishal9841"), write_user_csv)
