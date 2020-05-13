library(stringr)

#Create a function to read in a variety of Wind Cube data files by date range and extension type. 
#Does not currently read in .gyro data. Works for .sta, .rtd, .stdrtd, and .stdsta
#directory is path where unzipped files are located
#fromDate is the beginning date in YYYYMMDD format 
#toDate is the ending date in YYYYMMDD format
#extension is one of the file extensions listed above. 

read_windcube_data <- function(directory, fromDate, toDate, extension) { 
  save_wd = getwd()
  setwd(directory)
  #Get a list of files of the extension type in the directory
  matching.Files = list.files(path = directory, pattern = paste("\\.",extension,"$", sep = ""), ignore.case = TRUE)
  
  #Check to see if they contain the strings fromDate and toDate and get index of those files in matching.Files
  fromIdx = which(str_detect(matching.Files, fromDate))
  toIdx = which(str_detect(matching.Files, toDate))
  
  #Update the file list to be only the included files
  matching.Files = matching.Files[fromIdx:toIdx]
  myFiles = lapply(matching.Files, read.table, sep = "\t", skip = 41, header = FALSE, check.names = FALSE,stringsAsFactors=FALSE,encoding = "latin1", fill = TRUE)
  df <- as.data.frame(do.call(rbind, myFiles))
  headerRow = df[1,]
  df = df[-which(df$V1 == df$V1[1]),]
  names(df) = headerRow
  setwd(save_wd)
  row.names(df) = NULL
  return(df)
}



