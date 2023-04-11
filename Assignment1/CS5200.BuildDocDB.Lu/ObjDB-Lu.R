### ChenhaoLu CS5200 Assignment1
### Author: Chenhao Lu
### Date: 2023-01-14

# install.packages("tidyverse")
library(tidyverse)

# Define global variable
globalVar <- 0
rootDir <- "docDB"

# main function
main <- function()
{
  
  ### test configDB
  # Root and path is not charater
  configDB(root = 1, path = "assignment1/newDatabase")
  configDB(root = "docDB", path = 1)
  # When root is empty
  configDB(root = "", path = "assignment1/newDatabase")
  # When path is empty
  configDB(root = "docDB", path = "")
  configDB(root = "docDB", path = "assignment1/newDatabase")
  
  ### test genObjPath
  genObjPath(root = 1, tag = "#NEU")
  genObjPath(root = "docDB", tag = "#ISEC")
  
  ### test getTags
  getTags(fileName = 1)
  getTags(fileName = "")
  getTags(fileName = "Campus#NEU#ISEC.jpg")
  getTags(fileName = "Campus.pdf#NEU#SEATTLE")
  
  ### test getFileName
  getFileName(fileName = 1)
  getFileName(fileName = "")
  getFileName(fileName = "Campus.jpg#NEU#ISEC")
  getFileName(fileName = "Campus#NEU#ISEC.pdf")
  
  ### test storeObjs
  storeObjs(folder = "/Users/gary/Desktop/CS5200/Assignment/Assignment1/CS5200.BuildDocDB.Lu/Source", 
            root = "/Users/gary/Desktop/CS5200/Assignment/Assignment1/CS5200.BuildDocDB.Lu/docDB")
  
  ### test storeObjs with verbose
  storeObjs(folder = "/Users/gary/Desktop/CS5200/Assignment/Assignment1/CS5200.BuildDocDB.Lu/Source", 
            root = "/Users/gary/Desktop/CS5200/Assignment/Assignment1/CS5200.BuildDocDB.Lu/docDB",
            verbose = TRUE)
  
  ### test clearDB
  clearDB(root = "/Users/gary/Desktop/CS5200/Assignment/Assignment1/CS5200.BuildDocDB.Lu/docDB")
  
  print ("Hello, World")
}

# Database configuration function
# Function that sets up all folders and database related structure
# @param - root, the root of the path
# @param - path, the file path
# @return - no return
configDB <- function(root = "", path = "")
{
  # Check abnormal cases
  if (!is.character(root)) stop("Input root has to be a String!")
  if (!is.character(path)) stop("Input path has to be a String!")
  if (nchar(root) == 0) stop("Input root is empty!")
  
  rootPath <- paste(path,root,sep = "/")
  rootDir <<- rootPath
}

# Tag folder generation function
# Function that returns the correctly genereated path to a tag folder
# @param - root, the root of the path
# @param - tag, the tag folder of the file
# @return - path to the tag folder
genObjPath <- function(root = "", tag = "")
{
  # Check abnormal cases
  if(!is.character(root)) stop("Input root has to be a string!")
  if(!is.character(tag)) stop("Input tag has to be a string!")
  
  resTag <- str_replace_all(tag, "#", "")
  return(paste(root,resTag,sep = "/"))
}

# Get tag function
# Function that returns a vector of tags in the filename
# @param - fileName, the file name
# @return - a vector of tags
getTags <- function(fileName = "")
{
  # Check abnormal cases
  if (!is.character(fileName)) stop("Input fileName has to be a String!")
  if (nchar(fileName) == 0) stop("Input fileName is NA!")

  # Use strsplit to split the input file name to a vector
  fileSplits <- strsplit(fileName, "(?<=.)(?=[#.])",perl = TRUE)[[1]]
  fileTags <- fileSplits[grepl("#", fileSplits)]
  return(fileTags)
}

# Get file name function
# Function that returns file name
# @param - fileName, the file name
# @return - string, file name
getFileName <- function(fileName = "")
{
  # Check abnormal cases
  if (!is.character(fileName)) stop("Input fileName has to be a String!")
  if (nchar(fileName) == 0) stop("Input fileName is NA!")
  
  fileSplits <- strsplit(fileName, "(?<=.)(?=[#.])",perl = TRUE)[[1]]
  resFileName <- fileSplits[!grepl("#", fileSplits)]
  res <- paste(resFileName, collapse = "")
  return(res)
}

# Store object function
# Function that copies all files to root folder
# @param - folder, holds all the specified documents
# @param - root, the root of the path
# @return - no return
storeObjs <- function(folder, root)
{
  # Check abnormal cases
  if (!is.character(folder)) stop("Argument folder has to be a string!")
  if (!is.character(root)) stop("Argument root has to be a string!")
  
  if (!dir.exists(folder)) stop("Folder directory is not exist!")
  if (!dir.exists(root)) stop("Root directory is not exist!")
  
  files <- list.files(path = folder, full.names = TRUE)
  for (newFile in files) {
    newTags <- getTags(newFile)
    for (aTag in newTags) {
      newTagPath <- genObjPath(root, aTag)
      dir.create(newTagPath)
      file.copy(newFile, newTagPath)
      filesB <- list.files(path = newTagPath, full.names = TRUE)
      for (resFile in filesB) {
          newFileName = getFileName(resFile)
          file.rename(resFile,newFileName)
      }
    }
  }
}

# Store object function with Verbose
# Function that copies all files to root folder
# @param - folder, holds all the specified documents
# @param - root, the root of the path
# @param - verbose, boolean, print a message if true
# @return - no return
storeObjs <- function(folder, root, verbose = TRUE)
{
  # Check abnormal cases
  if (!is.character(folder)) stop("Argument folder has to be a string!")
  if (!is.character(root)) stop("Argument root has to be a string!")
  
  if (!dir.exists(folder)) stop("Folder directory is not exist!")
  if (!dir.exists(root)) stop("Root directory is not exist!")
  
  files <- list.files(path = folder, full.names = TRUE)
  for (newFile in files) {
    newTags <- getTags(newFile)
    for (aTag in newTags) {
      newTagPath <- genObjPath(root, aTag)
      dir.create(newTagPath)
      file.copy(newFile, newTagPath)
      filesB <- list.files(path = newTagPath, full.names = TRUE)
      for (resFile in filesB) {
        newFileName = getFileName(resFile)
        file.rename(resFile,newFileName)
      }
    }
    print(paste("Copying", basename(newFileName), "to", str_replace_all(newTags, "#", "")))
  }
}

# Clear database function
# function that removes all folders and files in the folder speficified by the root
# @param - root, root path
# @return - no return
clearDB <- function(root)
{
  if (!is.character(root)) stop("Argument root has to be a string!")
  if (!dir.exists(root)) stop("Root directory is not exist!")
  
  f <- list.files(root, include.dirs = FALSE, full.names = TRUE, recursive = FALSE)
  file.remove(f)
}


main()
quit()
