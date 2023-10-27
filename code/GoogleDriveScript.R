### Data download from Google Drive ----
#Purpose: Download data from central google drive

### Packages ----
library(googledrive)

### Download data ----

# Get authorization to access google drive. Browser will prompt you to sign in and copy paste code into console.
drive_auth(reset=TRUE) #Re-run this line if you want to clear authorization

# Download data into working directory 
drive_download(as_id("1qEcbk0DmtxpG0gYnLhbxVNTJo7Z4SXFb"),path="test_file.csv",overwrite=TRUE)

# Load data
data<-read.csv('test_file.csv')




### Alternative method (no authorization required - data directly read into R with no download) ----

id <- "1qEcbk0DmtxpG0gYnLhbxVNTJo7Z4SXFb" # google file ID
data<-read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
