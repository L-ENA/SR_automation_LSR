install.packages("mongolite")


library(mongolite)
#dat = read.csv("C:\\Users\\xf18155\\OneDrive - University of Bristol\\MyFiles-Migrated\\Documents\\SR automation review\\data extraction\\test.csv", stringsAsFactors = FALSE, encoding = "UTF-8", header = TRUE)
dat = read.csv("C:\\Users\\xf18155\\OneDrive - University of Bristol\\MyFiles-Migrated\\Documents\\SR automation review\\data extraction\\empty_db -working.csv", stringsAsFactors = FALSE, encoding = "UTF-8", header = TRUE)

names(dat)
#note should not include spaces in col names
#C:\Program Files\MongoDB\Server\4.4\data\


mongo_url <- "mongodb+srv://LS:LNQGQGC4@lsr.7ybnv.mongodb.net/LSR?retryWrites=true&w=majority"

collectionName= "test"
db <- mongo(collection = collectionName, url = mongo_url)
db$insert(dat)




