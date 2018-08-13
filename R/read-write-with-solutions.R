################################
## Reading and Writing Data in R
################################

## Create your own data (simulate)
set.seed(42)
dat <- data.frame(group   = rep(letters[1:3], c(10,10,10)),
                  endpoit = rnorm(30))
View(dat)

## R built-in data

data()
install.packages("reshape2")
data(package="reshape2")
help("french_fries", package="reshape2")
data("french_fries", package="reshape2")
View(french_fries)

## Local data in the filesystem

getwd()
setwd("~") # shortcut for home directory
getwd()
setwd(".") # shortcut for current directory
getwd()
# RStudio: Session -> Set Working Directory

download.file("http://www-bcf.usc.edu/~gareth/ISL/Auto.data", "Auto.data")
# RStudio: File -> Open File -> "Auto.data"

Auto=read.table("Auto.data")
fix(Auto)
Auto=read.table("Auto.data",header=T,na.strings="?")
dim(Auto)
Auto[1:4,]
names(Auto)
Auto.nona=na.omit(Auto)
dim(Auto.nona)
head(Auto$name)
Auto=read.table("Auto.data",header=T,na.strings="?",stringsAsFactors=F)
head(Auto$name)
Auto=read.table("Auto.data",header=T,na.strings="?")

save(Auto, file="Auto.RData", compress='xz') # ".rda" also common
load("Auto.RData")

file.size("Auto.data", "Auto.RData")

help(package="foreign")
install.packages("haven")
help(package="haven")
install.packages("xlsx")
help(package="xlsx")


## Reading files directly from the internet
Auto=read.table(url("http://www-bcf.usc.edu/~gareth/ISL/Auto.data"),
              header=T,na.strings="?")
dim(Auto)


## Data from local or remote databases
## DBI - R Database Interface
## ODBC - Open Database Connectivity

install.packages("RSQLite")
library("RSQLite")
con <- dbConnect(RSQLite::SQLite(), "Auto.sqlite")
dbWriteTable(con, "Auto", Auto)
dbListTables(con)
dbGetQuery(con, "SELECT * FROM Auto WHERE year=70 AND cylinders=8 AND weight<3500")

rs <- dbSendQuery(con, "SELECT * FROM Auto WHERE year=70 AND cylinders=8 AND weight<3500")
dbFetch(rs, n = 2)  # extract data in chunks of 2 rows
dbHasCompleted(rs)
dbFetch(rs, n = -1) # extract all remaining data
dbHasCompleted(rs)
dbClearResult(rs)

dbDisconnect(con)

file.size("Auto.data", "Auto.RData", "Auto.sqlite")

## Example with remote MySQL database
# con <- DBI::dbConnect(RMySQL::MySQL(),
#                       host      = [My Server],
#                       dbname    = [My Database],
#                       username  = [My User ID],
#                       password  = [My Password])




###########################
## Reading/Writing Data Lab
###########################

## 1. Change the working directory to your directory of choice.

setwd('~')

## 2. Use the "read.csv" and "url" functions to download and read the "Auto.csv"
##    file from the following URL: http://www-bcf.usc.edu/~gareth/ISL/Auto.csv".

Auto <- read.csv(url("http://www-bcf.usc.edu/~gareth/ISL/Auto.csv"),
                 header=TRUE, na.strings="?", stringsAsFactors=F)

## 3. Write the downloaded data to a local CSV file using "write.csv".

write.csv(Auto, file='Auto.csv')

## 4. Compare the size of the CSV file with that of the whitespace delimited,
##    RData, and SQLite files.

file.size("Auto.data", "Auto.RData", "Auto.sqlite", "Auto.csv")

## 5. Open the "Auto.sqlite" database and modify the SQL query to select only 
##    values from the "name" field, subsetting to cars made in 1970 with 8
##    cylinders. Close the database.

library("RSQLite")
con <- dbConnect(RSQLite::SQLite(), "Auto.sqlite")
dbGetQuery(con, "SELECT name FROM Auto WHERE year=70 AND cylinders=8")
dbDisconnect(con)

## 6. Write a simple function and save it to an RData file using the 'save' function.
##    Remove the function from the workspace using 'rm', then load it back again
##    using the 'load' function. Test that the function works.

test <- function(x)
  print('hello world')
save(test, file='test.RData')
rm(test)
load('test.RData')

## 7. Create a data frame with one column of 1000 unique IDs, one column that
##    indicates membership in one of two groups (e.g., first 500 in group 'A',
##    second 500 in group 'B'), and one column of normal random values with 
##    mean 0.0 for group 'A' and mean 5.0 for gorup 'B'. Use the 'head' and
##    'tail' functions to visualy verify.

dat <- data.frame(id    = 1:1000, 
                  group = rep(c('A','B'),c(500,500)))
dat$endpoint <- ifelse(dat$group == 'A', rnorm(1000, 0), rnorm(1000, 5))
head(dat)
tail(dat)
