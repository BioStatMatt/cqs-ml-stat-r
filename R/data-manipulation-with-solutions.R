#########################
## Data Manipulation in R
#########################

## Reshaping data

library('reshape2')

help("airquality")
data("airquality")
names(airquality) <- tolower(names(airquality))
head(airquality)

## multiple measurements per 'id' can be stored in 'wide' or 'long' format
## wide format - one record (row) per 'id'
## long format - multiple records per 'id'

## convert to long format using melt
aqm <- melt(airquality, id.vars=c("month", "day"))
head(aqm)
tail(aqm)

## convert back to wide format using dcast (cast to 'd'ata frame)
## - create one record per unique combination of LHS
## - create new variables from the values of RHS
aqc <- dcast(aqm, month + day ~ variable)
head(aqc)

## convert to long format using base function reshape
aql <- reshape(airquality,
               varying = c('ozone', 'solar.r', 'wind', 'temp'),
               v.names = 'value',
               timevar = 'variable',
               times = c('ozone', 'solar.r', 'wind', 'temp'),
               idvar = 'id', direction='long')
head(aql)

## convert back to wide format using base function reshape
aqw <- reshape(aql)
head(aqw)
        
## convert to a 3D array with [<month>,<day>,<variable>] with acast
aqa <- acast(aqm, month ~ day ~ variable)

## Aggregating using 'dcast'

## average over days within each month and convert back to wide
## using dcast (cast to 'd'ata frame)
## - create one record per unique combination of LHS
## - since multiple days per month, aggregate using the mean function
## - create new variables from the values of RHS
## - use 'na.rm' to remove NA's before computing mean
aqg <- dcast(aqm, month ~ variable, mean, na.rm=TRUE)
head(aqg)

## Searching/matching/subsetting

## test whether month == 9
aqm$month == 9

## get indices values that test TRUE
which(aqm$month == 9)

## 'match' gives the index of the *first* matching entry
match(9, aqm$month)

## subset to records where month == 9
subset(aqm, month == 9)
aqm[aqm$month == 9,]

subset(aqm, variable == 'temp')
subset(aqm, grepl('temp', variable))
subset(aqm, grepl('^t', variable))

## merging (merge)
## adapted from: https://stackoverflow.com/questions/1299871/
df1 = data.frame(CustomerId = c(1:6), 
                 Product = c(rep("Toaster", 3), 
                             rep("Radio", 3)))
df2 = data.frame(CustomerId = c(2, 4, 6, 8),
                 State = c(rep("Alabama", 2),
                           rep("Ohio", 1),
                           rep("Montana", 1)))
df1
df2


## An inner join of df1 and df2: Join and return only the rows that have matching keys.

merge(df1, df2, by="CustomerId")

## An outer join of df1 and df2: Returns all rows from both tables, join records
## that have matching keys, insert missing values for rows with no match.

merge(df1, df2, by="CustomerId", all=TRUE)

## A left join of df1 and df2: Return all rows from the left table, join recors
## from the right table that have matching keys in the left table.

merge(df1, df2, by="CustomerId", all.x = TRUE)

## A right join of df1 and df2: Return all rows from the right table, join recors
## from the left table that have matching keys in the right table.

merge(df1, df2, by="CustomerId", all.y = TRUE)

########################
## Data Manipulation Lab
########################

## Suppose the data below represent body weight measurements 
## for six participants in a weight loss intervention program,
## at three follow-up time points (t0, t1, and t2)
dat <- data.frame(id  = c( 1, 2, 3, 4, 5, 6),
                  age = c(47,52,35,28,62,44),
                  sex = factor(c("M","M","F","M","F","F")),
                  wt_t0 = c(278, 340, 239, 290, 244, 220),
                  wt_t1 = c(230, 302, 231, 277, 245, 201),
                  wt_t2 = c(211, 295, 231, 282, 243, 182))
                  
## 1. Convert these data to long format so that there is only one variable
##    with weight measurements, and three rows per participant ('id')

datm <- melt(dat, id.vars=c('id','age','sex'))

## 2. Convert back to wide format, averaging across participant sex 
##    (resulting data frame should have one row for males and one for females)

dcast(datm, sex ~ variable, mean)

## Suppose there is a second data frame that contains additional information
## on participants and others, including marital status and other variables.
## Add marital status (only) to the above data (either the wide or long version)
## using the 'merge' command. What type of 'join' is this?
demo <- data.frame(id = 1:20,
                   married = sample(c(T,F), 20, replace=TRUE, prob=c(0.5,0.5)),
                   income  = rnorm(20, 75000, 10000),
                   state   = sample(state.abb, 20, replace=TRUE))

dat_merged <- merge(dat, subset(demo, select=c('id','married')),
                    by = 'id', all.x = TRUE)


## Suppose that a measure of 'irritability' (scale from 0 to 10) was also
## collected at each time point:
dat <- data.frame(id  = c( 1, 2, 3, 4, 5, 6),
                  age = c(47,52,35,28,62,44),
                  sex = factor(c("M","M","F","M","F","F")),
                  wt_t0 = c(278, 340, 239, 290, 244, 220),
                  wt_t1 = c(230, 302, 231, 277, 245, 201),
                  wt_t2 = c(211, 295, 231, 282, 243, 182),
                  irr_t0 = factor(c(0, 1, 1, 0, 0, 0), levels=0:10, ordered=TRUE),
                  irr_t1 = factor(c(5, 3, 2, 1, 5, 7), levels=0:10, ordered=TRUE),
                  irr_t2 = factor(c(4, 3, 3, 1, 4, 6), levels=0:10, ordered=TRUE))

## Use melt and cast to convert this data to long format with three
## rows per participant, and one column for weights, and one column 
## for irritability scores. (hint: after melting, you must add a 
## 'measure' variable to indicate whether the corresponding value 
## is a weight measurement or irritability measurement, and you 
## must also add a time variable (i.e., 0, 1, or 2))

datm <- melt(dat, id.vars = c('id', 'age', 'sex'))
datm$measure <- ifelse(grepl('wt_', datm$variable), 'wt', 'irr')
datm$time    <- ifelse(grepl('_t0', datm$variable), 0,
                       ifelse(grepl('_t1', datm$variable), 1, 2))
dcast(datm, id+age+sex+time~measure)

## Repeat the above task using the 'reshape' function.

datl <- reshape(dat,
        varying = list(c('wt_t0', 'wt_t1', 'wt_t2'),
                       c('irr_t0', 'irr_t1', 'irr_t2')),
        v.names = c('wt', 'irr'),
        timevar = 'time',
        times = c(0, 1, 2),
        idvar = 'id',
        direction = 'long')
