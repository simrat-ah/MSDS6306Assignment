# Author: Benjamin Reddy
# Taken from pages 49-50 of O'Neil and Schutt

#require(gdata)
#require(plyr) #Added by Monnie McGee
#install the gdata and plyr packages and load in to R.
library(plyr)
library(gdata)
setwd(".")
getwd


# So, save the file as a csv and use read.csv instead
#bk <- read.csv("./Data/rollingsales_brooklyn.csv",skip=4,header=TRUE)

brx <- read.csv("./Data/rollingsales_bronx.csv",skip=4,header=TRUE)
## Check the data
head(brx)
summary(brx)
str(brx) # Very handy function!

## clean/format the data with regular expressions
## More on these later. For now, know that the
## pattern "[^[:digit:]]" refers to members of the variable name that
## start with digits. We use the gsub command to replace them with a blank space.
# We create a new variable that is a "clean' version of sale.price.
# And sale.price.n is numeric, not a factor.
brx$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","", brx$SALE.PRICE))
count(is.na(brx$SALE.PRICE.N))

names(brx) <- tolower(names(brx)) # make all variable names lower case
## Get rid of leading digits
brx$gross.sqft <- as.numeric(gsub("[^[:digit:]]","", brx$gross.square.feet))
brx$land.sqft <- as.numeric(gsub("[^[:digit:]]","", brx$land.square.feet))
brx$year.built <- as.numeric(as.character(brx$year.built))

## do a bit of exploration to make sure there's not anything
## weird going on with sale prices
attach(brx)
hist(sale.price.n) 
detach(brx)

## keep only the actual sales

brx.sale <- brx[brx$sale.price.n!=0,]
plot(brx.sale$gross.sqft,brx.sale$sale.price.n)
plot(log10(brx.sale$gross.sqft),log10(brx.sale$sale.price.n))

## for now, let's look at 1-, 2-, and 3-family homes
brx.homes <- brx.sale[which(grepl("FAMILY",brx.sale$building.class.category)),]
dim(brx.homes)
plot(log10(brx.homes$gross.sqft),log10(brx.homes$sale.price.n))
summary(brx.homes[which(brx.homes$sale.price.n<100000),])


## remove outliers that seem like they weren't actual sales
brx.homes$outliers <- (log10(brx.homes$sale.price.n) <=5) + 0
brx.homes <- brx.homes[which(brx.homes$outliers==0),]
plot(log10(brx.homes$gross.sqft),log10(brx.homes$sale.price.n))

## Added by Simrat

brx.sale.clean <- brx.homes[!(is.na(brx.homes$sale.price.n)),]

brx.sqft <- brx.sale.clean[!(is.na(brx.sale.clean$land.sqft)),]

brx.homes <- brx.sqft[order(brx.sqft$neighborhood),]

write.csv(file="./Data/brx_homes.csv", x=brx.homes)

str(brx)
str(brx.homes)