##############################################################################
## Exploring Data Anaylsis - Course Assignment 1 
## Author: Mark Natale
## email: mnatalex54@gmail.com
## Date: 06/1/2015
## Function names: plot1()
##############################################################################
## Purpose: Create a histogram of golbal household power over a 48 hour period.
## Use a dataset of measurements of electric power consumption in 
## one household with a one-minute sampling rate over a period of almost 
## 4 years. Different electrical quantities and some sub-metering values are 
## also available.

## Function to plot a histogram with preset parameters.
plot_spec <- function(x) {
## Set common paramters here
  par(bg = "white")

## Histogram generation
  hist(x$Global_active_power, 
        freq = TRUE, 
        xlab = "Global Active Power (Kilowatts)", 
       # ylab = "",
        main = "Global Active Power",
        col = "red"
        )

## Plotting parameters for png image type
  dev.copy(png, 
        width     = 480, 
        height    = 480, 
        units     = "px", 
        res     = 72,
        pointsize = 12, 
        file      = "plot1.png")
  dev.off()
}

## main function. Aquire data from file for plot. The file used can be found at:
## https://d396qusza40orc.cloudfront.net
##  /exdata%2Fdata%2Fhousehold_power_consumption.zip and is archive at the UC
## Irvine Machine learning Repository.
## For this exploratory graph, Date/Time and Global active power data will be
## used.

plot1 <- function() {

## Required libraries
 library(data.table)
 library(dplyr)
## Constant List
  ### File specifiers
  file      <- "./household_power_consumption.txt"
 separator <- ";"
 na_char   <- c("?", "NA", "")
#  col_types <- c("character", "character", "numeric", "numeric", "numeric",
#                   "numeric", "numeric", "numeric","numeric")
 col_types  <- ""

 ## Date specifiers
 startDate <- "2007-02-01 00:00:00"
 endDate   <- "2007-02-03 00:00:00"
 date_fmt  <- "%d/%m/%Y %H:%M:%S"

## Read file and make data table
 f <- fread(file, header = TRUE, sep = separator, 
            na.strings = na_char, colClasses = col_types)

## create data table
 dt1 <- tbl_df(f)

## Make a properly formated date and time and add as date to table
 date_time <- strptime(paste(dt1$Date, dt1$Time), date_fmt)
 date_time <- mutate(dt1, dateTime = as.POSIXct(date_time))

## Filter rows of interest by date and typed data columns to double.
 filteredbydate <- filter(date_time, dateTime >= startDate & dateTime <= endDate)
  filteredbydate[,3:9] <- sapply(filteredbydate[,3:9], as.double)

## Histogram of Global Active Power
 plot_spec(filteredbydate)
}
