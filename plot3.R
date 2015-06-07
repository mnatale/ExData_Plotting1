##############################################################################
# Exploring Data Anaylsis - Course Assignment 1 
# Author: Mark Natale
# email: mnatalex54@gmail.com
# Date: 06/1/2015
# Function names: plot3()
##############################################################################
## Purpose: Create a plot of golbal household power over a 48 hour period.
## Use a dataset of measurements of electric power consumption in 
## one household with a one-minute sampling rate over a period of almost 
## 4 years. Different electrical quantities and some sub-metering values are 
## also available.

## Function to plot a graph with preset parameters.
plot_spec <- function(x, startdate) {
## Set common paramters here
		par(bg = "white")
		time <- x$dateTime

## Plot generation
		plot(time, x$Sub_metering_1,
								ylab = "Energy sub metering", 
							 	xlab = "",
								xaxt = "n",
								main = "",
								col = "black",
								type = "l"
		)
## Additional measurements for graph
		lines(time, x$Sub_metering_2, type = "l", col = "red")
		lines(time, x$Sub_metering_3, type = "l", col = "blue")

## format the x-axis to show the date "Day" names
		axis.POSIXct(1, at = seq(as.POSIXct(startdate), max(time), "days"),
																												format = "%a")
## Create a legend with the following names and colors to match lines:
		legend_txt <- c("Sub_metering_1", 
										"Sub_metering_2", 
										"Sub_metering_3")

		legend_color <- c("black", 
											"red", 
											"blue")

		legend("topright", 
						legend_txt, 
						col = legend_color, 
						lwd = 1)

## Plotting parameters for png image type
		dev.copy(png, 
								width     = 1024, 
								height    = 1024, 
								units     = "px", 
								res			  = 72,
								pointsize = 12, 
								file      = "plot3.png")
		dev.off()
}

## main function. Aquire data from file for plot. The file used can be found at:
## https://d396qusza40orc.cloudfront.net
##  /exdata%2Fdata%2Fhousehold_power_consumption.zip and is archive at the UC
## Irvine Machine learning Repository.
## For this exploratory graph, Date/Time and Global active power data will be
## used.
plot3 <- function() {

## Required libraries
	library(data.table)
	library(dplyr)

## Constant List
  ### File specifiers
  file      <- "./household_power_consumption.txt"
	separator <- ";"
	na_char   <- c("?", "NA", "")
#  col_types <- c("character", "character", "numeric", "numeric", "numeric",
#																			"numeric", "numeric", "numeric","numeric")
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

## plot Global Active Power
	plot_spec(filteredbydate, startDate)
}
