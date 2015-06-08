##############################################################################
# Exploring Data Anaylsis - Course Assignment 1 
# Author: Mark Natale
# email: mnatalex54@gmail.com
# Date: 06/1/2015
# Function names: multiplot()
##############################################################################
## Purpose: Create a plot of golbal household power over a 48 hour period.
## Use a dataset of measurements of electric power consumption in 
## one household with a one-minute sampling rate over a period of almost 
## 4 years. Different electrical quantities and some sub-metering values are 
## also available.

## Function to plot a graph with preset parameters.
plot_spec1 <- function(x, y, xlabel, ylabel, graphname, startdate) {
  par(bg = "white")

## Plot generation
  plot(x, y, 
         xlab = xlabel,
        ylab = ylabel, 
        xaxt = "n",
        main = graphname,
        col  = "black",
        type = "l"
  )
## format the x-axis to show the date "Day" names
  axis.POSIXct(1, at = seq(as.POSIXct(startdate), max(x), "days"),
                                 format = "%a")
}

## Function to plot a graph with preset parameters and a legend box
plot_spec2 <- function(x, startdate) {
  par(bg = "white")
  time <- x$dateTime
## Plot generation
  plot(time, x$Sub_metering_1,
        ylab = "Energy sub metering", 
         xlab = "1-Min samples over 48 Hrs",
        xaxt = "n",
        main = "Energy Sub Metering",
        col = "black",
        type = "l"
  )
## Additional measurements for graph
  lines(time, x$Sub_metering_2, type = "l", col = "red")
  lines(time, x$Sub_metering_3, type = "l", col = "blue")

## format the x-axis to show the date "Day" names
  axis.POSIXct(1, at = seq(as.POSIXct(startdate), max(time), "days"),
                             format = "%a")

## Gen a legend box
  legend_txt <- c("Kitchen", 
          "Fridg/Laundry", 
          "hot-Water/Aircond")

  legend_color <- c("black", 
           "red", 
           "blue")

  legend("topright", 
      legend_txt, 
      col = legend_color, 
      bty = "n", 
      lwd = 1, 
      cex = 0.75)
}

plot4 <- function() {

## Required libraries
 library(data.table)
 library(dplyr)

# Debug support
# options(dplyr.print_max = Inf)

## Constant List
  ### File specifiers
  file      <- "./household_power_consumption.txt"
 separator <- ";"
 col_types  <- ""
 na_char   <- c("?", "NA", "")
#  col_types <- c("character", "character", "numeric", "numeric", "numeric",
#                   "numeric", "numeric", "numeric","numeric")

 ## Date specifiers
 startDate <- "2007-02-01 00:00:00 EST"
 endDate   <- "2007-02-03 00:00:00 EST"
 date_fmt  <- "%d/%m/%Y %H:%M:%S"

## Read file and make data table
 f <- fread(file, header = TRUE, sep = separator, 
         na.strings = na_char, colClasses = col_types)

## create data table
 dt1 <- tbl_df(f)

## Make a properly formated date and time and add as date to table
 date_time <- strptime(paste(dt1$Date, dt1$Time), date_fmt)
 date_time <- mutate(dt1, dateTime = as.POSIXct(date_time))

## Filter rows by date
 filteredbydate <- filter(date_time, dateTime >= startDate & dateTime <= endDate)
  filteredbydate[,3:9] <- sapply(filteredbydate[,3:9], as.double)

## Define the plot layout for a 2x2 multiple graph plot matrix.
 plotlayout <- par(mfrow = c(2, 2))

## Call to single Plot spec. Gen a time-series graph for Global active power
 plot_spec1(filteredbydate$dateTime, 
        filteredbydate$Global_active_power, 
        "1-Min samples over 48 Hrs",
        "Global Active Power",
        "Global Active Power",
        startDate)

## Call to single Plot spec. Gen a time-series graph for voltage
 plot_spec1(filteredbydate$dateTime, 
        filteredbydate$Voltage, 
        "1-Min samples over 48 Hrs",
        "Voltage",
        "Voltage",
        startDate)

## Call to graph Plot spec with legend. Gen a time-series graph for sub
## metering data.
 plot_spec2(filteredbydate, startDate)

## Call to single Plot spec. Gen a time-series graph for Global reactive
## power.
 plot_spec1(filteredbydate$dateTime, 
        filteredbydate$Global_reactive_power, 
        "1-Min samples over 48 Hrs",
        "Global Reactive Power",
        "Global Reactive Power",
        startDate)

## Time to generate a png file with all 4 graphs
 dev.copy(png, 
        width     = 480, 
        height    = 480, 
        units     = "px", 
        res     = 72,
        pointsize = 12, 
        file      = "plot4.png")
  dev.off()


}
