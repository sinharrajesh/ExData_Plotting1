# Filename: plot4.R
# Purpose: To generate the fourth  plot for assignment-1 / Exploratory Data Analysis
# 
# Generates 2 x 2 display of data
# 
# Author: Rajesh Sinha
# 
#
# NOTE TO PEER REVIEWER
#    1. The script will download and unzip the data if required and has not been done so far
#    2. It has 4 functions but will execute only the one of interest for the purpose of assessment which will run from runAssignment1()
#    3. Because EBImage is not available on R3.1.2 I cannot show the generated and reference image side by side for your
#       viewing pleasure, otherwise would have been good to put them out side by side at the end
#    4. I should have used ProjectTemplate for reading, caching, munging but then did not have time - so implemented my own caching solution
#       In short - the data of interest (i.e. 2 days is written to a cache file and read back again if available rather than 
#       loading the whole xx MB file and then filtering on it)
#    5. set the colors of bg to transparent rather than white - I cannot make out what it is
#    6. I took the class last month but never finished - so fork is old, the commit is new 
#    7. Have taken care of borderless legend for plot4 
#
# To Run
#    source('plot4.R')
#    runAssignment1()
#    
library(dplyr)
library(futile.logger)

checkAndCreateDir <- function(dir.name) {
#This function checks if a directory exists else creates is
# on success should return 0
    if (!file.exists(dir.name)) {
        flog.info("creating %s directory", dir.name)
        return(dir.create(dir.name))
    } else {
        flog.trace("directory %s already exists", dir.name)
        return(0)
    }
}



doEnvSetUpAndExtractData <- function(envList) {
# This function checks and creates the download directory, extracts and unzips the data file
# reads the whole file in 'data' dataframe and then subsets it on dates we want for assignment
# also sets the right classes for columns and addes a new POSIXct() column for datetime 
# returns only the required filtered data instead of all the data
# deletes the original data frame used to laod all the data
# will not extract or create the datafiles if already present
# Update - will now write a cached copy of the data of interest df and then read it back rather than doing the big 
# heavy lifting again and again
#

    dl.dir <- envList$dl.dir
    file.url <- envList$file.url
    op.dir <- envList$op.name
    refplots.dir <- envList$refplots.dir
    
    # refplots should exist, but a kludge in any case
    dirs.to.check <- c(dl.dir, op.dir, refplots.dir)
    x<- lapply(dirs.to.check, checkAndCreateDir)
    flog.trace("Summary of file creation is ", x, capture=TRUE)
    
    
    
    zipfile.name <- paste(dl.dir,"/","zipfile.zip", sep="");
    flog.trace("checking if zipfile %s is already available", zipfile.name)
    
    if (!file.exists(zipfile.name)) {
        x<- download.file(url=file.url, destfile=zipfile.name, method="curl");
        date.Download <- Sys.time()
        flog.info("Downloaded file with return code %d at time %s", x, date.Download);
    } else {
        flog.trace("rawdata source zip file %s already exists", zipfile.name)
    }
    

    rawdatafile.name <- paste(dl.dir,"/household_power_consumption.txt", sep="")
    if (!file.exists(rawdatafile.name)) {
        x<- unzip(zipfile=zipfile.name, exdir=dl.dir);
        flog.info("unzipped file and created following files:", x, capture=TRUE);
    } else {
        flog.trace("rawdata file %s already exists", rawdatafile.name)
    }

    cacheddatafile.name <- paste(dl.dir,"/cache.txt", sep="")
    if (file.exists(cacheddatafile.name)) {
        flog.info("reading cached copy of data frame %s instead of reading and filtering from source file",
		cacheddatafile.name)

	# when reading cached info note that there will be an added datetime column at the end anyway
        colClasses.vector = c("character", "character", "numeric", "numeric", 
                          "numeric", "numeric", "numeric","numeric", "numeric", "POSIXct")
        data.of.interest <- read.table(file=cacheddatafile.name,
                                    header=TRUE,
                                    sep=";",
                                    colClasses=colClasses.vector,
                                    na.strings="?",
                                    dec=".")
                      

    } else {
	    colClasses.vector = c("character", "character", "numeric", "numeric", 
				  "numeric", "numeric", "numeric","numeric", "numeric")
	    
	    flog.trace("Started Reading the BIG Data", startTime <- Sys.time())
	    data <- read.table(file=rawdatafile.name, 
			       header=TRUE, # raw data file has header row
			       sep=";",     # is separated by ;
			       stringsAsFactors=FALSE,  # pointless
			       dec=".",  # uses . as decimal point 
			       na.strings="?", # ? is used instead of NA
			       nrows=2075259,  # max no of rows to read in - faster load ops
			       comment.char="",
			       colClasses=colClasses.vector) # apply colClasss at read time 
	    flog.trace("Done reading big data. ", Sys.time() - startTime, capture=T)
	    # simple subset version %in%
	    # data.of.interest <- data[data$Date %in% envList$dates.of.interest, ]
	    # OR the faster dplyr version
	    flog.trace("filtering right rows")
	    data.of.interest <- filter(data, Date %in% envList$dates.of.interest)

	    
	    # Now let this big boy go to garbage
	    flog.info("Full Data Dimensions ", dim(data), capture=T)
	    rm(data)
	    
		
	    
	    # Lets mutate and add a new date time column as asked for in plots 
	    # if I do not do as.POSIXct() dplyr does not support POSIXlt data type at all - wierd
	    # but I guess now they have it documented. There goes my plan for shifting lock stock and
	    # barrel to only dplyr ;-)
	    flog.trace("adding a new column datetime to data")
	    data.of.interest <- mutate( data.of.interest,
				     datetime <- as.POSIXct(strptime(paste(data.of.interest$Date, 
								data.of.interest$Time, sep=" "), 
							  "%d/%m/%Y %H:%M:%S"))
				     )
	    
	    
        colnames(data.of.interest)[length(data.of.interest)] = "datetime"

        #Now write a cached copy of this data so that we do not do something again and again
	    flog.info("writing a cached version of data of interest");
        write.table(data.of.interest, file=cacheddatafile.name, append=FALSE, sep=";", na="?", row.names=F, col.names=T)


    }
    
    flog.info("Data relevant to this assignment Dimensions ", dim(data.of.interest), capture=T)
    flog.info("Names relevant to this assignment Dimensions ", names(data.of.interest), capture=T)
    
    
    # and return only the interested data 
    return (data.of.interest)
    
}
readEnvSetUp <- function() {
# all the values to be changed can be put in here
    
    # change the threshold value if you wish - I hate putting name = for my own logger so using root logger
    flog.threshold(TRACE)
    # change the appender to appender.file("name of file") if you want
    flog.appender(appender.console())
    
    # set this to forked and cloned plotting directory
    setwd("~/gitrepos/ExData_Plotting1")
    
    # do not muck with this else thou are fully culpable of wierd results
    file.url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"    
    
    # figure is already provided, change data and plots if you wish to do so
    # reflplots may be useful if I could show my and the other plots simulatenously instead of 
    # writing another weird way of comparing the output (e.g. bit by bit comparison)
    # eyeballing would be much easier for reviewer (please give me extra points for this ;-)
    dl.dir <- paste(getwd(),"/data", sep="")
    op.dir <- paste(getwd(), "/plots", sep="")
    refplots.dir <- paste(getwd(), "/figure", sep="")
    dates.of.interest <- c("1/2/2007","2/2/2007")
    
    return(list(dl.dir=dl.dir,
                op.dir=op.dir,
                refplots.dir=refplots.dir,
                file.url=file.url,
                dates.of.interest=dates.of.interest))
    
}


plot1 <- function (data.of.interest) {
    flog.trace("Plotting plot1")
    with(data.of.interest, hist(Global_active_power, col="red", 
                           main="Global Active Power", xlab="Global Active Power (kilowatts)", ylab="Frequency"))
    flog.trace("Plotted plot1")
    
}

plot2 <- function(data.of.interest) {
    flog.trace("Plotting plot2")
    with(data.of.interest, plot(datetime, Global_active_power, type="l", , main="", ylab="Global Active Power (kilowatts)", xlab=""))
    flog.trace("Plotted plot2")
    
}

plot3 <- function(data.of.interest) {
    flog.trace("Plotting plot3")
    with(data.of.interest, {
         plot(datetime,  Sub_metering_1, type="l", ylab="Energy Submetering", xlab="")
         lines(datetime, Sub_metering_2, type="l", col="red")
         lines(datetime, Sub_metering_3, type="l", col="blue")
         legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
           lty=, lwd=2.5, col=c("black", "red", "blue"), bty="o")
    }) 
    flog.trace("Plotted plot3")
    
}

plot4 <- function(data.of.interest) {
    flog.trace("Plotting plot4")


    # 2 row by 2 col display
    par(mfrow = c(2, 2)) 
    
    # Plot-1 
    with(data.of.interest, plot(datetime, Global_active_power, type="l", xlab="", ylab="Global Active Power", cex=0.2)) 

    #Plot 2
    with(data.of.interest, plot(datetime, Voltage, type="l", xlab="datetime", ylab="Voltage"))
    
    #Plot 3
    with(data.of.interest, {
         plot(datetime,  Sub_metering_1, type="l", ylab="Energy Submetering", xlab="")
         lines(datetime, Sub_metering_2, type="l", col="red")
         lines(datetime, Sub_metering_3, type="l", col="blue")

         #Devilish - this legend box has no border - have to use bty="n" - and then ignore border and fill
         x<- legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
           border="transparent", fill="transparent", lty=, lwd=2.5, col=c("black", "red", "blue"), bty="n")
    }) 
    
    #Plot 4
    with(data.of.interest, plot(datetime, Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power"))
    flog.trace("Plotted plot4")
    
}

# do the main execution - Thou shall now produce the Plot!!
#
runAssignment1 <- function () {
    data.of.interest <- doEnvSetUpAndExtractData(readEnvSetUp())
    png("plot4.png", width=480, height=480, units="px", pointsize=12, bg="transparent")
    plot4(data.of.interest)
    dev.off()
}
