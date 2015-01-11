setwd("/Users/3126147/courses/John Hopkins DS specialization/Exploratory Data Analysis")

data <- read.table("./data/household_power_consumption.txt", header=TRUE, sep=";", 
                   stringsAsFactors=FALSE, dec=".", na.strings="?", nrows=2075259,comment.char="")
interest.data <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]
rm(data)

#Conversions
globalActivePower <- as.numeric(interest.data$Global_active_power)
datetime <- strptime(paste(interest.data$Date, interest.data$Time, sep=" "), "%d/%m/%Y %H:%M:%S")

png("plot2.png", width=480, height=480, units="px", pointsize=12, bg="white")
plot( datetime, globalActivePower, type="l", col="red", main="", ylab="Global Active Power (kilowatts)", xlab="")
dev.off()
