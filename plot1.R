setwd("/Users/3126147/courses/John Hopkins DS specialization/Exploratory Data Analysis")

data <- read.table("./data/household_power_consumption.txt", header=TRUE, sep=";", 
                    stringsAsFactors=FALSE, dec=".", na.strings="?", nrows=2075259,comment.char="")
interest.data <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]
rm(data)

globalActivePower <- as.numeric(interest.data$Global_active_power)
png("plot1.png", width=480, height=480, units="px", pointsize=12, bg="white")
hist(globalActivePower, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)", ylab="Frequency")
dev.off()
