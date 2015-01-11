data <- read.table("./data/household_power_consumption.txt", header=TRUE, sep=";", 
                   stringsAsFactors=FALSE, dec=".", na.strings="?", nrows=2075259,comment.char="")
interest.data <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]
rm(data)

#Conversions
datetime <- strptime(paste(subSetData$Date, subSetData$Time, sep=" "), "%d/%m/%Y %H:%M:%S")
subMetering1 <- as.numeric(subSetData$Sub_metering_1)
subMetering2 <- as.numeric(subSetData$Sub_metering_2)
subMetering3 <- as.numeric(subSetData$Sub_metering_3)
globalActivePower <- as.numeric(subSetData$Global_active_power)
globalReactivePower <- as.numeric(subSetData$Global_reactive_power)
voltage <- as.numeric(subSetData$Voltage)


png("plot4.png", width=480, height=480, units="px", pointsize=12, bg="white")

# 2 row by 2 col display
par(mfrow = c(2, 2)) 

# Plot-1 
plot(datetime, globalActivePower, type="l", xlab="", ylab="Global Active Power", cex=0.2)

#Plot 2
plot(datetime, voltage, type="l", xlab="datetime", ylab="Voltage")

#Plot 3
plot(datetime, subMetering1, type="l", ylab="Energy Submetering", xlab="")
lines(datetime, subMetering2, type="l", col="red")
lines(datetime, subMetering3, type="l", col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=, lwd=2.5, col=c("black", "red", "blue"), bty="o")

#Plot 4
plot(datetime, globalReactivePower, type="l", xlab="datetime", ylab="Global_reactive_power")

dev.off()