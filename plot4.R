plot4 <- function (datafile="household_power_consumption.txt"){
    ## PREPARATION STAGE
    # Loading the data from provided file
    raw_data<-read.csv(datafile,sep=";",na.strings = "?")
    
    # Creating new POSIXlt datetime column from textual Date/Time columns
    hpc<-cbind(raw_data,DateTime=strptime(paste(as.character(raw_data$Date),as.character(raw_data$Time)),format="%d/%m/%Y%H:%M:%S"))
    
    # Filtering data by NAs and required date period
    hpc<-hpc[complete.cases(hpc),]
    hpc<-hpc[hpc$DateTime>='2007-02-01'&hpc$DateTime<'2007-02-03',3:10]
    
    ## PLOTTING STAGE
    # Opening PNG file device for writing
    png(filename = "plot4.png",width=480,height=480,units="px")
    # Plotting the graph itself
    # Setting up the graph area for 4 plots
    par(mfrow=c(2,2))
    
    # Adding 1st graph
    plot(hpc$DateTime,hpc$Global_active_power,type="l",xlab="",ylab="Global Active Power (kilowatts)")
    
    # Adding 2nd graph 
    plot(hpc$DateTime,hpc$Voltage,type="l",xlab="datetime",ylab="Voltage")
    
    # Adding 3rd graph 
    plot(hpc$DateTime,hpc$Sub_metering_1,type="n",xlab="",ylab="Energy sub metering")
    # Adding lines
    lines(hpc$DateTime,hpc$Sub_metering_1,col="black")
    lines(hpc$DateTime,hpc$Sub_metering_2,col="red")
    lines(hpc$DateTime,hpc$Sub_metering_3,col="blue")
    # Adding legend
    legend("topright",names(hpc[,5:7]),col=c("black","red","blue"),lty=1,cex=0.75,bty="n")
    
    # Adding 4th graph
    plot(hpc$DateTime,hpc$Global_reactive_power,type="l",xlab="datetime",ylab="Global reactive power")
    
    #Closing the file device
    dev.off()
    
    print("File plot4.png was successfully created.")
}