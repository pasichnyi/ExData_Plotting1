plot1 <- function (datafile="household_power_consumption.txt"){
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
    png(filename = "plot1.png",width=480,height=480,units="px")
    # Plotting the graph itself
    hist(hpc$Global_active_power,col="red",main="Global Active Power",xlab="Global Active Power (kilowatts)")
    #Closing the file device
    dev.off()
    
    print("File plot1.png was successfully created.")
}