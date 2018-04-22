plot4 <- function(directory = "C:/work/data_science/Course4_Exploratory/data") {
    setwd(directory);
    dt_all <- read.delim(file = "household_power_consumption.txt", header = TRUE, sep = ';');
    library(dplyr);
    dt <- filter(dt_all,  Date ==  "1/2/2007" | Date == "2/2/2007");
    
    dt <- mutate(dt
                 , w = format.Date(as.Date(Date, "%d/%m/%Y"), "%w")
                 , wday = format.Date(as.Date(Date, "%d/%m/%Y"), "%a")
                 , GAP = as.double(as.character(Global_active_power))
                 , GRP = as.double(as.character(Global_reactive_power))
                 , VOLT = as.double(as.character(Voltage))
                 , sm1 = as.numeric(as.character(Sub_metering_1))
                 , sm2 = as.numeric(as.character(Sub_metering_2))
                 , sm3 = as.numeric(as.character(Sub_metering_3))
                 , d = as.numeric(format(as.Date(Date, "%d/%m/%Y"), "%d"))
                 , t = as.numeric(strptime(Time, "%H:%M:%S") - strptime("00:00:00", "%H:%M:%S"))/60
    );
    dt <- mutate(dt, mt = 24*60*(d-1) + t);
    
    par(mfrow = c(2,2));
    suppressWarnings(
        with(dt, {
        plot(dt$mt, dt$GAP, type = "n", pch = 20, xlab = "", ylab = "Global Active Power (kilowatts)", labels = FALSE)
        lines(dt$mt, dt$GAP, col = "black")
        axis(2,c(0,2,4,6),c(0,2,4,6))
        axis(1,c(1,1500,2880),c("Thu","Fri","Sat"))
    
        plot(dt$mt, dt$VOLT, type = "n", pch = 20, xlab = "datetime", ylab = "Voltage", labels = FALSE)
        lines(dt$mt, dt$VOLT, col = "black")
        axis(2,c(234,238,242,246),c(234,238,242,246))
        axis(1,c(1,1500,2880),c("Thu","Fri","Sat"))
        
        plot(dt$mt, dt$sm1, type = "n", xlab = "", ylab = "Energy sub metering", labels = FALSE, pch = 20)
        lines(dt$mt, dt$sm1, col = "black")
        lines(dt$mt, dt$sm2, col = "red")
        lines(dt$mt, dt$sm3, col = "blue")
        axis(2,c(0,10,20,30),c(0,10,20,30))
        axis(1,c(1,1500,2880),c("Thu","Fri","Sat"))
        legend("topright", lty = 1, legend = c("Sub_metering_1   ","Sub_metering_2   ","Sub_metering_3   ")
           , col = c("black","red","blue"), y.intersp = 1, cex = 0.5)
    
        plot(dt$mt, dt$GRP, type = "n", pch = 20, xlab = "datetime", ylab = "Global_reactive_power", labels = FALSE)
        lines(dt$mt, dt$GRP, col = "black")
        axis(2,c(0.0,0.1,0.2,0.3,0.4,0.5),c(0.0,0.1,0.2,0.3,0.4,0.5))
        axis(1,c(1,1500,2880),c("Thu","Fri","Sat"))
    })
    );
    suppressWarnings(
        dev.copy(png, file = "plot4.png")
    );
    rm(dt_all);
    dev.off();
    #assign("last.warning", NULL, envir = baseenv());
    dt;
}
