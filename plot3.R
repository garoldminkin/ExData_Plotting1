plot3 <- function(directory = "C:/work/data_science/Course4_Exploratory/data") {
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
    
    par(mfrow = c(1,1));
    suppressWarnings(
      plot(dt$mt, dt$sm1, type = "n", xlab = "", ylab = "Energy sub metering", labels = FALSE, pch = 20)
    );
    lines(dt$mt, dt$sm1, col = "black")
    lines(dt$mt, dt$sm2, col = "red")
    lines(dt$mt, dt$sm3, col = "blue")
    axis(2,c(0,10,20,30),c(0,10,20,30))
    axis(1,c(1,1500,2880),c("Thu","Fri","Sat"))
    legend("topright", lty = 1, legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3")
           , col = c("black","red","blue"))
    suppressWarnings(
        dev.copy(png, file = "plot3.png")
    );
    rm(dt_all);
    dev.off();
    assign("last.warning", NULL, envir = baseenv());
    dt;
}
