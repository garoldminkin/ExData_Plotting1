plot2 <- function(directory = "C:/work/data_science/Course4_Exploratory/data") {
    setwd(directory);
    dt_all <- read.delim(file = "household_power_consumption.txt", header = TRUE, sep = ';');
    library(dplyr);
    dt <- filter(dt_all,  Date ==  "1/2/2007" | Date == "2/2/2007");
    
    dt <- mutate(dt
                 , w = format.Date(as.Date(Date, "%d/%m/%Y"), "%w")
                 , wday = format.Date(as.Date(Date, "%d/%m/%Y"), "%a")
                 , GAP = as.double(as.character(Global_active_power))
                 , d = as.numeric(format(as.Date(Date, "%d/%m/%Y"), "%d"))
                 , t = as.numeric(strptime(Time, "%H:%M:%S") - strptime("00:00:00", "%H:%M:%S"))/60
    );
    
    dt <- mutate(dt, mt = 24*60*(d-1) + t);
    par(mfrow = c(1,1));
    suppressWarnings(
       plot(dt$mt, dt$GAP, type = "n", pch = 20, xlab = "", ylab = "Global Active Power (kilowatts)", labels = FALSE)
    );
    lines(dt$mt, dt$GAP, col = "black")
    axis(2,c(0,2,4,6),c(0,2,4,6));
    axis(1,c(1,1500,2880),c("Thu","Fri","Sat"));
    
    suppressWarnings(
        dev.copy(png, file = "plot2.png")
    );
    rm(dt_all);
    dev.off();
    dt;
}
