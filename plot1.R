plot1 <- function(directory = "C:/work/data_science/Course4_Exploratory/data") {
    setwd(directory);
    dt_all <- read.delim(file = "household_power_consumption.txt", header = TRUE, sep = ';');
    library(dplyr);
    dt <- filter(dt_all,  Date ==  "1/2/2007" | Date == "2/2/2007" )
    par(mfrow = c(1,1))
    hist(as.double(as.character(dt$Global_active_power)), col="red"
         , xlab = "Global Active Power (kilowatts)"
         , main = "Global Active Power"
         , mfrow = 480, mfcol = 480);
    dev.copy(png, file = "plot1.png");
    rm(dt_all);
    dev.off();
    dt;
}

