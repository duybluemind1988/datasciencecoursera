
library(tidyverse)
setwd("/home/ad/Data_science/R_studio/Git/datasciencecoursera/4. Explore data analysis/EDA course project 2")
NEI_path <- "/media/ad/01D6B57CFBE4DB20/1.Linux/Data/JHU data/exdata_data_NEI_data/summarySCC_PM25.rds"
SCC_path <- "/media/ad/01D6B57CFBE4DB20/1.Linux/Data/JHU data/exdata_data_NEI_data/Source_Classification_Code.rds"
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS(NEI_path)
SCC <- readRDS(SCC_path)

vehiclesSCC <- grepl("vehicle", SCC[["SCC.Level.Two"]], ignore.case=TRUE)
vehiclesSCC <- SCC %>% filter(vehiclesSCC)
vehiclesNEI <- NEI %>% filter(SCC %in% vehiclesSCC$SCC )

baltimoreVehiclesNEI <- vehiclesNEI%>% filter(fips == 24510) 

png("plot5.png")

ggplot(baltimoreVehiclesNEI,aes(factor(year),Emissions)) +
  geom_bar(stat="identity", fill ="#FF9999" ,width=0.75) +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
  labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore from 1999-2008"))

dev.off()