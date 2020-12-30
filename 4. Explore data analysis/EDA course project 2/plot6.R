library(ggpubr)
library(tidyverse)
setwd("/home/ad/Data_science/R_studio/Git/datasciencecoursera/4. Explore data analysis/EDA course project 2")
NEI_path <- "/media/ad/01D6B57CFBE4DB20/1.Linux/Data/JHU data/exdata_data_NEI_data/summarySCC_PM25.rds"
SCC_path <- "/media/ad/01D6B57CFBE4DB20/1.Linux/Data/JHU data/exdata_data_NEI_data/Source_Classification_Code.rds"
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS(NEI_path)
SCC <- readRDS(SCC_path)

vehicles <- grepl("vehicle", SCC[["SCC.Level.Two"]], ignore.case=TRUE)
vehiclesSCC <- SCC %>% filter(vehicles)
vehiclesNEI <- NEI %>% filter(SCC %in% vehiclesSCC$SCC )
baltimoreVehiclesNEI <- vehiclesNEI%>% filter(fips == "24510") 
losangelessVehiclesNEI <- vehiclesNEI%>% filter(fips == "06037")
baltimoreVehiclesNEI<-baltimoreVehiclesNEI %>% mutate(city="Baltimore City")
losangelessVehiclesNEI<-losangelessVehiclesNEI %>% mutate(city="Los Angeles")

bothNEI <- rbind(baltimoreVehiclesNEI,losangelessVehiclesNEI)

png("plot6.png")
ggplot(bothNEI, aes(x=factor(year), y=Emissions, fill=city)) +
  geom_bar(aes(fill=year),stat="identity") +
  facet_grid(scales="free", space="free", .~city) +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Kilo-Tons)")) + 
  labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore & LA, 1999-2008"))

dev.off()