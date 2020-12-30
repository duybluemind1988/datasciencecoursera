library(ggpubr)
library(tidyverse)
setwd("/home/ad/Data_science/R_studio/Git/datasciencecoursera/4. Explore data analysis/EDA course project 2")
NEI_path <- "/media/ad/01D6B57CFBE4DB20/1.Linux/Data/JHU data/exdata_data_NEI_data/summarySCC_PM25.rds"
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS(NEI_path)


Baltimore_PM <-NEI %>% filter(fips == 24510)



png("plot3.png", width=480, height=480)
Baltimore_PM %>% 
  ggplot(aes(factor(year),Emissions,fill=type))+
  geom_bar(stat="identity") +
  facet_grid(.~type)+
  labs(title=expression("PM Emissions, Baltimore City 1999-2008 by Source Type"))
dev.off()