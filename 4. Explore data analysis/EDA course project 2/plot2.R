library(tidyverse)
setwd("/home/ad/Data_science/R_studio/Git/datasciencecoursera/4. Explore data analysis/EDA course project 2")
NEI_path <- "/media/ad/01D6B57CFBE4DB20/1.Linux/Data/JHU data/exdata_data_NEI_data/summarySCC_PM25.rds"
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS(NEI_path)

Baltimore_PM <-NEI %>% filter(fips == 24510) %>% 
  group_by(year) %>% 
  summarise(sum_all = sum(Emissions))

png("plot2.png", width=480, height=480)
with(Baltimore_PM,barplot(sum_all,names = year,
                          xlab = "Years", ylab = "Emissions",
                          main = "Emissions over the Years in Baltimore City"
))
dev.off()