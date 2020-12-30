library(ggpubr)
library(tidyverse)
setwd("/home/ad/Data_science/R_studio/Git/datasciencecoursera/4. Explore data analysis/EDA course project 2")
NEI_path <- "/media/ad/01D6B57CFBE4DB20/1.Linux/Data/JHU data/exdata_data_NEI_data/summarySCC_PM25.rds"
SCC_path <- "/media/ad/01D6B57CFBE4DB20/1.Linux/Data/JHU data/exdata_data_NEI_data/Source_Classification_Code.rds"
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS(NEI_path)
SCC <- readRDS(SCC_path)

combustionRelated <- grepl("comb", SCC[["SCC.Level.One"]], ignore.case=TRUE)
coalRelated <- grepl("coal", SCC[["SCC.Level.Four"]], ignore.case=TRUE) 
combustionSCC <- SCC %>% filter(combustionRelated & coalRelated)
combustionNEI <- NEI %>% filter(SCC %in% combustionSCC$SCC )

png("plot4.png")
ggplot(combustionNEI,aes(x = factor(year),y = Emissions/10^5)) +
  geom_bar(stat="identity", fill ="#FF9999", width=0.75) +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
  labs(title=expression("PM"[2.5]*" Coal Combustion Source Emissions Across US from 1999-2008"))
dev.off()