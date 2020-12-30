
NEI_path <- "/media/ad/01D6B57CFBE4DB20/1.Linux/Data/JHU data/exdata_data_NEI_data/summarySCC_PM25.rds"
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS(NEI_path)

total_emissions_by_year <- NEI %>% group_by(year) %>% summarise(sum_all = sum(Emissions))
total_emissions_by_year
setwd("/home/ad/Data_science/R_studio/Git/datasciencecoursera/4. Explore data analysis/EDA course project 2")

png("plot1.png", width=480, height=480)
with(total_emissions_by_year,barplot(sum_all,names = year,
                                     xlab = "Years", ylab = "Emissions",
                                     main = "Emissions over the Years"
))
dev.off()



