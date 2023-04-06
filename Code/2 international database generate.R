library(openxlsx)
library(reshape2)

#set directory
setwd("~/Documents/GitHub/MDI")

econ_data <- read.xlsx("./Data/Input/JSTdatasetR5.xlsx", 2)

unique(econ_data$country)

#merge with inequality data
#first top 10 decile share
inequality <- read.xlsx("./Data/Input/WID_Data_09112021-131223.xlsx")

colnames(inequality)[3]
names <- strsplit(colnames(inequality)[3:ncol(inequality)], "[.]")

names

names[2]

i <- 3

names[[4]][9]

for (i in 3:ncol(inequality)){
  colnames(inequality)[i] <- names[[i-2]][9]
}

#change UK and USA names to march other database
grep("United", colnames(inequality))

colnames(inequality)[grep("United", colnames(inequality))] <- "UK"

inequality <- melt(inequality, id.vars=c("Year", "Percentile"))

colnames(inequality)[c(1,3)] <- c("year", "country")

inequality <- dcast(inequality, year + country ~ Percentile)

colnames(inequality)[3:4] <- c("top10", "top1")

#merge datasets

econ_data <- merge(econ_data, inequality, by=c("country", "year"))

unique(econ_data$country)

#save file
write.csv(econ_data, file = "./Data/Output/data_merge.csv")
