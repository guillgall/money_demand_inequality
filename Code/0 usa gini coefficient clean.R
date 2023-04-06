library(openxlsx)
library(reshape2)

#set directory
setwd("~/Documents/GitHub/MDI")

#WIID DATA

gini <- read.xlsx("./Data/Input/WIID_31MAY2021_0.xlsx")

gini <- subset(gini, country=="Australia" | 
                 country=="Canada" | 
                 country=="Denmark" |
                 country=="Finland" |
                 country=="France" |
                 country=="Japan" |
                 country=="United States")

gini <- subset(gini, country=="United States")

gini <- subset(gini, resource=="Income (gross)" & 
                 (resource_detailed=="Income, gross" | resource_detailed=="Monetary income, gross") & 
                 scale=="No adjustment" &
                 scale_detailed=="No adjustment" & 
                 sharing_unit=="Household" & 
                 reference_unit=="Household")

for (i in 1:nrow(gini)){
  ifelse(gini$year[i]<1967, gini$source_comments[i] <- "Series 1944-1966", gini$source_comments[i])
  ifelse(gini$year[i]<1967, gini$source_comments[i] <- "Series 1944-1966", gini$source_comments[i])
}

unique(gini$source_comments)

gini <- subset(gini, source_comments=="Series 1944-1966" | 
                 source_comments=="Series 1967-2013" | 
                 source_comments=="Series 2013-2017")

#2013 repeated
#gini <- subset(gini, (year!="2013" & source_comments!="Series 2013-2017"))

gini <- gini[,c(3,5,6)]

gini$year   

ifelse(duplicated(gini$year)==TRUE,1,0)

#2013 repeated:
gini <- gini[-69,]

#gini <- subset(gini, year<2008)

write.csv(gini, file = "./Data/Output/gini_usa_1944_2008.csv")
