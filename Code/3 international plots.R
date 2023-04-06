library(ggplot2)

#set directory
setwd("~/Documents/GitHub/MDI")

econ_data <- read.csv("./Data/Output/data_merge.csv", stringsAsFactors=FALSE)

econ_data <- econ_data[,-1]

econ_data <- subset(econ_data, year>1913)

countries <- unique(econ_data$country)

##
###############
##all countries plots
#money
g <- ggplot(data=econ_data)
g <- g +  geom_line(aes(y=narrowm/gdp, x=year, color="Narrow Money"))
g <- g +  geom_line(aes(y=money/gdp, x=year, color="Broad Money"))
g <- g + theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6))
g <- g + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
g <- g + scale_colour_manual(name="", values=c("Narrow Money"="blue", 
                                               "Broad Money"="red"))
g <- g + facet_wrap(~country, scales = "free_y")
g

#save file
pdf("./Data/Output/all_countries_money_over_income.pdf")
g
dev.off()

#inequality
g <- ggplot(data=econ_data)
g <- g +  geom_line(aes(y=top10, x=year, color="Top 10"))
g <- g +  geom_line(aes(y=top1, x=year, color="Top 1"))
g <- g + theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6))
g <- g + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
g <- g + scale_colour_manual(name="", values=c("Top 1"="blue", 
                                               "Top 10"="red"))
g <- g + facet_wrap(~country, scales = "free_y")
g

#save file
pdf("./Data/Output/all_countries_inequality.pdf")
g
dev.off()

#interest rates
g <- ggplot(data=econ_data)
g <- g +  geom_line(aes(y=stir, x=year, color="Short Term"))
g <- g +  geom_line(aes(y=ltrate, x=year, color="Long Term"))
g <- g + scale_colour_manual(name="", values=c("Short Term"="blue", 
                                               "Long Term"="red"))
g <- g + theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6))
g <- g + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
g <- g + facet_wrap(~country, scales = "free_y")
g

#save file
pdf("./Data/Output/all_countries_interest_rate.pdf")
g
dev.off()

#money demand
g <- ggplot(data=econ_data)
g <- g + geom_point(aes(y=stir, x=narrowm/gdp))
g <- g + theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6))
g <- g + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
g <- g + facet_wrap(~country, scales = "free")
g

#save file
pdf("./Data/Output/all_countries_narrow_money_demand.pdf")
g
dev.off()

#broad money demand
g <- ggplot(data=econ_data)
g <- g + geom_point(aes(y=stir, x=money/gdp))
g <- g + theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6))
g <- g + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
g <- g + facet_wrap(~country, scales = "free")
g

#save file
pdf("./Data/Output/all_countries_broad_money_demand.pdf")
g
dev.off()

##check: C/Y

#real GDP
econ_data$gdp <- econ_data$gdp/econ_data$cpi
#real consumption
econ_data$cons <- econ_data$rgdppc*econ_data$pop

g <- ggplot(data=econ_data)
g <- g +  geom_line(aes(y=cons, x=year))
g <- g + theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6))
g <- g + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
g <- g + facet_wrap(~country, scales = "free_y")
g


