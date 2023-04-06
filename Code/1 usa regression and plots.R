library(stargazer)
library(plm)
library(openxlsx)
library(xtable)

#set directory
setwd("~/Documents/GitHub/MDI")

usa_m <- read.xlsx("./Data/Input/merge.xlsx")

usa_m <- usa_m[-c(1:3),c(1,20:24)]

colnames(usa_m)[1] <- "year"

sapply(usa_m, class)

i <- 1

for (i in 1:ncol(usa_m)){
  usa_m[,i] <- as.numeric(usa_m[,i])
}

#add rest of data

econ_data <- read.csv("./Data/Output/data_merge.csv", stringsAsFactors=FALSE)

usa <- subset(econ_data, country=="USA")

usa <- merge(usa, usa_m, by="year")

#add gini data (from WIID UNU WIDER database)
gini <- read.csv("./Data/Output/gini_usa_1944_2008.csv")

gini <- gini[,3:4]

usa <- merge(usa, gini, by="year", all=T)

average <- usa[,61:64]/usa$gdp
#monetary base in billions
average[,5] <- usa[,65]/1000/usa$gdp
average["Average",] <- colMeans(average, na.rm = TRUE)


#plots
g <- ggplot(data=usa)
g <- g +  geom_line(aes(y=Cash/gdp, x=year, color="Currency (own construction)"))
g <- g +  geom_line(aes(y=M1/gdp, x=year, color="M1 (own construction)"))
g <- g +  geom_line(aes(y=M2/gdp, x=year, color="M2 (own construction)"))
g <- g +  geom_line(aes(y=narrowm/gdp, x=year, color="Narrow Money (Macrohistory)"))
g <- g +  geom_line(aes(y=money/gdp, x=year, color="Broad Money (Macrohistory)"))
g <- g +  geom_line(aes(y=(M0/1000)/gdp, x=year, color="Monetary Base (FRED)"))
g <- g + theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6))
g <- g + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
g <- g + scale_colour_manual(name="", values=c("Currency (own construction)"="black",
                                               "M1 (own construction)"="blue", 
                                               "M2 (own construction)"="red",
                                               "Narrow Money (Macrohistory)"="green",
                                               "Broad Money (Macrohistory)"="purple",
                                               "Monetary Base (FRED)"="yellow"))
g

#save file
ggsave("./Data/Output/usa_monetary_aggregates.pdf",
       width = 10, height = 5)

##INEQUALITY
g <- ggplot(data=usa)
g <- g +  geom_line(aes(y=top10, x=year, color="Top 10"))
g <- g +  geom_line(aes(y=top1, x=year, color="Top 1"))
g <- g +  geom_line(aes(y=gini/100, x=year, color="Gini"))
g <- g + theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6))
g <- g + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
g <- g + scale_colour_manual(name="", values=c("Top 1"="blue", 
                                               "Top 10"="red",
                                               "Gini"="black"))
g

#save file
ggsave("./Data/Output/usa_inequality.pdf",
       width = 10, height = 5)


#prepare data for regressions

#real GDP
usa$gdp <- usa$gdp/usa$cpi

#real monetary balances
usa$m1 <- usa$M1/usa$cpi

#real currency
usa$currency <- usa$Cash/usa$cpi

#real m2
usa$m2 <- usa$M2/usa$cpi

#consumption
usa$cons <- usa$rconpc*usa$pop


#subset
usa <- subset(usa, year>1914)

##
usa$m1_diff <- NA
usa$currency_diff <- NA
usa$m2_diff <- NA
usa$gdp_diff <- NA
usa$cons_diff <- NA

usa$stir_diff <- NA
usa$top10_diff <- NA
usa$top1_diff <- NA
usa$gini_diff <- NA

usa$m1_lag <- NA
usa$currency_lag <- NA
usa$m2_lag <- NA

i <- 2

for (i in 2:nrow(usa)){
  usa$m1_diff[i] <- log(usa$m1[i])-log(usa$m1[i-1])
  usa$currency_diff[i] <- log(usa$currency[i])-log(usa$currency[i-1])
  usa$m2_diff[i] <- log(usa$m2[i])-log(usa$m2[i-1])
  usa$gdp_diff[i] <- log(usa$gdp[i])-log(usa$gdp[i-1])
  usa$cons_diff[i] <- log(usa$cons[i])-log(usa$cons[i-1])
  
  usa$stir_diff[i] <- usa$stir[i]-usa$stir[i-1]
  usa$top10_diff[i] <- usa$top10[i]-usa$top10[i-1]
  usa$top1_diff[i] <- usa$top1[i]-usa$top1[i-1]
  usa$gini_diff[i] <- usa$gini[i]-usa$gini[i-1]
  
  usa$m1_lag[i] <- usa$m1_diff[i-1]
  usa$currency_lag[i] <- usa$currency_diff[i-1]
  usa$m2_lag[i] <- usa$m2_diff[i-1]
}

#usa <- subset(usa, year<2008)

##REGRESSIONS
##LEVELS
ols1 <- lm(log(m1) ~ log(gdp) + log(stir), data = usa)

ols2 <- lm(log(m1) ~ log(gdp) + log(stir) + log(top10), data = usa)

ols3 <- lm(log(m1) ~ log(cons) + log(stir), data = usa)

ols4 <- lm(log(m1) ~ log(cons) + log(stir) + log(top10), data = usa)

ols5 <- lm(log(m1) ~ log(gdp) + log(stir) + log(top1), data = usa)

ols6 <- lm(log(m1) ~ log(cons) + log(stir) + log(top1), data = usa)

ols7 <- lm(log(m1) ~ log(gdp) + log(stir) + log(gini), data = usa)

ols8 <- lm(log(m1) ~ log(cons) + log(stir) + log(gini), data = usa)

stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8,
          dep.var.labels=c("M1"),
          covariate.labels=c("GDP",
                             "Cons.",
                             "Int. Rate",
                             "Top 10",
                             "Top 1",
                             "Gini"),
          label="tab:ols_usa_m1",
          title="USA: M1 Money Demand and Inequality",
          omit.stat=c("ser", "f"),
          digits=2,
          out="./Data/Output/ols_usa_m1.tex")

##DIFFERENCES

###WHOLE PERIOD
####M1
ols1 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag, data = usa)
summary(ols1)

ols2 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + top10_diff, data = usa)
summary(ols2)

ols3 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag, data = usa)
summary(ols3)

ols4 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + top10_diff, data = usa)
summary(ols3)

ols5 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + top1_diff, data = usa)
summary(ols5)

ols6 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + top1_diff, data = usa)
summary(ols6)

ols7 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + gini_diff, data = usa)
summary(ols7)

ols8 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + gini_diff, data = usa)
summary(ols8)


stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8, 
          dep.var.labels=c("FD Log M1"),
          covariate.labels=c("FD Log GDP",
                             "FD Log Cons.",
                             "FD Int. Rate",
                             "Lagged FD Log M",
                             "FD Top 10",
                             "FD Top 1",
                             "FD Gini"),
          label="tab:ols_m1_diff_usa",
          title="USA: M1 Money Demand and Inequality, First Difference Regression",
          omit.stat=c("ser", "f"),
          digits=2,
          out="./Data/Output/ols_m1_diff_usa.tex")

###
#Cover and Hooks subsample

usa_subsample <- subset(usa, year>1948 & year <1989)

ols1 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag, data = usa_subsample)
summary(ols1)

ols2 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + top10_diff, data = usa_subsample)
summary(ols2)

ols3 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag, data = usa_subsample)
summary(ols3)

ols4 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + top10_diff, data = usa_subsample)
summary(ols3)

ols5 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + top1_diff, data = usa_subsample)
summary(ols5)

ols6 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + top1_diff, data = usa_subsample)
summary(ols6)

ols7 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + gini_diff, data = usa_subsample)
summary(ols7)

ols8 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + gini_diff, data = usa_subsample)
summary(ols8)


stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8,
          dep.var.labels=c("FD Log M1"),
          covariate.labels=c("FD Log GDP",
                             "FD Log Cons.",
                             "FD Int. Rate",
                             "FD FD Lagged Log M",
                             "FD Top 10",
                             "FD Top 1",
                             "Gini"),
          label="tab:ols_m1_diff_usa_subsample_coverhooks",
          title="USA (Subsample 1949-1988): M1 Money Demand and Inequality, First Difference Regression",
          omit.stat=c("ser", "f"),
          digits=2,
          out="./Data/Output/ols_m1_diff_usa_subsample_coverhooks.tex")

###
#War and interwar period, 1914-1944 (no gini data)

usa_subsample <- subset(usa, year<1945)

ols1 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag, data = usa_subsample)
summary(ols1)

ols2 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + top10_diff, data = usa_subsample)
summary(ols2)

ols3 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag, data = usa_subsample)
summary(ols3)

ols4 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + top10_diff, data = usa_subsample)
summary(ols3)

ols5 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + top1_diff, data = usa_subsample)
summary(ols5)

ols6 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + top1_diff, data = usa_subsample)
summary(ols6)

ols7 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + gini_diff, data = usa_subsample)
summary(ols7)

ols8 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + gini_diff, data = usa_subsample)
summary(ols8)


stargazer(ols1, ols2, ols3, ols4, ols5, ols6, 
          dep.var.labels=c("FD Log M1"),
          covariate.labels=c("FD Log GDP",
                             "FD Log Cons.",
                             "FD Int. Rate",
                             "FD FD Lagged Log M",
                             "FD Top 10",
                             "FD Top 1"),
          label="tab:ols_m1_diff_usa_subsample_interwar",
          title="USA (Subsample 1914-1944): M1 Money Demand and Inequality, First Difference Regression",
          omit.stat=c("ser", "f"),
          digits=2,
          out="./Data/Output/ols_m1_diff_usa_subsample_interwar.tex")

###SUBSAMPLE, Bretton Woods period

###
#War and interwar period, 1914-1944 

usa_subsample <- subset(usa, year>1945 & year<1974)

ols1 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag, data = usa_subsample)
summary(ols1)

ols2 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + top10_diff, data = usa_subsample)
summary(ols2)

ols3 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag, data = usa_subsample)
summary(ols3)

ols4 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + top10_diff, data = usa_subsample)
summary(ols3)

ols5 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + top1_diff, data = usa_subsample)
summary(ols5)

ols6 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + top1_diff, data = usa_subsample)
summary(ols6)

ols7 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + gini_diff, data = usa_subsample)
summary(ols7)

ols8 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + gini_diff, data = usa_subsample)
summary(ols8)


stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8, 
          dep.var.labels=c("FD Log M1"),
          covariate.labels=c("FD Log GDP",
                             "FD Log Cons.",
                             "FD Int. Rate",
                             "FD FD Lagged Log M",
                             "FD Top 10",
                             "FD Top 1",
                             "Gini"),
          label="tab:ols_m1_diff_usa_subsample_brettonwoods",
          title="USA (Subsample 1945-1972): M1 Money Demand and Inequality, First Difference Regression",
          omit.stat=c("ser", "f"),
          digits=2,
          out="./Data/Output/ols_m1_diff_usa_subsample_brettonwoods.tex")


####Great Moderation, 1985-2007

usa_subsample <- subset(usa, year>1984 & year<2008)

ols1 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag, data = usa_subsample)
summary(ols1)

ols2 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + top10_diff, data = usa_subsample)
summary(ols2)

ols3 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag, data = usa_subsample)
summary(ols3)

ols4 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + top10_diff, data = usa_subsample)
summary(ols3)

ols5 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + top1_diff, data = usa_subsample)
summary(ols5)

ols6 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + top1_diff, data = usa_subsample)
summary(ols6)

ols7 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + gini_diff, data = usa_subsample)
summary(ols7)

ols8 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + gini_diff, data = usa_subsample)
summary(ols8)


stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8, 
          dep.var.labels=c("FD Log M1"),
          covariate.labels=c("FD Log GDP",
                             "FD Log Cons.",
                             "FD Int. Rate",
                             "FD FD Lagged Log M",
                             "FD Top 10",
                             "FD Top 1",
                             "Gini"),
          label="tab:ols_m1_diff_usa_subsample_greatmoderation",
          title="USA (Subsample 1985-2007): M1 Money Demand and Inequality, First Difference Regression",
          omit.stat=c("ser", "f"),
          digits=2,
          out="./Data/Output/ols_m1_diff_usa_subsample_greatmoderation.tex")



#####
#currency

ols1 <- lm(currency_diff ~ gdp_diff + stir_diff + currency_lag, data = usa)
summary(ols1)

ols2 <- lm(currency_diff ~ gdp_diff + stir_diff + currency_lag + top10_diff, data = usa)
summary(ols2)

ols3 <- lm(currency_diff ~ cons_diff + stir_diff + currency_lag, data = usa)
summary(ols3)

ols4 <- lm(currency_diff ~ cons_diff + stir_diff + currency_lag + top10_diff, data = usa)
summary(ols3)

ols5 <- lm(currency_diff ~ gdp_diff + stir_diff + currency_lag + top1_diff, data = usa)
summary(ols5)

ols6 <- lm(currency_diff ~ cons_diff + stir_diff + currency_lag + top1_diff, data = usa)
summary(ols6)

ols7 <- lm(currency_diff ~ gdp_diff + stir_diff + currency_lag + gini_diff, data = usa)
summary(ols7)

ols8 <- lm(currency_diff ~ cons_diff + stir_diff + currency_lag + gini_diff, data = usa)
summary(ols8)


stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8, 
          dep.var.labels=c("FD Log currency"),
          covariate.labels=c("FD Log GDP",
                             "FD Log Cons.",
                             "FD Int. Rate",
                             "Lagged FD Log M",
                             "FD Top 10",
                             "FD Top 1",
                             "FD Gini"),
          label="tab:ols_diff_usa",
          title="USA: Currency Demand and Inequality, First Difference Regression",
          omit.stat=c("ser", "f"),
          digits=2,
          out="./Data/Output/ols_currency_diff_usa.tex")

#Cover and Hooks subsample

usa_subsample <- subset(usa, year>1948 & year <1989)

ols1 <- lm(currency_diff ~ gdp_diff + stir_diff + currency_lag, data = usa_subsample)
summary(ols1)

ols2 <- lm(currency_diff ~ gdp_diff + stir_diff + currency_lag + top10_diff, data = usa_subsample)
summary(ols2)

ols3 <- lm(currency_diff ~ cons_diff + stir_diff + currency_lag, data = usa_subsample)
summary(ols3)

ols4 <- lm(currency_diff ~ cons_diff + stir_diff + currency_lag + top10_diff, data = usa_subsample)
summary(ols3)

ols5 <- lm(currency_diff ~ gdp_diff + stir_diff + currency_lag + top1_diff, data = usa_subsample)
summary(ols5)

ols6 <- lm(currency_diff ~ cons_diff + stir_diff + currency_lag + top1_diff, data = usa_subsample)
summary(ols6)

ols7 <- lm(currency_diff ~ gdp_diff + stir_diff + currency_lag + gini_diff, data = usa_subsample)
summary(ols7)

ols8 <- lm(currency_diff ~ cons_diff + stir_diff + currency_lag + gini_diff, data = usa_subsample)
summary(ols8)


stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8,
          dep.var.labels=c("FD Log currency"),
          covariate.labels=c("FD Log GDP",
                             "FD Log Cons.",
                             "FD Int. Rate",
                             "FD FD Lagged Log M",
                             "FD Top 10",
                             "FD Top 1",
                             "Gini"),
          label="tab:ols_diff_usa",
          title="USA (Subsample 1949-1988): Currency Demand and Inequality, First Difference Regression",
          omit.stat=c("ser", "f"),
          digits=2,
          out="./Data/Output/ols_currency_diff_usa_subsample.tex")

#######
#M2

ols1 <- lm(m2_diff ~ gdp_diff + stir_diff + m2_lag, data = usa)
summary(ols1)

ols2 <- lm(m2_diff ~ gdp_diff + stir_diff + m2_lag + top10_diff, data = usa)
summary(ols2)

ols3 <- lm(m2_diff ~ cons_diff + stir_diff + m2_lag, data = usa)
summary(ols3)

ols4 <- lm(m2_diff ~ cons_diff + stir_diff + m2_lag + top10_diff, data = usa)
summary(ols3)

ols5 <- lm(m2_diff ~ gdp_diff + stir_diff + m2_lag + top1_diff, data = usa)
summary(ols5)

ols6 <- lm(m2_diff ~ cons_diff + stir_diff + m2_lag + top1_diff, data = usa)
summary(ols6)

ols7 <- lm(m2_diff ~ gdp_diff + stir_diff + m2_lag + gini_diff, data = usa)
summary(ols7)

ols8 <- lm(m2_diff ~ cons_diff + stir_diff + m2_lag + gini_diff, data = usa)
summary(ols8)

stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8, 
          dep.var.labels=c("FD Log M2"),
          covariate.labels=c("FD Log GDP",
                             "FD Log Cons.",
                             "FD Int. Rate",
                             "Lagged FD Log M2",
                             "FD Top 10",
                             "FD Top 1",
                             "FD Gini"),
          label="tab:ols_diff_usa",
          title="USA: M2 Money Demand and Inequality, First Difference Regression",
          omit.stat=c("ser", "f"),
          digits=2,
          out="./Data/Output/ols_m2_diff_usa.tex")

#Cover and Hooks subsample

usa_subsample <- subset(usa, year>1948 & year <1989)

ols1 <- lm(m2_diff ~ gdp_diff + stir_diff + m2_lag, data = usa_subsample)
summary(ols1)

ols2 <- lm(m2_diff ~ gdp_diff + stir_diff + m2_lag + top10_diff, data = usa_subsample)
summary(ols2)

ols3 <- lm(m2_diff ~ cons_diff + stir_diff + m2_lag, data = usa_subsample)
summary(ols3)

ols4 <- lm(m2_diff ~ cons_diff + stir_diff + m2_lag + top10_diff, data = usa_subsample)
summary(ols3)

ols5 <- lm(m2_diff ~ gdp_diff + stir_diff + m2_lag + top1_diff, data = usa_subsample)
summary(ols5)

ols6 <- lm(m2_diff ~ cons_diff + stir_diff + m2_lag + top1_diff, data = usa_subsample)
summary(ols6)

ols7 <- lm(m2_diff ~ gdp_diff + stir_diff + m2_lag + gini_diff, data = usa_subsample)
summary(ols7)

ols8 <- lm(m2_diff ~ cons_diff + stir_diff + m2_lag + gini_diff, data = usa_subsample)
summary(ols8)


stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8,
          dep.var.labels=c("FD Log M2"),
          covariate.labels=c("FD Log GDP",
                             "FD Log Cons.",
                             "FD Int. Rate",
                             "FD FD Lagged Log M",
                             "FD Top 10",
                             "FD Top 1",
                             "Gini"),
          label="tab:ols_diff_usa",
          title="USA (Subsample 1949-1988): M2 Money Demand and Inequality, First Difference Regression",
          omit.stat=c("ser", "f"),
          digits=2,
          out="./Data/Output/ols_m2_diff_usa_subsample.tex")


####
###BOOTSTRAP
#based on https://towardsdatascience.com/bootstrap-regression-in-r-98bfe4ff5007
#and https://github.com/serafimpetrov1/bootstrap/blob/main/BootReg.R


####REGRESSION IN LEVEL BOOTSTRAP

#The bootstrap regression
estimates <- as.data.frame(matrix(ncol=6, nrow=1))

i <- 2

for (i in 1:1000) {
  
  sample_d = usa[sample(nrow(usa), 25, replace = TRUE),]
  
  ols1 <- lm(log(m1) ~ log(gdp) + log(stir) + log(top10), data = sample_d)
  ols2 <- lm(log(m1) ~ log(gdp) + log(stir) + log(top1), data = sample_d)
  ols3 <- lm(log(m1) ~ cons_diff + log(stir) + log(top10), data = sample_d)
  ols4 <- lm(log(m1) ~ cons_diff + log(stir) + log(top1), data = sample_d)
  ols5 <- lm(log(m1) ~ log(gdp) + log(stir) + log(gini), data = sample_d)
  ols6 <- lm(log(m1) ~ cons_diff + log(stir) + log(gini), data = sample_d)
  
  coef1 <- as.data.frame(t(summary(ols1)$coefficients[3,]))
  coef2 <- as.data.frame(t(summary(ols2)$coefficients[3,]))
  coef3 <- as.data.frame(t(summary(ols3)$coefficients[3,]))
  coef4 <- as.data.frame(t(summary(ols4)$coefficients[3,]))
  coef5 <- as.data.frame(t(summary(ols5)$coefficients[3,]))
  coef6 <- as.data.frame(t(summary(ols6)$coefficients[3,]))
  
  coef1$inequality <- "Top 10"
  coef2$inequality <- "Top 1"
  coef3$inequality <- "Top 10"
  coef4$inequality <- "Top 1"
  coef5$inequality <- "Gini"
  coef6$inequality <- "Gini"
  
  coef1$transaction <- "GDP"
  coef2$transaction <- "GDP"
  coef3$transaction <- "Consumption"
  coef4$transaction <- "Consumption"
  coef5$transaction <- "GDP"
  coef6$transaction <- "Consumption"
  
  colnames(estimates) <- colnames(coef1)
  
  estimates <- rbind(estimates, coef1)
  estimates <- rbind(estimates, coef2)
  estimates <- rbind(estimates, coef3)
  estimates <- rbind(estimates, coef4)
  estimates <- rbind(estimates, coef5)
  estimates <- rbind(estimates, coef6)
  
  
  print(paste(i/10, "%", sep=""))
}

estimates <- estimates[-1,]

estimates$tran.ineq <- paste(estimates$transaction, estimates$inequality,
                             sep="-")

colnames(estimates)[1:4] <- c("estimate", "sd", "t.value", "p.value")

estimates$lower <- 0
estimates$upper <- 0

estimates["Average",] <- rowMeans(estimates)

for (i in 1:nrow(estimates)){
  estimates$lower[i] <- estimates$estimate[i] - abs(qt(0.05, (30-1)))*estimates$sd[i]
  estimates$upper[i] <- estimates$estimate[i] + abs(qt(0.05, (30-1)))*estimates$sd[i]
}


#average

average <- as.data.frame(matrix(ncol=9, nrow=1))

colnames(average) <- colnames(estimates)

combinations <- unique(estimates$tran.ineq)

for (i in combinations){
  estimates_subset <- subset(estimates, tran.ineq==i)
  
  estimates_subset["Average",c(1:4,8:9)] <- colMeans(estimates_subset[,c(1:4,8:9)])
  
  estimates_subset[nrow(estimates_subset),5:7] <-   estimates_subset[nrow(estimates_subset)-1,5:7]
  
  colnames(average) <- colnames(estimates_subset)
  
  average <- rbind(average, estimates_subset["Average",])
  
}

average <- average[c(2:5),c(5,6,1,4)]

colnames(average) <- c("Inequality Measure", 
                       "Transacion Measure",
                       "Regression Estimate",
                       "P-Value")

rownames(average) <- NULL

print(xtable(average,
             caption = "USA: Bootstrap for Regression in Levels for 1914-2016",
             label = "tab:bootstrap_usa_levels",
             align = "llccc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      caption.placement = "top",
      file="./Data/Output/bootstrap_usa_levels.tex")

################
##BOOTSTRAP FOR 1944-2016 period (gini coefficient period)
usa_post1944 <- subset(usa, year>1944)

#The bootstrap regression
estimates <- as.data.frame(matrix(ncol=6, nrow=1))

i <- 2

for (i in 1:1000) {
  
  sample_d = usa_post1944[sample(nrow(usa_post1944), 25, replace = TRUE),]
  ols1 <- lm(log(m1) ~ log(gdp) + log(stir) + log(top10), data = sample_d)
  ols2 <- lm(log(m1) ~ log(gdp) + log(stir) + log(top1), data = sample_d)
  ols3 <- lm(log(m1) ~ cons_diff + log(stir) + log(top10), data = sample_d)
  ols4 <- lm(log(m1) ~ cons_diff + log(stir) + log(top1), data = sample_d)
  ols5 <- lm(log(m1) ~ log(gdp) + log(stir) + log(gini), data = sample_d)
  ols6 <- lm(log(m1) ~ cons_diff + log(stir) + log(gini), data = sample_d)
  
  coef1 <- as.data.frame(t(summary(ols1)$coefficients[3,]))
  coef2 <- as.data.frame(t(summary(ols2)$coefficients[3,]))
  coef3 <- as.data.frame(t(summary(ols3)$coefficients[3,]))
  coef4 <- as.data.frame(t(summary(ols4)$coefficients[3,]))
  coef5 <- as.data.frame(t(summary(ols5)$coefficients[3,]))
  coef6 <- as.data.frame(t(summary(ols6)$coefficients[3,]))
  
  coef1$inequality <- "Top 10"
  coef2$inequality <- "Top 1"
  coef3$inequality <- "Top 10"
  coef4$inequality <- "Top 1"
  coef5$inequality <- "Gini"
  coef6$inequality <- "Gini"
  
  coef1$transaction <- "GDP"
  coef2$transaction <- "GDP"
  coef3$transaction <- "Consumption"
  coef4$transaction <- "Consumption"
  coef5$transaction <- "GDP"
  coef6$transaction <- "Consumption"
  
  colnames(estimates) <- colnames(coef1)
  
  estimates <- rbind(estimates, coef1)
  estimates <- rbind(estimates, coef2)
  estimates <- rbind(estimates, coef3)
  estimates <- rbind(estimates, coef4)
  estimates <- rbind(estimates, coef5)
  estimates <- rbind(estimates, coef6)
  
  
  print(paste(i/10, "%", sep=""))
}

estimates <- estimates[-1,]

estimates$tran.ineq <- paste(estimates$transaction, estimates$inequality,
                             sep="-")

colnames(estimates)[1:4] <- c("estimate", "sd", "t.value", "p.value")

estimates$lower <- 0
estimates$upper <- 0

estimates["Average",] <- rowMeans(estimates)

for (i in 1:nrow(estimates)){
  estimates$lower[i] <- estimates$estimate[i] - abs(qt(0.05, (30-1)))*estimates$sd[i]
  estimates$upper[i] <- estimates$estimate[i] + abs(qt(0.05, (30-1)))*estimates$sd[i]
}


#average

average <- as.data.frame(matrix(ncol=9, nrow=1))

colnames(average) <- colnames(estimates)

combinations <- unique(estimates$tran.ineq)

for (i in combinations){
  estimates_subset <- subset(estimates, tran.ineq==i)
  
  estimates_subset["Average",c(1:4,8:9)] <- colMeans(estimates_subset[,c(1:4,8:9)])
  
  estimates_subset[nrow(estimates_subset),5:7] <-   estimates_subset[nrow(estimates_subset)-1,5:7]
  
  colnames(average) <- colnames(estimates_subset)
  
  average <- rbind(average, estimates_subset["Average",])
  
}

average <- average[c(2:7),c(5,6,1,4)]

colnames(average) <- c("Inequality Measure", 
                       "Transacion Measure",
                       "Regression Estimate",
                       "P-Value")

rownames(average) <- NULL

print(xtable(average,
             caption = "USA: Bootstrap for Regression in Levels for 1944-2016",
             label = "tab:bootstrap_usa_levels_post1944",
             align = "llccc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      caption.placement = "top",
      file="./Data/Output/bootstrap_usa_levels_post1944.tex")


####FIRST DIFFERENCE BOOTSTRAP
#The bootstrap regression
estimates <- as.data.frame(matrix(ncol=6, nrow=1))

i <- 2

for (i in 1:1000) {
  
  sample_d = usa[sample(nrow(usa), 25, replace = TRUE),]

  ols1 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + top10_diff, data = sample_d)
  ols2 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + top1_diff, data = sample_d)
  ols3 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + top10_diff, data = sample_d)
  ols4 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + top1_diff, data = sample_d)
  ols5 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + gini_diff, data = sample_d)
  ols6 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + gini_diff, data = sample_d)
  
  coef1 <- as.data.frame(t(summary(ols1)$coefficients[5,]))
  coef2 <- as.data.frame(t(summary(ols2)$coefficients[5,]))
  coef3 <- as.data.frame(t(summary(ols3)$coefficients[5,]))
  coef4 <- as.data.frame(t(summary(ols4)$coefficients[5,]))
  coef5 <- as.data.frame(t(summary(ols5)$coefficients[5,]))
  coef6 <- as.data.frame(t(summary(ols6)$coefficients[5,]))
  
  coef1$inequality <- "Top 10"
  coef2$inequality <- "Top 1"
  coef3$inequality <- "Top 10"
  coef4$inequality <- "Top 1"
  coef5$inequality <- "Gini"
  coef6$inequality <- "Gini"
  
  coef1$transaction <- "GDP"
  coef2$transaction <- "GDP"
  coef3$transaction <- "Consumption"
  coef4$transaction <- "Consumption"
  coef5$transaction <- "GDP"
  coef6$transaction <- "Consumption"
  
  colnames(estimates) <- colnames(coef1)
  
  estimates <- rbind(estimates, coef1)
  estimates <- rbind(estimates, coef2)
  estimates <- rbind(estimates, coef3)
  estimates <- rbind(estimates, coef4)
  estimates <- rbind(estimates, coef5)
  estimates <- rbind(estimates, coef6)
  

  print(paste(i/10, "%", sep=""))
}

estimates <- estimates[-1,]

estimates$tran.ineq <- paste(estimates$transaction, estimates$inequality,
                             sep="-")

colnames(estimates)[1:4] <- c("estimate", "sd", "t.value", "p.value")

estimates$lower <- 0
estimates$upper <- 0

estimates["Average",] <- rowMeans(estimates)

for (i in 1:nrow(estimates)){
  estimates$lower[i] <- estimates$estimate[i] - abs(qt(0.05, (30-1)))*estimates$sd[i]
  estimates$upper[i] <- estimates$estimate[i] + abs(qt(0.05, (30-1)))*estimates$sd[i]
}


#average

average <- as.data.frame(matrix(ncol=9, nrow=1))

colnames(average) <- colnames(estimates)

combinations <- unique(estimates$tran.ineq)

for (i in combinations){
  estimates_subset <- subset(estimates, tran.ineq==i)
  
  estimates_subset["Average",c(1:4,8:9)] <- colMeans(estimates_subset[,c(1:4,8:9)])

  estimates_subset[nrow(estimates_subset),5:7] <-   estimates_subset[nrow(estimates_subset)-1,5:7]
  
  colnames(average) <- colnames(estimates_subset)
  
  average <- rbind(average, estimates_subset["Average",])
  
}

average <- average[c(2:5),c(5,6,1,4)]

colnames(average) <- c("Inequality Measure", 
                       "Transacion Measure",
                       "Regression Estimate",
                       "P-Value")

rownames(average) <- NULL

print(xtable(average,
             caption = "USA: Bootstrap for Regression in First Difference for 1914-2016",
             label = "tab:bootstrap_usa_diff",
             align = "llccc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      caption.placement = "top",
      file="./Data/Output/bootstrap_usa_diff.tex")

################
##BOOTSTRAP FOR 1944-2016 period (gini coefficient period)
usa_post1944 <- subset(usa, year>1944)

#The bootstrap regression
estimates <- as.data.frame(matrix(ncol=6, nrow=1))

i <- 2

for (i in 1:1000) {
  
  sample_d = usa_post1944[sample(nrow(usa_post1944), 25, replace = TRUE),]
  
  ols1 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + top10_diff, data = sample_d)
  ols2 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + top1_diff, data = sample_d)
  ols3 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + top10_diff, data = sample_d)
  ols4 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + top1_diff, data = sample_d)
  ols5 <- lm(m1_diff ~ gdp_diff + stir_diff + m1_lag + gini_diff, data = sample_d)
  ols6 <- lm(m1_diff ~ cons_diff + stir_diff + m1_lag + gini_diff, data = sample_d)
  
  coef1 <- as.data.frame(t(summary(ols1)$coefficients[5,]))
  coef2 <- as.data.frame(t(summary(ols2)$coefficients[5,]))
  coef3 <- as.data.frame(t(summary(ols3)$coefficients[5,]))
  coef4 <- as.data.frame(t(summary(ols4)$coefficients[5,]))
  coef5 <- as.data.frame(t(summary(ols5)$coefficients[5,]))
  coef6 <- as.data.frame(t(summary(ols6)$coefficients[5,]))
  
  coef1$inequality <- "Top 10"
  coef2$inequality <- "Top 1"
  coef3$inequality <- "Top 10"
  coef4$inequality <- "Top 1"
  coef5$inequality <- "Gini"
  coef6$inequality <- "Gini"
  
  coef1$transaction <- "GDP"
  coef2$transaction <- "GDP"
  coef3$transaction <- "Consumption"
  coef4$transaction <- "Consumption"
  coef5$transaction <- "GDP"
  coef6$transaction <- "Consumption"
  
  colnames(estimates) <- colnames(coef1)
  
  estimates <- rbind(estimates, coef1)
  estimates <- rbind(estimates, coef2)
  estimates <- rbind(estimates, coef3)
  estimates <- rbind(estimates, coef4)
  estimates <- rbind(estimates, coef5)
  estimates <- rbind(estimates, coef6)
  
  
  print(paste(i/10, "%", sep=""))
}

estimates <- estimates[-1,]

estimates$tran.ineq <- paste(estimates$transaction, estimates$inequality,
                             sep="-")

colnames(estimates)[1:4] <- c("estimate", "sd", "t.value", "p.value")

estimates$lower <- 0
estimates$upper <- 0

estimates["Average",] <- rowMeans(estimates)

for (i in 1:nrow(estimates)){
  estimates$lower[i] <- estimates$estimate[i] - abs(qt(0.05, (30-1)))*estimates$sd[i]
  estimates$upper[i] <- estimates$estimate[i] + abs(qt(0.05, (30-1)))*estimates$sd[i]
}


#average

average <- as.data.frame(matrix(ncol=9, nrow=1))

colnames(average) <- colnames(estimates)

combinations <- unique(estimates$tran.ineq)

for (i in combinations){
  estimates_subset <- subset(estimates, tran.ineq==i)
  
  estimates_subset["Average",c(1:4,8:9)] <- colMeans(estimates_subset[,c(1:4,8:9)])
  
  estimates_subset[nrow(estimates_subset),5:7] <-   estimates_subset[nrow(estimates_subset)-1,5:7]
  
  colnames(average) <- colnames(estimates_subset)
  
  average <- rbind(average, estimates_subset["Average",])
  
}

average <- average[c(2:7),c(5,6,1,4)]

colnames(average) <- c("Inequality Measure", 
                       "Transacion Measure",
                       "Regression Estimate",
                       "P-Value")

rownames(average) <- NULL

print(xtable(average,
             caption = "USA: Bootstrap for Regression in First Difference for 1944-2016",
             label = "tab:bootstrap_usa_diff_post1944",
             align = "llccc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      caption.placement = "top",
      file="./Data/Output/bootstrap_usa_diff_post1944.tex")



