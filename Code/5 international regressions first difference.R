library(stargazer)
library(plm)

#set directory
setwd("~/Documents/GitHub/MDI")

econ_data <- read.csv("./Data/Output/data_merge.csv", stringsAsFactors=FALSE)

econ_data <- econ_data[,-1]

econ_data <- subset(econ_data, year>1912)

#econ_data <- subset(econ_data, country=="Australia" |
#                      country=="Canada" |
#                      country=="Denmark" |
#                      country=="Finland" |
#                      country=="France" |
#                      country=="Japan")

#real GDP
econ_data$gdp <- econ_data$gdp/econ_data$cpi

#real monetary balances
econ_data$narrowm <- econ_data$narrowm/econ_data$cpi

#consumption
econ_data$cons <- econ_data$rconpc*econ_data$pop

#other countries

econ_data$narrowm_diff <- NA
econ_data$gdp_diff <- NA
econ_data$cons_diff <- NA

econ_data$ltrate_diff <- NA
econ_data$top10_diff <- NA
econ_data$top1_diff <- NA
econ_data$gini_diff <- NA

econ_data$narrowm_lag <- NA


for (i in 2:nrow(econ_data)){
  econ_data$narrowm_diff[i] <- log(econ_data$narrowm[i])-log(econ_data$narrowm[i-1])
  econ_data$gdp_diff[i] <- log(econ_data$gdp[i])-log(econ_data$gdp[i-1])
  econ_data$cons_diff[i] <- log(econ_data$cons[i])-log(econ_data$cons[i-1])
  
  econ_data$ltrate_diff[i] <- econ_data$ltrate[i]-econ_data$ltrate[i-1]
  econ_data$top10_diff[i] <- econ_data$top10[i]-econ_data$top10[i-1]
  econ_data$top1_diff[i] <- econ_data$top1[i]-econ_data$top1[i-1]
  econ_data$gini_diff[i] <- econ_data$gini[i]-econ_data$gini[i-1]
  
  econ_data$narrowm_lag[i] <- econ_data$narrowm_diff[i-1]
}

econ_data <- subset(econ_data, year!=1913)

countries <- unique(econ_data$country)

###PANEL OLS

ols1 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top10_diff, data = econ_data)
ols2 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top1_diff, data = econ_data)
ols3 <- lm(narrowm_diff ~ cons_diff + ltrate_diff + narrowm_lag + top10_diff, data = econ_data)
ols4 <- lm(narrowm_diff ~ cons_diff + ltrate_diff + narrowm_lag + top1_diff, data = econ_data)
ols5 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top10_diff + country, data = econ_data)
ols6 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top1_diff + country, data = econ_data)
ols7 <- lm(narrowm_diff ~ cons_diff + ltrate_diff + narrowm_lag + top10_diff + country, data = econ_data)
ols8 <- lm(narrowm_diff ~ cons_diff + ltrate_diff + narrowm_lag + top1_diff + country, data = econ_data)

stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8,
          omit = "country",
          add.lines = list(c("Country Fixed effects?", "No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes")),
          dep.var.labels=c("FD Log M1"),
          covariate.labels=c("FD Log GDP",
                             "FD Log Cons.",
                             "FD Int. Rate",
                             "FD FD Lagged Log M",
                             "FD Top 10",
                             "FD Top 1"),
          label="tab:ols_panel_countries_diff",
          title="First Difference Panel Regression: M1 Money Demand and Inequality",
          omit.stat=c("ser", "f"),
          digits=2,
          out="./Data/Output/ols_panel_countries_diff.tex")

####
#OLS for each country
estimates <- as.data.frame(matrix(ncol=8, nrow=1))

i <- "USA"

for (i in countries){
  
  temp <- subset(econ_data, country==get("i"))
  
  ols1 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top10_diff, data = temp)
  ols2 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top1_diff, data = temp)
  ols3 <- lm(narrowm_diff ~ cons_diff + ltrate_diff + narrowm_lag + top10_diff, data = temp)
  ols4 <- lm(narrowm_diff ~ cons_diff + ltrate_diff + narrowm_lag + top1_diff, data = temp)
  
  coef1 <- as.data.frame(t(summary(ols1)$coefficients[5, ]))
  coef2 <- as.data.frame(t(summary(ols2)$coefficients[5, ]))
  coef3 <- as.data.frame(t(summary(ols3)$coefficients[5, ]))
  coef4 <- as.data.frame(t(summary(ols4)$coefficients[5, ]))
  
  coef1$country <- i
  coef2$country <- i
  coef3$country <- i
  coef4$country <- i
  
  coef1$n <- nobs(ols1)
  coef2$n <- nobs(ols2)
  coef3$n <- nobs(ols3)
  coef4$n <- nobs(ols4)
  
  coef1$inequality <- "top10"
  coef2$inequality <- "top1"
  coef3$inequality <- "top10"
  coef4$inequality <- "top1"
  
  coef1$transaction <- "gdp"
  coef2$transaction <- "gdp"
  coef3$transaction <- "consumption"
  coef4$transaction <- "consumption"
  
  colnames(estimates) <- colnames(coef1)
  
  estimates <- rbind(estimates, coef1)
  estimates <- rbind(estimates, coef2)
  estimates <- rbind(estimates, coef3)
  estimates <- rbind(estimates, coef4)
  
  print(i)
}

estimates <- estimates[-1,]

colnames(estimates)[1:4] <- c("estimate", "sd", "t.value", "p.value")

#confidence interval using critical values from student t distribution

estimates$lower <- 0
estimates$upper <- 0

for (i in 1:nrow(estimates)){
  estimates$lower[i] <- estimates$estimate[i] - abs(qt(0.05, (estimates$n[i]-1)))*estimates$sd[i]
  estimates$upper[i] <- estimates$estimate[i] + abs(qt(0.05, (estimates$n[i]-1)))*estimates$sd[i]
}

gdp_top10 <- subset(estimates, transaction=="gdp" & inequality=="top10")
gdp_top1 <- subset(estimates, transaction=="gdp" & inequality=="top1")
cons_top10 <- subset(estimates, transaction=="consumption" & inequality=="top10")
cons_top1 <- subset(estimates, transaction=="consumption" & inequality=="top1")


gdp_top10$country <- factor(gdp_top10$country, 
                            levels = gdp_top10$country[order(gdp_top10$estimate)])

g <- ggplot(gdp_top10, aes(estimate,country))
g <- g + geom_point()
g <- g + geom_text(aes(label=round(estimate,2)),hjust=-.1, vjust=.5)
g <- g + geom_errorbar(aes(y = country, xmin = lower, xmax = upper)) 
g

#save file
pdf("./Data/Output/country_gdp_top10_coefficients_diff.pdf")
g
dev.off()

gdp_top1$country <- factor(gdp_top1$country, 
                           levels = gdp_top1$country[order(gdp_top1$estimate)])

g <- ggplot(gdp_top1, aes(estimate,country))
g <- g + geom_point()
g <- g + geom_text(aes(label=round(estimate,2)),hjust=-.1, vjust=.5)
g <- g + geom_errorbar(aes(y = country, xmin = lower, xmax = upper)) 
g

#save file
pdf("./Data/Output/country_gdp_top1_coefficients_diff.pdf")
g
dev.off()

cons_top10$country <- factor(cons_top10$country, 
                             levels = cons_top10$country[order(cons_top10$estimate)])

g <- ggplot(cons_top10, aes(estimate,country))
g <- g + geom_point()
g <- g + geom_text(aes(label=round(estimate,2)),hjust=-.1, vjust=.5)
g <- g + geom_errorbar(aes(y = country, xmin = lower, xmax = upper)) 
g

#save file
pdf("./Data/Output/country_cons_top10_coefficients_diff.pdf")
g
dev.off()


cons_top1$country <- factor(cons_top1$country, 
                            levels = cons_top1$country[order(cons_top1$estimate)])

g <- ggplot(cons_top1, aes(estimate,country))
g <- g + geom_point()
g <- g + geom_text(aes(label=round(estimate,2)),hjust=-.1, vjust=.5)
g <- g + geom_errorbar(aes(y = country, xmin = lower, xmax = upper)) 
g

#save file
pdf("./Data/Output/country_cons_top1_coefficients_diff.pdf")
g
dev.off()




summary(ols2)




####
#OLDER:

###
#USA
usa <- subset(econ_data, country=="USA")

#add gini data (from WIID UNU WIDER database)
gini <- read.csv("./Data/Output/gini_usa_1944_2008.csv")

gini <- gini[,3:4]

usa <- merge(usa, gini, by="year", all=T)

usa$narrowm_diff <- NA
usa$gdp_diff <- NA
usa$cons_diff <- NA

usa$ltrate_diff <- NA
usa$top10_diff <- NA
usa$top1_diff <- NA
usa$gini_diff <- NA

usa$narrowm_lag <- NA

i <- 2

for (i in 2:nrow(usa)){
  usa$narrowm_diff[i] <- log(usa$narrowm[i])-log(usa$narrowm[i-1])
  usa$gdp_diff[i] <- log(usa$gdp[i])-log(usa$gdp[i-1])
  usa$cons_diff[i] <- log(usa$cons[i])-log(usa$cons[i-1])
  
  usa$ltrate_diff[i] <- usa$ltrate[i]-usa$ltrate[i-1]
  usa$top10_diff[i] <- usa$top10[i]-usa$top10[i-1]
  usa$top1_diff[i] <- usa$top1[i]-usa$top1[i-1]
  usa$gini_diff[i] <- usa$gini[i]-usa$gini[i-1]
  
  usa$narrowm_lag[i] <- usa$narrowm_diff[i-1]
}

#usa <- usa[-1,]

sapply(usa, class)

ols1 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag, data = usa)
summary(ols1)

ols2 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top10_diff, data = usa)
summary(ols2)

ols3 <- lm(narrowm_diff ~ cons_diff + ltrate_diff + narrowm_lag, data = usa)
summary(ols3)

ols4 <- lm(narrowm_diff ~ cons_diff + ltrate_diff + narrowm_lag + top10_diff, data = usa)
summary(ols3)

ols5 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top1_diff, data = usa)
summary(ols5)

ols6 <- lm(narrowm_diff ~ cons_diff + ltrate_diff + narrowm_lag + top1_diff, data = usa)
summary(ols6)

ols7 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + gini_diff, data = usa)
summary(ols7)

ols8 <- lm(narrowm_diff ~ cons_diff + ltrate_diff + narrowm_lag + gini_diff, data = usa)
summary(ols8)


stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8, 
          dep.var.labels=c("FD Log Narrow Money"),
          covariate.labels=c("FD Log GDP",
                             "FD Log Cons.",
                             "FD Int. Rate",
                             "Lagged FD Log M",
                             "FD Top 10",
                             "FD Top 1",
                             "FD Gini"),
          label="tab:ols_diff_usa",
          title="USA: Money Demand and Inequality, First Difference Regression",
          omit.stat=c("ser", "f"),
          digits=2,
          out="./Data/Output/ols_diff_usa.tex")

#Cover and Hooks subsample

usa_subsample <- subset(usa, year>1948 & year <1989)

ols1 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag, data = usa_subsample)
summary(ols1)

ols2 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top10_diff, data = usa_subsample)
summary(ols2)

ols3 <- lm(narrowm_diff ~ cons_diff + ltrate_diff + narrowm_lag, data = usa_subsample)
summary(ols3)

ols4 <- lm(narrowm_diff ~ cons_diff + ltrate_diff + narrowm_lag + top10_diff, data = usa_subsample)
summary(ols3)

ols5 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top1_diff, data = usa_subsample)
summary(ols5)

ols6 <- lm(narrowm_diff ~ cons_diff + ltrate_diff + narrowm_lag + top1_diff, data = usa_subsample)
summary(ols6)

ols7 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + gini_diff, data = usa_subsample)
summary(ols7)

ols8 <- lm(narrowm_diff ~ cons_diff + ltrate_diff + narrowm_lag + gini_diff, data = usa_subsample)
summary(ols8)


stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8,
          dep.var.labels=c("FD Log Narrow Money"),
          covariate.labels=c("FD Log GDP",
                             "FD Log Cons.",
                             "FD Int. Rate",
                             "FD FD Lagged Log M",
                             "FD Top 10",
                             "FD Top 1",
                             "Gini"),
          label="tab:ols_diff_usa",
          title="USA (Subsample 1949-1988): Money Demand and Inequality, First Difference Regression",
          omit.stat=c("ser", "f"),
          digits=2,
          out="./Data/Output/ols_diff_usa_subsample.tex")

###
##OLDER

ols1 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top10_diff, data = subset(econ_data, country=="Australia"))
ols2 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top10_diff, data = subset(econ_data, country=="Canada"))
ols3 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top10_diff, data = subset(econ_data, country=="Denmark"))
ols4 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top10_diff, data = subset(econ_data, country=="Finland"))
ols5 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top10_diff, data = subset(econ_data, country=="France"))
ols6 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top10_diff, data = subset(econ_data, country=="Japan"))

stargazer(ols1, ols2, ols3, ols4, ols5, ols6, 
          dep.var.labels=c("FD Log Narrow Money"),
          column.labels=c("Australia",
                          "Canada",
                          "Denmark",
                          "Finland",
                          "France",
                          "Japan",
                          "USA"),
          covariate.labels=c("FD Log GDP",
                             "FD Int. Rate",
                             "Lagged FD Log M",
                             "FD Top 10"),
          label="tab:ols_diff_int",
          title="International Evidence: Money Demand and Inequality, First Difference Regression",
          omit.stat=c("ser", "f"),
          digits=2,
          out="./Data/Output/ols_diff_international_top10.tex")

#GDP, TOP 1
ols1 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top1_diff, data = subset(econ_data, country=="Australia"))
ols2 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top1_diff, data = subset(econ_data, country=="Canada"))
ols3 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top1_diff, data = subset(econ_data, country=="Denmark"))
ols4 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top1_diff, data = subset(econ_data, country=="Finland"))
ols5 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top1_diff, data = subset(econ_data, country=="France"))
ols6 <- lm(narrowm_diff ~ gdp_diff + ltrate_diff + narrowm_lag + top1_diff, data = subset(econ_data, country=="Japan"))

stargazer(ols1, ols2, ols3, ols4, ols5, ols6, 
          dep.var.labels=c("FD Log Narrow Money"),
          column.labels=c("Australia",
                          "Canada",
                          "Denmark",
                          "Finland",
                          "France",
                          "Japan",
                          "USA"),
          covariate.labels=c("FD Log GDP",
                             "FD Int. Rate",
                             "Lagged FD Log M",
                             "FD Top 1"),
          label="tab:ols_diff_int",
          title="International Evidence: Money Demand and Inequality, First Difference Regression",
          omit.stat=c("ser", "f"),
          digits=2,
          out="./Data/Output/ols_diff_international_top1.tex")


