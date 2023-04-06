library(stargazer)
library(plm)

#set directory
setwd("~/Documents/GitHub/MDI")

econ_data <- read.csv("./Data/Output/data_merge.csv", stringsAsFactors=FALSE)

econ_data <- econ_data[,-1]

econ_data <- subset(econ_data, year>1913)

#econ_data <- subset(econ_data, country=="Australia" |
#                      country=="Canada" |
#                      country=="Denmark" |
#                      country=="Finland" |
#                      country=="France" |
#                      country=="Japan")

#real money balances
econ_data$narrowm <- econ_data$narrowm/econ_data$cpi
#real GDP
econ_data$gdp <- econ_data$gdp/econ_data$cpi
#real consumption
econ_data$cons <- econ_data$rgdppc*econ_data$pop


countries <- unique(econ_data$country)


#####PANEL REGRESSION
#all countries

ols1 <- lm(log(narrowm) ~ log(gdp) + log(stir), data = econ_data)

ols2 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top10), data = econ_data)

ols3 <- lm(log(narrowm) ~ log(cons) + log(stir), data = econ_data)

ols4 <- lm(log(narrowm) ~ log(cons) + log(stir) + log(top10), data = econ_data)

ols5 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top1), data = econ_data)

ols6 <- lm(log(narrowm) ~ log(cons) + log(stir) + log(top1), data = econ_data)

#COUNTRY FIXED EFFECTS

ols7 <- lm(log(narrowm) ~ log(gdp) + log(stir) + country, data = econ_data)

ols8 <- lm(log(narrowm) ~ log(cons) + log(stir) + country, data = econ_data)

ols9 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top10) + country, data = econ_data)

ols10 <- lm(log(narrowm) ~ log(cons) + log(stir) + log(top10) + country, data = econ_data)

ols11 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top1) + country, data = econ_data)

ols12 <- lm(log(narrowm) ~ log(cons) + log(stir) + log(top1) + country, data = econ_data)

summary(ols7)

stargazer(ols7, ols8, ols9, ols10, ols11, ols12,
          omit = "country",
          #add.lines = list(c("Country FE?", "No", "No", "No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes")),
          dep.var.labels=c("Narrow Money"),
          covariate.labels=c("GDP",
                             "Cons",
                             "Int. Rate",
                             "Top 10",
                             "Top 1"),
          label="tab:ols_all_countries",
          title="Panel (Country Fixed Effect) Regression in Levels: Money Demand and Inequality",
          omit.stat=c("ser", "f"),
          digits=2,
          font.size = "footnotesize",
          out="./Data/Output/ols_all_countries.tex")


ols1 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top10),
           data = subset(econ_data, country=="Ireland"))
summary(ols1)

####
#OLS for each country
estimates <- as.data.frame(matrix(ncol=8, nrow=1))

i <- "Ireland"

for (i in countries){
  
  temp <- subset(econ_data, country==get("i"))
  
  ols1 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top10), data = temp)
  ols2 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top1), data = temp)
  ols3 <- lm(log(narrowm) ~ log(cons) + log(stir) + log(top10), data = temp)
  ols4 <- lm(log(narrowm) ~ log(cons) + log(stir) + log(top1), data = temp)
  
  coef1 <- as.data.frame(t(summary(ols1)$coefficients[4, ]))
  coef2 <- as.data.frame(t(summary(ols2)$coefficients[4, ]))
  coef3 <- as.data.frame(t(summary(ols3)$coefficients[4, ]))
  coef4 <- as.data.frame(t(summary(ols4)$coefficients[4, ]))
  
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
pdf("./Data/Output/country_gdp_top10_coefficients.pdf")
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
pdf("./Data/Output/country_gdp_top1_coefficients.pdf")
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
pdf("./Data/Output/country_cons_top10_coefficients.pdf")
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
pdf("./Data/Output/country_cons_top1_coefficients.pdf")
g
dev.off()



####OLDER FIRST DIFFERENCE REGRESSION (no lagged money)
#panel regression
#reorder columns (see plm documentation)
#http://ftp.uni-bayreuth.de/math/statlib/R/CRAN/doc/vignettes/plm/plmEN.pdf
#Observations are assumed to be sorted by individuals first, and by period.

plm <- plm(log(narrowm)~-1+log(gdp)+log(stir),data=econ_data,model="within")
summary(plm)

plm <- plm(log(narrowm)~-1+log(gdp)+log(stir)+log(top1),data=econ_data,model="within")
summary(plm)

plm <- plm(log(narrowm)~-1+log(gdp)+log(stir)+log(top10),data=econ_data,model="within")
summary(plm)

#same estimates than OLS with dummy variable
#check first difference

plm1 <- plm(log(narrowm)~-1+log(gdp)+log(stir),data=econ_data,model="fd")
summary(plm1)

plm2 <- plm(log(narrowm)~-1+log(gdp)+log(stir)+log(top1),data=econ_data,model="fd")
summary(plm2)

plm3 <- plm(log(narrowm)~-1+log(gdp)+log(stir)+log(top10),data=econ_data,model="fd")
summary(plm3)

stargazer(plm1, plm2, plm3,
          dep.var.labels=c("Narrow Money"),
          covariate.labels=c("GDP",
                             "Interest Rate",
                             "Top 1 Income Share",
                             "Top 10 Income Share"),
          label="tab:ols_all_countries_first_diff",
          title="All Countries, First Difference Panel Regression",
          omit.stat=c("ser", "f"),
          out="./Data/Output/ols_all_countries_first_diff.tex")






##OLDER
###########


#USA
usa <- subset(econ_data, country=="USA")

#add gini data (from WIID UNU WIDER database)
gini <- read.csv("./Data/Output/gini_usa_1944_2008.csv")

gini <- gini[,3:4]

usa <- merge(usa, gini, by="year", all=T)


ols1 <- lm(log(narrowm) ~ log(gdp) + log(stir), data = usa)

summary(ols1)

ols2 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top10), data = usa)

ols3 <- lm(log(narrowm) ~ log(cons) + log(stir), data = usa)

ols4 <- lm(log(narrowm) ~ log(cons) + log(stir) + log(top10), data = usa)

ols5 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top1), data = usa)

ols6 <- lm(log(narrowm) ~ log(cons) + log(stir) + log(top1), data = usa)

ols7 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(gini), data = usa)

ols8 <- lm(log(narrowm) ~ log(cons) + log(stir) + log(gini), data = usa)

stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7, ols8,
          dep.var.labels=c("Narrow Money"),
          covariate.labels=c("GDP",
                             "Cons.",
                             "Int. Rate",
                             "Top 10",
                             "Top 1",
                             "Gini"),
          label="tab:ols_usa",
          title="USA: Money Demand and Inequality",
          omit.stat=c("ser", "f"),
          digits=2,
          out="./Data/Output/ols_usa.tex")

#all countries comparison. Top decile
ols1 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top10), 
           data = subset(econ_data, country=="Australia"))

ols2 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top10), 
           data = subset(econ_data, country=="Canada"))

ols3 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top10), 
           data = subset(econ_data, country=="Denmark"))

ols4 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top10), 
           data = subset(econ_data, country=="Finland"))

ols5 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top10), 
           data = subset(econ_data, country=="France"))

ols6 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top10), 
           data = subset(econ_data, country=="Japan"))

ols7 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top10), 
           data = subset(econ_data, country=="USA"))

stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7,
          dep.var.labels  = "Narrow Money (log)",
          column.labels=c("Australia",
                           "Canada",
                           "Denmark",
                           "Finland",
                           "France",
                           "Japan",
                           "USA"),
          covariate.labels=c("GDP",
                             "Interest Rate",
                             "Inequality"),
          label="tab:ols_all_countries_top10_comparison",
          title="All Countries: Money Demand and Inequality (Top Decile Income Share)",
          omit.stat=c("ser", "f"),
          digits=2,
          out="./Data/Output/ols_all_countries_top10_comparison.tex")

#all countries comparison. Top percentile
ols1 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top1), 
           data = subset(econ_data, country=="Australia"))

ols2 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top1), 
           data = subset(econ_data, country=="Canada"))

ols3 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top1), 
           data = subset(econ_data, country=="Denmark"))

ols4 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top1), 
           data = subset(econ_data, country=="Finland"))

ols5 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top1), 
           data = subset(econ_data, country=="France"))

ols6 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top1), 
           data = subset(econ_data, country=="Japan"))

ols7 <- lm(log(narrowm) ~ log(gdp) + log(stir) + log(top1), 
           data = subset(econ_data, country=="USA"))

stargazer(ols1, ols2, ols3, ols4, ols5, ols6, ols7,
          dep.var.labels  = "Narrow Money (log)",
          column.labels=c("Australia",
                          "Canada",
                          "Denmark",
                          "Finland",
                          "France",
                          "Japan",
                          "USA"),
          covariate.labels=c("GDP",
                             "Interest Rate",
                             "Inequality"),
          label="tab:ols_all_countries_top1_comparison",
          title="All Countries: Money Demand and Inequality (Top Percentile Income Share)",
          omit.stat=c("ser", "f"),
          digits=2,
          out="./Data/Output/ols_all_countries_top1_comparison.tex")


