# Final analysis to produce report

library(MASS)
library(leaps)
library(xtable)

source('functions.R')
load('country.Rda')


# Try adding KOR / USD variables
ratio = with(country, 
             data.frame(exchange_rate = exchange_rate
                        , gdp = gdp_KOR / gdp_USA
                        , unemployment = unemployment_KOR / unemployment_USA
                        , debt = debt_KOR / debt_USA
                        , imports = imports_KOR / imports_USA
                        , exports = exports_KOR / exports_USA
                        , interest_rate = interest_rate_KOR / interest_rate_USA
                        , inflation_KOR = inflation_KOR
                        , inflation_USA = inflation_USA
                        , cpi = cpi_KOR / cpi_USA
                        , govspending = govspending_KOR / govspending_USA
                        ))

# test train split
set.seed(978)
a = ttsplit(ratio, 1/3)
tr = a[[1]]
test = a[[2]]
dim(tr)
head(tr)

# Stepwise regression
fit6 = regsubsets(I(1 / exchange_rate) ~ ., data=tr, nvmax=11)
sfit6 = summary(fit6)

modelstats = data.frame(
    p = 2:11,
    sse = sfit6$rss,
    bic = sfit6$bic,
    r2 = sfit6$rsq,
    r2a = sfit6$adjr2,
    cp = sfit6$cp)

modelstats
xtable(modelstats)

finalvars = sfit6$which[5, ]

tdata = tr[, sfit6$which[5, ]]
dim(tdata)
names(tdata)

fit7 = lm(I(1 / exchange_rate) ~ ., data=tdata)
summary(fit7)
pdf('boxcox.pdf')
boxcox(exchange_rate ~ ., data=tdata)
dev.off()

qqnorm(fit7$residuals)
# This looks more reasonable
pdf('cooks.pdf')
plot(fit7, which=4)
dev.off()
# 

infm = influence.measures(fit7)
summary(infm)

# DFFIT is a little large. Shouldn't be bigger than this:
2 * sqrt(6 / 121)
# 113 and 117 are our big ones
# What happened then?
country[c(113, 117), 'Date']
# Prime mortgage rate crash!!

# How is the fit when we remove the two outliers?
outindex = which(row.names(tdata) %in% c('112', '113', '117'))
#outindex = which(row.names(tdata) == '113')
tdata8 = tdata[-outindex, ]

finalform = I(1 / exchange_rate) ~ .

fit8 = lm(I(1 / exchange_rate) ~ ., data=tdata8)
summary(fit8)

plot(fit8, which=4)
qqnorm(fit8$residuals)

# This is looking much better.
# Try it on the test set
test9 = test[, finalvars]
fit9 = lm(I(1 / exchange_rate) ~ ., data=test9)
summary(fit9)
plot.lm(fit9, which=4)

coef(fit8)
coef(fit9)

form10 = exchange_rate ~ gdp + interest_rate + inflation_KOR + inflation_USA
fit10 = lm(form10, data= tdata8)
# Do we still need to do inverse transform if outliers are gone? 
boxcox(fit10)
# Still calls for the inverse transform even with the outliers removed.
summary(fit10)

