library(ggplot2)

# Exploratory data analysis
load('country.Rda')
source('functions.R')

# Why is debt going down for the US? Because it's a huge negative number!
# Plot it and look at the range in order to see this.

# Exchange rate over time
qplot(Date, exchange_rate, data = country, ylab = 'Won / Dollar')
ggsave('exchange_rate.pdf', width = 6, height = 8, units = 'in')

# What's the max difference?
r = range(country$exchange_rate)
r[2] / r[1]

# Will it fit?
fit1 = lm(exchange_rate ~ ., data = trainset[, -1])
summary(fit1)
plot(fit1)

# Intentionally Remove the effects of time
timefit = lm(exchange_rate ~ Date, data=trainset)
plot(timefit)
# This is a terrible fit - exchange rate is not linear over time.
# Anyways, it would be difficult to fully remove time

country_notime = cbind(exchange_notime = trainset$exchange_rate - predict(timefit),
                       trainset[, -c(1, 2)])

fit2 = lm(exchange_notime ~ ., data = country_notime)
summary(fit2)
plot(fit2)


# Thu Dec 11 14:18:14 PST 2014

set.seed(123)

# Drop the date column
nodate = country[, names(country) != 'Date']
dim(nodate)

# Exploratory data analysis
pdf('scatterplot.pdf')
plot(nodate[, 1:5])
dev.off()

plot(nodate[, 1:5])
# Time dependent patterns are very obvious here
# We want to be seeing a cloud
# Who put these snakes in my data?!!
plot(nodate[, c(1, 6:10)])
plot(nodate[, c(1, 11:16)])
plot(nodate[, c(1, 17:21)])

# Suppose we pick out the variables where the patterns between
# exchange_rate and the other variables are least obvious?
# We can see them from the plots above.
nopattern = c('exchange_rate', 'unemployment_KOR', 'interest_rate_KOR',
              'inflation_KOR', 'interest_rate_USA', 'inflation_USA')

# What happens if we just regress on these?
fitnopattern = lm(exchange_rate ~ ., data = nodate[, nopattern])
summary(fitnopattern)
# R^2 is around 0.6, so it doesn't do all that well.

# Get the difference between each previous row
# As suggested by Taeyen
countrydiff = as.data.frame(sapply(nodate, diff))

ttsplit(countrydiff, 1 / 3, makeglobal=TRUE)

plot(trainset[, c('exchange_rate', 'interest_rate_KOR', 'interest_rate_USA')]) 

# Many of these 'diffed' variables are close to discrete

plot(trainset[, 1:5])
# This looks much more like a proper data cloud
# But I worry that exchange rate is too flat when seen as a function of the
# other variables
plot(trainset[, c(1, 6:10)])
plot(trainset[, c(1, 11:16)])
plot(trainset[, c(1, 17:21)])


mod1 = lm(exchange_rate ~ ., data=trainset)
summary(mod1)

library(leaps)

rs1 = regsubsets(exchange_rate ~ ., data=trainset, nbest=1, nvmax=21)
rs1s = summary(rs1)

modelstats = data.frame(p = 2:21,
    sse = rs1s$rss,
    r2 = rs1s$rsq,
    r2a = rs1s$adjr2,
    cp = rs1s$cp)

modelstats


fullmod = mod1
fullMSE = summary(fullmod)$sigma ** 2

ssto = sum((trainset$exchange_rate - mean(trainset$exchange_rate)) ** 2)

n = nrow(trainset)

############################################################
# SSE should be decreasing and nonnegative
# Very surprisingly, rss here is what we have been calling sse!
modelstats = data.frame(p = 1:ncol(trainset) + 1,
    sse = rs1s$rss,
    r2 = rs1s$rsq,
    r2a = rs1s$adjr2,
    cp = rs1s$cp)

# define the first row
row1 = data.frame(p = 1, sse = ssto, r2 = 0, r2a = 0,
                  cp = ssto / fullMSE - (n - 2))

# Add the first row with the others
modelstats = rbind(row1, modelstats)

# Add AIC, BIC columns
modelstats$aic = with(modelstats, n * log(sse / n) + 2 * p)
modelstats$bic = with(modelstats, n * log(sse / n) + log(n) * p)

modelstats
sapply(modelstats, which.min)
#


showmodel = function(p, leapsummary){
    # returns the coefficient names from the model with p coefficients
    # p - 1 used because first term includes intercept
    pick = leapsummary$which[p - 1, ]
    names(pick[pick])
}

# Only picked 9- and they look like the ones that were more of a cloud from
# the EDA- the nopattern ones
nopattern
sapply(1:9, showmodel, rs1s)

hist(country$inflation_USA)

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

fitratio = lm(exchange_rate ~ ., data=ratio)
summary(fitratio)

# Another test train split
set.seed(978)
a = ttsplit(ratio, 1/3)
tr = a[[1]]
test = a[[2]]
dim(tr)
head(tr)

library(MASS)

fit3 = lm(exchange_rate ~ ., data=tr)
boxcox(fit3)

# Try an inverse square transformation
fit4 = lm(I(1 / exchange_rate^2) ~ ., data=tr)
summary(fit4)
plot(fit4, which=1)

# Not plotting QQ plot
qqnorm(fit4$residuals)
qqline(fit4$residuals)


# Try an inverse transformation
# We like this one better :)
fit5 = lm(I(1 / exchange_rate) ~ ., data=tr)
summary(fit5)
plot(fit5, which=1)

# Not plotting QQ plot
qqnorm(fit5$residuals)

library(leaps)

fit6 = regsubsets(I(1 / exchange_rate) ~ ., data=tr, nvmax=11)
#fit6 = regsubsets(, data=trainset, nbest=1, nvmax=21)
sfit6 = summary(fit6)

modelstats = data.frame(
    sse = sfit6$rss,
    bic = sfit6$bic,
    r2 = sfit6$rsq,
    r2a = sfit6$adjr2,
    cp = sfit6$cp)

modelstats

finalvars = sfit6$which[5, ]

tdata = tr[, sfit6$which[5, ]]
dim(tdata)
names(tdata)

fit7 = lm(I(1 / exchange_rate) ~ ., data=tdata)
summary(fit7)

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

