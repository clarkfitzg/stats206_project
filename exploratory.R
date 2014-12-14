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

df(0.2, 6, 121 - 6)

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
test99 = test9[-outindex1,]
#outlier 114 118
outindex1 = which(row.names(test9) %in% c('114', '113', '117','118'))

fit9 = lm(I(1 / exchange_rate) ~ ., data=test9)
summary(fit9)
plot(fit9, which=4)

#no outlier
fit99 = lm(I(1 / exchange_rate) ~ ., data=test99)
summary(fit99)
plot(fit99, which=4)


coef(fit8)
coef(fit9)
form10 = exchange_rate ~ gdp + interest_rate + inflation_KOR + inflation_USA
fit10 = lm(form10, data= tdata8)
# Do we still need to do inverse transform if outliers are gone? 
boxcox(fit10)
# Still calls for the inverse transform even with the outliers removed.
summary(fit10)

add = lm(I(1/exchange_rate) ~ gdp + interest_rate + inflation_KOR + inflation_USA, data = alldata1)
add1 = lm(govspending ~ gdp + interest_rate + inflation_KOR + inflation_USA, data = alldata1)

pdf('Addedvariable.pdf')
plot(add1$resi, add$resi, main = 'Added-variable Plot for govspending', xlab = 'e(govspending|otherX)', ylab = 'e(1/(exchangerate)|otherX)')
dev.off()

summary(add)
summary(add1)

alldata = rbind(test, tr)
outlier = which(row.names(alldata) %in% c('114', '113', '117','118'))
alldata1 = alldata[-outlier]

fit11 = lm(I(1/exchange_rate) ~ gdp + interest_rate + inflation_KOR + inflation_USA, data = alldata)
summary(fit11)
anova(fit11)
plot(fit11)
coef(fit11)


### scaling version ###
sdata = sapply(alldata, scale)
sdata = as.data.frame(sdata)

#I made alldata = c(tr, test), so I cut cut back to get randomized.
str = sdata[1:121,] #train data
stest = sdata[122:181, ] #validation data

boxcox(exchange_rate ~., data = sdata) #boxcox for positive values
rgs = regsubsets(exchange_rate ~., data = str)
srgs = summary(rgs)
srgs$cp #best 5
srgs$bic #best 5

sbest = srgs$which[5,]
bsdata = str[,sbest]

sfit1 = lm(exchange_rate ~ . , data = bsdata)
summary(sfit1)
plot(sfit1)
plot(sfit1, which = 4)
#outliers: 57, 55, 32
summary(influence.measures(sfit1))
soutlier = which(row.names(bsdata) %in% c('55', '57'))
country[soutlier, 'Date']
#[1] "2003-12-31" "2004-02-29"

bsdata_n = bsdata[-soutlier,] #took off the outliers
sfit2 = lm(exchange_rate ~ . , data = bsdata_n)
summary(sfit2)
plot(sfit2)
plot(sfit2, which = 4) #much better

#test set
btest = stest[,sbest]
sfit3 = lm(exchange_rate ~ . , data = btest)
summary(sfit3)
par(mfrow = c(2,2))
plot(sfit3) 
#outlier 134, 135, 145
summary(influence.measures(sfit3))
soutlier2 = which(row.names(btest) %in% c('134', '135'))
country[soutlier2, 'Date']
#[1] "2000-06-30" "2000-07-31" 
btest_n = btest[-soutlier2,]

sfit4 = lm(exchange_rate ~ . , data = btest_n)
summary(sfit4) #still export not significant
#took off.
outs = which(row.names(sdata) %in% c('55', '57','134', '135'))
sdata_n = sdata[-outs,]
sfit5 = lm(exchange_rate ~ gdp + interest_rate + inflation_KOR + inflation_USA, data = sdata_n)
adds = lm(exports ~ gdp + interest_rate + inflation_KOR + inflation_USA, data = sdata_n)
plot(sfit5$residual, adds$residual) # show definitely negative trend, so shouldn't drop off.

sfinal = lm(exchange_rate ~ gdp + exports + interest_rate + inflation_KOR + inflation_USA, data = sdata_n)
summary(sfinal) # all variables are significants
pdf('final_residual.pdf')
par(mfrow=c(2,1))
plot.lm(sfinal, which=c(1,2))
dev.off()

#Actually this result from scaling data is very interesting. The resason I tried it because MSE is too small number(third decimal point)(all are in small range) in the not scaled data, so I wanted to get more reasonable number, so scale the data and try it again.
#By scaling effects, the coefficents and MSE become closer to whole numbers, but comparing the two types of outliers(from scaled data and from non-scaled data) are not common. Non-scaled data outlier showes 2007-2008 financial crisis, and scaled data outlier shows 2000 financial crisis. Why they showed up only one financial crisis not both of them??

# Begin Clark's work
dim(sdata)
colMeans(sdata)

sumfinal = summary(sfinal) 

xtable(data.frame(round(coef(sfinal), 4)))

sinf = influence.measures(sfinal)
summary(sinf)

# Bonferroni's for simultaneous 95 percent confidence of 5 coefficients
multiplier = qt(1 - 0.05 / 5, nrow(sdata) - 5) * sumfinal$coefficients[, 'Std. Error']
sfinalconf = data.frame(lower = coef(sfinal) - multiplier,
                        upper = coef(sfinal) + multiplier)
sfinalconf
xtable(sfinalconf)
