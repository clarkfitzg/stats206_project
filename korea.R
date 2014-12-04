library(Quandl)
library(reshape2)
library(ggplot2)

authcode = scan('authcode.txt', what='')

template = read.csv('template.csv', stringsAsFactors=FALSE)

# Target Y variable:
# Exchange rate: won / USD
#exchange = Quandl("FRED/EXKOUS", trim_start="1981-04-01", trim_end="2014-10-01", authcode = authcode)

getcountry = function(countrycode){
    codes = sprintf(template$code, countrycode)

    out = Quandl(codes, trim_start="1981-04-01", trim_end="2014-10-01"
                , collapse='monthly', sort='asc', authcode=authcode)

    names(out) = c('Date', paste(template$name, countrycode, sep = '_'))
    return(out)
} 

#korea = getcountry('KOR')

# Impute missing data

# When does each column start and end?
begindate = sapply(korea, function(x) which(!is.na(x))[1])
enddate = sapply(korea, function(x) tail(which(!is.na(x)), 1))

# So unemployment, interest rate, and debt start later, in 1999
# That's not so bad. We can truncate the data set to start there and still
# have about 200 rows. That may actually be better because we cut out the
# IMF period in 1999.

# Truncated
k2 = korea[max(begindate):min(enddate), ]

# On which columns do we need to impute?
needimpute = sapply(k2, function(x) any(is.na(x)))
needimpute[needimpute]
# Need to plot GDP, gdp deflator, debt, govspending
which(is.na(k2$interest_rate_KOR))
# Only one observation missing from interest rate

# If plots are roughly linear, then imputing through linear
# interpolation should be ok. Also could use 
# exponentially weighted moving average

# Plot the scaled variables to check for linearity
sk = data.frame(Date = k2$Date, scale(k2[, which(needimpute)]))
sk_long = reshape2::melt(sk, id = 'Date')

ggplot(data=sk_long[complete.cases(sk_long), ]
       , aes(x=Date, y=value, color=variable)) + 
       geom_line()

ggsave('figure/scaled_variable_kor.pdf')

# Interest rate stays mostly flat
# We could add a column that records the number of NA's
# in that particular row.

num_nas = apply(sk, 1, function(x) sum(is.na(x)))
plot(as.factor(num_nas))
# We see that there are lots of NA's. Could add a boolean variable with
# TRUE for 3 or 4 NA's. May make a difference in regression.

# Now for the linear interpolation. 
# The Date column was causing problems. Maybe that should be
# the index?
kfull = zoo(na.approx(k2[, -1]))
index(kfull) = k2$Date

write.zoo(kfull, 'korea.csv')
