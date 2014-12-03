library(Quandl)

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

# When does each column start?
beginning = sapply(korea, function(x) which(!is.na(x))[1])

# So unemployment, interest rate, and debt start later, in 1999
# That's not so bad. We can truncate the data set to start there and still
# have about 200 rows. That may actually be better because we cut out the
# IMF period in 1999.

# Truncated
k2 = korea[max(beginning):nrow(korea), ]

# May want to chop off the recent end as well

# On which columns do we need to impute?
needimpute = sapply(k2, function(x) any(is.na(x)))

# Need to plot GDP, gdp deflator, debt, govspending
# If plots are roughly linear, then imputing through linear
# interpolation should be ok

# Plot the scaled variables to check for linearity
sk = scale(k2[, which(needimpute)])
sk[, 'x'] = 1:nrow(sk)


#write.csv(korea, 'korea.csv')
