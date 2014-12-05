library(Quandl)
library(reshape2)
library(ggplot2)
library(magrittr)

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

na_truncate = function(dframe){
    # Truncates a data frame by identifying the smallest range for
    # all variables

    # When does each column start and end?
    begindate = sapply(dframe, function(x) which(!is.na(x))[1])
    enddate = sapply(dframe, function(x) tail(which(!is.na(x)), 1))

    dframe[max(begindate):min(enddate), ]
}

na_plot = function(dframe, filename){
    # Plot the scaled variables to check for linearity.
    # If plots are roughly linear, then imputing through linear
    # interpolation should be ok.

    # On which columns do we need to impute?
    needimpute = sapply(dframe, function(x) any(is.na(x)))

    scaled = data.frame(Date = dframe$Date, scale(dframe[, which(needimpute)]))
    scaled_long = reshape2::melt(scaled, id = 'Date')

    ggplot(data=scaled_long[complete.cases(scaled_long), ]
           , aes(x=Date, y=value, color=variable)) + 
           geom_line()

    ggsave(filename)
}

interpolate = function(dframe){
    # Impute missing data using linear interpolation.

    # The Date column was causing problems.
    data.frame(Date = dframe$Date, (na.approx(dframe[, -1])))
}

pipecountry = function(country){
    # Takes a single country through the data processing pipeline
    country %>% interpolate %>% na_truncate
}

a = pipecountry(korea)

# Now they should be all populated
sum(complete.cases(a)) == nrow(a)

#kfull = data.frame(Date = k2$Date, (na.approx(k2[, -1])))

#write.csv(kfull, 'korea.csv')
