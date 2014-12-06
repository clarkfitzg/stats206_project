# Fri Dec  5 13:48:09 PST 2014
# Clark Fitzgerald
# 
# Collects data from Quandl.com for regression analysis of exchange rates
# between South Korea and USA.
# 
# - Downloaded and massaged data structures will be saved in 'country.Rda'.
#   To reload them without downloading again run this in R:
#       > load('country.Rda')
# - Compare other countries by changing the main code in the bottom and
#   using other 3 letter ISO codes
#
# - Save your Quandl authcode in the same directory in a plain text file
#   called 'authcode.txt'.
#
# - Must have a folder in this directory called 'figure' to save plots.
#
# - Edit template.csv to add more parameters using 3 letter ISO codes
#


library(Quandl)
library(reshape2)
library(ggplot2)
library(magrittr)


# Date in Quandl format
today = format(Sys.time(), '%Y-%m-%d')

# Could also put your authcode here as a string
authcode = scan('authcode.txt', what='')

template = read.csv('template.csv', stringsAsFactors=FALSE)

getcountry = function(countrycode){
    # Fetches live data from Quandl

    codes = sprintf(template$code, countrycode)

    out = Quandl(codes, trim_start="1981-04-01", trim_end=today
                , collapse='monthly', sort='asc', authcode=authcode)

    names(out) = c('Date', paste(template$name, countrycode, sep = '_'))
    return(out)
} 

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

    # We'll only impute on the results of na_truncate
    truncd = na_truncate(dframe)

    # On which columns do we need to impute?
    needimpute = sapply(truncd, function(x) any(is.na(x)))

    scaled = data.frame(Date = truncd$Date, scale(truncd[, which(needimpute)]))
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

pipecountry = function(isocode){
    # Takes a single country through the data processing pipeline

    country = getcountry(isocode)
    na_plot(country, sprintf('figure/na_plot_%s.pdf', isocode))

    country %>% interpolate %>% na_truncate
}

ttsplit = function(dframe, testportion, makeglobal=FALSE){
    # Splits the rows of dframe, returning a list of train and 
    # validate dataframes 
    # If makeglobal is TRUE then variables train and validate are created
    # in the global namespace and nothing is returned.

    n = nrow(country)
    testsize = round(n * testportion)
    testindex = sample(1:n, size = testsize, replace=FALSE)
    out = list(train = country[-testindex, ], 
               validate = country[testindex, ])
    if (makeglobal){
        trainset <<- out[['train']]
        validate <<- out[['validate']]
    }
    else{
        return(out)
    }
}

############################################################
# All the action code is here:
############################################################

main = function(){
    # Target Y variable:
    # Exchange rate: won / USD
    exchange = Quandl("FRED/EXKOUS", trim_start="1981-04-01", trim_end=today
                      , collapse='monthly', sort='asc', authcode=authcode)

    names(exchange)[2] = 'exchange_rate'

    USA = pipecountry('USA')
    KOR = pipecountry('KOR')

    # Merge on common 'Date' column. This is the primary table for analysis
    country = merge(exchange, merge(KOR, USA))

    # Reserve one third of the data for a validation set
    ttsplit(country, testportion = 1/3, makeglobal=TRUE)

    save(validate, trainset, country, exchange, USA, KOR, file='country.Rda')
}
