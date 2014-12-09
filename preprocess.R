# Preprocessing steps

library(reshape2)
library(ggplot2)
library(magrittr)


na_truncate = function(dframe){
    # Truncates a data frame by identifying the smallest range for
    # all variables

    # When does each column start and end?
    begindate = sapply(dframe, function(x) which(!is.na(x))[1])
    enddate = sapply(dframe, function(x) tail(which(!is.na(x)), 1))

    dframe[max(begindate):min(enddate), ]
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

x = 10
save(x, file='country.Rda')
############################################################
# All the action code is here:
############################################################
#
#USA = pipecountry('USA')
#KOR = pipecountry('KOR')
#
## Merge on common 'Date' column. This is the primary table for analysis
#country = merge(exchange, merge(KOR, USA))
#
## Reserve one third of the data for a validation set
#ttsplit(country, testportion = 1/3, makeglobal=TRUE)
#
#save(validate, trainset, country, exchange, USA, KOR, file='country.Rda')
#}
