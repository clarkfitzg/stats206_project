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
# - Edit quandl_template.csv to add more parameters using 3 letter ISO codes
#


library(Quandl)

# Date in Quandl format
today = format(Sys.time(), '%Y-%m-%d')

# Could also put your authcode here as a string
authcode = scan('authcode.txt', what='')

template = read.csv('quandl_template.csv', stringsAsFactors=FALSE)

getcountry = function(countrycode){
    # Fetches live data from Quandl

    codes = sprintf(template$code, countrycode)

    out = Quandl(codes, trim_start="1981-04-01", trim_end=today
                , collapse='monthly', sort='asc', authcode=authcode)

    names(out) = c('Date', paste(template$name, countrycode, sep = '_'))
    return(out)
} 

# Target Y variable:
# Exchange rate: won / USD
exchange = Quandl("FRED/EXKOUS", trim_start="1981-04-01", trim_end=today
                  , collapse='monthly', sort='asc', authcode=authcode)

names(exchange)[2] = 'exchange_rate'

USA = getcountry('USA')
KOR = getcountry('KOR')

save(exchange, USA, KOR, file='cache.Rda')
