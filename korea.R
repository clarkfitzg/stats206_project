library(Quandl)

authcode = scan('authcode.txt', what='')

template = read.csv('template.csv', stringsAsFactors=FALSE)

# Target Y variable:
# Exchange rate: won / USD
#exchange.rate = Quandl("FRED/EXKOUS", trim_start="1981-04-01", trim_end="2014-10-01", authcode = authcode)

countrycode = 'KOR'

getcountry = function(countrycode){
    codes = sprintf(template$code, countrycode)

    out = Quandl(codes, trim_start="1981-04-01", trim_end="2014-10-01"
                , collapse='monthly', sort='asc', authcode=authcode)

    names(out) = c('Date', paste(template$name, countrycode, sep = '_'))
    return(out)
} 

korea = getcountry('KOR')

write.csv(korea, 'korea.csv')
