# Preprocessing steps

# Cache from downloader
load('cache.Rda')

source('functions.R')


set.seed(893)

USA_clean = pipecountry(USA)
KOR_clean = pipecountry(KOR)

# Merge on common 'Date' column. This is the primary table for analysis
country = merge(exchange, merge(KOR_clean, USA_clean))

# Reserve one third of the data for a validation set
ttsplit(country, testportion = 1/3, makeglobal=TRUE)

save(validate, trainset, country, exchange, USA, KOR, file='country.Rda')
