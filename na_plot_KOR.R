# Justification of linear interpolation
library(ggplot2)
library(reshape2)

load('country.Rda')

imputed = c('gdp_KOR', 'gdp_deflator_KOR', 'debt_KOR', 
            'interest_rate_KOR', 'govspending_KOR')

scaled = data.frame(Date = KOR$Date, scale(KOR[, imputed]))

scaled_long = reshape2::melt(scaled, id = 'Date')

ggplot(data=scaled_long[complete.cases(scaled_long), ]
       , aes(x=Date, y=value, color=variable)) + 
       geom_line()

ggsave('na_plot_KOR.pdf', height = 4, units = 'in')
