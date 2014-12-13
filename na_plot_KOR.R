# Justification of linear interpolation
library(ggplot2)
library(reshape2)

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

    p = ggplot(data=scaled_long[complete.cases(scaled_long), ]
           , aes(x=Date, y=value, color=variable)) + 
           geom_line()

    return(p)
}

p = na_plot(KOR)
ggsave('na_plot_KOR2.pdf', height = 4, units = 'in')

#
#
#load('country.Rda')
#
#imputed = c('gdp_KOR', 'gdp_deflator_KOR', 'debt_KOR', 
#            'interest_rate_KOR', 'govspending_KOR')
#
#scaled = data.frame(Date = KOR$Date, scale(KOR[, imputed]))
#
#scaled_long = reshape2::melt(scaled, id = 'Date')
#
#ggplot(data=scaled_long[complete.cases(scaled_long), ]
#       , aes(x=Date, y=value, color=variable)) + 
#       geom_line()
#
#ggsave('na_plot_KOR.pdf', height = 4, units = 'in')
