# Final analysis to produce report

library(ggplot2)

set.seed(37)

source('functions.R')
load('country.Rda')

############################################################
# Exploratory data analysis
############################################################

# Exchange rate over time
qplot(Date, exchange_rate, data = country, ylab = 'Won / Dollar')
ggsave('exchange_rate.pdf', width = 6, height = 8, units = 'in')

# Most recent US debt figures
# Something is very wrong, starting in 2014-05-31
# Debt is way too high.
tail(USA[!is.na(USA$debt_USA), c('Date', 'debt_USA')])

# Justifies linear interpolation
na_plot(KOR)
ggsave('na_plot_KOR.pdf', height = 4, units = 'in')
na_plot(USA)
ggsave('na_plot_USA.pdf', height = 4, units = 'in')

# Drop the date column, so we know that it's not used in the analysis
nodate = country[, names(country) != 'Date']
dim(nodate)

pdf('scatterplot.pdf')
plot(nodate[, 1:5])
dev.off()

pdf('correlation.pdf')
corhist(trainset)
dev.off()

# How many pairs of columns had sample correlation greater than 0.9?
cn = cor(nodate)
cnv = cn[upper.tri(cn)]
sum((abs(cnv) >= 0.9) / length(cnv))

# Get the difference between subsequent rows
pdf('diff_scatterplot.pdf')
countrydiff = as.data.frame(sapply(nodate, diff))
plot(countrydiff[, 1:5])
dev.off()
