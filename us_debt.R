# Why is the US debt decreasing? 
# Because of the scale - it's quite negative.

pdf('us_debt.pdf')
load('country.Rda')

plot(USA$Date, USA$debt_USA)

dev.off()
