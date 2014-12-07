# We expect that the pairwise correlations will be pretty high

pdf('correlation.pdf')
load('country.Rda')

corhist = function(dframe){
    # Plot a histogram of correlations in a dataframe

    # Only doing the numeric columns
    numcols = sapply(dframe, is.numeric)
    cormatrix = cor(dframe[, numcols])

    hist(cormatrix[upper.tri(cormatrix)], main = '',
         xlab = 'Correlations')
}

corhist(trainset)

dev.off()
