## Read in raw data, grab our variables, & clean out completely missing cases
raw_data = read.table("judicial_data.txt", row.names = NULL)
vars = c(
    'country', 'year',                      ## identifiers
    'LJI', 'frac', 'contest', 'vol',        ## outcome & predictors of interest
    'system', 'colonial', 'polity', 'years' ## controls
)
raw_data = raw_data[ , vars]
drop = apply(raw_data, 1, function(x) all(is.na(x)))
raw_data = raw_data[!drop,]

## Use {mice} to create imputed datasets
m = 10
imputed_data = mice::mice(raw_data, m)
for ( i in 1:m ) {
    write.table(mice::complete(imputed_data, i), file = paste0('Data', i))
}

## Use {xtable} to create table showing missingness in the variables
missingness = t(as.matrix(apply(raw_data, 2, function(x) sum(is.na(x)))[4:6]))
xtable::xtable(missingness, caption = 'Missingness')

## Use {maps} to create map of data availability
library(maps)
pdf('map.pdf', width = 6, height = 4)
    ## Start with a blank map of all countries
    map('world')

    ## Plot availability for 'frac'
    countries = unique(raw_data$country[!is.na(raw_data$frac)])
    map('world', region = countries, add = TRUE, col = 'grey80', fill = TRUE)
    map('world', region = 'USA', col = 'grey80', add = TRUE, fill = TRUE)

    ## Plot availability for 'contest'
    countries = unique(raw_data$country[!is.na(raw_data$contest)])
    map('world', region = countries, add = TRUE, col = 'grey50', fill = TRUE)

    ## Plot availability for 'vol'
    countries = unique(raw_data$country[!is.na(raw_data$vol)])
    map('world', region = countries, add = TRUE, col = 'grey20', fill = TRUE)

    ## Add a legend
    legend(
        'bottomright',
        legend = c(
            'Fraction, Contest, and Volatility',
            'Fraction and Contest',
            'Fraction'
        ),
        col = c('grey20', 'grey50', 'grey80'),
        pch = 20, cex = 0.7, bg = 'white'
    )

dev.off()
