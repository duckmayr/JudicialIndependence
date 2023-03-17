#' Function to pool results across imputed datasets
#'
#' @param k Which variable of interest should we pool results for?
#'   1 corresponds to 'frac', 2 to 'contest', 3 to 'vol'
#' @param n How many observations should we expect? (This changes based on lag)
#' @param m How many imputed datasets are we looking for?
#' @param lag How many lags?
#'
#' @return A numeric matrix where each row is for a predictor, with columns
#'   'mean', 'se', 't value', 'Pr(>|t|)', '2.5%', and '97.5%
pool_results = function(k, n, m = 10, lag = 0){
    coef = numeric()
    coef_sd = numeric()
    filename_base = "fitSum"
    if ( lag > 0 ) {
        filename_base = paste0(filename_base, "L", lag)
    }
    for ( d in 1:m ) {
        hold = read.csv(paste0(filename_base, ".", k, ".", d, ".csv"))
        coef = cbind(coef, hold$mean)
        coef_sd = cbind(coef_sd, hold$sd)
    }
    variable_names = hold$X
    coef_mean = apply(coef, 1, mean)
    between.var = apply(coef, 1, var)
    within.var = apply(coef_sd^2, 1, mean)
    impute.se.vec = sqrt(within.var + ((m+1)/m)*between.var)
    impute.df = (m-1) * (1 + (m/(m+1)) * within.var/between.var)^2
    gamma = (m/(m+1)) * between.var / (within.var + ((m+1)/m) * between.var)
    full.df = n - 1
    adj.full.df = ((full.df+1) / (full.df+3)) * full.df * (1-gamma)
    adj.imp.df = 1 / (1 / impute.df + 1 / adj.full.df)
    result = round(cbind(
        coef_mean, impute.se.vec,
        coef_mean / impute.se.vec,
        1 - pt(abs(coef_mean / impute.se.vec), adj.imp.df),
        coef_mean - 1.96 * impute.se.vec,
        coef_mean + 1.96 * impute.se.vec
    ), 2)
    colnames(result) = c('mean', 'se', 't value', 'Pr(>|t|)', '2.5%', '97.5%')
    rownames(result) = variable_names
    return(result)
}

## Set the number of observations for each model
n = c(936, 892, 848, 805)

## Pool results for the main model
out.table = list()
for(k in 1:3) out.table[[k]] = pool_results(k, n[1])

## Pool results for each of the lagged models in the appendix
out.tableL1 = list()
for(k in 1:3) out.tableL1[[k]] = pool_results(k, n[2], lag = 1)
out.tableL2 = list()
for(k in 1:3) out.tableL2[[k]] = pool_results(k, n[3], lag = 2)
out.tableL3 = list()
for(k in 1:3) out.tableL3[[k]] = pool_results(k, n[4], lag = 3)


#' Function to create coefficient plots
#'
#' @param lag How many lags does the model we're created plots for have?
#'
#' @return Returns `NULL`; called for its byproducts, coefficient plots saved
#'   as PDFs
plot_results = function(lag = 0){
    if ( lag > 0 ) {
        lag = paste0("L", lag)
    } else {
        lag = ""
    }

    to_plot = get(paste0('out.table', lag), envir = .GlobalEnv)
    vars = c(paste0('B[', 1:4, ']'), paste0('G[', 1:4, ']'), 'sigmaa', 'sigmay')
    coefs = to_plot[[1]][vars, 'mean']
    lower = to_plot[[1]][vars, '2.5%']
    upper = to_plot[[1]][vars, '97.5%']

    yticklabs = c(
        'System', 'Polity', 'Years', 'Spain', 'UK', 'Portugal', 'France',
        expression(sigma~'a'), expression(sigma~'y')
    )

    pdf(paste0('figures/mod1_coefplot', lag, '.pdf'))
    par(mar = c(4, 6, 2, 2), family = 'Times')
    plot(
        x = coefs, y = 10:1, xlim = c(-3.2, 1.2), pch = 18,
        ylab = '', xlab = '', yaxt = 'n', xaxt = 'n'
    )
    abline(v = 0, lty = 2)
    axis(
        side = 2, at = 10:1, labels = c('Fraction', yticklabs),
        las = 2, tick = FALSE, cex.axis = 1.5
    )
    axis(side = 1, at = -3:1, tick = FALSE, padj = 1, cex.axis = 1.5)
    segments(x0 = lower, x1 = upper, y0 = 10:1, y1 = 10:1)
    dev.off()

    coefs = to_plot[[2]][vars, 'mean']
    lower = to_plot[[2]][vars, '2.5%']
    upper = to_plot[[2]][vars, '97.5%']


    pdf(paste0('figures/mod2_coefplot', lag, '.pdf'))
    par(mar = c(4, 6, 2, 2), family = 'Times')
    plot(
        x = coefs, y = 10:1, pch = 18, xlim = c(-3.2, 1.2),
        ylab = '', xlab = '', yaxt = 'n', xaxt = 'n'
    )
    abline(v = 0, lty = 2)
    axis(
        side = 2, at = 10:1, labels = c('Contest', yticklabs),
        las = 2, tick = FALSE, cex.axis = 1.5
    )
    axis(side = 1, at = -3:1, tick = FALSE, padj = 1, cex.axis = 1.5)
    segments(x0 = lower, x1 = upper, y0 = 10:1, y1 = 10:1)
    dev.off()

    coefs = to_plot[[3]][vars, 'mean']
    lower = to_plot[[3]][vars, '2.5%']
    upper = to_plot[[3]][vars, '97.5%']

    pdf(paste0('figures/mod3_coefplot', lag, '.pdf'))
    par(mar = c(4, 6, 2, 2), family = 'Times')
    plot(
        x = coefs, y = 10:1, pch = 18, xlim = c(-3.2, 1.2),
        ylab = '', xlab = '', yaxt = 'n', xaxt = 'n'
    )
    abline(v = 0, lty = 2)
    axis(
        side = 2, at = 10:1, labels = c('Volatility', yticklabs),
        las = 2, tick = FALSE, cex.axis = 1.5
    )
    axis(side = 1, at = -3:1, tick = FALSE, padj = 1, cex.axis = 1.5)
    segments(x0 = lower, x1 = upper, y0 = 10:1, y1 = 10:1)
    dev.off()

    NULL
}

## Generate the coefficient plots
lapply(0:3, plot_results)
