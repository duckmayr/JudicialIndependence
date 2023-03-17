## Load required packages
library(parallel)
library(rstan)
library(boot)

## Set options for parallel and rstan
options(mc.cores = max(1, parallel::detectCores() - 1))
rstan_options(auto_write = TRUE)

## Read in data
data_impute = lapply(paste0("Data", 1:10), read.table, row.names = NULL)

## Define Stan model
judicial = '
data {
  int<lower=1> N;
  int<lower=1> N_coun;
  real y[N];
  int<lower=1, upper=N_coun> coun[N]; //country indicator
  matrix[N, 4] X; //variables (interest, polity, years, system)
  matrix[N_coun, 4] cln; //colonial history
}
parameters {
  vector[4] B;
  real b0;
  vector[N_coun] a;
  real g0;
  vector[4] G;
  real<lower=0> sigmay;
  real<lower=0> sigmaa;
}
model {
  B ~ normal(0, 3);
  b0 ~ std_normal();
  g0 ~ normal(0, 3);
  G ~ normal(0, 3);
  sigmaa ~ inv_gamma(2,.5);
  sigmay ~ inv_gamma(2,.5);
  for(cn in 1:N_coun) a[cn] ~ normal(g0 + cln[cn,]*G, sigmaa);
  for(n in 1:N) y[n] ~ normal(b0 + a[coun[n]] + X[n,]*B, sigmay);
}
'
model = stan_model(model_code = judicial)

## Run appendix models and save raw output
predictors = c('frac', 'contest', 'vol')
all_RHS = c('system', 'colonial', 'polity', 'years', predictors)
export_list = list('k', 'model', 'data_impute', 'predictors')
for ( lag in 1:3 ) {
    cat("Running lag", lag, "models\n")
    original_data = data_impute
    for(dd in 1:m){
        hold = data_impute[[dd]]
        toBind = data.frame()
        for(cn in unique(hold$country)){
            hold.cn = hold[hold$country == cn,]
            hold.cn = hold.cn[order(hold.cn$year), ]
            rows1 = (lag+1):nrow(hold)
            rows2 = 1:(nrow(hold)-lag)
            hold.cn[rows1, all_RHS] = hold.cn[rows2, all_RHS]
            hold.cn[1:lag, ] = NA
            hold.cn = na.omit(hold.cn)
            toBind = rbind(toBind, hold.cn)
        }
        data_impute[[dd]] = toBind
    }
    for ( k in 1:3 ) {
        cat("\tRunning model for predictor", predictors[k], "\n")
        cl = makeCluster(20)
        clusterExport(cl, varlist = export_list)
        parLapply(cl, 1:length(data_impute), function(i){
            tmp = data_impute[[i]]
            y = boot::logit(tmp$LJI)
            n = dim(tmp)[1]
            countries = unique(tmp$country)
            J = length(countries)
            colonial = character(J)
            country = numeric(J)
            for ( j in 1:J ) {
                colonial[j] = tmp[tmp$country == countries[j], 'colonial'][1]
                country[tmp$country == countries[j]] = j
            }
            colonial = as.factor(colonial)
            colonial_ES = ifelse(colonial == 2, 1, 0)
            colonial_GB = ifelse(colonial == 3, 1, 0)
            colonial_PT = ifelse(colonial == 4, 1, 0)
            colonial_FR = ifelse(colonial == 5, 1, 0)
            polity = tmp$polity
            years = tmp$years
            system = tmp$system
            predictor = tmp[, predictors[k]]
            stan_data = list(
                N = n, N_coun = J,
                y = y,
                coun = country,
                X = cbind(predictor, polity, years, system),
                cln = cbind(colonial_ES, colonial_GB, colonial_PT, colonial_FR)
            )
            set.seed(100*k + i)
            fit = rstan::sampling(
                object = model,
                data = stan_data,
                iter = 1500,
                chains = 2,
                cores = 2
            )
            outfile = paste0('fitSum.', k, '.', i, '.csv')
            write.csv(rstan::summary(fit)$summary, file = outfile)
            rm(fit)
            NULL
        })
        stopCluster(cl)
    }
    data_impute = original_data
}
