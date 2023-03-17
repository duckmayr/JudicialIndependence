# Replication Data and Code for "Political Competition and Judicial Independence: How Courts Fill the Void When Legislatures Are Ineffective"

To reproduce the results reported in the paper:

- Install R (>=4.1 ideally) and the following R add-on packages:
  + parallel
  + boot
  + rstan
  + mice
  + xtable
  + maps
- Run the R script `01-data-prep.R` to generate the imputed datasets and reproduce Table 1 and Figure 1
- Run the R scripts `02-main-model.R` and `03-lagged-models.R` to fit the models from the paper
- Run the R script `04-coef-plots.R` to reproduce Figures 2 and A.1--3
