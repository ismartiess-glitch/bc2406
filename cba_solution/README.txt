BC2406 CBA — R analyses for Questions 1–7

1. Open this folder as your working directory in R/RStudio.
2. Ensure the recommended R package rpart is installed.
3. Run source("solution.R") or Rscript solution.R.
4. Outputs are written into results/.

Input CSV files are preserved in data/. ITI is added in memory to all three datasets.
All tuning and feature selection use training data only. The test set is used only
for final evaluation. Positive outcome: Charged Off. Logistic threshold: 0.5.

Q6 logistic optimisation: backward term-wise likelihood-ratio p-value elimination,
alpha 0.05. Parameter counts include the intercept.
Q6 CART: cp=0 maximal tree under minsplit=20, minbucket=7, maxdepth=30;
optimisation uses the 1SE rule on 10-fold cross-validation error.
Q7: recursive removal by CART variable importance, with feature count selected
by stratified 10-fold validation; each fold learns its own elimination path and
uses inner 10-fold CV/1SE pruning. The final model is refitted on trainset.bal.csv.
CART complexity is its number of splits. FPR and FNR are proportions (0 to 1).

Written explanations and the Q8 advisory are deferred as requested.
No statistical significance claim is made from accuracy changes alone.
