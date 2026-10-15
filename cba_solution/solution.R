# Run from the folder containing this file: Rscript solution.R
# Required package: rpart (included in standard R recommended packages).
# Models are selected using training data only. Positive = Charged Off.
options(stringsAsFactors = FALSE, width = 120)
library(rpart)
set.seed(2406)
dir.create('results', showWarnings = FALSE)
sink('results/R_output.txt', split = TRUE)
write_result <- function(x, name) {
  print(x)
  write.csv(x, file.path('results', paste0(name, '.csv')), row.names = FALSE)
}
train <- read.csv('data/trainset.csv')
bal <- read.csv('data/trainset.bal.csv')
test <- read.csv('data/testset.csv')
cat('\nDATA CHECKS\n')
for (nm in c('train','bal','test')) {
  d <- get(nm)
  cat(nm, ':', nrow(d), 'rows;', sum(is.na(d)), 'missing cells;',
      sum(duplicated(d$id)), 'duplicate IDs\n')
  stopifnot(!anyNA(d), !anyDuplicated(d$id), all(d$annual_income > 0))
}
stopifnot(!length(intersect(train$id, test$id)), all(bal$id %in% train$id))
# No imputation, winsorisation or row deletion is necessary for these files.
# Add ITI to ALL THREE data frames.
train$ITI <- 12 * train$installment / train$annual_income
bal$ITI <- 12 * bal$installment / bal$annual_income
test$ITI <- 12 * test$installment / test$annual_income

cat('\nQ1: grade / sub_grade and loan_status (original training set)\n')
rate_table <- function(d, variable) {
  t <- table(d[[variable]], factor(d$loan_status, levels=c('Fully Paid','Charged Off')))
  data.frame(Level=rownames(t), Fully_Paid=t[,1], Charged_Off=t[,2],
             Charged_Off_Percent=100*t[,2]/rowSums(t), row.names=NULL)
}
grade_tab <- rate_table(train, 'grade')
subgrade_tab <- rate_table(train, 'sub_grade')
write_result(grade_tab, 'Q1_grade')
write_result(subgrade_tab, 'Q1_sub_grade')
pdf('results/Q1_stacked_bars.pdf', width=12, height=7)
for (v in c('grade','sub_grade')) {
  tab <- table(train$loan_status, train[[v]])
  barplot(prop.table(tab, 2), col=c('tomato','steelblue'), las=2,
          main=paste('Loan status by', v), ylab='Proportion', xlab=v,
          legend.text=rownames(tab), args.legend=list(x='topleft', bty='n'))
}
dev.off()

cat('\nQ2: concise exploration using original training data\n')
write_result(rate_table(train, 'loan_status'), 'Q2_outcome')
for (v in c('term','home_ownership','purpose'))
  write_result(rate_table(train,v), paste0('Q2_',v))
num_vars <- c('annual_income','dti','installment','int_rate','loan_amount','total_acc','ITI')
num_summary <- do.call(rbind, lapply(split(train, train$loan_status), function(d)
  do.call(rbind, lapply(num_vars, function(v) data.frame(
    Outcome=d$loan_status[1], Variable=v, N=nrow(d), Min=min(d[[v]]),
    Median=median(d[[v]]), Mean=mean(d[[v]]), Max=max(d[[v]]))))))
write_result(num_summary, 'Q2_numeric_summary')
pdf('results/Q2_boxplots.pdf', width=10, height=8)
par(mfrow=c(2,2))
for (v in c('int_rate','dti','annual_income','ITI'))
  boxplot(train[[v]] ~ train$loan_status, main=v, xlab='Loan status', ylab=v,
          col=c('tomato','steelblue'))
dev.off()

cat('\nQ3: observed term values\n')
print(table(train$term))
# Use an unordered factor: the observed loan products are 36 and 60 months.
cat('\nQ4: ITI = 12 * installment / annual_income\n')
write_result(data.frame(Min=min(train$ITI), Median=median(train$ITI),
                       Mean=mean(train$ITI), Max=max(train$ITI)), 'Q4_ITI_summary')

# Set response reference so binomial glm estimates P(Charged Off).
classes <- c('Fully Paid','Charged Off')
cat_vars <- c('state','emp_length','home_ownership','purpose','sub_grade',
              'term','verification_status')
for (nm in c('train','bal','test')) {
  d <- get(nm)
  d$loan_status <- factor(d$loan_status, levels=classes)
  for (v in cat_vars) {
    # Learn levels from original training data, never from test outcomes.
    lv <- sort(unique(train[[v]]))
    d[[v]] <- factor(d[[v]], levels=lv)
  }
  stopifnot(!anyNA(d))
  assign(nm, d)
}
xvars <- setdiff(names(train), c('id','grade','loan_status'))
form <- reformulate(xvars, response='loan_status')
metrics <- function(actual, predicted) {
  cm <- table(Actual=factor(actual,levels=classes), Predicted=factor(predicted,levels=classes))
  tn<-cm[1,1]; fp<-cm[1,2]; fn<-cm[2,1]; tp<-cm[2,2]
  data.frame(TN=unname(tn), FP=unname(fp), FN=unname(fn), TP=unname(tp),
    FPR=unname(fp/(fp+tn)), FNR=unname(fn/(fn+tp)),
    Accuracy=unname((tn+tp)/sum(cm)))
}
glm_class <- function(m,d) factor(ifelse(predict(m,d,type='response')>=0.5,
                                         'Charged Off','Fully Paid'), levels=classes)
complexity_glm <- function(m) m$rank # estimable coefficients, including intercept
complexity_cart <- function(m) sum(m$frame$var != '<leaf>')

cat('\nQ5: logistic regression on original training data\n')
lr_original <- glm(form, data=train, family=binomial())
stopifnot(lr_original$converged)
pred_original <- glm_class(lr_original, test)
cm_original <- table(Actual=test$loan_status, Predicted=pred_original)
print(cm_original)
write.csv(as.data.frame.matrix(cm_original), 'results/Q5_confusion_matrix.csv')
write_result(metrics(test$loan_status,pred_original), 'Q5_metrics')

cat('\nQ6: balance, full logistic regression, backward elimination, maximal and pruned CART\n')
print(table(bal$loan_status)); print(prop.table(table(bal$loan_status)))
lr_full <- glm(form, data=bal, family=binomial())
stopifnot(lr_full$converged)
# Backward elimination by term-wise likelihood-ratio p-values (alpha = 0.05).
# Multi-level factors are removed as whole variables, not individual levels.
lr_opt <- lr_full
elimination <- data.frame(Step=integer(), Removed=character(), P_value=numeric())
repeat {
  dt <- drop1(lr_opt, test='Chisq')
  pv <- dt[-1, 'Pr(>Chi)']; names(pv) <- rownames(dt)[-1]
  if (!length(pv) || all(is.na(pv)) || max(pv,na.rm=TRUE)<=0.05) break
  worst <- names(which.max(pv))
  elimination <- rbind(elimination, data.frame(Step=nrow(elimination)+1,
                         Removed=worst, P_value=unname(pv[worst])))
  lr_opt <- update(lr_opt, paste('. ~ . -', worst))
  stopifnot(lr_opt$converged)
}
write_result(elimination, 'Q6_backward_elimination')
cat('Optimised logistic formula:\n'); print(formula(lr_opt))
write.csv(coef(summary(lr_opt)), 'results/Q6_logistic_coefficients.csv')
# Maximal = no cost-complexity pre-pruning; standard rpart size/depth limits retained.
# minsplit=20, minbucket=7, maxdepth=30; not a guaranteed fully pure tree.
fit_cart <- function(d, vars, seed) {
  set.seed(seed)
  rpart(reformulate(vars,'loan_status'), data=d, method='class',
        parms=list(split='gini'),
        control=rpart.control(cp=0, minsplit=20, minbucket=7, maxdepth=30,
                              xval=10, maxsurrogate=0))
}
prune_1se <- function(m) {
  ct <- m$cptable
  i <- which.min(ct[,'xerror'])
  cutoff <- ct[i,'xerror'] + ct[i,'xstd']
  eligible <- which(ct[,'xerror']<=cutoff)
  j <- eligible[which.min(ct[eligible,'nsplit'])]
  prune(m, cp=ct[j,'CP'])
}
cart_max <- fit_cart(bal, xvars, 2406)
cart_opt <- prune_1se(cart_max)
write_result(as.data.frame(cart_max$cptable), 'Q6_CART_cv')
models <- list('Logistic (all predictors)'=lr_full, 'Logistic (backward p-value)'=lr_opt,
               'Maximal CART'=cart_max, 'CART (10-fold CV, 1SE)'=cart_opt)
preds <- lapply(models, function(m) if (inherits(m,'glm')) glm_class(m,test)
                else predict(m,test,type='class'))
q6 <- do.call(rbind,lapply(seq_along(models),function(i) {
  m <- models[[i]]
  data.frame(Model=names(models)[i], Complexity=if(inherits(m,'glm'))
    complexity_glm(m) else complexity_cart(m), metrics(test$loan_status,preds[[i]]))
}))
rownames(q6)<-NULL
write_result(q6, 'Q6_model_comparison')
pdf('results/Q6_pruned_CART.pdf', width=14,height=9)
if (complexity_cart(cart_opt)>0) {plot(cart_opt,uniform=TRUE,margin=0.15); text(cart_opt,use.n=TRUE,cex=0.7)} else {plot.new();text(0.5,0.5,'1SE selected an intercept-only tree')}
dev.off()

cat('\nQ7: CART recursive feature elimination\n')
# Recursive elimination ranks original variables using CART importance.
# Each OUTER training fold builds its own elimination path; its validation fold
# never influences rankings or inner 10-fold pruning. Test set is used only at end.
# At each size, prune by the SAME 1SE rule as Q6 for a like-for-like comparison.
rfe_path <- function(d, seed, validation=NULL) {
  remaining <- xvars; out <- list()
  while(length(remaining)) {
    maximal <- fit_cart(d, remaining, seed)
    fitted <- prune_1se(maximal)
    imp <- setNames(rep(0,length(remaining)), remaining)
    available <- intersect(names(maximal$variable.importance),remaining)
    imp[available] <- maximal$variable.importance[available]
    entry <- list(vars=remaining, model=fitted)
    if(!is.null(validation)) {
      p <- predict(fitted,validation,type='class')
      entry$correct <- sum(p==validation$loan_status)
      entry$n <- nrow(validation)
    }
    out[[as.character(length(remaining))]] <- entry
    # Deterministic alphabetical tie-break when importances tie.
    remove <- names(sort(imp))[1]
    remaining <- setdiff(remaining,remove)
  }
  out
}
set.seed(2406)
fold <- integer(nrow(bal))
for (cl in classes) {
  idx <- which(bal$loan_status==cl)
  fold[idx] <- sample(rep(1:10,length.out=length(idx)))
}
cv_scores <- matrix(NA_real_,10,length(xvars))
for (k in 1:10) {
  cat('RFE validation fold',k,'of 10\n'); flush.console()
  path <- rfe_path(bal[fold!=k,], seed=2406+100*k, validation=bal[fold==k,])
  for(s in seq_along(xvars)) cv_scores[k,s] <- path[[as.character(s)]]$correct/path[[as.character(s)]]$n
}
rfe_cv <- data.frame(Variables=seq_along(xvars), CV_Accuracy=colMeans(cv_scores),
                     SE=apply(cv_scores,2,sd)/sqrt(10))
write_result(rfe_cv,'Q7_RFE_CV')
# Maximise CV accuracy; select fewer variables if tied.
best_size <- which.max(rfe_cv$CV_Accuracy)
full_path <- rfe_path(bal,seed=2406)
cart_rfe <- full_path[[as.character(best_size)]]$model
selected <- full_path[[as.character(best_size)]]$vars
cat('RFE selected',best_size,'variables:\n'); print(selected)
write_result(data.frame(Variable=selected), 'Q7_RFE_selected_variables')
pred_rfe <- predict(cart_rfe,test,type='class')
q7 <- rbind(data.frame(Model='CART before RFE (1SE)', Complexity=complexity_cart(cart_opt),
                        metrics(test$loan_status,preds[[4]])),
            data.frame(Model='CART after RFE (1SE)', Complexity=complexity_cart(cart_rfe),
                        metrics(test$loan_status,pred_rfe)))
write_result(q7,'Q7_RFE_comparison')
cm_rfe <- table(Actual=test$loan_status,Predicted=pred_rfe)
print(cm_rfe)
write.csv(as.data.frame.matrix(cm_rfe),'results/Q7_RFE_confusion_matrix.csv')
cat('Accuracy change (percentage points):',100*diff(q7$Accuracy),'\n')
# No statistical significance claim is inferred from a point-accuracy change.
# Interpret the observed change in the requested written follow-up.
saveRDS(list(original_logistic=lr_original, balanced_models=models, RFE_CART=cart_rfe,
             selected_variables=selected, fold=fold), 'results/models.rds')
writeLines(capture.output(sessionInfo()),'results/sessionInfo.txt')
cat('\nQ8 and written explanations deferred as requested.\n')
sink()
