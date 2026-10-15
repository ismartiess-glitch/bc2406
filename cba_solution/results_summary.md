# R results — Questions 1–7

Positive = Charged Off. FPR, FNR and accuracy are proportions.

Q3: Term was modelled as a categorical variable (36 or 60 months).

Q6: Balanced training set: 3,733 Fully Paid and 3,733 Charged Off.


## Q1: Grade

| Level | Fully_Paid | Charged_Off | Charged_Off_Percent |
| --- | --- | --- | --- |
| A | 6355 | 383 | 5.6841793 |
| B | 7009 | 933 | 11.747671 |
| C | 4471 | 897 | 16.710134 |
| D | 2749 | 756 | 21.569187 |
| E | 1332 | 482 | 26.571114 |
| F | 453 | 218 | 32.488823 |
| G | 133 | 64 | 32.48731 |

## Q1: Sub-grade

| Level | Fully_Paid | Charged_Off | Charged_Off_Percent |
| --- | --- | --- | --- |
| A1 | 705 | 15 | 2.0833333 |
| A2 | 946 | 49 | 4.9246231 |
| A3 | 1162 | 67 | 5.4515867 |
| A4 | 1829 | 118 | 6.0606061 |
| A5 | 1713 | 134 | 7.2550081 |
| B1 | 1116 | 105 | 8.5995086 |
| B2 | 1206 | 152 | 11.192931 |
| B3 | 1704 | 228 | 11.801242 |
| B4 | 1458 | 214 | 12.799043 |
| B5 | 1525 | 234 | 13.303013 |
| C1 | 1187 | 215 | 15.335235 |
| C2 | 1104 | 218 | 16.490166 |
| C3 | 852 | 181 | 17.521781 |
| C4 | 663 | 137 | 17.125 |
| C5 | 665 | 146 | 18.002466 |
| D1 | 516 | 109 | 17.44 |
| D2 | 726 | 182 | 20.044053 |
| D3 | 597 | 190 | 24.142313 |
| D4 | 503 | 135 | 21.159875 |
| D5 | 407 | 140 | 25.59415 |
| E1 | 356 | 138 | 27.935223 |
| E2 | 316 | 104 | 24.761905 |
| E3 | 258 | 78 | 23.214286 |
| E4 | 208 | 88 | 29.72973 |
| E5 | 194 | 74 | 27.61194 |
| F1 | 152 | 63 | 29.302326 |
| F2 | 101 | 44 | 30.344828 |
| F3 | 94 | 40 | 29.850746 |
| F4 | 63 | 35 | 35.714286 |
| F5 | 43 | 36 | 45.56962 |
| G1 | 43 | 18 | 29.508197 |
| G2 | 34 | 17 | 33.333333 |
| G3 | 17 | 15 | 46.875 |
| G4 | 22 | 7 | 24.137931 |
| G5 | 17 | 7 | 29.166667 |

## Q2: Outcome

| Level | Fully_Paid | Charged_Off | Charged_Off_Percent |
| --- | --- | --- | --- |
| Charged Off | 0 | 3733 | 100 |
| Fully Paid | 22502 | 0 | 0 |

## Q2: Term

| Level | Fully_Paid | Charged_Off | Charged_Off_Percent |
| --- | --- | --- | --- |
| 36 | 17712 | 2099 | 10.595124 |
| 60 | 4790 | 1634 | 25.435866 |

## Q2: Home ownership

| Level | Fully_Paid | Charged_Off | Charged_Off_Percent |
| --- | --- | --- | --- |
| MORTGAGE | 10014 | 1568 | 13.538249 |
| OTHER | 57 | 16 | 21.917808 |
| OWN | 1638 | 282 | 14.6875 |
| RENT | 10793 | 1867 | 14.747235 |

## Q2: Purpose

| Level | Fully_Paid | Charged_Off | Charged_Off_Percent |
| --- | --- | --- | --- |
| car | 877 | 109 | 11.054767 |
| credit card | 3067 | 354 | 10.347852 |
| Debt consolidation | 10524 | 1836 | 14.854369 |
| educational | 180 | 32 | 15.09434 |
| home improvement | 1717 | 239 | 12.218814 |
| house | 199 | 40 | 16.736402 |
| major purchase | 1289 | 144 | 10.048849 |
| medical | 378 | 72 | 16 |
| moving | 350 | 53 | 13.151365 |
| other | 2196 | 426 | 16.24714 |
| renewable_energy | 51 | 14 | 21.538462 |
| small business | 879 | 323 | 26.87188 |
| vacation | 206 | 33 | 13.807531 |
| wedding | 589 | 58 | 8.9644513 |

## Q2: Numeric summaries

| Outcome | Variable | N | Min | Median | Mean | Max |
| --- | --- | --- | --- | --- | --- | --- |
| Charged Off | annual_income | 3733 | 4080 | 54000 | 63530.674 | 1250000 |
| Charged Off | dti | 3733 | 0 | 0.1433 | 0.14004806 | 0.2985 |
| Charged Off | installment | 3733 | 22.79 | 297.7 | 341.07427 | 1305.19 |
| Charged Off | int_rate | 3733 | 0.0542 | 0.1372 | 0.13879537 | 0.244 |
| Charged Off | loan_amount | 3733 | 1000 | 10000 | 12321.785 | 35000 |
| Charged Off | total_acc | 3733 | 2 | 20 | 21.682561 | 70 |
| Charged Off | ITI | 3733 | 0.0016637778 | 0.0663 | 0.073746693 | 0.29755846 |
| Fully Paid | annual_income | 22502 | 4000 | 60000 | 70351.922 | 6000000 |
| Fully Paid | dti | 22502 | 0 | 0.1322 | 0.13159049 | 0.2993 |
| Fully Paid | installment | 22502 | 15.69 | 278.48 | 323.0928 | 1283.5 |
| Fully Paid | int_rate | 22502 | 0.0542 | 0.1149 | 0.11644586 | 0.2411 |
| Fully Paid | loan_amount | 22502 | 500 | 9600 | 10949.853 | 35000 |
| Fully Paid | total_acc | 22502 | 2 | 21 | 22.184028 | 90 |
| Fully Paid | ITI | 22502 | 0.00033568 | 0.056125467 | 0.064124135 | 0.320262 |

## Q4: ITI

| Min | Median | Mean | Max |
| --- | --- | --- | --- |
| 0.00033568 | 0.057499091 | 0.065493336 | 0.320262 |

## Q5: Confusion matrix

|  | Fully Paid | Charged Off |
| --- | --- | --- |
| Fully Paid | 9601 | 42 |
| Charged Off | 1563 | 37 |

## Q5: Metrics

| TN | FP | FN | TP | FPR | FNR | Accuracy |
| --- | --- | --- | --- | --- | --- | --- |
| 9601 | 42 | 1563 | 37 | 0.004355491 | 0.976875 | 0.85724451 |

## Q6: Model comparison

| Model | Complexity | TN | FP | FN | TP | FPR | FNR | Accuracy |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Logistic (all predictors) | 114 | 6207 | 3436 | 590 | 1010 | 0.35632065 | 0.36875 | 0.64191052 |
| Logistic (backward p-value) | 97 | 6225 | 3418 | 579 | 1021 | 0.35445401 | 0.361875 | 0.6444899 |
| Maximal CART | 392 | 5318 | 4325 | 659 | 941 | 0.44851187 | 0.411875 | 0.55670195 |
| CART (10-fold CV, 1SE) | 5 | 5159 | 4484 | 472 | 1128 | 0.46500052 | 0.295 | 0.55919239 |

## Q6: Backward elimination

| Step | Removed | P_value |
| --- | --- | --- |
| 1 | emp_length | 0.44541844 |
| 2 | annual_income | 0.31631707 |
| 3 | total_acc | 0.21867022 |
| 4 | loan_amount | 0.18635975 |
| 5 | home_ownership | 0.079433494 |
| 6 | dti | 0.076740381 |

## Q7: RFE comparison

| Model | Complexity | TN | FP | FN | TP | FPR | FNR | Accuracy |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| CART before RFE (1SE) | 5 | 5159 | 4484 | 472 | 1128 | 0.46500052 | 0.295 | 0.55919239 |
| CART after RFE (1SE) | 5 | 5159 | 4484 | 472 | 1128 | 0.46500052 | 0.295 | 0.55919239 |

## Q7: Selected variables

| Variable |
| --- |
| state |
| emp_length |
| purpose |
| sub_grade |
| term |
| annual_income |
| dti |
| installment |
| int_rate |
| ITI |

Q7: Test accuracy change = 0 percentage points.

Written explanations and Q8 advisory deferred.