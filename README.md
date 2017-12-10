# validate-prediction-algos-paper
Validate Prediction Algorithms for Solar Flares Occurrence with respresentative sample

To run the R code you will need:

1. A working installation of >R3.0
2. packages 'tseries','nnet','xtable','pracma','matrixStats','e1071','ROCR','verification','randomForest' in R installed


The runs have been checked in a windows 64 and a linux 64 machine.


## MX (or >M1) CLASS FLARES case

Put

```
a) Script_Monte_MX_tune.R  (R main) 
b) functions_Monte_MX_tune.R (R functions) 
c) Xall_MX.txt (input file) 
d) Yall_MX.txt (input file) 
```
in same directory 

Run Script_Monte_MX_tune.R in R or RStudio
The script takes a few minutes to run.


In the current directory many files are produced.

The final result is the skill score statistics vector: Accuracy (ACC), True Skill Statistic (TSS) and Heidke Skill Score (HSS), for all methods which is also produced as a latex file, and displayed at the console tab window of R or RStudio with latex code.

Also, figures are produced for all five methods validated.

The methods are
```
0: Neural network
1: Linear regression
2: Probit regression
3: Logit regression
4: Random forest
5: Support vector Machine
```

## CMX (or >C1) CLASS FLARES case

Put

```
a) Script_Monte_Cclass_tune.R  (R main)
b) functions_Monte_Cclass_tune.R (R functions)
c) Xall.txt (input file)
d) Yall.txt (input file)
```
in same directory 

Run Script_Monte_Cclass_tune.R in R or RStudio
The script takes a few minutes to run.


In the current directory many files are produced.

The final result is the skill score statistics vector: Accuracy (ACC), True Skill Statistic (TSS) and Heidke Skill Score (HSS), for all methods which is also produced as a latex file, and displayed at the console tab window of R or RStudio with latex code.

Also, figures are produced for all five methods validated.

The methods are
```
0: Neural network
1: Linear regression
2: Probit regression
3: Logit regression
4: Random forest
5: Support vector Machine
```
