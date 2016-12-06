# validate-prediction-algos-paper
Validate Prediction Algorithms for Solar Flares Occurrence with respresentative sample

To run the R code you will need:

1. A working installation of >R3.0
2. packages 'tseries','nnet','xtable','pracma','matrixStats','e1071','ROCR','verification','randomForest' in R installed


Run has been checked in a windows 64 machine.


MX CLASS FLARES case

Put

a) Script_Monte_MX_tune.R  (R main)
b) functions_Monte_MX_tune.R (R functions)
c) dataset_for_wp3_v6_merged.txt (input file)

in same directory 

Run Script_Monte_MX_tune.R in R or RStudio
The script takes a few minutes to run.


In the current directory many files are produced.

The final result is the skill score statistics ACC, TSS, HSS for all methods which is also produced as a latex file,
and displayed at the console tab window of R or RStudio with latex code.

Also, figures are produced for all five methods validated.

The methods are
0: Neural network
1: Linear regression
2: Probit regression
3: Logit regression
4: Random forest
5: Support vector Machine


C CLASS FLARES case

Put

a) Script_Monte_Cclass_tune.R  (R main)
b) functions_Monte_Cclass_tune.R (R functions)
c) dataset_for_wp3_v6_merged.txt (input file)

in same directory 

Run Script_Monte_Cclass_tune.R in R or RStudio
The script takes a few minutes to run.


In the current directory many files are produced.

The final result is the skill score statistics ACC, TSS, HSS for all methods which is also produced as a latex file,
and displayed at the console tab window of R or RStudio with latex code.

Also, figures are produced for all five methods validated.

The methods are
0: Neural network
1: Linear regression
2: Probit regression
3: Logit regression
4: Random forest
5: Support vector Machine
