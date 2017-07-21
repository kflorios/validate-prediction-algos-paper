
root <- getwd()

source("functions_Monte_MX_tune.R")

Scenario <- 1

R=200

res <- vector("list", R)

brierValues <- vector("list",R)
AUCvalues <- vector("list",R)
brierSkillValues <- vector("list",R)

#fname <- paste("dataset_for_wp3_v6_merged",".txt",sep="")
fnameX <- paste("Xall_MX",".txt",sep="")
fnameY <- paste("Yall_MX",".txt",sep="")

ptm <- proc.time() # Start the clock!

#for (m in 1:10) {
#for (m in 1:200) {  
 for (m in 200:200) {  
  
  modelFit <- try({
    # Calculate Results
    evaluateModels(fnameX,fnameY,m)                 
  }, TRUE)
  
  
  res[[m]] <- if (!inherits(modelFit$mat, "try-error")) modelFit$mat else NULL
  brierValues[[m]] <- if (!inherits(modelFit$brierScores, "try-error")) modelFit$brierScores else NULL
  AUCvalues[[m]] <- if (!inherits(modelFit$AUCvalues, "try-error")) modelFit$AUCvalues else NULL
  brierSkillValues[[m]] <- if (!inherits(modelFit$brierSkillScores, "try-error")) modelFit$brierSkillScores else NULL
  
  cat("Data-set:", m, "finished.\n")
  
  save.image(file=paste(m,".RData"))
  
}

elapsed_time <- proc.time() - ptm  # Stop the clock


resN <- res[!sapply(res, is.null)]
brierValuesN <- brierValues[!sapply(brierValues, is.null)]
AUCvaluesN <- AUCvalues[!sapply(AUCvalues, is.null)]
brierSkillValuesN <- brierSkillValues[!sapply(brierSkillValues, is.null)]

out <- Reduce("+", resN) / length(resN)
outBrier <- Reduce("+", brierValuesN) / length(brierValuesN)
outAUC <- Reduce("+", AUCvaluesN) / length(AUCvaluesN)
outBrierSkill <- Reduce("+", brierSkillValuesN) / length(brierSkillValuesN)  

outD <- as.data.frame(cbind("thres" = out[,2],
                            "0ACC" = out[,3], 
                            "0TSS" = out[,4], 
                            "0HSS" = out[,5], 
                            "1ACC" = out[,8], 
                            "1TSS" = out[,9], 
                            "1HSS" = out[,10],                             
                            "2ACC" = out[,13], 
                            "2TSS" = out[,14], 
                            "2HSS" = out[,15],                             
                            "3ACC" = out[,18], 
                            "3TSS" = out[,19], 
                            "3HSS" = out[,20],                             
                            "4ACC" = out[,23], # random Forest
                            "4TSS" = out[,24], 
                            "4HSS" = out[,25],
                            "5ACC" = out[,28], # SVM
                            "5TSS" = out[,29], 
                            "5HSS" = out[,30]))

outBrierD <- as.data.frame(cbind("0Brier" = outBrier[1],
                                 "1Brier" = outBrier[2],
                                 "2Brier" = outBrier[3],
                                 "3Brier" = outBrier[4],
                                 "4Brier" = outBrier[5],
                                 "5Brier" = outBrier[6]))

outAUCD <- as.data.frame(cbind("0AUCvalue" = outAUC[1],
                               "1AUCvalue" = outAUC[2],
                               "2AUCvalue" = outAUC[3],
                               "3AUCvalue" = outAUC[4],
                               "4AUCvalue" = outAUC[5],
                               "5AUCvalue" = outAUC[6]))

outBrierSkillD <- as.data.frame(cbind("0BrierSkill" = outBrierSkill[1],
                                      "1BrierSkill" = outBrierSkill[2],
                                      "2BrierSkill" = outBrierSkill[3],
                                      "3BrierSkill" = outBrierSkill[4],
                                      "4BrierSkill" = outBrierSkill[5],
                                      "5BrierSkill" = outBrierSkill[6]))


#write.table(outD, file = paste(root, "/Results/coefTable_Scnr", Scenario, ".txt", sep = ""), row.names=F)                            
write.table(outD, file = paste(root, "/Results/coefTable_Scnr", Scenario, ".txt", sep = ""), row.names=FALSE)                            

write.table(outBrierD, file = paste(root, "/Results/BrierTable_Scnr", Scenario, ".txt", sep = ""), row.names=FALSE)

write.table(outAUCD, file = paste(root, "/Results/AUCTable_Scnr", Scenario, ".txt", sep = ""), row.names=FALSE)                            

write.table(outBrierSkillD, file = paste(root, "/Results/BrierSkillTable_Scnr", Scenario, ".txt", sep = ""), row.names=FALSE)


# LaTeX output
#outD <- cbind("Param" = rep(paste("$\\beta_", 1:4, "$", sep = ""), len = nrow(outD)), outD)
outD <- cbind("Par" = rep(paste("$val_{", 0:100, "}$", sep = ""), len = nrow(outD)), outD)
###outD <- cbind(" " = c(sapply(1:Q, function (i) c(paste("Outcome", i), 
###                                                 rep("", length(betas) - 1)))), outD)
cap <- paste("Scenario ", Scenario, ", based on ", R, " datasets: ", 
             "Using SHYKJG1 dataset: ",
             "Method 0 is Neural Network. ",
             "Method 1 is Linear Regression. ",
             "Method 2 is Probit Regression. ",
             "Method 3 is Logit Regression. ", 
             "Method 4 is Random Forest Regression. ",
             "Method 5 is Support Vector Regresion.", sep = "")
print(xtable(outD, caption = cap,digits=c(2)), math.style.negative = TRUE, include.rownames = FALSE, 
      sanitize.text.function = function(x) x)


# LaTeX output Brier
#outD <- cbind("Param" = rep(paste("$\\beta_", 1:4, "$", sep = ""), len = nrow(outD)), outD)
outBrierD <- cbind("Par" = rep(paste("$method_{", 0:5, "}$", sep = ""), len = nrow(outBrierD)), outBrierD)
###outD <- cbind(" " = c(sapply(1:Q, function (i) c(paste("Outcome", i), 
###                                                 rep("", length(betas) - 1)))), outD)
cap <- paste("Scenario ", Scenario, ", based on ", R, " datasets: ", 
             "Using SHYKJG1 dataset: ",
             "Method 0 is Neural Network. ",
             "Method 1 is Linear Regression. ",
             "Method 2 is Probit Regression. ",
             "Method 3 is Logit Regression. ", 
             "Method 4 is Random Forest Regression. ",
             "Method 5 is Support Vector Regresion.", sep = "")
print(xtable(outBrierD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
      sanitize.text.function = function(x) x)


# LaTeX output AUC
#outD <- cbind("Param" = rep(paste("$\\beta_", 1:4, "$", sep = ""), len = nrow(outD)), outD)
outAUCD <- cbind("Par" = rep(paste("$method_{", 0:5, "}$", sep = ""), len = nrow(outAUCD)), outAUCD)
###outD <- cbind(" " = c(sapply(1:Q, function (i) c(paste("Outcome", i), 
###                                                 rep("", length(betas) - 1)))), outD)
cap <- paste("Scenario ", Scenario, ", based on ", R, " datasets: ", 
             "Using SHYKJG1 dataset: ",
             "Method 0 is Neural Network. ",
             "Method 1 is Linear Regression. ",
             "Method 2 is Probit Regression. ",
             "Method 3 is Logit Regression. ", 
             "Method 4 is Random Forest Regression. ",
             "Method 5 is Support Vector Regresion.", sep = "")
print(xtable(outAUCD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
      sanitize.text.function = function(x) x)


# LaTeX output Brier SKILL
#outD <- cbind("Param" = rep(paste("$\\beta_", 1:4, "$", sep = ""), len = nrow(outD)), outD)
outBrierSkillD <- cbind("Par" = rep(paste("$method_{", 0:5, "}$", sep = ""), len = nrow(outBrierSkillD)), outBrierSkillD)
###outD <- cbind(" " = c(sapply(1:Q, function (i) c(paste("Outcome", i), 
###                                                 rep("", length(betas) - 1)))), outD)
cap <- paste("Scenario ", Scenario, ", based on ", R, " datasets: ", 
             "Using SHYKJG1 dataset: ",
             "Method 0 is Neural Network. ",
             "Method 1 is Linear Regression. ",
             "Method 2 is Probit Regression. ",
             "Method 3 is Logit Regression. ", 
             "Method 4 is Random Forest Regression. ",
             "Method 5 is Support Vector Regresion.", sep = "")
print(xtable(outBrierSkillD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
      sanitize.text.function = function(x) x)




save.image(file="Monte_SHYKJG1_5methods_R200_tuneSVM_MX_Blos_and_Br.RData")


gc()
