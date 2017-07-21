
evaluateModels <- function(fnameX,fnameY,m) {
  
  require(tseries)
  require(nnet)
  require(xtable)
  require(pracma)
  require(matrixStats)
  
  CurWd <- getwd()
  
  setwd(CurWd)
  
  
  # first create split in training and testing set, just once, for basic analysis
  ###set.seed(1000+1)
  
  
  
  #first pool data for all time points together
  dataMatALL <- matrix(NA,0,16)
  cp=0
  
  #fname <- paste("data_neural_C",".txt",sep="")
  ###data <- scan(file=fname,what=numeric())
  dataX <- scan(file=fnameX,what=numeric())
  dataMatX <- matrix(dataX,nrow=length(dataX)/15,ncol=15,byrow=T)
  dataY <- scan(file=fnameY,what=numeric())
  dataMatY <- matrix(dataY,nrow=length(dataY)/1,ncol=1,byrow=T)
  
  dataMat <- cbind(dataMatX,dataMatY)
  
  dataFram <- as.data.frame(dataMat[,1:16])
  
  dataFram <- dataFram[complete.cases(dataFram),]
  
  
  
  #names(dataFram) <- c("TUMagneticFlux","Hel1","Hel2","Hel3","Hel4","Hel5","Hel6",
  #                     "Hel7","Hel8","Hel9","Hel10","Hel11","Hel12",
  #                     "FreeEn1","FreeEn2","FreeEn3","FreeEn4","FreeEn5","FreeEn6",
  #                     "FreeEn7","FreeEn8","FreeEn9","FreeEn10","FreeEn11","FreeEn12",
  #                     "TotalNoMagneticNullPoints","MFluxNearMPolarityILines","yGTM1","yIN-C1-C9")
  #names(dataFram) <- c("BEff","IsingEn1","IsingEn2","ImbalanceRatio","GlobalNonNeutralityF","NetNonNeutralizedC",
  #                     "yGTM1")
  names(dataFram) <- c("alpha_exp_fft_blos","alpha_exp_fft_br","decay_index_blos",
                       "decay_index_br","ising_energy_blos","ising_energy_br",
                       "ising_energy_part_blos","ising_energy_part_br","mpil_blos",
                       "mpil_br","nn_currents","r_value_blos_logr",
                       "r_value_br_logr","wlsg_blos","wlsg_br",
                       "y_GT_M1")
  
  names(dataMat) <- c("alpha_exp_fft_blos","alpha_exp_fft_br","decay_index_blos",
                      "decay_index_br","ising_energy_blos","ising_energy_br",
                      "ising_energy_part_blos","ising_energy_part_br","mpil_blos",
                      "mpil_br","nn_currents","r_value_blos_logr",
                      "r_value_br_logr","wlsg_blos","wlsg_br",
                      "y_GT_M1")
  
  
  dataMatf <- as.matrix(dataFram)
  
  
  #dependent variables
  
  y_GT_M1_flare <- y_GT_C1_flare <- numeric(dim(dataMatf)[1])
  for (i in 1:dim(dataMatf)[1]) {
     y_GT_M1_flare[i]    <- as.numeric(dataMatf[i,16] )
    #y_GT_C1_flare[i] <- as.numeric(dataMatf[i,16] )
  }
  
  
  #now proceed to nnet
  
  #M class flare
   y<- y_GT_M1_flare
  #C class flare
  #y<- y_GT_C1_flare
  #Χ class flare
  ###y<- y_X1_flare
  
  
  te <- dataFram
  teRaw <- te
  
  
  #max elements of columns
  #colMaxs(as.matrix(dataFram))
  #[1]  5.4766035  5.4554725 -0.5723605 -0.4970109  7.4255200  5.4827600  1.0207100  4.0845900  5.9072200  1.0000000  1.0000000
  #min elements of columns
  #colMins(as.matrix(dataFram))
  #[1]  0.000000  0.000000 -3.479032 -2.521050  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000  0.000000
  
  #re-scaling
  #col. 1,  divide by 1
  #col. 2,  divide by 1
  #col. 3,  divide by 1
  #col. 4,  divide by 1
  #col. 5,  divide by 1
  #col. 6,  divide by 1
  #col. 7,  divide by 1
  #col. 8,  divide by 1
  #col. 9,  divide by 1
  
  te$alpha_exp_fft_blos <-     te$alpha_exp_fft_blos / 1
  te$alpha_exp_fft_br   <-     te$alpha_exp_fft_br / 1
  te$decay_index_blos   <-     te$decay_index_blos / 1
  te$decay_index_br     <-     te$decay_index_br / 1
  te$ising_energy_blos  <-     te$ising_energy_blos / 1
  te$ising_energy_br    <-     te$ising_energy_br / 1
  te$ising_energy_part_blos     <-     te$ising_energy_part_blos / 1
  te$ising_energy_part_br       <-     te$ising_energy_part_br / 1
  te$mpil_blos          <-     te$mpil_blos / 1
  te$mpil_br            <-     te$mpil_br / 1
  te$nn_currents        <-     te$nn_currents / 1
  te$r_value_blos_logr  <-     te$r_value_blos_logr / 1
  te$r_value_br_logr    <-     te$r_value_br_logr / 1
  te$wlsg_blos          <-     te$wlsg_blos / 1
  te$wlsg_br            <-     te$wlsg_br / 1
  
  #maxs <- colMaxs(as.matrix(dataFram))
  #mins <- colMins(as.matrix(dataFram))
  
  #for (j in 1:(dim(te)[2]-2)){
  #for (i in 1:dim(te)[1]) {
  #    te[i,j] <- -3+3*((te[i,j] - mins[j]) / (maxs[j] - mins[j]))
  #}
  #}
  
  #for (j in 1:(dim(te)[2]-2)){
  #for (i in 1:dim(te)[1]) {
  #    te[i,j] <- 5*((te[i,j]) / (maxs[j]))
  #}
  #}
  
  means <- colMeans(as.matrix(te))
  sds   <- colSds(as.matrix(te))
  
  for (j in 1:(dim(te)[2]-1)){
    #for (i in 1:dim(te)[1]) {
    te[,j] <- (te[,j] - means[j]) / sds[j]
    #}
  }
  
  
  n=dim(dataFram)[1]
  
  set.seed(1000+m)
  
  idx <- 1:n
  randpermVec <- randperm(idx)
  idx <- randpermVec
  
  N1 <- floor(n*0.5)
  N2 <- n -N1
  N  <- N1 + N2
  
  idx1 <- idx[1:N1]
  idx2 <- idx[(N1+1):(N)]
  
  
  dataMatf1 <- dataMatf[idx1,]
  te1       <- te[idx1,]
  
  dataMatf2 <- dataMatf[idx2,]
  te2       <- te[idx2,]
  
  #now on:
  te <- te1  # for training
  teTst <- te2
  
  #keep vector magnetogram predictors only
  #te <- te[,-c(1,3,5,7,9,11,13)]         #remove los information predictors
  #teTst <- teTst[,-c(1,3,5,7,9,11,13)]   #remove los information predictors
  #######################################################################################
  #te <- te[,-c(2,4,6,8,10,12,14)]         #remove vector information predictors
  #teTst <- teTst[,-c(2,4,6,8,10,12,14)]   #remove vector information predictors
  #######################################################################################
  #keep 3 predictors: DI,Beff,NNC
  #te <- te[,-c(1,2,3,6)]
  #teTst <- teTst[,-c(1,2,3,6)]
  
  #lm
  #m1 <- lm( y ~ sunspot_Area + dMW2 + dMW3+ dMW4+ dMW5+ dMW6+ dMW7,data=te)
  m1 <- lm( y_GT_M1 ~ alpha_exp_fft_blos+alpha_exp_fft_br+decay_index_blos+decay_index_br+
              ising_energy_blos+ising_energy_br+ising_energy_part_blos+ising_energy_part_br+
              mpil_blos+mpil_br+nn_currents+r_value_blos_logr+r_value_br_logr+wlsg_blos+wlsg_br,data=te)
  
  summary(m1)
  
  
  jpeg(paste("linear_model_combined_DATABASE_1st_trn_orig_split",".jpeg",sep=""))
  plot(te$y_GT_M1,xlab="observation",ylab="value of y",main=paste("lm model training original split",sep=""))
  lines(m1$fitted.values,col=2)
  dev.off()
  
  
  #xtable
  modelFit <- as.data.frame( round(coef(m1),6))
  modelFit <- as.data.frame( round(summary(m1)[4]$coefficients,6))
  outD <- modelFit
  outD <- xtable(modelFit)
  #outD <- cbind("Param" = rep(paste("$\\theta_", 1:55, "$", sep = ""), len = nrow(outD)), outD)
  mychar <- character(16)
  mychar[1]  <- "$\\INTCPT$"
  mychar[2]  <- "$\\alpha_exp_fft_blos$"
  mychar[3]  <- "$\\alpha_exp_fft_br$"  
  mychar[4]  <- "$\\decay_index_blos$"
  mychar[5]  <- "$\\decay_index_br$"
  mychar[6] <- "$\\ising_energy_blos$"
  mychar[7] <- "$\\ising_energy_br$"
  mychar[8] <- "$\\ising_energy_part_blos$"
  mychar[9] <- "$\\ising_energy_part_br$"
  mychar[10] <- "$\\mpil_blos$"
  mychar[11] <- "$\\mpil_br$"
  mychar[12] <- "$\\nn_currents$"
  mychar[13] <- "$\\r_value_blos_logr$"
  mychar[14] <- "$\\r_value_br_logr$"
  mychar[15] <- "$\\wlsg_blos$"
  mychar[16] <- "$\\wlsg_br$"
  
  outD <- cbind("Param" = rep(paste(mychar[1:16],sep = " "), len = nrow(outD)), outD)
  
  cap <- paste("Linear model original split", "training results", sep = " ")
  print(xtable(outD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
        sanitize.text.function = function(x) x)
  print(xtable(outD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
        sanitize.text.function = function(x) x, file=paste("linear_model_original_split","_train.tex",sep=""))
  #end xtable
  
  #glm
  #m2 <- glm( y ~ sunspot_Area + dMW2 + dMW3+ dMW4+ dMW5+ dMW6+ dMW7, data=te,family=binomial(probit))
  m2 <- glm( y_GT_M1 ~ alpha_exp_fft_blos+alpha_exp_fft_br+decay_index_blos+decay_index_br+
               ising_energy_blos+ising_energy_br+ising_energy_part_blos+ising_energy_part_br+
               mpil_blos+mpil_br+nn_currents+r_value_blos_logr+r_value_br_logr+wlsg_blos+wlsg_br,data=te,family=binomial(probit))
  
  summary(m2)
  
  jpeg(paste("probit_model_combined_DATABASE_1st_trn_orig_split",".jpeg",sep=""))
  plot(te$y_GT_M1,xlab="observation",ylab="value of y",main=paste("glm training with probit link original split ",sep=""))
  lines(m2$fitted.values,col=3)
  dev.off()
  
  
  #xtable
  #modelFit <- as.data.frame( round(coef(m1),6))
  modelFit <- as.data.frame( round(summary(m2)[12]$coefficients,6))
  outD <- modelFit
  outD <- xtable(modelFit)
  #outD <- cbind("Param" = rep(paste("$\\theta_", 1:55, "$", sep = ""), len = nrow(outD)), outD)
  mychar <- character(16)
  mychar[1]  <- "$\\INTCPT$"
  mychar[2]  <- "$\\alpha_exp_fft_blos$"
  mychar[3]  <- "$\\alpha_exp_fft_br$"  
  mychar[4]  <- "$\\decay_index_blos$"
  mychar[5]  <- "$\\decay_index_br$"
  mychar[6] <- "$\\ising_energy_blos$"
  mychar[7] <- "$\\ising_energy_br$"
  mychar[8] <- "$\\ising_energy_part_blos$"
  mychar[9] <- "$\\ising_energy_part_br$"
  mychar[10] <- "$\\mpil_blos$"
  mychar[11] <- "$\\mpil_br$"
  mychar[12] <- "$\\nn_currents$"
  mychar[13] <- "$\\r_value_blos_logr$"
  mychar[14] <- "$\\r_value_br_logr$"
  mychar[15] <- "$\\wlsg_blos$"
  mychar[16] <- "$\\wlsg_br$"
  
  
  outD <- cbind("Param" = rep(paste(mychar[1:16],sep = " "), len = nrow(outD)), outD)
  
  cap <- paste("Probit model original split", "training results", sep = " ")
  print(xtable(outD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
        sanitize.text.function = function(x) x)
  print(xtable(outD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
        sanitize.text.function = function(x) x, file=paste("probit_model_original_split","_train.tex",sep=""))
  #end xtable
  
  
  #glm
  #m3 <- glm( y ~ sunspot_Area + dMW2 + dMW3+ dMW4+ dMW5+ dMW6+ dMW7, data=te,family=binomial(logit))
  
  m3 <- glm( y_GT_M1 ~ alpha_exp_fft_blos+alpha_exp_fft_br+decay_index_blos+decay_index_br+
               ising_energy_blos+ising_energy_br+ising_energy_part_blos+ising_energy_part_br+
               mpil_blos+mpil_br+nn_currents+r_value_blos_logr+r_value_br_logr+wlsg_blos+wlsg_br,data=te,family=binomial(logit))
  
  summary(m3)
  
  
  jpeg(paste("logit_model_combined_DATABASE_1st_trn_orig_split",".jpeg",sep=""))
  plot(te$y_GT_M1,xlab="observation",ylab="value of y",main=paste("glm training with logit link original split ",sep=""))
  lines(m3$fitted.values,col=4)
  dev.off()
  
  
  #xtable
  #modelFit <- as.data.frame( round(coef(m1),6))
  modelFit <- as.data.frame( round(summary(m3)[12]$coefficients,6))
  outD <- modelFit
  outD <- xtable(modelFit)
  #outD <- cbind("Param" = rep(paste("$\\theta_", 1:55, "$", sep = ""), len = nrow(outD)), outD)
  mychar <- character(16)
  mychar[1]  <- "$\\INTCPT$"
  mychar[2]  <- "$\\alpha_exp_fft_blos$"
  mychar[3]  <- "$\\alpha_exp_fft_br$"  
  mychar[4]  <- "$\\decay_index_blos$"
  mychar[5]  <- "$\\decay_index_br$"
  mychar[6] <- "$\\ising_energy_blos$"
  mychar[7] <- "$\\ising_energy_br$"
  mychar[8] <- "$\\ising_energy_part_blos$"
  mychar[9] <- "$\\ising_energy_part_br$"
  mychar[10] <- "$\\mpil_blos$"
  mychar[11] <- "$\\mpil_br$"
  mychar[12] <- "$\\nn_currents$"
  mychar[13] <- "$\\r_value_blos_logr$"
  mychar[14] <- "$\\r_value_br_logr$"
  mychar[15] <- "$\\wlsg_blos$"
  mychar[16] <- "$\\wlsg_br$"
  
  
  outD <- cbind("Param" = rep(paste(mychar[1:16],sep = " "), len = nrow(outD)), outD)
  
  cap <- paste("Logit model original split", "training results", sep = " ")
  print(xtable(outD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
        sanitize.text.function = function(x) x)
  print(xtable(outD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
        sanitize.text.function = function(x) x, file=paste("logit_model_original_split","_train.tex",sep=""))
  #end xtable
  
  
  #bayesQR
  ###require(bayesQR)    
  #m4 <- bayesQR( y ~ sunspot_Area + dMW2 + dMW3+ dMW4+ dMW5+ dMW6+ dMW7, data=te, quantile=seq(.1,.9,.1), ndraw=4000)
  ##m4 <- bayesQR( yIN_C1_C9 ~ TUMagneticFlux + Hel1 + Hel2+ Hel3+ Hel4+ Hel5+ Hel6 +
  ##                 Hel7+Hel8+Hel9+Hel10+Hel11+Hel12 +
  ##                 FreeEn1+FreeEn2+FreeEn3+FreeEn4+FreeEn5+FreeEn6 +
  ##                 FreeEn7+FreeEn8+FreeEn9+FreeEn10+FreeEn11+FreeEn12 +              
  ##                 TotalNoMagneticNullPoints + MFluxNearMPolarityILines,data=te,quantile=seq(.1,.9,.1), ndraw=4000)
  #m4 <- bayesQR( yIN_C1_C9 ~ BEff+IsingEn1+IsingEn2+ImbalanceRatio+
  #                 GlobalNonNeutralityF+NetNonNeutralizedC,data=te,quantile=seq(.1,.7,0.6/8), ndraw=4000)
  ###m4 <- bayesQR( yIN_C1_C9 ~ BEff+IsingEn1+IsingEn2+ImbalanceRatio+
  ###                 GlobalNonNeutralityF+NetNonNeutralizedC,data=te,quantile=seq(.1,.9,0.1), ndraw=4000)
  
  
  ###summary(m4)[5]
  
  #regression
  #randomForest
  ###require(randomForest)
  ###teRF <- rbind(te,teTst)
  ###trainRF <- 1:dim(te)[1]
  ###p.randomForest <- randomForest(yIN_C1_C9 ~ logRBl+logRBr+FSPIBl+FSPIBr+TLMPILBl+TLMPILBr+
  ###                                             DIBl+DIBr+BeffBl+BeffBr+IsinEn1Bl+IsinEn1Br+IsinEn2Bl+IsinEn2Br+NNC, data=teRF, subset = trainRF,
  ###                                              importance=TRUE, na.action=na.omit)
  
  #classification
  require(randomForest)
  teRF <- rbind(te,teTst)
  trainRF <- 1:dim(te)[1]
  p.randomForest <- randomForest(as.factor(y_GT_M1) ~ alpha_exp_fft_blos+alpha_exp_fft_br+decay_index_blos+decay_index_br+
                                   ising_energy_blos+ising_energy_br+ising_energy_part_blos+ising_energy_part_br+
                                   mpil_blos+mpil_br+nn_currents+r_value_blos_logr+r_value_br_logr+wlsg_blos+wlsg_br, data=teRF, subset = trainRF,
                                 importance=TRUE, na.action=na.omit)
  
  
  require(matrixStats)
  
  
  #svm
  #require(e1071)    
  
  #p.svm <- svm(cbind(te$logRBl,te$FSPIBl,te$TLMPILBl,te$DIBl,te$BeffBl,te$IsinEn1Bl,te$IsinEn2Bl,te$NNC),
  #             te$yIN_C1_C9,probability=TRUE)
  
  #jpeg(paste("SVM_model_combined_SHYKJG_trn_orig_split",".jpeg",sep=""))
  #plot(te$yIN_C1_C9,xlab="observation",ylab="value of y",main=paste("SVM with probability option, combined regressors\n", "Training, Original Split",sep=" "))
  #lines(p.svm$fitted, col=6)
  #dev.off()
  
  
  #tune SVM
  require(e1071)    
  
  # t.svm <- tune.svm(cbind(te$alpha_exp_fft_blos,te$alpha_exp_fft_br,te$decay_index_blos,te$decay_index_br,
  #                         te$ising_energy_blos,te$ising_energy_br,te$ising_energy_part_blos,te$ising_energy_part_br,
  #                         te$mpil_blos,te$mpil_br,te$nn_currents,
  #                         te$r_value_blos_logr,te$r_value_br_logr,te$wlsg_blos,te$wlsg_br)[1:1500,],
  #                   as.factor(te$y_GT_C1)[1:1500],type="C-classification",gamma = 10^(-6:-1), cost = 10^(1:2),
  #                     tunecontrol = tune.control(nrepeat = 1), probability=TRUE)
  
  #p.svm <- t.svm$best.model
  
  #t.svm <- tune.svm(cbind(te$logRBl,te$FSPIBl,te$TLMPILBl,te$DIBl,te$BeffBl,te$IsinEn1Bl,te$IsinEn2Bl,te$NNC),
  #                  te$yIN_C1_C9,gamma = 0.125, cost = 1, probability=TRUE)
  
  
  #p.svm <- t.svm$best.model
  
  #p.svm <- svm(cbind(te$logRBl,te$FSPIBl,te$TLMPILBl,te$DIBl,te$BeffBl,te$IsinEn1Bl,te$IsinEn2Bl,te$NNC),
  #             te$yIN_C1_C9,gamma = 0.125, cost = 1, probability=TRUE)
  
  
  ###te$y_GT_C1 <- as.factor(te$y_GT_C1)
  #levels(te$y_GT_C1)
  #summary(te$y_GT_C1)
  #wts <- 10000 / table(te$y_GT_C1)
  #wts <- (100 / table(te$y_GT_C1)) * (table(te$y_GT_C1)/100)
  #wts
  
  p.svm <- svm(factor(y_GT_M1,levels=factor(c("0","1"))) ~ alpha_exp_fft_blos+alpha_exp_fft_br+decay_index_blos+decay_index_br+
                 ising_energy_blos+ising_energy_br+ising_energy_part_blos+ising_energy_part_br+
                 mpil_blos+mpil_br+nn_currents+r_value_blos_logr+r_value_br_logr+wlsg_blos+wlsg_br,data=te,
               #type="C-classification",gamma = 0.125, cost = 1,class.weights = wts,probability=TRUE)
               #type="C-classification",gamma = 0.01, cost = 100, probability=TRUE)
               type="C-classification",gamma = 0.1, cost = 10, probability=TRUE)
  
  pred.svm <- predict(p.svm,cbind(te$alpha_exp_fft_blos,te$alpha_exp_fft_br,te$decay_index_blos,te$decay_index_br,
                                  te$ising_energy_blos,te$ising_energy_br,te$ising_energy_part_blos,te$ising_energy_part_br,
                                  te$mpil_blos,te$mpil_br,te$nn_currents,
                                  te$r_value_blos_logr,te$r_value_br_logr,te$wlsg_blos,te$wlsg_br),
                      probability=TRUE)
  
  # if (as.factor(te$y_GT_C1)[1] == 1) {
  #   pred.svm.num <- attr(pred.svm,"prob")[,1]
  # } 
  # if (as.factor(te$y_GT_C1)[1] == 0) {
  #   pred.svm.num <- attr(pred.svm,"prob")[,2]
  # } 
  pred.svm.num <- attr(pred.svm,"prob")[,2]
  
  #pred.svm.num <- as.numeric(pred.svm)-1   ### class "0" is factor "1", class "1" is factor "2"
  #p.svm$fitted
  
  ###te$y_GT_C1 <- as.numeric(te$y_GT_C1) -1
  
  
  jpeg(paste("SVM_model_combined_DATABASE_trn_orig_split",".jpeg",sep=""))
  plot(as.numeric(te$y_GT_M1),xlab="observation",ylab="value of y",main=paste("SVM with probability option, combined regressors\n", "Training, Original Split",sep=" "))
  #lines(p.svm$fitted, col=6)
  lines(pred.svm.num, col=6)
  dev.off()
  
  
  #predict
  ##p.svm.predict <- predict(p.svm, newdata=teTst[,-c(28,29)])
  ##later ...
  
  require(nnet)
  
  #set.seed(1000+1)
  #set.seed(1000+2)
  #set.seed(1000+3)
  ###set.seed(1000+jj)
  ###set.seed(1000*cp+10*iNode+tries)    #suitable for parallel loops in jj, tries, iNode (segmentations
  
  #M class flare
  #y<- y_M1_flare
  
  
  #for(tries in 1:1) {
  
  #for (iNode in 21:55) {
  #for (iNode in 11:21) {
  #for (iNode in 11:55) {
  #for (iNode in 16:16) {
  
  #set.seed(1000*cp+10*iNode+tries)    #suitable for parallel loops in jj, tries, iNode (segmentations
  ###set.seed(10*iNode+tries)             #suitable for parallel loops in tries, iNode (segmentations         
  ###set.seed(1000*m+10*iNode+tries)             #suitable for parallel loops in tries, iNode (segmentations         
  
  ###cat("MCiter:... ",m,"tries:... ",tries," iNode:... ",iNode,"\n")
  cat("MCiter:... ",m,"\n") 
  
  
  #nnet
  
  ###p.nnet <- nnet(cbind(te$logRBl,te$FSPIBl,te$TLMPILBl,te$DIBl,te$BeffBl,te$IsinEn1Bl,te$IsinEn2Bl,te$NNC),
  ###               te$yIN_C1_C9,entropy=T,maxit=2000,MaxNWts=2000,size=iNode)  #binary outcome, used CML = entropy 
  #te$yIN_C1_C9,linout=TRUE, maxit=2000,MaxNWts=2000,size=iNode) # use linear outcome, to see what happens
  
  
  #tune nnet
  
  #t.nnet <- tune.nnet(cbind(te$logRBl,te$logRBr,te$FSPIBl,te$FSPIBr,te$TLMPILBl,te$TLMPILBr,
  #                          te$DIBl,te$DIBr,te$BeffBl,te$BeffBr,te$IsinEn1Bl,te$IsinEn1Br,te$IsinEn2Bl,te$IsinEn2Br,te$NNC),
  #                    te$yIN_C1_C9, size = 8*2^(-1:2), decay = 10^(-3:-1),
  #                    entropy=T,maxit=2000,MaxNWts=2000  )
  
  #p.nnet <- t.nnet$best.model
  
  
  #t.nnet <- tune.nnet(cbind(te$logRBl,te$FSPIBl,te$TLMPILBl,te$DIBl,te$BeffBl,te$IsinEn1Bl,te$IsinEn2Bl,te$NNC),
  #                    te$yIN_C1_C9, size = 4, decay = 10^(-1),
  #                    entropy=T,maxit=2000,MaxNWts=2000  )
  
  #p.nnet <- t.nnet$best.model
  
  p.nnet <- nnet(cbind(te$alpha_exp_fft_blos,te$alpha_exp_fft_br,te$decay_index_blos,te$decay_index_br,
                       te$ising_energy_blos,te$ising_energy_br,te$ising_energy_part_blos,te$ising_energy_part_br,
                       te$mpil_blos,te$mpil_br,te$nn_currents,
                       te$r_value_blos_logr,te$r_value_br_logr,te$wlsg_blos,te$wlsg_br),
                 te$y_GT_M1, size = 32, decay = 10^(-1),
                 entropy=T,maxit=2000,MaxNWts=2000  )
  
  p <- p.nnet
  jpeg(paste("MLP_model_combined_DATABASE_trn_orig_split",".jpeg",sep=""))
  plot(te$y_GT_M1,xlab="observation",ylab="value of y",main=paste("MLP with entropy option, combined regressors\n", "Training, Original Split",sep=" "))
  lines(p$fitted.values,col=5)
  dev.off()
  
  save.image(paste("mlp_object_p_","orig_split",".RData",sep=""))
  ###  }
  ###}
  
  #predict with nnet
  #newdata:
  #teTst
  
  
  #safe mode
  yTst <- teTst$y_GT_M1
  teTst$y_GT_M1 <- -999
  
  #p.nnet.predict <- predict(p.nnet,newdata=teTst[,-c(9,10)])
  p.nnet.predict <- predict(p.nnet,newdata=teTst[,-c(16)])
  
  #restore
  teTst$y_GT_M1 = yTst
  
  jpeg(paste("MLP_model_combined_DATABASE_testing_orig_split",".jpeg",sep=""))
  plot(teTst$y_GT_M1,xlab="observation",ylab="value of y",main=paste("MLP with entropy option, combined regressors,\n",
                                                                     "Testing, original split",sep=" "))
  #plot(teTst$yIN_C1_C9,xlab="observation",ylab="value of y",main=paste("MLP with linout option, combined regressors,\n",
  #                                                             "Testing, original split,","tries:",tries,"iNode:",iNode,sep=" "))
  lines(p.nnet.predict,col=5)
  dev.off()
  
  
  p.nnet.predict.01 <- numeric(length(p.nnet.predict))
  
  accuracy0_all <- numeric(101)   # mlp is "0"
  tss0_all      <- numeric(101)
  hss0_all      <- numeric(101)
  accuracy1_all <- numeric(101)   # lm is  "1"
  tss1_all      <- numeric(101)
  hss1_all      <- numeric(101)
  accuracy2_all <- numeric(101)   # probit is "2"
  tss2_all      <- numeric(101)
  hss2_all      <- numeric(101)
  accuracy3_all <- numeric(101)   # logit is "3"
  tss3_all      <- numeric(101)
  hss3_all      <- numeric(101)
  accuracy4_all <- numeric(101)   # randomForestC is "4"
  tss4_all      <- numeric(101)
  hss4_all      <- numeric(101)
  accuracy5_all <- numeric(101)   # SVC is "5"
  tss5_all      <- numeric(101)
  hss5_all      <- numeric(101)
  accuracy5a_all <- numeric(101)   # SVCa is "5"
  tss5a_all      <- numeric(101)
  hss5a_all      <- numeric(101)
  accuracy5b_all <- numeric(101)   # SVCb is "5"
  tss5b_all      <- numeric(101)
  hss5b_all      <- numeric(101)
  
  
  ccp <- 0
  accuracy <- NA
  trueSS   <- NA
  HeidkeSS <- NA
  for (thresHOLD in seq(0.00,1.00,0.01) )
  {
    #thresHOLD <- 0.50
    #thresHOLD <- 0.25
    
    if ( mod(100*thresHOLD,10) == 0) {
      cat("threshold:...",100*thresHOLD," % completed...","\n")
    }
    ccp <- ccp + 1
    for (i in 1:length(p.nnet.predict)) {
      response=0
      if (!is.na(p.nnet.predict[i])) {
        if (p.nnet.predict[i] > thresHOLD) {
          response=1
        } 
      }
      p.nnet.predict.01[i] <- response
    }
    
    cfmat <- table(p.nnet.predict.01, teTst$y_GT_M1)
    
    #write.table(cfmat,paste("cfmat","_combined_SHYKJG_testing_MLP_","Orig_Split_","_thresHOLD_",thresHOLD,".txt",sep=""))
    
    accuracy <- NA
    trueSS   <- NA
    HeidkeSS <- NA
    
    if(dim(cfmat)[1]==2 && dim(cfmat)[2]==2) {
      
      accuracy <- sum(diag(cfmat)) / sum(cfmat)
      
      a=cfmat[2,2]
      d=cfmat[1,1]
      b=cfmat[2,1] 
      c=cfmat[1,2]
      
      #trueSS <- TSS.Stat(cfmat)
      
      trueSS <- (a*d-b*c) / ((a+c)*(b+d))
      
      HeidkeSS <- 2 * (a*d-b*c) / ( (a+c)*(c+d) + (a+b)*(b+d) )
      
      accuracy0_all[ccp] <- accuracy
      tss0_all[ccp] <- trueSS
      hss0_all[ccp] <- HeidkeSS
      
      
      #The score has a range of -1 to +1, with 0 representing no skill.
      #Negative values would be associated with "perverse" forecasts,
      #and could be converted to positive skill simply by replacing
      #all the yes forecasts with no and vice-versa.
      #The KSS is also the difference between the hit rate and false alarm rate,
      #KSS=H-F. 
      
      #hit rate and false alarm
      
      H = a / (a+c)    #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko2.htm
      F = (b) / (b+d)  #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko3.htm
      
      flag = abs(trueSS - (H-F)) < 10^(-4)
      
      #cat("dataset used as test set finished:...",jj)
      ##cat("accuracy:...",accuracy,"\n")
      ##cat("TSS:... ",paste(round(trueSS,6),round(H-F,6),sep=" "))
      ##cat("verify:...",flag,"\n")
      ##cat("HSS:... ",round(HeidkeSS,6),"\n")
      
    }
    
    
    
    #predict lm
    m1.predict <- predict(m1,newdata=teTst)
    
    # jpeg(paste("linear_model_model_DATABASE_testing_Orig_Split",".jpeg",sep=""))
    # plot(teTst$y_GT_M1,xlab="observation",ylab="value of y",main=paste("linear model, combined regressors,\n",
    #                                                                    "Testing, Original Split",sep=" "))
    # 
    # lines(m1.predict,col=2)
    # dev.off()
    
    m1.predict.01 <- numeric(length(m1.predict))
    #thresHOLD <- 0.50
    #thresHOLD <- 0.25
    
    for (i in 1:length(m1.predict)) {
      response=0
      if (!is.na(m1.predict[i])) {
        if (m1.predict[i] > thresHOLD) {
          response=1
        } 
      }
      m1.predict.01[i] <- response
    }
    
    cfmat <- table(m1.predict.01, teTst$y_GT_M1)
    
    #write.table(cfmat,paste("cfmat","_combined_SHYKJG_testing_lm_","Orig_Split","_thresHOLD_",thresHOLD,".txt",sep=""))
    
    accuracy <- NA
    trueSS   <- NA
    HeidkeSS <- NA
    
    if(dim(cfmat)[1]==2 && dim(cfmat)[2]==2) {
      
      accuracy <- sum(diag(cfmat)) / sum(cfmat)
      
      a=cfmat[2,2]
      d=cfmat[1,1]
      b=cfmat[2,1] 
      c=cfmat[1,2]
      
      #trueSS <- TSS.Stat(cfmat)
      
      trueSS <- (a*d-b*c) / ((a+c)*(b+d))
      
      HeidkeSS <- 2 * (a*d-b*c) / ( (a+c)*(c+d) + (a+b)*(b+d) )
      
      accuracy1_all[ccp] <- accuracy
      tss1_all[ccp] <- trueSS
      hss1_all[ccp] <- HeidkeSS
      
      
      #The score has a range of -1 to +1, with 0 representing no skill.
      #Negative values would be associated with "perverse" forecasts,
      #and could be converted to positive skill simply by replacing
      #all the yes forecasts with no and vice-versa.
      #The KSS is also the difference between the hit rate and false alarm rate,
      #KSS=H-F. 
      
      #hit rate and false alarm
      
      H = a / (a+c)    #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko2.htm
      F = (b) / (b+d)  #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko3.htm
      
      flag = abs(trueSS - (H-F)) < 10^(-4)
      
      #cat("dataset used as test set finished:...",jj)
      ##cat("accuracy:...",accuracy,"\n")
      ##cat("TSS:... ",paste(round(trueSS,6),round(H-F,6),sep=" "))
      ##cat("verify:...",flag,"\n")
      ##cat("HSS:... ",round(HeidkeSS,6),"\n")
      
    }
    
    
    #predict glm probit
    ##m2.predict <- predict(m2,newdata=teTst)
    m2.predict <- predict(m2,newdata=teTst,type="response")
    
    
    # jpeg(paste("probit_model_model_DATABASE_testing_orig_split",".jpeg",sep=""))
    # plot(teTst$y_GT_M1,xlab="observation",ylab="value of y",main=paste("probit model, combined regressors,\n",
    #                                                                    "Testing, Original Split",sep=" "))
    # lines(m2.predict,col=3)
    # dev.off()
    
    m2.predict.01 <- numeric(length(m2.predict))
    #thresHOLD <- 0.50
    #thresHOLD <- 0.25
    
    for (i in 1:length(m2.predict)) {
      response=0
      if (!is.na(m2.predict[i])) {
        if (m2.predict[i] > thresHOLD) {
          response=1
        } 
      }
      m2.predict.01[i] <- response
    }
    
    cfmat <- table(m2.predict.01, teTst$y_GT_M1)
    
    #write.table(cfmat,paste("cfmat","_combined_SHYKJG_testing_probit_","Orig_Split_","_thresHOLD_",thresHOLD,".txt",sep=""))
    
    accuracy <- NA
    trueSS   <- NA
    HeidkeSS <- NA
    
    
    if(dim(cfmat)[1]==2 && dim(cfmat)[2]==2) {
      
      accuracy <- sum(diag(cfmat)) / sum(cfmat)
      
      a=cfmat[2,2]
      d=cfmat[1,1]
      b=cfmat[2,1] 
      c=cfmat[1,2]
      
      #trueSS <- TSS.Stat(cfmat)
      
      trueSS <- (a*d-b*c) / ((a+c)*(b+d))
      
      HeidkeSS <- 2 * (a*d-b*c) / ( (a+c)*(c+d) + (a+b)*(b+d) )
      
      accuracy2_all[ccp] <- accuracy
      tss2_all[ccp] <- trueSS
      hss2_all[ccp] <- HeidkeSS
      
      
      #The score has a range of -1 to +1, with 0 representing no skill.
      #Negative values would be associated with "perverse" forecasts,
      #and could be converted to positive skill simply by replacing
      #all the yes forecasts with no and vice-versa.
      #The KSS is also the difference between the hit rate and false alarm rate,
      #KSS=H-F. 
      
      #hit rate and false alarm
      
      H = a / (a+c)    #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko2.htm
      F = (b) / (b+d)  #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko3.htm
      
      flag = abs(trueSS - (H-F)) < 10^(-4)
      
      #cat("dataset used as test set finished:...",jj)
      ##cat("accuracy:...",accuracy,"\n")
      ##cat("TSS:... ",paste(round(trueSS,6),round(H-F,6),sep=" "))
      ##cat("verify:...",flag,"\n")
      ##cat("HSS:... ",round(HeidkeSS,6),"\n")
      
    }
    
    
    #predict glm logit
    
    ##m3.predict <- predict(m3,newdata=teTst)
    m3.predict <- predict(m3,newdata=teTst,type="response")
    
    # jpeg(paste("logit_model_model_DATABASE_testing_orig_split",".jpeg",sep=""))
    # plot(teTst$y_GT_M1,xlab="observation",ylab="value of y",main=paste("logit model, combined regressors","\n",
    #                                                                    "Testing, Original Split",sep=" "))
    # 
    # lines(m3.predict,col=4)
    # dev.off()
    
    m3.predict.01 <- numeric(length(m3.predict))
    #thresHOLD <- 0.50
    #thresHOLD <- 0.25
    
    for (i in 1:length(m3.predict)) {
      response=0
      if (!is.na(m3.predict[i])) {
        if (m3.predict[i] > thresHOLD) {
          response=1
        } 
      }
      m3.predict.01[i] <- response
    }
    
    cfmat <- table(m3.predict.01, teTst$y_GT_M1)
    
    #write.table(cfmat,paste("cfmat","_combined_SHYKJG_testing_logit_","Orig_Split","_thresHOLD_",thresHOLD,".txt",sep=""))
    
    accuracy <- NA
    trueSS   <- NA
    HeidkeSS <- NA
    
    if(dim(cfmat)[1]==2 && dim(cfmat)[2]==2) {
      
      accuracy <- sum(diag(cfmat)) / sum(cfmat)
      
      a=cfmat[2,2]
      d=cfmat[1,1]
      b=cfmat[2,1] 
      c=cfmat[1,2]
      
      #trueSS <- TSS.Stat(cfmat)
      
      trueSS <- (a*d-b*c) / ((a+c)*(b+d))
      
      HeidkeSS <- 2 * (a*d-b*c) / ( (a+c)*(c+d) + (a+b)*(b+d) )
      
      accuracy3_all[ccp] <- accuracy
      tss3_all[ccp] <- trueSS
      hss3_all[ccp] <- HeidkeSS
      
      
      #The score has a range of -1 to +1, with 0 representing no skill.
      #Negative values would be associated with "perverse" forecasts,
      #and could be converted to positive skill simply by replacing
      #all the yes forecasts with no and vice-versa.
      #The KSS is also the difference between the hit rate and false alarm rate,
      #KSS=H-F. 
      
      #hit rate and false alarm
      
      H = a / (a+c)    #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko2.htm
      F = (b) / (b+d)  #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko3.htm
      
      flag = abs(trueSS - (H-F)) < 10^(-4)
      
      #cat("dataset used as test set finished:...",jj)
      ##cat("accuracy:...",accuracy,"\n")
      ##cat("TSS:... ",paste(round(trueSS,6),round(H-F,6),sep=" "))
      ##cat("verify:...",flag,"\n")
      ##cat("HSS:... ",round(HeidkeSS,6),"\n")
      
    }
    
    
    #predict bayesQR
    #not done
    
    #p.randomForest.predict <- predict(p.randomForest,newdata=teTst[,-c(9,10)],type="response")
    ###p.randomForest.predict <- predict(p.randomForest,newdata=teTst[,-c(16,17)],type="response")
    p.randomForest.predict <- predict(p.randomForest,newdata=teTst[,-c(16)],type="prob")[,2]
    
    # jpeg(paste("randomForest_model_model_DATABASE_testing_orig_split",".jpeg",sep=""))
    # plot(teTst$y_GT_M1,xlab="observation",ylab="value of y",main=paste("random Forest model, combined regressors","\n",
    #                                                                    "Testing, Original Split",sep=" "))
    # 
    # lines(p.randomForest.predict,col=5)
    # dev.off()
    
    
    #because randomForest gives probabilities <0 and >1
    #first bound left to 0 and right to 1
    ### p.randomForest.predict.new <- numeric(length(p.randomForest.predict))
    ### for (i in 1:length(p.randomForest.predict)) {
    ###   p.randomForest.predict.new[i] <- max(0,min(p.randomForest.predict[i],1))
    ### }
    
    p.randomForest.predict.new <- p.randomForest.predict
    
    p.randomForest.predict.01 <- numeric(length(p.randomForest.predict.new))
    
    #thresHOLD <- 0.50
    #thresHOLD <- 0.25
    
    for (i in 1:length(p.randomForest.predict.new)) {
      response=0
      if (!is.na(p.randomForest.predict.new[i])) {
        if (p.randomForest.predict.new[i] > thresHOLD) {
          response=1
        } 
      }
      p.randomForest.predict.01[i] <- response
    }        
    
    cfmat <- table(p.randomForest.predict.01, teTst$y_GT_M1)
    
    #write.table(cfmat,paste("cfmat","_combined_SHYKJG_testing_randomForest_","Orig_Split","_thresHOLD_",thresHOLD,".txt",sep=""))
    
    accuracy <- NA
    trueSS   <- NA
    HeidkeSS <- NA
    
    if(dim(cfmat)[1]==2 && dim(cfmat)[2]==2) {
      
      accuracy <- sum(diag(cfmat)) / sum(cfmat)
      
      a=cfmat[2,2]
      d=cfmat[1,1]
      b=cfmat[2,1] 
      c=cfmat[1,2]
      
      #trueSS <- TSS.Stat(cfmat)
      
      trueSS <- (a*d-b*c) / ((a+c)*(b+d))
      
      HeidkeSS <- 2 * (a*d-b*c) / ( (a+c)*(c+d) + (a+b)*(b+d) )
      
      accuracy4_all[ccp] <- accuracy
      tss4_all[ccp] <- trueSS
      hss4_all[ccp] <- HeidkeSS
      
      
      #The score has a range of -1 to +1, with 0 representing no skill.
      #Negative values would be associated with "perverse" forecasts,
      #and could be converted to positive skill simply by replacing
      #all the yes forecasts with no and vice-versa.
      #The KSS is also the difference between the hit rate and false alarm rate,
      #KSS=H-F. 
      
      #hit rate and false alarm
      
      H = a / (a+c)    #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko2.htm
      F = (b) / (b+d)  #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko3.htm
      
      flag = abs(trueSS - (H-F)) < 10^(-4)
      
      #cat("dataset used as test set finished:...",jj)
      ##cat("accuracy:...",accuracy,"\n")
      ##cat("TSS:... ",paste(round(trueSS,6),round(H-F,6),sep=" "))
      ##cat("verify:...",flag,"\n")
      ##cat("HSS:... ",round(HeidkeSS,6),"\n")
      
    }
    
    
    #SVC = best(SVCa,SVCb)
    #SVCa
    #predict svm
    p.svm.predict <- predict(p.svm, newdata=teTst[,-c(16)],probability=TRUE)
    ###p.svm.predict.num <- attr(p.svm.predict,"prob")[,2]
    # if (as.factor(teTst$y_GT_M1)[1] == 1) {
    #   p.svm.predict.num <- attr(p.svm.predict,"prob")[,1]
    # } 
    # if (as.factor(teTst$y_GT_M1)[1] == 0) {
    #   p.svm.predict.num <- attr(p.svm.predict,"prob")[,2]
    # } 
    p.svm.predict.num <- attr(p.svm.predict,"prob")[,2]
    p.svm.predict.num.A <- p.svm.predict.num
    
    # jpeg(paste("SVM_model_model_DATABASE_testing_orig_split",".jpeg",sep=""))
    # plot(teTst$y_GT_M1,xlab="observation",ylab="value of y",main=paste("SVM model, combined regressors","\n",
    #                                                                    "Testing, Original Split",sep=" "))
    # 
    # lines(p.svm.predict.num,col=6)
    # dev.off()
    
    #p.svm.predict.new <- p.svm.predict
    p.svm.predict.01 <- numeric(length(p.svm.predict.num))
    #p.svm.predict.01 <- p.svm.predict.num
    #thresHOLD <- 0.50
    #thresHOLD <- 0.25
    
    for (i in 1:length(p.svm.predict.num)) {
      response=0
      if (!is.na(p.svm.predict.num[i])) {
        if (p.svm.predict.num[i] > thresHOLD) {
          response=1
        } 
      }
      p.svm.predict.01[i] <- response
    }
    
    cfmat <- table(p.svm.predict.01, teTst$y_GT_M1)
    
    #write.table(cfmat,paste("cfmat","_combined_SHYKJG_testing_SVM_","Orig_Split","_thresHOLD_",thresHOLD,".txt",sep=""))
    
    accuracy <- NA
    trueSS   <- NA
    HeidkeSS <- NA
    
    
    if(dim(cfmat)[1]==2 && dim(cfmat)[2]==2) {
      
      accuracy <- sum(diag(cfmat)) / sum(cfmat)
      
      a=cfmat[2,2]
      d=cfmat[1,1]
      b=cfmat[2,1] 
      c=cfmat[1,2]
      
      #trueSS <- TSS.Stat(cfmat)
      
      trueSS <- (a*d-b*c) / ((a+c)*(b+d))
      
      HeidkeSS <- 2 * (a*d-b*c) / ( (a+c)*(c+d) + (a+b)*(b+d) )
      
      accuracy5a_all[ccp] <- accuracy
      tss5a_all[ccp] <- trueSS
      hss5a_all[ccp] <- HeidkeSS
      
      
      #The score has a range of -1 to +1, with 0 representing no skill.
      #Negative values would be associated with "perverse" forecasts,
      #and could be converted to positive skill simply by replacing
      #all the yes forecasts with no and vice-versa.
      #The KSS is also the difference between the hit rate and false alarm rate,
      #KSS=H-F. 
      
      #hit rate and false alarm
      
      H = a / (a+c)    #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko2.htm
      F = (b) / (b+d)  #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko3.htm
      
      flag = abs(trueSS - (H-F)) < 10^(-4)
      
      #cat("dataset used as test set finished:...",jj)
      ##cat("accuracy:...",accuracy,"\n")
      ##cat("TSS:... ",paste(round(trueSS,6),round(H-F,6),sep=" "))
      ##cat("verify:...",flag,"\n")
      ##cat("HSS:... ",round(HeidkeSS,6),"\n")
      
    }
    
    
    #SVCb
    #predict svm
    p.svm.predict <- predict(p.svm, newdata=teTst[,-c(16)],probability=TRUE)
    ###p.svm.predict.num <- attr(p.svm.predict,"prob")[,2]
    # if (as.factor(teTst$y_GT_M1)[1] == 1) {
    #   p.svm.predict.num <- attr(p.svm.predict,"prob")[,1]
    # } 
    # if (as.factor(teTst$y_GT_M1)[1] == 0) {
    #   p.svm.predict.num <- attr(p.svm.predict,"prob")[,2]
    # } 
    p.svm.predict.num <- attr(p.svm.predict,"prob")[,1]
    p.svm.predict.num.B <- p.svm.predict.num    
    
    # jpeg(paste("SVM_model_model_DATABASE_testing_orig_split",".jpeg",sep=""))
    # plot(teTst$y_GT_C1,xlab="observation",ylab="value of y",main=paste("SVM model, combined regressors","\n",
    #                                                                    "Testing, Original Split",sep=" "))
    # 
    # lines(p.svm.predict.num,col=6)
    # dev.off()
    
    #p.svm.predict.new <- p.svm.predict
    p.svm.predict.01 <- numeric(length(p.svm.predict.num))
    #p.svm.predict.01 <- p.svm.predict.num
    #thresHOLD <- 0.50
    #thresHOLD <- 0.25
    
    for (i in 1:length(p.svm.predict.num)) {
      response=0
      if (!is.na(p.svm.predict.num[i])) {
        if (p.svm.predict.num[i] > thresHOLD) {
          response=1
        } 
      }
      p.svm.predict.01[i] <- response
    }
    
    cfmat <- table(p.svm.predict.01, teTst$y_GT_M1)
    
    #write.table(cfmat,paste("cfmat","_combined_SHYKJG_testing_SVM_","Orig_Split","_thresHOLD_",thresHOLD,".txt",sep=""))
    
    accuracy <- NA
    trueSS   <- NA
    HeidkeSS <- NA
    
    
    if(dim(cfmat)[1]==2 && dim(cfmat)[2]==2) {
      
      accuracy <- sum(diag(cfmat)) / sum(cfmat)
      
      a=cfmat[2,2]
      d=cfmat[1,1]
      b=cfmat[2,1] 
      c=cfmat[1,2]
      
      #trueSS <- TSS.Stat(cfmat)
      
      trueSS <- (a*d-b*c) / ((a+c)*(b+d))
      
      HeidkeSS <- 2 * (a*d-b*c) / ( (a+c)*(c+d) + (a+b)*(b+d) )
      
      accuracy5b_all[ccp] <- accuracy
      tss5b_all[ccp] <- trueSS
      hss5b_all[ccp] <- HeidkeSS
      
      
      #The score has a range of -1 to +1, with 0 representing no skill.
      #Negative values would be associated with "perverse" forecasts,
      #and could be converted to positive skill simply by replacing
      #all the yes forecasts with no and vice-versa.
      #The KSS is also the difference between the hit rate and false alarm rate,
      #KSS=H-F. 
      
      #hit rate and false alarm
      
      H = a / (a+c)    #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko2.htm
      F = (b) / (b+d)  #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko3.htm
      
      flag = abs(trueSS - (H-F)) < 10^(-4)
      
      #cat("dataset used as test set finished:...",jj)
      ##cat("accuracy:...",accuracy,"\n")
      ##cat("TSS:... ",paste(round(trueSS,6),round(H-F,6),sep=" "))
      ##cat("verify:...",flag,"\n")
      ##cat("HSS:... ",round(HeidkeSS,6),"\n")
      
    }
    
  } ### thresHOLD loop
  
  #predict svm
  #SVC=best(SVCa,SVCb)
  if (accuracy5a_all[51] > accuracy5b_all[51]) {
    accuracy5_all <- accuracy5a_all
    tss5_all <- tss5a_all
    hss5_all <- hss5a_all
    p.svm.predict.num <- p.svm.predict.num.A
  }
  if (accuracy5b_all[51] > accuracy5a_all[51]) {
    accuracy5_all <- accuracy5b_all
    tss5_all <- tss5b_all
    hss5_all <- hss5b_all
    p.svm.predict.num <- p.svm.predict.num.B
  }
  
  
  #save all wrt thresHOLD arrays
  #mlp
  write.table(cbind(1:101,seq(0,1,0.01),accuracy0_all,tss0_all,hss0_all),file=paste("mlp_SKILLS_testing_orig_split_",".out",sep=""),row.names=FALSE,col.names=FALSE)
  #lm
  write.table(cbind(1:101,seq(0,1,0.01),accuracy1_all,tss1_all,hss1_all),file=paste("lm_SKILLS_testing_orig_split_",".out",sep=""),row.names=FALSE,col.names=FALSE)
  #probit
  write.table(cbind(1:101,seq(0,1,0.01),accuracy2_all,tss2_all,hss2_all),file=paste("probit_SKILLS_testing_orig_split_",".out",sep=""),row.names=FALSE,col.names=FALSE)
  #logit
  write.table(cbind(1:101,seq(0,1,0.01),accuracy3_all,tss3_all,hss3_all),file=paste("logit_SKILLS_testing_orig_split_",".out",sep=""),row.names=FALSE,col.names=FALSE)
  #randomForest
  write.table(cbind(1:101,seq(0,1,0.01),accuracy4_all,tss4_all,hss4_all),file=paste("randomForestC_SKILLS_testing_orig_split_",".out",sep=""),row.names=FALSE,col.names=FALSE)
  #SVM
  write.table(cbind(1:101,seq(0,1,0.01),accuracy5_all,tss5_all,hss5_all),file=paste("SVC_SKILLS_testing_orig_split_",".out",sep=""),row.names=FALSE,col.names=FALSE)
  
  res0 <- cbind(1:101,seq(0,1,0.01),accuracy0_all,tss0_all,hss0_all)
  
  
  jpeg(paste("mlp_model_DATABASE_testing_orig_split_","_SKILLS_",".jpeg",sep=""))
  plot(res0[,2],res0[,3],type="l",col=2,lwd=2.5,xlab="probability threshold",ylab="skills values",ylim=c(0,1),main=paste("skills wrt threshold for mlp\n",
                                                                                                                         "testing",sep=" "))
  lines(res0[,2],res0[,4],type="l",lwd=2.5,col=3)
  lines(res0[,2],res0[,5],type="l",lwd=2.5,col=4)    
  
  legend(0.7,0.6, # places a legend at the appropriate place 
         c("acc","tss","hss"), # puts text in the legend
         lty=c(1,1,1), # gives the legend appropriate symbols (lines)
         lwd=c(2.5,2.5,2.5),col=c(2,3,4)) # gives the legend lines the correct color and width
  
  dev.off()
  
  
  
  res1 <- cbind(1:101,seq(0,1,0.01),accuracy1_all,tss1_all,hss1_all)
  
  
  jpeg(paste("lm_model_DATABASE_testing_orig_split_","_SKILLS_",".jpeg",sep=""))
  plot(res1[,2],res1[,3],type="l",col=2,lwd=2.5,xlab="probability threshold",ylab="skills values",main=paste("skills wrt threshold for linear model\n",
                                                                                                             "testing",sep=" "),
       ylim=c(0,1))
  lines(res1[,2],res1[,4],type="l",lwd=2.5,col=3)
  lines(res1[,2],res1[,5],type="l",lwd=2.5,col=4)    
  
  legend(0.7,0.6, # places a legend at the appropriate place 
         c("acc","tss","hss"), # puts text in the legend
         lty=c(1,1,1), # gives the legend appropriate symbols (lines)
         lwd=c(2.5,2.5,2.5),col=c(2,3,4)) # gives the legend lines the correct color and width
  
  dev.off()
  
  
  
  res2 <- cbind(1:101,seq(0,1,0.01),accuracy2_all,tss2_all,hss2_all)
  
  jpeg(paste("probit_model_DATABASE_testing_orig_split_","_SKILLS_",".jpeg",sep=""))
  plot(res2[,2],res2[,3],type="l",col=2,lwd=2.5,xlab="probability threshold",ylab="skills values",xlim=c(0,1),ylim=c(0,1),
       main=paste("skills wrt threshold for probit\n",
                  "testing",sep=" "))
  lines(res2[,2],res2[,4],type="l",lwd=2.5,col=3)
  lines(res2[,2],res2[,5],type="l",lwd=2.5,col=4)    
  
  legend(0.7,0.6, # places a legend at the appropriate place 
         c("acc","tss","hss"), # puts text in the legend
         lty=c(1,1,1), # gives the legend appropriate symbols (lines)
         lwd=c(2.5,2.5,2.5),col=c(2,3,4)) # gives the legend lines the correct color and width
  
  dev.off()
  
  
  
  res3 <- cbind(1:101,seq(0,1,0.01),accuracy3_all,tss3_all,hss3_all)
  
  jpeg(paste("logit_model_DATABASE_testing_orig_split_","_SKILLS_",".jpeg",sep=""))    
  plot(res3[,2],res3[,3],type="l",col=2,lwd=2.5,xlab="probability threshold",ylab="skills values",xlim=c(0,1),ylim=c(0,1),
       main=paste("skills wrt threshold for logit\n",
                  "testing",sep=" "))
  lines(res3[,2],res3[,4],type="l",lwd=2.5,col=3)
  lines(res3[,2],res3[,5],type="l",lwd=2.5,col=4)    
  
  legend(0.7,0.6, # places a legend at the appropriate place 
         c("acc","tss","hss"), # puts text in the legend
         lty=c(1,1,1), # gives the legend appropriate symbols (lines)
         lwd=c(2.5,2.5,2.5),col=c(2,3,4)) # gives the legend lines the correct color and width
  dev.off()
  
  res4 <- cbind(1:101,seq(0,1,0.01),accuracy4_all,tss4_all,hss4_all)
  jpeg(paste("randomForest_model_DATASET_testing_orig_split_","_SKILLS_",".jpeg",sep=""))    
  plot(res4[,2],res4[,3],type="l",col=2,lwd=2.5,xlab="probability threshold",ylab="skills values",xlim=c(0,1),ylim=c(0,1),
       main=paste("skills wrt threshold for random Forest\n",
                  "testing",sep=" "))
  lines(res4[,2],res4[,4],type="l",lwd=2.5,col=3)
  lines(res4[,2],res4[,5],type="l",lwd=2.5,col=4)    
  
  legend(0.7,0.6, # places a legend at the appropriate place 
         c("acc","tss","hss"), # puts text in the legend
         lty=c(1,1,1), # gives the legend appropriate symbols (lines)
         lwd=c(2.5,2.5,2.5),col=c(2,3,4)) # gives the legend lines the correct color and width
  dev.off()
  
  
  res5 <- cbind(1:101,seq(0,1,0.01),accuracy5_all,tss5_all,hss5_all)
  
  jpeg(paste("SVM_model_DATABASE_testing_orig_split_","_SKILLS_",".jpeg",sep=""))    
  plot(res5[,2],res5[,3],type="l",col=2,lwd=2.5,xlab="probability threshold",ylab="skills values",xlim=c(0,1),ylim=c(0,1),
       main=paste("skills wrt threshold for SVM\n",
                  "testing",sep=" "))
  lines(res5[,2],res5[,4],type="l",lwd=2.5,col=3)
  lines(res5[,2],res5[,5],type="l",lwd=2.5,col=4)    
  
  legend(0.7,0.6, # places a legend at the appropriate place 
         c("acc","tss","hss"), # puts text in the legend
         lty=c(1,1,1), # gives the legend appropriate symbols (lines)
         lwd=c(2.5,2.5,2.5),col=c(2,3,4)) # gives the legend lines the correct color and width
  dev.off()
  
  
  #} ### iNode loop
  #} ### tries loop
  #} ### jj loop - crossvalidation in every year
  
  #ROC curves
  require(ROCR)
  
  #neural network model    
  jpeg(paste("ROC_neural_network_model_",".jpeg",sep=""))
  pred0 <- prediction(p.nnet.predict,teTst$y_GT_M1)
  perf0 <- performance(pred0,"tpr","fpr")
  plot(perf0,main=c("ROC curve for Neural Network Model\n","All Br and Blos predictors")) 
  lines(seq(0,1,0.01),seq(0,1,0.01))
  dev.off()
  
  auc.perf0 = performance(pred0, measure = "auc")
  auc.perf0@y.values
  
  
  #linear model
  jpeg("ROC_linear_model.jpeg")
  pred1 <- prediction(m1.predict,teTst$y_GT_M1)
  perf1 <- performance(pred1,"tpr","fpr")
  plot(perf1,main=c("ROC curve for Linear Model\n","All Br and Blos predictors")) 
  lines(seq(0,1,0.01),seq(0,1,0.01))
  dev.off()
  
  auc.perf1 = performance(pred1, measure = "auc")
  auc.perf1@y.values
  
  
  #probit model
  jpeg("ROC_probit_model.jpeg")
  pred2 <- prediction(m2.predict,teTst$y_GT_M1)
  perf2 <- performance(pred2,"tpr","fpr")
  plot(perf2,main=c("ROC curve for Probit Model\n","All Br and Blos predictors")) 
  lines(seq(0,1,0.01),seq(0,1,0.01))
  dev.off()  
  
  auc.perf2 = performance(pred2, measure = "auc")
  auc.perf2@y.values
  
  
  
  #logit model
  jpeg("ROC_logit_model.jpeg")
  pred3 <- prediction(m3.predict,teTst$y_GT_M1)
  perf3 <- performance(pred3,"tpr","fpr")
  plot(perf3,main=c("ROC curve for Logit Model\n","All Br and Blos predictors")) 
  lines(seq(0,1,0.01),seq(0,1,0.01))
  dev.off()  
  
  auc.perf3 = performance(pred3, measure = "auc")
  auc.perf3@y.values     
  
  
  #randomForest model
  jpeg("ROC_randomForest_model.jpeg")
  pred4 <- prediction(p.randomForest.predict.new,teTst$y_GT_M1)
  perf4 <- performance(pred4,"tpr","fpr")
  plot(perf4,main=c("ROC curve for random Forest Model\n","All Br and Blos predictors")) 
  lines(seq(0,1,0.01),seq(0,1,0.01))
  dev.off()     
  
  auc.perf4 = performance(pred4, measure = "auc")
  auc.perf4@y.values     
  
  
  #SVM model
  jpeg("ROC_SVM_model.jpeg")
  pred5 <- prediction(p.svm.predict.num,teTst$y_GT_M1)
  perf5 <- performance(pred5,"tpr","fpr")
  plot(perf5,main=c("ROC curve for SVM Model\n","All Br and Blos predictors")) 
  lines(seq(0,1,0.01),seq(0,1,0.01))
  dev.off()     
  
  auc.perf5 = performance(pred5, measure = "auc")
  auc.perf5@y.values     
  
  
  AUCvalues <- c(as.numeric(auc.perf0@y.values),as.numeric(auc.perf1@y.values),as.numeric(auc.perf2@y.values),
                 as.numeric(auc.perf3@y.values),as.numeric(auc.perf4@y.values),as.numeric(auc.perf5@y.values))
  
  
  #Reliability Diagrams
  
  #neural network model
  library(verification)
  
  mod0 <- verify(obs = teTst$y_GT_M1, pred = p.nnet.predict,thresholds=seq(0,1,0.05))
  
  #plot(mod1, CI=TRUE)
  #plot(mod1)
  jpeg(paste("ReliabilityDiagram_Neural_Network_model",".jpeg",sep=""))
  plot(mod0, freq=FALSE, main=c("Reliability Diagram for Neural Network Model\n","All Br and Blos predictors"))
  lines(seq(0,1,0.01),seq(0,1,0.01))
  dev.off()
  
  jpeg(paste("ReliabilityDiagram_withErrorsBars_Neural_Network_model",".jpeg",sep=""))
  plot(mod0, freq=FALSE, CI=T, main=c("Reliability Diagram for Neural Network Model\n","All Br and Blos predictors"))
  lines(seq(0,1,0.01),seq(0,1,0.01))
  dev.off()     
  
  
  #brier neural network
  #p.nnet.predict
  #teTst$yGTM1
  #https://en.wikipedia.org/wiki/Brier_score
  sd0 <- (p.nnet.predict - teTst$y_GT_M1)^2
  brier0 <- mean(sd0)
  
  
  #linear model
  library(verification)
  
  #because lm gives probabilities <0 and >1
  #first bound left to 0 and right to 1
  m1.predict.new <- numeric(length(m1.predict))
  for (i in 1:length(m1.predict)) {
    m1.predict.new[i] <- max(0,min(m1.predict[i],1))
  }
  #mod1 <- verify(obs = teTst$y_GT_M1, pred = m1.predict.new)
  mod1 <- verify(obs = teTst$y_GT_M1, pred = m1.predict.new,thresholds=seq(0,1,0.05))
  
  
  #plot(mod1, CI=TRUE)
  #plot(mod1)
  jpeg("ReliabilityDiagram_linear_model.jpeg")
  plot(mod1, freq=FALSE, main=c("Reliability Diagram for Linear Model\n","All Br and Blos predictors"))
  lines(seq(0,1,0.01),seq(0,1,0.01))
  dev.off()
  
  jpeg("ReliabilityDiagram_withErrorsBars_linear_model.jpeg")
  plot(mod1, freq=FALSE, CI=T, main=c("Reliability Diagram for Linear Model\n","All Br and Blos predictors"))
  lines(seq(0,1,0.01),seq(0,1,0.01))
  dev.off()     
  
  
  #brier linear model
  #m1.predict
  #teTst$yGTM1
  #https://en.wikipedia.org/wiki/Brier_score
  sd1 <- (m1.predict - teTst$y_GT_M1)^2
  brier1 <- mean(sd1)
  
  
  #probit model
  library(verification)
  
  #mod2 <- verify(obs = teTst$y_GT_M1, pred = m2.predict)
  mod2 <- verify(obs = teTst$y_GT_M1, pred = m2.predict,thresholds=seq(0,1,0.05))
  
  #plot(mod2, CI=TRUE)
  #plot(mod2)
  jpeg("ReliabilityDiagram_probit_model.jpeg")
  plot(mod2, freq=FALSE, main=c("Reliability Diagram for Probit Model\n","All Br and Blos predictors"))
  lines(seq(0,1,0.01),seq(0,1,0.01))
  dev.off()
  
  jpeg("ReliabilityDiagram_withErrorsBars_probit_model.jpeg")
  plot(mod2, freq=FALSE, CI=T, main=c("Reliability Diagram for Probit Model\n","All Br and Blos predictors"))
  lines(seq(0,1,0.01),seq(0,1,0.01))
  dev.off()          
  
  
  #brier probit model
  #m2.predict
  #teTst$yGTM1
  #https://en.wikipedia.org/wiki/Brier_score
  sd2 <- (m2.predict - teTst$y_GT_M1)^2
  brier2 <- mean(sd2)
  
  
  #logit model
  library(verification)
  
  #mod3 <- verify(obs = teTst$y_GT_M1, pred = m3.predict)
  mod3 <- verify(obs = teTst$y_GT_M1, pred = m3.predict,thresholds=seq(0,1,0.05))
  
  #plot(mod3, CI=TRUE)
  #plot(mod3)
  jpeg("ReliabilityDiagram_logit_model.jpeg")
  plot(mod3, freq=FALSE, main=c("Reliability Diagram for Logit Model\n","All Br and Blos predictors"))
  lines(seq(0,1,0.01),seq(0,1,0.01))
  dev.off()
  
  jpeg("ReliabilityDiagram_withErrorsBars_logit_model.jpeg")
  plot(mod3, freq=FALSE, CI=T, main=c("Reliability Diagram for Logit Model\n","All Br and Blos predictors"))
  lines(seq(0,1,0.01),seq(0,1,0.01))
  dev.off()               
  
  
  #brier logit model
  #m3.predict
  #teTst$yGTM1
  #https://en.wikipedia.org/wiki/Brier_score
  sd3 <- (m3.predict - teTst$y_GT_M1)^2
  brier3 <- mean(sd3)
  
  
  #randomForest model
  #mod4 <- verify(obs = teTst$y_GT_M1, pred = p.randomForest.predict.new)
  mod4 <- verify(obs = teTst$y_GT_M1, pred = p.randomForest.predict.new,thresholds=seq(0,1,0.05))
  
  #plot(mod4, CI=TRUE)
  #plot(mod4)
  jpeg("ReliabilityDiagram_randomForest_model.jpeg")
  plot(mod4, freq=FALSE, main=c("Reliability Diagram for random Forest Model\n","All Br and Blos predictors"))
  lines(seq(0,1,0.01),seq(0,1,0.01))
  dev.off()
  
  jpeg("ReliabilityDiagram_withErrorsBars_randomForest_model.jpeg")
  plot(mod4, freq=FALSE, CI=T, main=c("Reliability Diagram for random Forest Model\n","All Br and Blos predictors"))
  lines(seq(0,1,0.01),seq(0,1,0.01))
  dev.off()               
  
  
  #brier randomForest model
  #p.randomForest.predict
  #teTst$yGTM1
  #https://en.wikipedia.org/wiki/Brier_score
  sd4 <- (p.randomForest.predict - teTst$y_GT_M1)^2
  brier4 <- mean(sd4)
  
  
  #SVM model
  library(verification)
  
  #because svm gives probabilities <0 and >1
  #first bound left to 0 and right to 1
  ###p.svm.predict.new <- numeric(length(p.svm.predict))
  ###for (i in 1:length(p.svm.predict)) {
  ###  p.svm.predict.new[i] <- max(0,min(p.svm.predict[i],1))
  ###}
  #mod5 <- verify(obs = teTst$y_GT_M1, pred = p.svm.predict)
  #mod5 <- verify(obs = teTst$y_GT_M1, pred = p.svm.predict,thresholds=seq(0,1,0.05))
  mod5 <- verify(obs = teTst$y_GT_M1, pred = p.svm.predict.num,thresholds=seq(0,1,0.05))
  
  #plot(mod5, CI=TRUE)
  #plot(mod5)
  jpeg("ReliabilityDiagram_SVM_model.jpeg")
  plot(mod5, freq=FALSE, main=c("Reliability Diagram for SVM Model\n","All Br and Blos predictors"))
  lines(seq(0,1,0.01),seq(0,1,0.01))
  dev.off()
  
  
  jpeg("ReliabilityDiagram_withErrorsBars_SVM_model.jpeg")
  plot(mod5, freq=FALSE, CI=T, main=c("Reliability Diagram for SVM Model\n","All Br and Blos predictors"))
  lines(seq(0,1,0.01),seq(0,1,0.01))
  dev.off()               
  
  
  #brier svm model
  #p.svm.predict
  #teTst$yGTM1
  #https://en.wikipedia.org/wiki/Brier_score
  sd5 <- (p.svm.predict.num - teTst$y_GT_M1)^2
  brier5 <- mean(sd5)
  
  
  #brier score update
  brierScores <- c(brier0,brier1,brier2,brier3,brier4,brier5)
  
  #now BSS: brier SKILL scores
  BSref <- mean((sum(teTst$y_GT_M1) / length(teTst$y_GT_M1) - teTst$y_GT_M1)^2)   
  
  brierSS0 <- 1 - (brier0/BSref)
  brierSS1 <- 1 - (brier1/BSref)
  brierSS2 <- 1 - (brier2/BSref)
  brierSS3 <- 1 - (brier3/BSref)
  brierSS4 <- 1 - (brier4/BSref)
  brierSS5 <- 1 - (brier5/BSref)
  
  brierSkillScores <- c(brierSS0,brierSS1,brierSS2,brierSS3,brierSS4,brierSS5)
  
  
  #} ### iNode loop
  #} ### tries loop
  
  
  #return(cbind(res0,res1,res2,res3,res4,res5))
  resObj <- list(mat=matrix(NA,101,30),brierScores=numeric(6),brierSkillScores=numeric(6),AUCvalues=numeric(6))
  resObj$mat <- cbind(res0,res1,res2,res3,res4,res5)
  resObj$brierScores <- brierScores
  resObj$brierSkillScores <- brierSkillScores
  resObj$AUCvalues <- AUCvalues
  return(resObj)
  #return(cbind(res0,res1,res2,res3,res4,res5))      
}


calc_SDs <- function(res) {
  
  n1 <- dim(res[[1]])[1]
  n2 <- dim(res[[1]])[2]
  m  <- length(res)
  values <- numeric(m)
  
  myStds <- matrix(NA,n1,n2)
  
  for (i in 1:n1) {
    for (j in 1:n2) {
      values <- numeric(m)
      for (k in 1:m) {
        values[k] <- res[[k]][i,j]
      }
      myStds[i,j] <- sd(values)
    }
  }
  return(myStds) 
}

evaluateScore <- function(betas,X,y) {
  
  n <- length(y)
  p <- dim(X)[2]
  y <- -1+2*y
  firep <- numeric(n)
  sump  <- numeric(n)
  score <-0
  for (i in 1:n) {
    for (j in 1:p) {
      sump[i] <- sump[i] + X[i,j]*betas[j]
    }
    
    if (sump[i] > 0) {
      firep[i]=+1
    }
    if (sump[i] < 0) {
      firep[i]=-1
    }
    if (sump[i] == 0) {
      firep[i]=y[i]
    }
    score=score+firep[i] * y[i]
  }
  score <- score/n
  return(score)  
}  


makePlots1 <- function(teTst) {
  
  n=dim(teTst)[1]
  plot(0,0,ylab="y",xlab="x",main="title",xlim=c(-5,5),ylim=c(-5,5))
  for (i in 1:n) {
    x=te$DI[i]
    y=te$Beff[i]
    #if (as.numeric(teTst$yIN_C1_C9[i])==0) { points(x,y,col="blue") }
    if (as.numeric(teTst$yIN_C1_C9[i])==1) { points(x,y,col="red") }
  }
  
}


makePlots2 <- function(teTst) {
  
  n=dim(teTst)[1]
  plot(0,0,ylab="y",xlab="x",main="title",xlim=c(-5,5),ylim=c(-5,5))
  for (i in 1:n) {
    x=te$DI[i]
    y=te$NNC[i]
    #if (as.numeric(teTst$yIN_C1_C9[i])==0) { points(x,y,col="blue") }
    if (as.numeric(teTst$yIN_C1_C9[i])==1) { points(x,y,col="red") }
  }
  
}


makePlots3 <- function(teTst) {
  
  n=dim(teTst)[1]
  plot(0,0,ylab="y",xlab="x",main="title",xlim=c(-5,5),ylim=c(-5,5))
  for (i in 1:n) {
    x=te$Beff[i]
    y=te$NNC[i]
    #if (as.numeric(teTst$yIN_C1_C9[i])==0) { points(x,y,col="blue") }
    if (as.numeric(teTst$yIN_C1_C9[i])==1) { points(x,y,col="red") }
  }
  
}


colIndexMaxs <- function(out) {
  
  ncol=dim(out)[2]
  nrow=dim(out)[1]-1
  indices <- numeric(ncol)
  
  
  for (j in 1:ncol) {
    indices[j]=which.max(out[,j])
    indices[j]=indices[j] /nrow
  }
  
  thresholdsWhereMax <- indices
  return(thresholdsWhereMax)
}



