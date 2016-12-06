
evaluateModels <- function(fname,m) {
  
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
  dataMatALL <- matrix(NA,0,19)
  cp=0
  
  #fname <- paste("data_neural_C",".txt",sep="")
  data <- scan(file=fname,what=numeric())
  dataMat <- matrix(data,nrow=length(data)/19,ncol=19,byrow=T)
  
  
  dataFram <- as.data.frame(dataMat[,1:17])
  
  dataFram <- dataFram[complete.cases(dataFram),]
  
  
  
  #names(dataFram) <- c("TUMagneticFlux","Hel1","Hel2","Hel3","Hel4","Hel5","Hel6",
  #                     "Hel7","Hel8","Hel9","Hel10","Hel11","Hel12",
  #                     "FreeEn1","FreeEn2","FreeEn3","FreeEn4","FreeEn5","FreeEn6",
  #                     "FreeEn7","FreeEn8","FreeEn9","FreeEn10","FreeEn11","FreeEn12",
  #                     "TotalNoMagneticNullPoints","MFluxNearMPolarityILines","yGTM1","yIN-C1-C9")
  #names(dataFram) <- c("BEff","IsingEn1","IsingEn2","ImbalanceRatio","GlobalNonNeutralityF","NetNonNeutralizedC",
  #                     "yGTM1")
  names(dataFram) <- c("logRBl","logRBr","FSPIBl","FSPIBr","TLMPILBl","TLMPILBr","DIBl","DIBr","BeffBl","BeffBr","IsinEn1Bl","IsinEn1Br","IsinEn2Bl","IsinEn2Br","NNC",
                       "yGTM1","yIN_C1_C9")
  
  names(dataMat) <- c("logRBl","logRBr","FSPIBl","FSPIBr","TLMPILBl","TLMPILBr","DIBl","DIBr","BeffBl","BeffBr","IsinEn1Bl","IsinEn1Br","IsinEn2Bl","IsinEn2Br","NNC",
                      "yGTM1","yIN_C1_C9")
  
  
  dataMatf <- as.matrix(dataFram)
  
  
  #dependent variables
  
  y_M1_flare <- y_C1_C9_flare <- numeric(dim(dataMatf)[1])
  for (i in 1:dim(dataMatf)[1]) {
    #y_M1_flare[i]    <- as.numeric(dataMatf[i,16] )
     y_C1_C9_flare[i] <- as.numeric(dataMatf[i,17] )
  }
  
  
  #now proceed to nnet
  
  #M class flare
  #y<- y_M1_flare
  #C class flare
  y<- y_C1_C9_flare
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
  
  te$logRBl <-     te$logRBl / 1
  te$logRBr <-     te$logRBr / 1
  te$FSPIBl <-     te$FSPIBl / 1
  te$FSPIBr <-     te$FSPIBr / 1
  te$TLMPILBl <-     te$TLMPILBl / 1
  te$TLMPILBr <-     te$TLMPILBr / 1
  te$DIBl     <-     te$DIBl / 1
  te$DIBr     <-     te$DIBr / 1
  te$BeffBl   <-     te$BeffBl / 1
  te$BeffBr   <-     te$BeffBr / 1
  te$IsinEn1Bl <-     te$IsinEn1Bl / 1
  te$IsinEn1Br <-     te$IsinEn1Br / 1
  te$IsinEn2Bl <-     te$IsinEn2Bl / 1
  te$IsinEn2Br <-     te$IsinEn2Br / 1
  te$NNC    <-     te$NNC / 1
  
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

  for (j in 1:(dim(te)[2]-2)){
    for (i in 1:dim(te)[1]) {
      te[i,j] <- (te[i,j] - means[j]) / sds[j]
    }
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
  m1 <- lm( yIN_C1_C9 ~ logRBl+logRBr+FSPIBl+FSPIBr+TLMPILBl+TLMPILBr+
                          DIBl+DIBr+BeffBl+BeffBr+IsinEn1Bl+IsinEn1Br+IsinEn2Bl+IsinEn2Br+NNC,data=te)
  
  summary(m1)
  

  jpeg(paste("linear_model_combined_SHYKJG_1st_trn_orig_split",".jpeg",sep=""))
  plot(te$yIN_C1_C9,xlab="observation",ylab="value of y",main=paste("lm model training original split",sep=""))
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
  mychar[2]  <- "$\\logRBl$"
  mychar[3]  <- "$\\logRBr$"  
  mychar[4]  <- "$\\FSPIBl$"
  mychar[5]  <- "$\\FSPIBr$"
  mychar[6] <- "$\\TLMPILBl$"
  mychar[7] <- "$\\TLMPILBr$"
  mychar[8] <- "$\\DIBl$"
  mychar[9] <- "$\\DIBr$"
  mychar[10] <- "$\\BeffBl$"
  mychar[11] <- "$\\BeffBr$"
  mychar[12] <- "$\\IsinEn1Bl$"
  mychar[13] <- "$\\IsinEn1Br$"
  mychar[14] <- "$\\IsinEn2Bl$"
  mychar[15] <- "$\\IsinEn2Br$"
  mychar[16] <- "$\\NNC$"
  
  outD <- cbind("Param" = rep(paste(mychar[1:16],sep = " "), len = nrow(outD)), outD)
  
  cap <- paste("Linear model original split", "training results", sep = " ")
  print(xtable(outD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
        sanitize.text.function = function(x) x)
  print(xtable(outD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
        sanitize.text.function = function(x) x, file=paste("linear_model_original_split","_train.tex",sep=""))
  #end xtable
  
  #glm
  #m2 <- glm( y ~ sunspot_Area + dMW2 + dMW3+ dMW4+ dMW5+ dMW6+ dMW7, data=te,family=binomial(probit))
  m2 <- glm( yIN_C1_C9 ~ logRBl+logRBr+FSPIBl+FSPIBr+TLMPILBl+TLMPILBr+
                          DIBl+DIBr+BeffBl+BeffBr+IsinEn1Bl+IsinEn1Br+IsinEn2Bl+IsinEn2Br+NNC,data=te,family=binomial(probit))
  
  summary(m2)
  
  jpeg(paste("probit_model_combined_SHYKJG_1st_trn_orig_split",".jpeg",sep=""))
  plot(te$yIN_C1_C9,xlab="observation",ylab="value of y",main=paste("glm training with probit link original split ",sep=""))
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
  mychar[2]  <- "$\\logRBl$"
  mychar[3]  <- "$\\logRBr$"  
  mychar[4]  <- "$\\FSPIBl$"
  mychar[5]  <- "$\\FSPIBr$"
  mychar[6] <- "$\\TLMPILBl$"
  mychar[7] <- "$\\TLMPILBr$"
  mychar[8] <- "$\\DIBl$"
  mychar[9] <- "$\\DIBr$"
  mychar[10] <- "$\\BeffBl$"
  mychar[11] <- "$\\BeffBr$"
  mychar[12] <- "$\\IsinEn1Bl$"
  mychar[13] <- "$\\IsinEn1Br$"
  mychar[14] <- "$\\IsinEn2Bl$"
  mychar[15] <- "$\\IsinEn2Br$"
  mychar[16] <- "$\\NNC$"
  
  
  outD <- cbind("Param" = rep(paste(mychar[1:16],sep = " "), len = nrow(outD)), outD)
  
  cap <- paste("Probit model original split", "training results", sep = " ")
  print(xtable(outD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
        sanitize.text.function = function(x) x)
  print(xtable(outD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
        sanitize.text.function = function(x) x, file=paste("probit_model_original_split","_train.tex",sep=""))
  #end xtable
  
  
  #glm
  #m3 <- glm( y ~ sunspot_Area + dMW2 + dMW3+ dMW4+ dMW5+ dMW6+ dMW7, data=te,family=binomial(logit))
  
  m3 <- glm( yIN_C1_C9 ~ logRBl+logRBr+FSPIBl+FSPIBr+TLMPILBl+TLMPILBr+
                           DIBl+DIBr+BeffBl+BeffBr+IsinEn1Bl+IsinEn1Br+IsinEn2Bl+IsinEn2Br+NNC,data=te,family=binomial(logit))
  
  summary(m3)
  
  
  jpeg(paste("logit_model_combined_SHYKJG_1st_trn_orig_split",".jpeg",sep=""))
  plot(te$yIN_C1_C9,xlab="observation",ylab="value of y",main=paste("glm training with logit link original split ",sep=""))
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
  mychar[2]  <- "$\\logRBl$"
  mychar[3]  <- "$\\logRBr$"  
  mychar[4]  <- "$\\FSPIBl$"
  mychar[5]  <- "$\\FSPIBr$"
  mychar[6] <- "$\\TLMPILBl$"
  mychar[7] <- "$\\TLMPILBr$"
  mychar[8] <- "$\\DIBl$"
  mychar[9] <- "$\\DIBr$"
  mychar[10] <- "$\\BeffBl$"
  mychar[11] <- "$\\BeffBr$"
  mychar[12] <- "$\\IsinEn1Bl$"
  mychar[13] <- "$\\IsinEn1Br$"
  mychar[14] <- "$\\IsinEn2Bl$"
  mychar[15] <- "$\\IsinEn2Br$"
  mychar[16] <- "$\\NNC$"
  
  
  outD <- cbind("Param" = rep(paste(mychar[1:16],sep = " "), len = nrow(outD)), outD)
  
  cap <- paste("Logit model original split", "training results", sep = " ")
  print(xtable(outD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
        sanitize.text.function = function(x) x)
  print(xtable(outD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
        sanitize.text.function = function(x) x, file=paste("logit_model_original_split","_train.tex",sep=""))
  #end xtable
  
  
  #bayesQR
  require(bayesQR)    
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
  
  #randomForest
  require(randomForest)
  teRF <- rbind(te,teTst)
  trainRF <- 1:dim(te)[1]
  p.randomForest <- randomForest(yIN_C1_C9 ~ logRBl+logRBr+FSPIBl+FSPIBr+TLMPILBl+TLMPILBr+
                                               DIBl+DIBr+BeffBl+BeffBr+IsinEn1Bl+IsinEn1Br+IsinEn2Bl+IsinEn2Br+NNC, data=teRF, subset = trainRF,
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
  
  #t.svm <- tune.svm(cbind(te$logRBl,te$logRBr,te$FSPIBl,te$FSPIBr,te$TLMPILBl,te$TLMPILBr,
  #                        te$DIBl,te$DIBr,te$BeffBl,te$BeffBr,te$IsinEn1Bl,te$IsinEn1Br,te$IsinEn2Bl,te$IsinEn2Br,te$NNC),
  #                  te$yIN_C1_C9,gamma = 2^(-3:0), cost = 2^(0:3), probability=TRUE)

  #p.svm <- t.svm$best.model
  
  #t.svm <- tune.svm(cbind(te$logRBl,te$FSPIBl,te$TLMPILBl,te$DIBl,te$BeffBl,te$IsinEn1Bl,te$IsinEn2Bl,te$NNC),
  #                  te$yIN_C1_C9,gamma = 0.125, cost = 1, probability=TRUE)
  
    
  #p.svm <- t.svm$best.model
  
  #p.svm <- svm(cbind(te$logRBl,te$FSPIBl,te$TLMPILBl,te$DIBl,te$BeffBl,te$IsinEn1Bl,te$IsinEn2Bl,te$NNC),
  #             te$yIN_C1_C9,gamma = 0.125, cost = 1, probability=TRUE)

  p.svm <- svm(cbind(te$logRBl,te$logRBr,te$FSPIBl,te$FSPIBr,te$TLMPILBl,te$TLMPILBr,
                    te$DIBl,te$DIBr,te$BeffBl,te$BeffBr,te$IsinEn1Bl,te$IsinEn1Br,te$IsinEn2Bl,te$IsinEn2Br,te$NNC),
                    te$yIN_C1_C9,gamma = 0.125, cost = 1, probability=TRUE)
  
  
  jpeg(paste("SVM_model_combined_SHYKJG_trn_orig_split",".jpeg",sep=""))
  plot(te$yIN_C1_C9,xlab="observation",ylab="value of y",main=paste("SVM with probability option, combined regressors\n", "Training, Original Split",sep=" "))
  lines(p.svm$fitted, col=6)
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

      p.nnet <- nnet(cbind(te$logRBl,te$logRBr,te$FSPIBl,te$FSPIBr,te$TLMPILBl,te$TLMPILBr,
                           te$DIBl,te$DIBr,te$BeffBl,te$BeffBr,te$IsinEn1Bl,te$IsinEn1Br,te$IsinEn2Bl,te$IsinEn2Br,te$NNC),
                     te$yIN_C1_C9, size = 4, decay = 10^(-1),
                     entropy=T,maxit=2000,MaxNWts=2000  )
      
      p <- p.nnet
      jpeg(paste("MLP_model_combined_SHYKJG_trn_orig_split",".jpeg",sep=""))
      plot(te$yIN_C1_C9,xlab="observation",ylab="value of y",main=paste("MLP with entropy option, combined regressors\n", "Training, Original Split",sep=" "))
      lines(p$fitted.values,col=5)
      dev.off()
      
      save.image(paste("mlp_object_p_","orig_split",".RData",sep=""))
      ###  }
      ###}
      
      #predict with nnet
      #newdata:
      #teTst
      
      
      #safe mode
      yTst <- teTst$yIN_C1_C9
      teTst$yIN_C1_C9 <- -999
      
      #p.nnet.predict <- predict(p.nnet,newdata=teTst[,-c(9,10)])
      p.nnet.predict <- predict(p.nnet,newdata=teTst[,-c(16,17)])
      
      #restore
      teTst$yIN_C1_C9 = yTst
      
      jpeg(paste("MLP_model_combined_SHYKJG_testing_orig_split",".jpeg",sep=""))
      plot(teTst$yIN_C1_C9,xlab="observation",ylab="value of y",main=paste("MLP with entropy option, combined regressors,\n",
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
      accuracy4_all <- numeric(101)   # randomForest is "4"
      tss4_all      <- numeric(101)
      hss4_all      <- numeric(101)
      accuracy5_all <- numeric(101)   # SVM is "5"
      tss5_all      <- numeric(101)
      hss5_all      <- numeric(101)
      
      
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
        
        cfmat <- table(p.nnet.predict.01, teTst$yIN_C1_C9)
        
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
        
        jpeg(paste("linear_model_model_SHYKJG_testing_Orig_Split",".jpeg",sep=""))
        plot(teTst$yIN_C1_C9,xlab="observation",ylab="value of y",main=paste("linear model, combined regressors,\n",
                                                                         "Testing, Original Split",sep=" "))
        
        lines(m1.predict,col=2)
        dev.off()
        
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
        
        cfmat <- table(m1.predict.01, teTst$yIN_C1_C9)
        
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
        
        
        jpeg(paste("probit_model_model_SHYKJG_testing_orig_split",".jpeg",sep=""))
        plot(teTst$yIN_C1_C9,xlab="observation",ylab="value of y",main=paste("probit model, combined regressors,\n",
                                                                         "Testing, Original Split",sep=" "))
        lines(m2.predict,col=3)
        dev.off()
        
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
        
        cfmat <- table(m2.predict.01, teTst$yIN_C1_C9)
        
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
        
        jpeg(paste("logit_model_model_SHYKJG_testing_orig_split",".jpeg",sep=""))
        plot(teTst$yIN_C1_C9,xlab="observation",ylab="value of y",main=paste("logit model, combined regressors","\n",
                                                                         "Testing, Original Split",sep=" "))
        
        lines(m3.predict,col=4)
        dev.off()
        
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
        
        cfmat <- table(m3.predict.01, teTst$yIN_C1_C9)
        
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
        p.randomForest.predict <- predict(p.randomForest,newdata=teTst[,-c(16,17)],type="response")

        jpeg(paste("randomForest_model_model_SHYKJG_testing_orig_split",".jpeg",sep=""))
        plot(teTst$yIN_C1_C9,xlab="observation",ylab="value of y",main=paste("random Forest model, combined regressors","\n",
                                                                         "Testing, Original Split",sep=" "))
        
        lines(p.randomForest.predict,col=5)
        dev.off()

        
        #because randomForest gives probabilities <0 and >1
        #first bound left to 0 and right to 1
        p.randomForest.predict.new <- numeric(length(p.randomForest.predict))
        for (i in 1:length(p.randomForest.predict)) {
          p.randomForest.predict.new[i] <- max(0,min(p.randomForest.predict[i],1))
        }
        
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
        
        cfmat <- table(p.randomForest.predict.01, teTst$yIN_C1_C9)
        
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
        
                
        #predict svm
        ###p.svm.predict <- predict(p.svm, newdata=teTst[,-c(28,29)])
        #p.svm.predict <- predict(p.svm, newdata=teTst[,-c(9,10)])
        p.svm.predict <- predict(p.svm, newdata=teTst[,-c(16,17)])
        jpeg(paste("SVM_model_model_SHYKJG_testing_orig_split",".jpeg",sep=""))
        plot(teTst$yIN_C1_C9,xlab="observation",ylab="value of y",main=paste("SVM model, combined regressors","\n",
                                                                         "Testing, Original Split",sep=" "))
        
        lines(p.svm.predict,col=6)
        dev.off()
        
        #because svm gives probabilities <0 and >1
        #first bound left to 0 and right to 1
        p.svm.predict.new <- numeric(length(p.svm.predict))
        for (i in 1:length(p.svm.predict)) {
          p.svm.predict.new[i] <- max(0,min(p.svm.predict[i],1))
        }
        
        p.svm.predict.01 <- numeric(length(p.svm.predict.new))
        #thresHOLD <- 0.50
        #thresHOLD <- 0.25
        
        for (i in 1:length(p.svm.predict.new)) {
          response=0
          if (!is.na(p.svm.predict.new[i])) {
            if (p.svm.predict.new[i] > thresHOLD) {
              response=1
            } 
          }
          p.svm.predict.01[i] <- response
        }
        
        cfmat <- table(p.svm.predict.01, teTst$yIN_C1_C9)
        
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
          
          accuracy5_all[ccp] <- accuracy
          tss5_all[ccp] <- trueSS
          hss5_all[ccp] <- HeidkeSS
          
          
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
      write.table(cbind(1:101,seq(0,1,0.01),accuracy4_all,tss4_all,hss4_all),file=paste("randomForest_SKILLS_testing_orig_split_",".out",sep=""),row.names=FALSE,col.names=FALSE)
      #SVM
      write.table(cbind(1:101,seq(0,1,0.01),accuracy5_all,tss5_all,hss5_all),file=paste("SVM_SKILLS_testing_orig_split_",".out",sep=""),row.names=FALSE,col.names=FALSE)
      
      res0 <- cbind(1:101,seq(0,1,0.01),accuracy0_all,tss0_all,hss0_all)
      
      
      jpeg(paste("mlp_model_SHYKJG_testing_orig_split_","_SKILLS_",".jpeg",sep=""))
      plot(res0[,2],res0[,3],type="l",col=2,lwd=2.5,xlab="probability threshold",ylab="skills values",main=paste("skills wrt threshold for mlp\n",
                                                                                                                 "testing, original split",sep=" "))
      lines(res0[,2],res0[,4],type="l",lwd=2.5,col=3)
      lines(res0[,2],res0[,5],type="l",lwd=2.5,col=4)    
      
      legend(0.7,0.6, # places a legend at the appropriate place 
             c("acc","tss","hss"), # puts text in the legend
             lty=c(1,1,1), # gives the legend appropriate symbols (lines)
             lwd=c(2.5,2.5,2.5),col=c(2,3,4)) # gives the legend lines the correct color and width
      
      dev.off()
      
      
      
      res1 <- cbind(1:101,seq(0,1,0.01),accuracy1_all,tss1_all,hss1_all)
      
      
      jpeg(paste("lm_model_SHYKJG_testing_orig_split_","_SKILLS_",".jpeg",sep=""))
      plot(res1[,2],res1[,3],type="l",col=2,lwd=2.5,xlab="probability threshold",ylab="skills values",main=paste("skills wrt threshold for linear model\n",
                                                                                                                 "testing, original split",sep=" "),
                                                                                                      ylim=c(0,1))
      lines(res1[,2],res1[,4],type="l",lwd=2.5,col=3)
      lines(res1[,2],res1[,5],type="l",lwd=2.5,col=4)    
      
      legend(0.7,0.6, # places a legend at the appropriate place 
             c("acc","tss","hss"), # puts text in the legend
             lty=c(1,1,1), # gives the legend appropriate symbols (lines)
             lwd=c(2.5,2.5,2.5),col=c(2,3,4)) # gives the legend lines the correct color and width
      
      dev.off()
      
      
      
      res2 <- cbind(1:101,seq(0,1,0.01),accuracy2_all,tss2_all,hss2_all)
      
      jpeg(paste("probit_model_SHYKJG_testing_orig_split_","_SKILLS_",".jpeg",sep=""))
      plot(res2[,2],res2[,3],type="l",col=2,lwd=2.5,xlab="probability threshold",ylab="skills values",xlim=c(0,1),ylim=c(0,1),
           main=paste("skills wrt threshold for probit\n",
                      "testing, original split",sep=" "))
      lines(res2[,2],res2[,4],type="l",lwd=2.5,col=3)
      lines(res2[,2],res2[,5],type="l",lwd=2.5,col=4)    
      
      legend(0.7,0.6, # places a legend at the appropriate place 
             c("acc","tss","hss"), # puts text in the legend
             lty=c(1,1,1), # gives the legend appropriate symbols (lines)
             lwd=c(2.5,2.5,2.5),col=c(2,3,4)) # gives the legend lines the correct color and width
      
      dev.off()
      
      
      
      res3 <- cbind(1:101,seq(0,1,0.01),accuracy3_all,tss3_all,hss3_all)
      
      jpeg(paste("logit_model_SHYKJG_testing_orig_split_","_SKILLS_",".jpeg",sep=""))    
      plot(res3[,2],res3[,3],type="l",col=2,lwd=2.5,xlab="probability threshold",ylab="skills values",xlim=c(0,1),ylim=c(0,1),
           main=paste("skills wrt threshold for logit\n",
                      "testing, original split",sep=" "))
      lines(res3[,2],res3[,4],type="l",lwd=2.5,col=3)
      lines(res3[,2],res3[,5],type="l",lwd=2.5,col=4)    
      
      legend(0.7,0.6, # places a legend at the appropriate place 
             c("acc","tss","hss"), # puts text in the legend
             lty=c(1,1,1), # gives the legend appropriate symbols (lines)
             lwd=c(2.5,2.5,2.5),col=c(2,3,4)) # gives the legend lines the correct color and width
      dev.off()
      
      res4 <- cbind(1:101,seq(0,1,0.01),accuracy4_all,tss4_all,hss4_all)
      jpeg(paste("randomForest_model_SHYKJG_testing_orig_split_","_SKILLS_",".jpeg",sep=""))    
      plot(res4[,2],res4[,3],type="l",col=2,lwd=2.5,xlab="probability threshold",ylab="skills values",xlim=c(0,1),ylim=c(0,1),
           main=paste("skills wrt threshold for random Forest\n",
                      "testing, original split",sep=" "))
      lines(res4[,2],res4[,4],type="l",lwd=2.5,col=3)
      lines(res4[,2],res4[,5],type="l",lwd=2.5,col=4)    
      
      legend(0.7,0.6, # places a legend at the appropriate place 
             c("acc","tss","hss"), # puts text in the legend
             lty=c(1,1,1), # gives the legend appropriate symbols (lines)
             lwd=c(2.5,2.5,2.5),col=c(2,3,4)) # gives the legend lines the correct color and width
      dev.off()
      
      
      res5 <- cbind(1:101,seq(0,1,0.01),accuracy5_all,tss5_all,hss5_all)
      
      jpeg(paste("SVM_model_SHYKJG_testing_orig_split_","_SKILLS_",".jpeg",sep=""))    
      plot(res5[,2],res5[,3],type="l",col=2,lwd=2.5,xlab="probability threshold",ylab="skills values",xlim=c(0,1),ylim=c(0,1),
           main=paste("skills wrt threshold for SVM\n",
                      "testing, original split",sep=" "))
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
      pred <- prediction(p.nnet.predict,teTst$yIN_C1_C9)
      perf <- performance(pred,"tpr","fpr")
      plot(perf,main=c("ROC curve for Neural Network Model","\n","All Br and Blos predictors")) 
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()
      
      auc.perf = performance(pred, measure = "auc")
      auc.perf@y.values
      
      
      #linear model
      jpeg("ROC_linear_model.jpeg")
      pred <- prediction(m1.predict,teTst$yIN_C1_C9)
      perf <- performance(pred,"tpr","fpr")
      plot(perf,main=c("ROC curve for Linear Model","\n","All Br and Blos predictors")) 
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()
      
      auc.perf = performance(pred, measure = "auc")
      auc.perf@y.values
      
      
      #probit model
      jpeg("ROC_probit_model.jpeg")
      pred2 <- prediction(m2.predict,teTst$yIN_C1_C9)
      perf2 <- performance(pred2,"tpr","fpr")
      plot(perf2,main=c("ROC curve for Probit Model","\n","All Br and Blos predictors")) 
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()  
      
      auc.perf2 = performance(pred2, measure = "auc")
      auc.perf2@y.values
      
      
      
      #logit model
      jpeg("ROC_logit_model.jpeg")
      pred3 <- prediction(m3.predict,teTst$yIN_C1_C9)
      perf3 <- performance(pred3,"tpr","fpr")
      plot(perf3,main=c("ROC curve for Logit Model","\n","All Br and Blos predictors")) 
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()  
      
      auc.perf3 = performance(pred3, measure = "auc")
      auc.perf3@y.values     
      
      
      #randomForest model
      jpeg("ROC_randomForest_model.jpeg")
      pred4 <- prediction(p.randomForest.predict.new,teTst$yIN_C1_C9)
      perf4 <- performance(pred4,"tpr","fpr")
      plot(perf4,main=c("ROC curve for random Forest Model","\n","All Br and Blos predictors")) 
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()     
      
      auc.perf4 = performance(pred4, measure = "auc")
      auc.perf4@y.values     
      
      
      #SVM model
      jpeg("ROC_SVM_model.jpeg")
      pred5 <- prediction(p.svm.predict.new,teTst$yIN_C1_C9)
      perf5 <- performance(pred5,"tpr","fpr")
      plot(perf5,main=c("ROC curve for SVM Model","\n","All Br and Blos predictors")) 
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()     
      
      auc.perf5 = performance(pred5, measure = "auc")
      auc.perf5@y.values     
      
      
      #Reliability Diagrams
      
      #neural network model
      library(verification)
      
      mod0 <- verify(obs = teTst$yIN_C1_C9, pred = p.nnet.predict,thresholds=seq(0,1,0.05))
      
      #plot(mod1, CI=TRUE)
      #plot(mod1)
      jpeg(paste("ReliabilityDiagram_Neural_Network_model",".jpeg",sep=""))
      plot(mod0, main=c("Reliability Diagram for Neural Network Model","\n","All Br and Blos predictors"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()
      
      jpeg(paste("ReliabilityDiagram_withErrorsBars_Neural_Network_model",".jpeg",sep=""))
      plot(mod0, CI=T, main=c("Reliability Diagram for Neural Network Model","\n","All Br and Blos predictors"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()     
      
      
      #linear model
      library(verification)
      
      #because lm gives probabilities <0 and >1
      #first bound left to 0 and right to 1
      m1.predict.new <- numeric(length(m1.predict))
      for (i in 1:length(m1.predict)) {
        m1.predict.new[i] <- max(0,min(m1.predict[i],1))
      }
      #mod1 <- verify(obs = teTst$yIN_C1_C9, pred = m1.predict.new)
      mod1 <- verify(obs = teTst$yIN_C1_C9, pred = m1.predict.new,thresholds=seq(0,1,0.05))
      
      
      #plot(mod1, CI=TRUE)
      #plot(mod1)
      jpeg("ReliabilityDiagram_linear_model.jpeg")
      plot(mod1, main=c("Reliability Diagram for Linear Model","\n","All Br and Blos predictors"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()
      
      jpeg("ReliabilityDiagram_withErrorsBars_linear_model.jpeg")
      plot(mod1, CI=T, main=c("Reliability Diagram for Linear Model","\n","All Br and Blos predictors"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()     
      
      
      
      #probit model
      library(verification)
      
      #mod2 <- verify(obs = teTst$yIN_C1_C9, pred = m2.predict)
      mod2 <- verify(obs = teTst$yIN_C1_C9, pred = m2.predict,thresholds=seq(0,1,0.05))
      
      #plot(mod2, CI=TRUE)
      #plot(mod2)
      jpeg("ReliabilityDiagram_probit_model.jpeg")
      plot(mod2, main=c("Reliability Diagram for Probit Model","\n","All Br and Blos predictors"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()
      
      jpeg("ReliabilityDiagram_withErrorsBars_probit_model.jpeg")
      plot(mod2, CI=T, main=c("Reliability Diagram for Probit Model","\n","All Br and Blos predictors"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()          
      
      
      
      #logit model
      library(verification)
      
      #mod3 <- verify(obs = teTst$yIN_C1_C9, pred = m3.predict)
      mod3 <- verify(obs = teTst$yIN_C1_C9, pred = m3.predict,thresholds=seq(0,1,0.05))
      
      #plot(mod3, CI=TRUE)
      #plot(mod3)
      jpeg("ReliabilityDiagram_logit_model.jpeg")
      plot(mod3, main=c("Reliability Diagram for Logit Model","\n","All Br and Blos predictors"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()
      
      jpeg("ReliabilityDiagram_withErrorsBars_logit_model.jpeg")
      plot(mod3, CI=T, main=c("Reliability Diagram for Logit Model","\n","All Br and Blos predictors"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()               
      
      
      
      #randomForest model
      #mod4 <- verify(obs = teTst$yIN_C1_C9, pred = p.randomForest.predict.new)
      mod4 <- verify(obs = teTst$yIN_C1_C9, pred = p.randomForest.predict.new,thresholds=seq(0,1,0.05))
      
      #plot(mod4, CI=TRUE)
      #plot(mod4)
      jpeg("ReliabilityDiagram_randomForest_model.jpeg")
      plot(mod4, main=c("Reliability Diagram for random Forest Model","\n","All Br and Blos predictors"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()
      
      jpeg("ReliabilityDiagram_withErrorsBars_randomForest_model.jpeg")
      plot(mod4, CI=T, main=c("Reliability Diagram for random Forest Model","\n","All Br and Blos predictors"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()               
      
      
      #SVM model
      library(verification)
      
      #because svm gives probabilities <0 and >1
      #first bound left to 0 and right to 1
      p.svm.predict.new <- numeric(length(p.svm.predict))
      for (i in 1:length(p.svm.predict)) {
        p.svm.predict.new[i] <- max(0,min(p.svm.predict[i],1))
      }
      #mod5 <- verify(obs = teTst$yIN_C1_C9, pred = p.svm.predict)
      #mod5 <- verify(obs = teTst$yIN_C1_C9, pred = p.svm.predict,thresholds=seq(0,1,0.05))
      mod5 <- verify(obs = teTst$yIN_C1_C9, pred = p.svm.predict.new,thresholds=seq(0,1,0.05))
      
      #plot(mod5, CI=TRUE)
      #plot(mod5)
      jpeg("ReliabilityDiagram_SVM_model.jpeg")
      plot(mod5, main=c("Reliability Diagram for SVM Model","\n","All Br and Blos predictors"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()
      
      
      jpeg("ReliabilityDiagram_withErrorsBars_SVM_model.jpeg")
      plot(mod5, CI=T, main=c("Reliability Diagram for SVM Model","\n","All Br and Blos predictors"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()               
      
    #} ### iNode loop
  #} ### tries loop
  

  return(cbind(res0,res1,res2,res3,res4,res5))
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



