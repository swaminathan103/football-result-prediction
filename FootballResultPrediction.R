trset<-read.csv("Final_Train.csv")
testset<-read.csv("Final_Test.csv")


te<-testset$FTR


data<- trset
str(data)

data1 <- testset
str(data1)


sum_last_few <- function(data,w){
  if(length(data) >= w){
    ptail <- tail(data,w)
    sum(ptail)
  }else{
    return(0)
  }
  
}


l6Total <- function(CURRENT, PREV6, feat){ 
  
  sapply(seq_along(data[[feat]]), function(x) sum_last_few( subset(data[1:(x-1),] , ((eval(parse(text = PREV6)) == data[x,CURRENT]) | (eval(parse(text = 'AwayTeam')) == data[x,CURRENT]) ))[[feat]],6))
  
  

}


l6TotalAway <- function(CURRENT, PREV6, feat){ 
  sapply(seq_along(data[[feat]]), function(x) sum_last_few( subset(data[1:(x-1),] , ((eval(parse(text = PREV6)) == data[x,CURRENT]) | (eval(parse(text = 'HomeTeam')) == data[x,CURRENT]) ))[[feat]],6))
  
  
}



sum_last_few_test <- function(data1,w){
  if(length(data1) >= w){
    ptail <- tail(data1,w)
    sum(ptail)
  }else{
    return(0)
  }
  
}

l6Total_test <- function(CURRENT, PREV6, feat){ 
  sapply(seq_along(data1[[feat]]), function(x) sum_last_few_test( subset(data1[1:(x-1),] , ((eval(parse(text = PREV6)) == data1[x,CURRENT]) | (eval(parse(text = 'AwayTeam')) == data1[x,CURRENT]) ))[[feat]],6))
  
  
}


l6TotalAway_test <- function(CURRENT, PREV6, feat){ 
  sapply(seq_along(data1[[feat]]), function(x) sum_last_few_test( subset(data1[1:(x-1),] , ((eval(parse(text = PREV6)) == data1[x,CURRENT]) | (eval(parse(text = 'HomeTeam')) == data1[x,CURRENT]) ))[[feat]],6))
  
  
}


temp<-trset

phsot <- l6Total('HomeTeam',"HomeTeam",'HST')  
phs <- l6Total('HomeTeam',"HomeTeam",'HS')  
phf<- l6Total('HomeTeam',"HomeTeam",'HF')  
phc<- l6Total('HomeTeam',"HomeTeam",'HC')  
phgf<- l6Total('HomeTeam' , 'HomeTeam' , 'FTHG')
phga<- l6Total('HomeTeam' , 'HomeTeam' , 'FTAG')

hgd <- phgf - phga

temp["PHST"] <- NA
temp["PHS"] <- NA
temp["PHF"] <- NA
temp["PHC"] <- NA


temp$PHST <- phsot
temp$PHS <- phs
temp$PHF <- phf
temp$PHC <- phc



pasot <- l6TotalAway('AwayTeam',"AwayTeam",'AST')  
pas <- l6TotalAway('AwayTeam',"AwayTeam",'AS')  
paf<- l6TotalAway('AwayTeam',"AwayTeam",'AF')  
pac<- l6TotalAway('AwayTeam',"AwayTeam",'AC')  
pagf<- l6TotalAway('AwayTeam' , 'AwayTeam' , 'FTAG')
paga<- l6TotalAway('AwayTeam' , 'AwayTeam' , 'FTHG')


agd <- pagf - paga

temp["PAST"] <- NA
temp["PAS"] <- NA
temp["PAF"] <- NA
temp["PAC"] <- NA

temp$PAST <- pasot
temp$PAS <- pas
temp$PAF <- paf
temp$PAC <- pac

temp["STD"] <- NA
temp["SD"] <- NA
temp["FD"] <- NA
temp["CD"] <- NA
temp["GDD"] <- NA
temp["HForm"] <- NA
temp["AForm"] <- NA

temp$STD <- temp$PHST - temp$PAST
temp$SD <- temp$PHS - temp$PAS
temp$FD <- temp$PHF - temp$PAF
temp$CD <- temp$PHC - temp$PAC
temp$GDD <- hgd - agd
temp$HForm <- trset$HomeForm
temp$AForm <- trset$AwayForm

ttemp <- testset

tphsot <- l6Total_test('HomeTeam',"HomeTeam",'HST')  
tphs <- l6Total_test('HomeTeam',"HomeTeam",'HS')  
tphf<- l6Total_test('HomeTeam',"HomeTeam",'HF')  
tphc<- l6Total_test('HomeTeam',"HomeTeam",'HC')  
tphgf<- l6Total_test('HomeTeam' , 'HomeTeam' , 'FTHG')
tphga<- l6Total_test('HomeTeam' , 'HomeTeam' , 'FTAG')

thgd <- tphgf - tphga


ttemp["PHST"] <- NA
ttemp["PHS"] <- NA
ttemp["PHF"] <- NA
ttemp["PHC"] <- NA

ttemp$PHST <- tphsot
ttemp$PHS <- tphs
ttemp$PHF <- tphf
ttemp$PHC <- tphc



tpasot <- l6TotalAway_test('AwayTeam',"AwayTeam",'AST')  
tpas <- l6TotalAway_test('AwayTeam',"AwayTeam",'AS')  
tpaf<- l6TotalAway_test('AwayTeam',"AwayTeam",'AF')  
tpac<- l6TotalAway_test('AwayTeam',"AwayTeam",'AC')  
tpagf<- l6TotalAway_test('AwayTeam' , 'AwayTeam' , 'FTAG')
tpaga<- l6TotalAway_test('AwayTeam' , 'AwayTeam' , 'FTHG')

tagd <- tpagf - tpaga


ttemp["PAST"] <- NA
ttemp["PAS"] <- NA
ttemp["PAF"] <- NA
ttemp["PAC"] <- NA

ttemp$PAST <- tpasot
ttemp$PAS <- tpas
ttemp$PAF <- tpaf
ttemp$PAC <- tpac

ttemp["STD"] <- NA
ttemp["SD"] <- NA
ttemp["FD"] <- NA
ttemp["CD"] <- NA
ttemp["GDD"] <- NA
ttemp["HForm"] <- NA
ttemp["AForm"] <- NA


ttemp$STD <- ttemp$PHST - ttemp$PAST
ttemp$SD <- ttemp$PHS - ttemp$PAS
ttemp$FD <- ttemp$PHF - ttemp$PAF
ttemp$CD <- ttemp$PHC - ttemp$PAC
ttemp$GDD <- thgd - tagd
ttemp$HForm <- testset$HomeForm
ttemp$AForm <- testset$AwayForm

trctrl <- trainControl(method = "cv", number = 10 )


svm_Linear <- train(FTR ~ PHST+PAST+PHS+PAS+PHF+PAF+PHC+PAC+GDD+HForm+AForm , data = temp, method = "svmLinear",
                    trControl=trctrl)

svm_Radial <- train(FTR ~ PHST+PAST+PHS+PAS+PHF+PAF+PHC+PAC+GDD+HForm+AForm , data = temp, method = "svmRadial",
                    trControl=trctrl)

nb<- naiveBayes(FTR ~ ., data = temp)

randfor<-randomForest(FTR ~ PHST+PAST+PHS+PAS+PHF+PAF+PHC+PAC+GDD+HForm+AForm, data = temp, ntree = 500, trControl = trctrl)

gbmodel <- train(  FTR ~ PHST+PAST+PHS+PAS+PHF+PAF+PHC+PAC+GDD+HForm+AForm, data = temp, method = "gbm", trControl = trctrl )


summary(gbmodel)
print(gbmodel)


lsvm <- predict(svm_Linear,ttemp)
rsvm <- predict(svm_Radial,  ttemp)
nbp <- predict(nb ,  ttemp )
rand <- predict(randfor,  ttemp )
gbresult <- predict(object = gbmodel , ttemp , probability = TRUE , type = 'raw')

confusionMatrix(gbresult , factor(te))


lsvm_res<-as.matrix(confusionMatrix(factor(lsvm),factor(te)))
rsvm_res<-as.matrix(confusionMatrix(factor(rsvm),factor(te)))
nbp_res<-as.matrix(confusionMatrix(factor(nbp),factor(te)))
randfor_res<-as.matrix(confusionMatrix(factor(rand),factor(te)))
gb_res<-as.matrix(confusionMatrix(factor(gbresult),factor(te)))


cm_lsvm<-confusionMatrix(factor(lsvm),factor(te))
str(cm_lsvm)
overall <- cm_lsvm$overall
lsvm_Accuracy<-overall['Accuracy']

cm_rsvm<-confusionMatrix(factor(rsvm),factor(te))
str(cm_rsvm)
overall<-cm_rsvm$overall
rsvm_Accuracy<-overall['Accuracy']

cm_nbp<-confusionMatrix(factor(nbp),factor(te))
str(cm_nbp)
overall<-cm_nbp$overall
nbp_Accuracy<-overall['Accuracy']

cm_rand<-confusionMatrix(factor(rand),factor(te))
str(cm_rand)
overall<-cm_rand$overall
rand_Accuracy<-overall['Accuracy']

cm_gbresult<-confusionMatrix(factor(gbresult),factor(te))
str(cm_gbresult)
overall<-cm_gbresult$overall
gb_result_Accuracy<-overall['Accuracy']



n = sum(lsvm_res) 
nc = nrow(lsvm_res)
diag = diag(lsvm_res)


rowsums = apply(lsvm_res, 1, sum) 
colsums = apply(lsvm_res, 2, sum) 
p = rowsums / n 
q = colsums / n 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

lsvm_A_actual<-lsvm_res[1,1]
lsvm_A_predicted<-rowsums[1]
lsvm_D_actual<-lsvm_res[2,2]
lsvm_D_predicted<-rowsums[2]
lsvm_H_actual<-lsvm_res[3,3]
lsvm_H_predicted<-rowsums[3]


prels<-round(precision , 3)
recls<-round(recall , 3)
f1sls<-round(f1,3)


lsvm_prf <- data.frame(prels, recls, f1sls) 


n = sum(rsvm_res) 
nc = nrow(rsvm_res)
diag = diag(rsvm_res)


rowsums = apply(rsvm_res, 1, sum) 
colsums = apply(rsvm_res, 2, sum) 
p = rowsums / n 
q = colsums / n 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

rsvm_A_actual<-rsvm_res[1,1]
rsvm_A_predicted<-rowsums[1]
rsvm_D_actual<-rsvm_res[2,2]
rsvm_D_predicted<-rowsums[2]
rsvm_H_actual<-rsvm_res[3,3]
rsvm_H_predicted<-rowsums[3]


prers<-round(precision , 3)
recrs<-round(recall , 3)
f1srs<-round(f1,3)

rsvm_prf <- data.frame(prers,recrs,f1srs) 


n = sum(nbp_res) 
nc = nrow(nbp_res)
diag = diag(nbp_res)


rowsums = apply(nbp_res, 1, sum) 
colsums = apply(nbp_res, 2, sum) 
p = rowsums / n 
q = colsums / n 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

nbp_A_actual<-nbp_res[1,1]
nbp_A_predicted<-rowsums[1]
nbp_D_actual<-nbp_res[2,2]
nbp_D_predicted<-rowsums[2]
nbp_H_actual<-nbp_res[3,3]
nbp_H_predicted<-rowsums[3]


prenb<-round(precision , 3)
recnb<-round(recall , 3)
f1snb<-round(f1,3)

nbp_prf <- data.frame(prenb,recnb,f1snb) 


n = sum(randfor_res) 
nc = nrow(randfor_res)
diag = diag(randfor_res)


rowsums = apply(randfor_res, 1, sum) 
colsums = apply(randfor_res, 2, sum) 
p = rowsums / n 
q = colsums / n 

rf_A_actual<-randfor_res[1,1]
rf_A_predicted<-rowsums[1]
rf_D_actual<-randfor_res[2,2]
rf_D_predicted<-rowsums[2]
rf_H_actual<-randfor_res[3,3]
rf_H_predicted<-rowsums[3]

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

prerf<-round(precision , 3)
recrf<-round(recall , 3)
f1srf<-round(f1,3)

randf_prf <- data.frame(prerf,recrf,f1srf) 


n = sum(gb_res) 
nc = nrow(gb_res)
diag = diag(gb_res)


rowsums = apply(gb_res, 1, sum) 
colsums = apply(gb_res, 2, sum) 
p = rowsums / n 
q = colsums / n 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

gb_A_actual<-gb_res[1,1]
gb_A_predicted<-rowsums[1]
gb_D_actual<-gb_res[2,2]
gb_D_predicted<-rowsums[2]
gb_H_actual<-gb_res[3,3]
gb_H_predicted<-rowsums[3]

pregb<-round(precision , 3)
recgb<-round(recall , 3)
f1sgb<-round(f1,3)

gb_prf <- data.frame(pregb,recgb,f1sgb) 





#OUTPUT 

#COnfusionMAtrix

nbp_res
lsvm_res
rsvm_res
randfor_res
gb_res

#ACCuracy

nbp_Accuracy*100
lsvm_Accuracy*100
rsvm_Accuracy*100
rand_Accuracy*100
gb_result_Accuracy*100

#Bar Graph

feeds<-c(lsvm_Accuracy,rsvm_Accuracy,nbp_Accuracy,rand_Accuracy , gb_result_Accuracy)
barplot(feeds, main = "ACCURACY OF VARIOUS CLASSIFIERS  " , names.arg = c("LSVm" , "RSVM" , "NB" , "RNF" , "GB") , xlab = "CLASSIFIERS" , ylab = "Accuracy" , col = c("blue" , "blue" , "red" , "coral" , "yellow") , space = 0.5 )

#Comparison Graphs

nbp_feed<- c(nbp_A_actual,nbp_A_predicted,nbp_D_actual,nbp_D_predicted , nbp_H_actual ,nbp_H_predicted)
gb_feed <- c(gb_A_actual , gb_A_predicted , gb_D_actual , gb_D_predicted , gb_H_actual , gb_H_predicted)
randf_feed <- c(rf_A_actual , rf_A_predicted , rf_D_actual , rf_D_predicted , rf_H_actual , rf_H_predicted)
lsvm_feed <- c(lsvm_A_actual , lsvm_A_predicted , lsvm_D_actual , lsvm_D_predicted , lsvm_H_actual , lsvm_H_predicted)
rsvm_feed <- c(rsvm_A_actual , rsvm_A_predicted , rsvm_D_actual , rsvm_D_predicted , rsvm_H_actual , rsvm_H_predicted)

barplot(matrix(nbp_feed , nr=2), beside=T, 
        col=c("aquamarine3","coral" ), 
        names.arg=c("A","D" , "H") , xlab="NAIVE BAYES" , main = "Actual vs Predicted")


barplot(matrix(randf_feed , nr=2), beside=T, 
        col=c("aquamarine3","coral" ), 
        names.arg=c("A","D" , "H") , xlab="RANDOM FOREST" , main = "Actual vs Predicted")


barplot(matrix(lsvm_feed , nr=2), beside=T, 
        col=c("aquamarine3","coral" ), 
        names.arg=c("A","D" , "H") , xlab="LINEAR SVM" , main = "Actual vs Predicted")

barplot(matrix(rsvm_feed , nr=2), beside=T, 
        col=c("aquamarine3","coral" ), 
        names.arg=c("A","D" , "H") , xlab="RADIAL SVM" , main = "Actual vs Predicted")

barplot(matrix(gb_feed , nr=2), beside=T, 
        col=c("aquamarine3","coral" ), 
        names.arg=c("A","D" , "H") , xlab="GRADIENT BOOSTING" , main = "Actual vs Predicted")

#Precision - Recall - F1Score

nbp_prf
randf_prf
gb_prf
lsvm_prf
rsvm_prf

