# Quinn Fargen
# SDSU Math 401 S2
# 
# Data Summarizing 

######################################################################################################################
#
#                       Data Summary and Random Forest Models
#
######################################################################################################################
require(rpart)
require(partykit)
require(randomForest)
require(gbm)



#Want to predict on just 2015 to compare to ESPN scores
TData.2014 <- TournData.R[694:756, 4:23]
TData.2015 <- TournData.R[757:819, 4:36]
TData.2016 <- TournData.R[820:882, 4:23]



#This will be every other entry besides 2015, has 13 seasons
TData <- TournData.R[-c(694:882),]







#####################################################################################################################
head(TData)
summary(TData)


#############################################
#Testing and Training Datasets of TournData
set.seed(10)
split <- sample(seq_len(nrow(TData)), size = floor(.60 * nrow(TData)))
March.Train.R <- TData[split, 4:36]  #491 games
March.Test.R <- TData[-split, 4:36]  #328 games



#############################################
#Decision Tree
MarchTree <- rpart(Win.Loss ~ ., data = March.Train.R, na.action = na.omit)
#print(MarchTree)
#plot(as.party(MarchTree), tp_args = list(id = F))

table(predict(MarchTree ,newdata = March.Test.R, type = "vector") - 1, March.Test.R$Win.Loss)


#attaching predicted values to March.Test.R
Tree.Test <- predict(MarchTree, newdata = March.Test.R, type = "vector")
March.Test.R$P.WL.Tree <- as.numeric(Tree.Test) - 1
#attaching predicted values to TData.2015
Tree.2015 <- predict(MarchTree, newdata = TData.2015, type = "vector")
TData.2015$P.WL.Tree <- as.numeric(Tree.2015) - 1


Tree.P.14 <- TData.SM.2014$P.WL.Bag  #switched $ to correct model before running
Correct.14 <- as.numeric(TData.SM.2014$Win.Loss) -1
Correct.15 <- as.numeric(TData.SM.2015$Win.Loss) -1

Tree.P.15 <- TData.SM.2015$P.WL.Bag  #switched $ to correct model before running
Correct.15 <- as.numeric(TData.SM.2015$Win.Loss) -1

Tree.P.16 <- TData.SM.2016$P.WL.Bag  #switched $ to correct model before running
Correct.16 <- as.numeric(TData.SM.2016$Win.Loss) -1

Tree.P <- list(Tree.P.14, Tree.P.15, Tree.P.16)
Correct <- list(Correct.14, Correct.15, Correct.16)







#############################################
#RandomForest
set.seed(5)
March.RF <- randomForest(Win.Loss ~ . , data = March.Train.R, na.action = na.omit, control = list(maxit = 50))
March.RF
summary(March.RF)
table(predict(March.RF ,newdata = March.Test.R, type = "response"), March.Test.R$Win.Loss)

#importance
March.RF.Import <- data.frame(variable = names(March.RF$importance[,1]), importance = March.RF$importance[,1])
March.RF.Import <- March.RF.Import[order(-March.RF.Import[,2]),]
Import.RF <- barplot(March.RF.Import$importance)
text(Import.RF, par("usr")[3], labels = rownames(March.RF.Import), srt = 45, adj = c(1,1,1,1), xpd = TRUE, cex = .9)

#attaching predicted values to March.Test.R
RF.P.Test <- predict(March.RF ,newdata = March.Test.R, type = "response")
March.Test.R$P.WL.RF<- as.numeric(RF.P.Test) - 1
#attaching predicted values to TData.2015

RF.P.2014 <- predict(March.RF ,newdata = TData.2014, type = "response")
TData.2015$P.WL.RF <- as.numeric(RF.P.2015) - 1

RF.P.2015 <- predict(March.RF ,newdata = TData.2015, type = "response")
TData.2015$P.WL.RF <- as.numeric(RF.P.2015) - 1

RF.P.2015 <- predict(March.RF ,newdata = TData.2015, type = "response")
TData.2015$P.WL.RF <- as.numeric(RF.P.2015) - 1






#############################################
#Bagging
set.seed(5)
March.Bag <- randomForest(Win.Loss ~ . , data = March.Train.R, mtry = 32, na.action = na.omit, control = list(maxit = 50))
March.Bag

  #importance
March.Bag.Import <- data.frame(variable = names(March.Bag$importance[,1]), importance = March.Bag$importance[,1])
March.Bag.Import <- March.Bag.Import[order(-March.Bag.Import[,2]),]
Import.Bag <- barplot(March.Bag.Import$importance)
text(Import.Bag, par("usr")[3], labels = rownames(March.Bag.Import), srt = 45, adj = c(1,1,1,1), xpd = TRUE, cex = .9)
table(predict(March.Bag ,newdata = March.Test.R, type = "response"), March.Test.R$Win.Loss)


#attaching predicted values to March.Test.R
Bag.P.Test <- predict(March.Bag ,newdata = March.Test.R, type = "response") 
March.Test.R$P.WL.Bag<- as.numeric(Bag.P.Test) - 1
#attaching predicted values to TData.2015
Bag.P.2015 <- predict(March.Bag ,newdata = TData.2015, type = "response")
TData.2015$P.WL.Bag <- as.numeric(Bag.P.2015) - 1 



bag.yhat <- predict(March.Bag, newdata =March.Test.R)
plot(bag.yhat, March.Test.R$Win.Loss)



#############################################
#Boosting
library(gbm)
set.seed(10)
March.Boost <- gbm(Win.Loss ~ . , data = March.Train.R, distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
summary(March.Boost)
table(round(predict(March.Boost ,newdata = March.Test.R, n.trees = 5000, type = "response")) - 1, March.Test.R$Win.Loss)


#attaching predicted values to March.Test.R
Boost.P.Test <- predict(March.Boost ,newdata = March.Test.R, n.trees = 5000, type = "response") 
March.Test.R$P.WL.Boost <- round(as.numeric(Boost.P.Test) - 1)
#attaching predicted values to TData.2015
Boost.P.2015 <- predict(March.Boost ,newdata = TData.2015, n.trees = 5000, type = "response")
TData.2015$P.WL.Boost <- round(as.numeric(Boost.P.2015) - 1) 





for(j in 1:100)
{
  print(seed[j])
  j <- j +1
}


poo <- 10



############################################################################################################################################################################################################

######################################################################################################
######################################################################################################
######        For the TournData.R.SM
######################################################################################################
######################################################################################################

#Want to predict on just 2015 to compare to ESPN scores
TData.SM.2014 <- TournData.R.SM[694:756, 4:23]
TData.SM.2015 <- TournData.R.SM[757:819, 4:23]
TData.SM.2016 <- TournData.R.SM[820:882, 4:23]

#This will be every other entry besides 2014-2016, has 13 seasons
TData.SM <- TournData.R.SM[-c(694:882),]

#############################################
#Testing and Training Datasets of TournData

set.seed(11555)
split <- sample(seq_len(nrow(TData.SM)), size = floor(.60 * nrow(TData.SM)))
March.Train.R.SM <- TData.SM[split, 4:23]  #491 games
March.Test.R.SM <- TData.SM[-split, 4:23]  #328 games

seed <- seq(100, 1090, 10)

Correct.14 <- as.numeric(TData.SM.2014$Win.Loss) -1
Correct.15 <- as.numeric(TData.SM.2015$Win.Loss) -1
Correct.16 <- as.numeric(TData.SM.2016$Win.Loss) -1
Correct <- list(Correct.14, Correct.15, Correct.16)


March.Pred <- rbind(March.Test.R.SM, TData.SM.2014, TData.SM.2015, TData.SM.2016) #467 games




##########################################################################################
####################            #Decision Tree                      ######################
##########################################################################################


Three.Score <- vector()
Dec.Tree <- list()
i <- 1
j <- 1

for(j in 1:100){
  
set.seed(seed[j]) 
split <- sample(seq_len(nrow(March.Train.R.SM)), size = floor(.60 * nrow(March.Train.R.SM)))
Tree.SM.Score <- March.Train.R.SM[split, ]
    
MarchTree.SM <- rpart(Win.Loss ~ ., data = Tree.SM.Score, na.action = na.omit)
#print(MarchTree.SM)
#plot(as.party(MarchTree.SM), tp_args = list(id = F))

T.T <- table(predict(MarchTree.SM ,newdata = March.Pred, type = "vector") - 1, March.Pred$Win.Loss)
error <- mean(c((T.T[2,1]/sum(T.T[1,1],T.T[2,1])), (T.T[1,2]/sum(T.T[2,2],T.T[1,2]))))


#attaching predicted values to March.Test.R
Tree.Test.SM <- predict(MarchTree.SM, newdata = March.Test.R.SM, type = "vector")
March.Test.R.SM$P.WL.Tree <- as.numeric(Tree.Test.SM) - 1
#attaching predicted values to TData.2014 - 16
Tree.SM.2014 <- predict(MarchTree.SM, newdata = TData.SM.2014, type = "vector")
TData.SM.2014$P.WL.Tree <- as.numeric(Tree.SM.2014) - 1
Tree.P.14 <- TData.SM.2014$P.WL.Tree
Tree.SM.2015 <- predict(MarchTree.SM, newdata = TData.SM.2015, type = "vector")
TData.SM.2015$P.WL.Tree <- as.numeric(Tree.SM.2015) - 1
Tree.P.15 <- TData.SM.2015$P.WL.Tree
Tree.SM.2016 <- predict(MarchTree.SM, newdata = TData.SM.2016, type = "vector")
TData.SM.2016$P.WL.Tree <- as.numeric(Tree.SM.2016) - 1
Tree.P.16 <- TData.SM.2016$P.WL.Tree

Tree.P <- list(Tree.P.14, Tree.P.15, Tree.P.16)


for(i in 1:3){
  R1 <- Tree.P[[i]][1:32] == Correct[[i]][1:32]
  R1
  score <- (unname(table(R1)[2]) * 10)
  score
  
  R2 <- Tree.P[[i]][33:48] == Correct[[i]][33:48]
  R2
  score2 <- score + (unname(table(R2)[2]) * 20)
  score2 <- ifelse(is.na(score2), score, score2)
  score2
  
  R3 <- Tree.P[[i]][49:56] == Correct[[i]][49:56]
  R3
  score3 <- score2 + (unname(table(R3)[2]) * 40)
  score3 <- ifelse(is.na(score3), score2, score3)
  score3
  
  R4 <- Tree.P[[i]][57:60] == Correct[[i]][57:60]
  R4
  score4 <- score3 + (unname(table(R4)[2]) * 80)
  score4 <- ifelse(is.na(score4), score3, score4)
  score4
  
  R5 <- Tree.P[[i]][61:62] == Correct[[i]][61:62]
  R5
  score5 <- score4 + (unname(table(R5)[2])) * 160
  score5 <- ifelse(is.na(score5), score4, score5)
  score5
  
  R6 <- Tree.P[[i]][63] == Correct[[i]][63]
  R6
  score6 <- score5 + R6 * 320
  score6 <- ifelse(is.na(score6), score5, score6)
  score6
  
  Three.Score[i] <- score6
}

Dec.Tree[[j]]<- c(Three.Score[1:3], error)
}

DECTREE <- vector()
for(i in 1:100)
{
  for(k in 1:3)
  {
    DECTREE <- c(DECTREE, Dec.Tree[[i]][[k]])
  }
}  

T.Error <- vector()
for(i in 1:100)
{
  T.Error <- c(T.Error, Dec.Tree[[i]][[4]])
};summary(T.Error)

S2014 <- cumsum(c(1,rep(3,99)))
S2015 <- cumsum(c(2,rep(3,99)))
S2016 <- cumsum(c(3,rep(3,99)))

DEC.T.14 <- DECTREE[S2014]
DEC.T.15 <- DECTREE[S2015]
DEC.T.16 <- DECTREE[S2016]
summary(DECTREE); summary(DEC.T.14); summary(DEC.T.15); summary(DEC.T.16); summary(T.Error)
sd(DECTREE) #211

# 1008  906  1026  1093  .335


##########################################################################################
####################            #Random Forest                      ######################
##########################################################################################

Three.Score <- vector()
RF.Scores <- list()
i <- 1
j <- 1

for(j in 1:100){
  
  set.seed(seed[j]) 
March.RF.SM <- randomForest(Win.Loss ~ . , data = March.Train.R.SM, na.action = na.omit, control = list(maxit = 50))
March.RF.SM
summary(March.RF.SM)


T.T <- table(predict(March.RF.SM, newdata = March.Pred, type = "response"), March.Pred$Win.Loss)

error <- mean(c((T.T[2,1]/sum(T.T[1,1],T.T[2,1])), (T.T[1,2]/sum(T.T[2,2],T.T[1,2]))))

#importance
#March.RF.Import.SM <- data.frame(variable = names(March.RF.SM$importance[,1]), importance = March.RF.SM$importance[,1])
#March.RF.Import.SM <- March.RF.Import.SM[order(-March.RF.Import.SM[,2]),]
#Import.RF.SM <- barplot(March.RF.Import.SM$importance)
#barplot(March.RF.Import.SM$importance, col = c(rep("burlywood3", 5), rep("dodgerblue3", 14)))
#text(Import.RF.SM, par("usr")[3], labels = rownames(March.RF.Import.SM), srt = 45, adj = c(1,1,1,1), xpd = TRUE, cex = .9)

#attaching predicted values to March.Test.R
RF.P.Test.SM <- predict(March.RF.SM ,newdata = March.Test.R.SM, type = "response")
March.Test.R.SM$P.WL.RF<- as.numeric(RF.P.Test.SM) - 1
#attaching predicted values to TData.2015
RF.P.2014.SM <- predict(March.RF.SM ,newdata = TData.SM.2014, type = "response")
TData.SM.2014$P.WL.RF <- as.numeric(RF.P.2014.SM) - 1
RF.P.14 <- TData.SM.2014$P.WL.RF
RF.P.2015.SM <- predict(March.RF.SM ,newdata = TData.SM.2015, type = "response")
TData.SM.2015$P.WL.RF <- as.numeric(RF.P.2015.SM) - 1
RF.P.15 <- TData.SM.2015$P.WL.RF
RF.P.2016.SM <- predict(March.RF.SM ,newdata = TData.SM.2016, type = "response")
TData.SM.2016$P.WL.RF <- as.numeric(RF.P.2016.SM) - 1
RF.P.16 <- TData.SM.2016$P.WL.RF

RF.P <- list(RF.P.14, RF.P.15, RF.P.16)


for(i in 1:3){
  R1 <- RF.P[[i]][1:32] == Correct[[i]][1:32]
  R1
  score <- (unname(table(R1)[2]) * 10)
  score
  
  R2 <- RF.P[[i]][33:48] == Correct[[i]][33:48]
  R2
  score2 <- score + (unname(table(R2)[2]) * 20)
  score2 <- ifelse(is.na(score2), score, score2)
  score2
  
  R3 <- RF.P[[i]][49:56] == Correct[[i]][49:56]
  R3
  score3 <- score2 + (unname(table(R3)[2]) * 40)
  score3 <- ifelse(is.na(score3), score2, score3)
  score3
  
  R4 <- RF.P[[i]][57:60] == Correct[[i]][57:60]
  R4
  score4 <- score3 + (unname(table(R4)[2]) * 80)
  score4 <- ifelse(is.na(score4), score3, score4)
  score4
  
  R5 <- RF.P[[i]][61:62] == Correct[[i]][61:62]
  R5
  score5 <- score4 + (unname(table(R5)[2])) * 160
  score5 <- ifelse(is.na(score5), score4, score5)
  score5
  
  R6 <- RF.P[[i]][63] == Correct[[i]][63]
  R6
  score6 <- score5 + R6 * 320
  score6 <- ifelse(is.na(score6), score5, score6)
  score6
  
  Three.Score[i] <- score6
}

RF.Scores[[j]]<- c(Three.Score[1:3], error)
}

RF <- vector()
for(i in 1:100)
{
  for(k in 1:3)
  {
    RF <- c(RF, RF.Scores[[i]][[k]])
  }
}  
RF.Error <- vector()
for(i in 1:100)
{
  RF.Error <- c(RF.Error, RF.Scores[[i]][[4]])
}; summary(RF.Error)

RF.14 <- RF[S2014]
RF.15 <- RF[S2015]
RF.16 <- RF[S2016]

summary(RF); summary(RF.14); summary(RF.15); summary(RF.16); summary(RF.Error)
sd(RF) #212

#1085   811   1147    1296   .265



##########################################################################################
####################            #Bagging                            ######################
##########################################################################################
Three.Score <- vector()
Bag.Scores <- list()
i <- 1
j <- 1

for(j in 1:100){
  
set.seed(seed[j])
March.Bag.SM <- randomForest(Win.Loss ~ . , data = March.Train.R.SM, mtry = 19, na.action = na.omit, control = list(maxit = 50))
March.Bag.SM

#importance
#March.Bag.Import.SM <- data.frame(variable = names(March.Bag.SM$importance[,1]), importance = March.Bag.SM$importance[,1])
#March.Bag.Import.SM <- March.Bag.Import.SM[order(-March.Bag.Import.SM[,2]),]
#Import.Bag.SM <- barplot(March.Bag.Import.SM$importance)
#text(Import.Bag.SM, par("usr")[3], labels = rownames(March.Bag.Import.SM), srt = 45, adj = c(1,1,1,1), xpd = TRUE, cex = .9)


T.T <- table(predict(March.Bag.SM ,newdata = March.Pred, type = "response"), March.Pred$Win.Loss)
error <- mean(c((T.T[2,1]/sum(T.T[1,1],T.T[2,1])), (T.T[1,2]/sum(T.T[2,2],T.T[1,2]))))


#attaching predicted values to March.Test.R
Bag.P.Test.SM <- predict(March.Bag.SM ,newdata = March.Test.R.SM, type = "response") 
March.Test.R.SM$P.WL.Bag<- as.numeric(Bag.P.Test.SM) - 1
#attaching predicted values to TData.2015
Bag.P.2014.SM <- predict(March.Bag.SM ,newdata = TData.SM.2014, type = "response")
TData.SM.2014$P.WL.Bag <- as.numeric(Bag.P.2014.SM) - 1 
Bag.P.14 <- TData.SM.2014$P.WL.Bag
Bag.P.2015.SM <- predict(March.Bag.SM ,newdata = TData.SM.2015, type = "response")
TData.SM.2015$P.WL.Bag <- as.numeric(Bag.P.2015.SM) - 1
Bag.P.15 <- TData.SM.2015$P.WL.Bag
Bag.P.2016.SM <- predict(March.Bag.SM ,newdata = TData.SM.2016, type = "response")
TData.SM.2016$P.WL.Bag <- as.numeric(Bag.P.2016.SM) - 1
Bag.P.16 <- TData.SM.2016$P.WL.Bag


#bag.yhat <- predict(March.Bag.SM, newdata =March.Test.R.SM)
#plot(bag.yhat, March.Test.R.SM$Win.Loss)

BAG.P <- list(Bag.P.14, Bag.P.15, Bag.P.16)


for(i in 1:3){
  R1 <- BAG.P[[i]][1:32] == Correct[[i]][1:32]
  R1
  score <- (unname(table(R1)[2]) * 10)
  score
  
  R2 <- BAG.P[[i]][33:48] == Correct[[i]][33:48]
  R2
  score2 <- score + (unname(table(R2)[2]) * 20)
  score2 <- ifelse(is.na(score2), score, score2)
  score2
  
  R3 <- BAG.P[[i]][49:56] == Correct[[i]][49:56]
  R3
  score3 <- score2 + (unname(table(R3)[2]) * 40)
  score3 <- ifelse(is.na(score3), score2, score3)
  score3
  
  R4 <- BAG.P[[i]][57:60] == Correct[[i]][57:60]
  R4
  score4 <- score3 + (unname(table(R4)[2]) * 80)
  score4 <- ifelse(is.na(score4), score3, score4)
  score4
  
  R5 <- BAG.P[[i]][61:62] == Correct[[i]][61:62]
  R5
  score5 <- score4 + (unname(table(R5)[2])) * 160
  score5 <- ifelse(is.na(score5), score4, score5)
  score5
  
  R6 <- BAG.P[[i]][63] == Correct[[i]][63]
  R6
  score6 <- score5 + R6 * 320
  score6 <- ifelse(is.na(score6), score5, score6)
  score6
  
  Three.Score[i] <- score6
}

Bag.Scores[[j]]<- c(Three.Score[1:3], error)
}

BAG <- vector()
for(i in 1:100)
{
  for(k in 1:3)
  {
    BAG <- c(BAG, Bag.Scores[[i]][[k]])
  }
}  

B.Error <- vector()
for(i in 1:100)
{
  B.Error <- c(B.Error, Bag.Scores[[i]][[4]])
};summary(B.Error)

BAG.14 <- BAG[S2014]
BAG.15 <- BAG[S2015]
BAG.16 <- BAG[S2016]
summary(BAG); summary(BAG.14); summary(BAG.15); summary(BAG.16)
sd(BAG) #221

#1011   807   915   1311   .269


##########################################################################################
####################            #Boosting                           ######################
##########################################################################################

Three.Score <- vector()
Boost.Scores <- list()
i <- 1
j <- 1

for(j in 1:100){
  
  set.seed(seed[j])

March.Boost.SM <- gbm(Win.Loss ~ . , data = March.Train.R.SM, distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
March.Boost.SM
summary(March.Boost.SM)

T.T <- table(round(predict(March.Boost.SM ,newdata = March.Pred, n.trees = 5000, type = "response")) - 1, March.Pred$Win.Loss)
error <- mean(c((T.T[2,1]/sum(T.T[1,1],T.T[2,1])), (T.T[1,2]/sum(T.T[2,2],T.T[1,2]))))


#attaching predicted values to March.Test.R
Boost.P.Test.SM <- predict(March.Boost.SM ,newdata = March.Test.R.SM, n.trees = 5000, type = "response") 
March.Test.R.SM$P.WL.Boost <- round(as.numeric(Boost.P.Test.SM) - 1)
#attaching predicted values to TData.2015
Boost.P.2014.SM <- predict(March.Boost.SM ,newdata = TData.SM.2014, n.trees = 5000, type = "response")
TData.SM.2014$P.WL.Boost <- round(as.numeric(Boost.P.2014.SM) - 1) 
Boost.P.14 <- TData.SM.2014$P.WL.Boost
Boost.P.2015.SM <- predict(March.Boost.SM ,newdata = TData.SM.2015, n.trees = 5000, type = "response")
TData.SM.2015$P.WL.Boost <- round(as.numeric(Boost.P.2015.SM) - 1) 
Boost.P.15 <- TData.SM.2015$P.WL.Boost
Boost.P.2016.SM <- predict(March.Boost.SM ,newdata = TData.SM.2016, n.trees = 5000, type = "response")
TData.SM.2016$P.WL.Boost <- round(as.numeric(Boost.P.2016.SM) - 1) 
Boost.P.16 <- TData.SM.2016$P.WL.Boost


Boost.P <- list(Boost.P.14, Boost.P.15, Boost.P.16)


for(i in 1:3){
  R1 <- Boost.P[[i]][1:32] == Correct[[i]][1:32]
  R1
  score <- (unname(table(R1)[2]) * 10)
  score
  
  R2 <- Boost.P[[i]][33:48] == Correct[[i]][33:48]
  R2
  score2 <- score + (unname(table(R2)[2]) * 20)
  score2 <- ifelse(is.na(score2), score, score2)
  score2
  
  R3 <- Boost.P[[i]][49:56] == Correct[[i]][49:56]
  R3
  score3 <- score2 + (unname(table(R3)[2]) * 40)
  score3 <- ifelse(is.na(score3), score2, score3)
  score3
  
  R4 <- Boost.P[[i]][57:60] == Correct[[i]][57:60]
  R4
  score4 <- score3 + (unname(table(R4)[2]) * 80)
  score4 <- ifelse(is.na(score4), score3, score4)
  score4
  
  R5 <- Boost.P[[i]][61:62] == Correct[[i]][61:62]
  R5
  score5 <- score4 + (unname(table(R5)[2])) * 160
  score5 <- ifelse(is.na(score5), score4, score5)
  score5
  
  R6 <- Boost.P[[i]][63] == Correct[[i]][63]
  R6
  score6 <- score5 + R6 * 320
  score6 <- ifelse(is.na(score6), score5, score6)
  score6
  
  Three.Score[i] <- score6
}

Boost.Scores[[j]]<- c(Three.Score[1:3], error)
}

Boost <- vector()
for(i in 1:100)
{
  for(k in 1:3)
  {
    Boost <- c(Boost, Boost.Scores[[i]][[k]])
  }
}  

Bo.Error <- vector()
for(i in 1:100)
{
  Bo.Error <- c(Bo.Error, Boost.Scores[[i]][[4]])
};summary(Bo.Error)

Boost.14 <- Boost[S2014]
Boost.15 <- Boost[S2015]
Boost.16 <- Boost[S2016]
summary(Boost); summary(Boost.14); summary(Boost.15); summary(Boost.16)
sd(Boost) #217

#1030   792   984   1315    .262



#############################
#     Results
#############################
                                                #Tree
DEC.T.14 <- DECTREE[S2014]
DEC.T.15 <- DECTREE[S2015]
DEC.T.16 <- DECTREE[S2016]
summary(DECTREE); summary(DEC.T.14); summary(DEC.T.15); summary(DEC.T.16)
sd(DECTREE) #211

# 1008  906  1026  1093 

                                                #RF
RF.14 <- RF[S2014]
RF.15 <- RF[S2015]
RF.16 <- RF[S2016]
summary(RF); summary(RF.14); summary(RF.15); summary(RF.16)
sd(RF) #212

#1085   811   1147    1296

                                                #Bag
BAG.14 <- BAG[S2014]
BAG.15 <- BAG[S2015]
BAG.16 <- BAG[S2016]
summary(BAG); summary(BAG.14); summary(BAG.15); summary(BAG.16)
sd(BAG) #221

#1011   807   915   1311

                                                #Boost
Boost.14 <- Boost[S2014]
Boost.15 <- Boost[S2015]
Boost.16 <- Boost[S2016]
summary(Boost); summary(Boost.14); summary(Boost.15); summary(Boost.16)
sd(Boost) #217

#1030   792   984   1315

                                                #ALL
ALL2014 <- c(DEC.T.14, RF.14, BAG.14, Boost.14)
ALL2015 <- c(DEC.T.15, RF.15, BAG.15, Boost.15)
ALL2016 <- c(DEC.T.16, RF.16, BAG.16, Boost.16)
ALL <- c(DECTREE, RF, Bag, Boost)
summary(ALL); summary(ALL2014); summary(ALL2015); summary(ALL2016)
sd(ALL); sd(ALL2014); sd(ALL2015); sd(ALL2016)
#211  122   169   106

# 1040    829   1018    1254











####################################################################################################################################################################################
############################################################
############################################################
####################      5 Variables
###########################################################
###########################################################









##########################################################################################
####################            #Decision Tree                      ######################
##########################################################################################


Three.Score <- vector()
Dec.Tree <- list()
i <- 1
j <- 1

for(j in 1:100){
  
  set.seed(seed[j]) 
  split <- sample(seq_len(nrow(March.Train.R.SM)), size = floor(.60 * nrow(March.Train.R.SM)))
  Tree.SM.Score <- March.Train.R.SM[split, ]
  
  MarchTree.SM <- rpart(Win.Loss ~ Rank + SOS + Seed + OSRS + DSRS, data = Tree.SM.Score, na.action = na.omit)
  #print(MarchTree.SM)
  #plot(as.party(MarchTree.SM), tp_args = list(id = F))
  
  T.T <- table(predict(MarchTree.SM ,newdata = March.Pred, type = "vector") - 1, March.Pred$Win.Loss)
  error <- mean(c((T.T[2,1]/sum(T.T[1,1],T.T[2,1])), (T.T[1,2]/sum(T.T[2,2],T.T[1,2]))))

  
  #attaching predicted values to March.Test.R
  Tree.Test.SM <- predict(MarchTree.SM, newdata = March.Test.R.SM, type = "vector")
  March.Test.R.SM$P.WL.Tree <- as.numeric(Tree.Test.SM) - 1
  #attaching predicted values to TData.2014 - 16
  Tree.SM.2014 <- predict(MarchTree.SM, newdata = TData.SM.2014, type = "vector")
  TData.SM.2014$P.WL.Tree <- as.numeric(Tree.SM.2014) - 1
  Tree.P.14 <- TData.SM.2014$P.WL.Tree
  Tree.SM.2015 <- predict(MarchTree.SM, newdata = TData.SM.2015, type = "vector")
  TData.SM.2015$P.WL.Tree <- as.numeric(Tree.SM.2015) - 1
  Tree.P.15 <- TData.SM.2015$P.WL.Tree
  Tree.SM.2016 <- predict(MarchTree.SM, newdata = TData.SM.2016, type = "vector")
  TData.SM.2016$P.WL.Tree <- as.numeric(Tree.SM.2016) - 1
  Tree.P.16 <- TData.SM.2016$P.WL.Tree
  
  Tree.P <- list(Tree.P.14, Tree.P.15, Tree.P.16)
  
  
  for(i in 1:3){
    R1 <- Tree.P[[i]][1:32] == Correct[[i]][1:32]
    R1
    score <- (unname(table(R1)[2]) * 10)
    score
    
    R2 <- Tree.P[[i]][33:48] == Correct[[i]][33:48]
    R2
    score2 <- score + (unname(table(R2)[2]) * 20)
    score2 <- ifelse(is.na(score2), score, score2)
    score2
    
    R3 <- Tree.P[[i]][49:56] == Correct[[i]][49:56]
    R3
    score3 <- score2 + (unname(table(R3)[2]) * 40)
    score3 <- ifelse(is.na(score3), score2, score3)
    score3
    
    R4 <- Tree.P[[i]][57:60] == Correct[[i]][57:60]
    R4
    score4 <- score3 + (unname(table(R4)[2]) * 80)
    score4 <- ifelse(is.na(score4), score3, score4)
    score4
    
    R5 <- Tree.P[[i]][61:62] == Correct[[i]][61:62]
    R5
    score5 <- score4 + (unname(table(R5)[2])) * 160
    score5 <- ifelse(is.na(score5), score4, score5)
    score5
    
    R6 <- Tree.P[[i]][63] == Correct[[i]][63]
    R6
    score6 <- score5 + R6 * 320
    score6 <- ifelse(is.na(score6), score5, score6)
    score6
    
    Three.Score[i] <- score6
  }
  
  Dec.Tree[[j]]<- c(Three.Score[1:3], error)
}

DECTREE.5 <- vector()
for(i in 1:100)
{
  for(k in 1:3)
  {
    DECTREE.5 <- c(DECTREE.5, Dec.Tree[[i]][[k]])
  }
}

T.Error <- vector()
for(i in 1:100)
{
  T.Error <- c(T.Error, Dec.Tree[[i]][[4]])
};summary(T.Error)

S2014 <- cumsum(c(1,rep(3,99)))
S2015 <- cumsum(c(2,rep(3,99)))
S2016 <- cumsum(c(3,rep(3,99)))

DEC.T.14.5 <- DECTREE.5[S2014]
DEC.T.15.5 <- DECTREE.5[S2015]
DEC.T.16.5 <- DECTREE.5[S2016]
summary(DECTREE.5); summary(DEC.T.14.5); summary(DEC.T.15.5); summary(DEC.T.16.5)
sd(DECTREE.5) #210

# 1054    935   1061    1164  ..315


##########################################################################################
####################            #Random Forest                      ######################
##########################################################################################

Three.Score <- vector()
RF.Scores <- list()
i <- 1
j <- 1

for(j in 1:100){
  
  set.seed(seed[j]) 
  March.RF.SM <- randomForest(Win.Loss ~ Rank + SOS + Seed + OSRS + DSRS, data = March.Train.R.SM, na.action = na.omit, control = list(maxit = 50))
  March.RF.SM
  summary(March.RF.SM)
  T.T <- table(predict(March.RF.SM, newdata = March.Pred, type = "response"), March.Pred$Win.Loss)
  error <- mean(c((T.T[2,1]/sum(T.T[1,1],T.T[2,1])), (T.T[1,2]/sum(T.T[2,2],T.T[1,2]))))

  
  #importance
 #March.RF.Import.SM <- data.frame(variable = names(March.RF.SM$importance[,1]), importance = March.RF.SM$importance[,1])
  #March.RF.Import.SM <- March.RF.Import.SM[order(-March.RF.Import.SM[,2]),]
  #Import.RF.SM <- barplot(March.RF.Import.SM$importance)
  #barplot(March.RF.Import.SM$importance, col = c(rep("burlywood3", 5), rep("dodgerblue3", 14)))
  
  #text(Import.RF.SM, par("usr")[3], labels = rownames(March.RF.Import.SM), srt = 45, adj = c(1,1,1,1), xpd = TRUE, cex = .9)
  
  #attaching predicted values to March.Test.R
  RF.P.Test.SM <- predict(March.RF.SM ,newdata = March.Test.R.SM, type = "response")
  March.Test.R.SM$P.WL.RF<- as.numeric(RF.P.Test.SM) - 1
  #attaching predicted values to TData.2015
  RF.P.2014.SM <- predict(March.RF.SM ,newdata = TData.SM.2014, type = "response")
  TData.SM.2014$P.WL.RF <- as.numeric(RF.P.2014.SM) - 1
  RF.P.14 <- TData.SM.2014$P.WL.RF
  RF.P.2015.SM <- predict(March.RF.SM ,newdata = TData.SM.2015, type = "response")
  TData.SM.2015$P.WL.RF <- as.numeric(RF.P.2015.SM) - 1
  RF.P.15 <- TData.SM.2015$P.WL.RF
  RF.P.2016.SM <- predict(March.RF.SM ,newdata = TData.SM.2016, type = "response")
  TData.SM.2016$P.WL.RF <- as.numeric(RF.P.2016.SM) - 1
  RF.P.16 <- TData.SM.2016$P.WL.RF
  
  
  
  
  RF.P <- list(RF.P.14, RF.P.15, RF.P.16)
  
  
  for(i in 1:3){
    R1 <- RF.P[[i]][1:32] == Correct[[i]][1:32]
    R1
    score <- (unname(table(R1)[2]) * 10)
    score
    
    R2 <- RF.P[[i]][33:48] == Correct[[i]][33:48]
    R2
    score2 <- score + (unname(table(R2)[2]) * 20)
    score2 <- ifelse(is.na(score2), score, score2)
    score2
    
    R3 <- RF.P[[i]][49:56] == Correct[[i]][49:56]
    R3
    score3 <- score2 + (unname(table(R3)[2]) * 40)
    score3 <- ifelse(is.na(score3), score2, score3)
    score3
    
    R4 <- RF.P[[i]][57:60] == Correct[[i]][57:60]
    R4
    score4 <- score3 + (unname(table(R4)[2]) * 80)
    score4 <- ifelse(is.na(score4), score3, score4)
    score4
    
    R5 <- RF.P[[i]][61:62] == Correct[[i]][61:62]
    R5
    score5 <- score4 + (unname(table(R5)[2])) * 160
    score5 <- ifelse(is.na(score5), score4, score5)
    score5
    
    R6 <- RF.P[[i]][63] == Correct[[i]][63]
    R6
    score6 <- score5 + R6 * 320
    score6 <- ifelse(is.na(score6), score5, score6)
    score6
    
    Three.Score[i] <- score6
  }
  
  error <- 
  
  RF.Scores[[j]]<- c(Three.Score[1:3], error)
}

RF.5 <- vector()
for(i in 1:100)
{
  for(k in 1:3)
  {
    RF.5 <- c(RF.5, RF.Scores[[i]][[k]])
  }
}  

R.Error <- vector()
for(i in 1:100)
{
  R.Error <- c(R.Error, RF.Scores[[i]][[4]])
};summary(R.Error)

RF.14.5 <- RF.5[S2014]
RF.15.5 <- RF.5[S2015]
RF.16.5 <- RF.5[S2016]
summary(RF.5); summary(RF.14.5); summary(RF.15.5); summary(RF.16.5)
sd(RF.5) #240

#1100   769   1219    1313   .304



##########################################################################################
####################            #Bagging                            ######################
##########################################################################################
Three.Score <- vector()
Bag.Scores <- list()
i <- 1
j <- 1

for(j in 1:100){
  
  set.seed(seed[j])
  March.Bag.SM <- randomForest(Win.Loss ~ Rank + SOS + Seed + OSRS + DSRS, data = March.Train.R.SM, mtry = 5, na.action = na.omit, control = list(maxit = 50))
  March.Bag.SM
  
  #importance
  #March.Bag.Import.SM <- data.frame(variable = names(March.Bag.SM$importance[,1]), importance = March.Bag.SM$importance[,1])
  #March.Bag.Import.SM <- March.Bag.Import.SM[order(-March.Bag.Import.SM[,2]),]
  #Import.Bag.SM <- barplot(March.Bag.Import.SM$importance)
  #text(Import.Bag.SM, par("usr")[3], labels = rownames(March.Bag.Import.SM), srt = 45, adj = c(1,1,1,1), xpd = TRUE, cex = .9)
  
  T.T <- table(predict(March.Bag.SM ,newdata = March.Pred, type = "response"), March.Pred$Win.Loss)
  error <- mean(c((T.T[2,1]/sum(T.T[1,1],T.T[2,1])), (T.T[1,2]/sum(T.T[2,2],T.T[1,2]))))

  
  #attaching predicted values to March.Test.R
  Bag.P.Test.SM <- predict(March.Bag.SM ,newdata = March.Test.R.SM, type = "response") 
  March.Test.R.SM$P.WL.Bag<- as.numeric(Bag.P.Test.SM) - 1
  #attaching predicted values to TData.2015
  Bag.P.2014.SM <- predict(March.Bag.SM ,newdata = TData.SM.2014, type = "response")
  TData.SM.2014$P.WL.Bag <- as.numeric(Bag.P.2014.SM) - 1 
  Bag.P.14 <- TData.SM.2014$P.WL.Bag
  Bag.P.2015.SM <- predict(March.Bag.SM ,newdata = TData.SM.2015, type = "response")
  TData.SM.2015$P.WL.Bag <- as.numeric(Bag.P.2015.SM) - 1
  Bag.P.15 <- TData.SM.2015$P.WL.Bag
  Bag.P.2016.SM <- predict(March.Bag.SM ,newdata = TData.SM.2016, type = "response")
  TData.SM.2016$P.WL.Bag <- as.numeric(Bag.P.2016.SM) - 1
  Bag.P.16 <- TData.SM.2016$P.WL.Bag
  
  
  #bag.yhat <- predict(March.Bag.SM, newdata =March.Test.R.SM)
  #plot(bag.yhat, March.Test.R.SM$Win.Loss)
  
  BAG.P <- list(Bag.P.14, Bag.P.15, Bag.P.16)
  
  
  for(i in 1:3){
    R1 <- BAG.P[[i]][1:32] == Correct[[i]][1:32]
    R1
    score <- (unname(table(R1)[2]) * 10)
    score
    
    R2 <- BAG.P[[i]][33:48] == Correct[[i]][33:48]
    R2
    score2 <- score + (unname(table(R2)[2]) * 20)
    score2 <- ifelse(is.na(score2), score, score2)
    score2
    
    R3 <- BAG.P[[i]][49:56] == Correct[[i]][49:56]
    R3
    score3 <- score2 + (unname(table(R3)[2]) * 40)
    score3 <- ifelse(is.na(score3), score2, score3)
    score3
    
    R4 <- BAG.P[[i]][57:60] == Correct[[i]][57:60]
    R4
    score4 <- score3 + (unname(table(R4)[2]) * 80)
    score4 <- ifelse(is.na(score4), score3, score4)
    score4
    
    R5 <- BAG.P[[i]][61:62] == Correct[[i]][61:62]
    R5
    score5 <- score4 + (unname(table(R5)[2])) * 160
    score5 <- ifelse(is.na(score5), score4, score5)
    score5
    
    R6 <- BAG.P[[i]][63] == Correct[[i]][63]
    R6
    score6 <- score5 + R6 * 320
    score6 <- ifelse(is.na(score6), score5, score6)
    score6
    
    Three.Score[i] <- score6
  }
  
  Bag.Scores[[j]]<- c(Three.Score[1:3], error)
}

BAG.5 <- vector()
for(i in 1:100)
{
  for(k in 1:3)
  {
    BAG.5 <- c(BAG.5, Bag.Scores[[i]][[k]])
  }
}  

B.Error <- vector()
for(i in 1:100)
{
  B.Error <- c(B.Error, Bag.Scores[[i]][[4]])
};summary(B.Error)

BAG.14.5 <- BAG.5[S2014]
BAG.15.5 <- BAG.5[S2015]
BAG.16.5 <- BAG.5[S2016]
summary(BAG.5); summary(BAG.14.5); summary(BAG.15.5); summary(BAG.16.5)
sd(BAG.5) #250

#1084  736   1222    1294   .314


##########################################################################################
####################            #Boosting                           ######################
##########################################################################################

Three.Score <- vector()
Boost.Scores <- list()
i <- 1
j <- 1

for(j in 1:100){
  
  set.seed(seed[j])
  
  March.Boost.SM <- gbm(Win.Loss ~ Rank + SOS + Seed + OSRS + DSRS, data = March.Train.R.SM, distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
  March.Boost.SM
  summary(March.Boost.SM)
  
  T.T<- table(round(predict(March.Boost.SM ,newdata = March.Pred, n.trees = 5000, type = "response")) - 1, March.Pred$Win.Loss)
  error <- mean(c((T.T[2,1]/sum(T.T[1,1],T.T[2,1])), (T.T[1,2]/sum(T.T[2,2],T.T[1,2]))))
 
  
  #attaching predicted values to March.Test.R
  Boost.P.Test.SM <- predict(March.Boost.SM ,newdata = March.Test.R.SM, n.trees = 5000, type = "response") 
  March.Test.R.SM$P.WL.Boost <- round(as.numeric(Boost.P.Test.SM) - 1)
  #attaching predicted values to TData.2015
  Boost.P.2014.SM <- predict(March.Boost.SM ,newdata = TData.SM.2014, n.trees = 5000, type = "response")
  TData.SM.2014$P.WL.Boost <- round(as.numeric(Boost.P.2014.SM) - 1) 
  Boost.P.14 <- TData.SM.2014$P.WL.Boost
  Boost.P.2015.SM <- predict(March.Boost.SM ,newdata = TData.SM.2015, n.trees = 5000, type = "response")
  TData.SM.2015$P.WL.Boost <- round(as.numeric(Boost.P.2015.SM) - 1) 
  Boost.P.15 <- TData.SM.2015$P.WL.Boost
  Boost.P.2016.SM <- predict(March.Boost.SM ,newdata = TData.SM.2016, n.trees = 5000, type = "response")
  TData.SM.2016$P.WL.Boost <- round(as.numeric(Boost.P.2016.SM) - 1) 
  Boost.P.16 <- TData.SM.2016$P.WL.Boost
  
  
  Boost.P <- list(Boost.P.14, Boost.P.15, Boost.P.16)
  
  
  for(i in 1:3){
    R1 <- Boost.P[[i]][1:32] == Correct[[i]][1:32]
    R1
    score <- (unname(table(R1)[2]) * 10)
    score
    
    R2 <- Boost.P[[i]][33:48] == Correct[[i]][33:48]
    R2
    score2 <- score + (unname(table(R2)[2]) * 20)
    score2 <- ifelse(is.na(score2), score, score2)
    score2
    
    R3 <- Boost.P[[i]][49:56] == Correct[[i]][49:56]
    R3
    score3 <- score2 + (unname(table(R3)[2]) * 40)
    score3 <- ifelse(is.na(score3), score2, score3)
    score3
    
    R4 <- Boost.P[[i]][57:60] == Correct[[i]][57:60]
    R4
    score4 <- score3 + (unname(table(R4)[2]) * 80)
    score4 <- ifelse(is.na(score4), score3, score4)
    score4
    
    R5 <- Boost.P[[i]][61:62] == Correct[[i]][61:62]
    R5
    score5 <- score4 + (unname(table(R5)[2])) * 160
    score5 <- ifelse(is.na(score5), score4, score5)
    score5
    
    R6 <- Boost.P[[i]][63] == Correct[[i]][63]
    R6
    score6 <- score5 + R6 * 320
    score6 <- ifelse(is.na(score6), score5, score6)
    score6
    
    Three.Score[i] <- score6
  }
  
  Boost.Scores[[j]]<- c(Three.Score[1:3], error)
}

Boost.5 <- vector()
for(i in 1:100)
{
  for(k in 1:3)
  {
    Boost.5 <- c(Boost.5, Boost.Scores[[i]][[k]])
  }
}  

Bo.Error <- vector()
for(i in 1:100)
{
  Bo.Error <- c(Bo.Error, Boost.Scores[[i]][[4]])
};summary(Bo.Error)


Boost.14.5 <- Boost.5[S2014]
Boost.15.5 <- Boost.5[S2015]
Boost.16.5 <- Boost.5[S2016]
summary(Boost.5); summary(Boost.14.5); summary(Boost.15.5); summary(Boost.16.5)
sd(Boost.5) #169

#1026   900   916   1263   .260
 


#############################
#     Results
#############################
                                    #Tree
DEC.T.14.5 <- DECTREE.5[S2014]
DEC.T.15.5 <- DECTREE.5[S2015]
DEC.T.16.5 <- DECTREE.5[S2016]
summary(DECTREE.5); summary(DEC.T.14.5); summary(DEC.T.15.5); summary(DEC.T.16.5)
sd(DECTREE.5) #210

# 1054    935   1061    1164

                                    #RF
RF.14.5 <- RF.5[S2014]
RF.15.5 <- RF.5[S2015]
RF.16.5 <- RF.5[S2016]
summary(RF.5); summary(RF.14.5); summary(RF.15.5); summary(RF.16.5)
sd(RF.5) #240

#1100   769   1219    1313

                                    #Bag
BAG.14.5 <- BAG.5[S2014]
BAG.15.5 <- BAG.5[S2015]
BAG.16.5 <- BAG.5[S2016]
summary(BAG.5); summary(BAG.14.5); summary(BAG.15.5); summary(BAG.16.5)
sd(BAG.5) #250

#1084  736   1222    1294

                                    #Boost
Boost.14.5 <- Boost.5[S2014]
Boost.15.5 <- Boost.5[S2015]
Boost.16.5 <- Boost.5[S2016]
summary(Boost.5); summary(Boost.14.5); summary(Boost.15.5); summary(Boost.16.5)
sd(Boost.5) #169

#1026   900   916   1263

              #ALL
ALL2014.5 <- c(DEC.T.14.5, RF.14.5, BAG.14.5, Boost.14.5)
ALL2015.5 <- c(DEC.T.15.5, RF.15.5, BAG.15.5, Boost.15.5)
ALL2016.5 <- c(DEC.T.16.5, RF.16.5, BAG.16.5, Boost.16.5)
ALL.5 <- c(DECTREE.5, RF.5, BAG.5, Boost.5)
summary(ALL.5); summary(ALL2014.5); summary(ALL2015.5); summary(ALL2016.5)
sd(ALL.5) ; sd(ALL2014.5); sd(ALL2015.5); sd(ALL2016.5)
#221  122   169   106

# 1058    835   1104    1259


#############
# Error rates
#############

















#Menzel probability
Menzel <- c(
M1985 = 1/193942364551905,
M1986 = 1/1373669937815220,
M1987 = 1/43704551022667,
M1988 = 1/276774001999059,
M1989 = 1/259280474310958,
M1990 = 1/1678075681014890,
M1991 = 1/338249880652442,
M1992 = 1/469047582793730,
M1993 = 1/13408278991866,
M1994 = 1/27727263851333,
M1995 = 1/26186110007156,
M1996 = 1/104551080338069,
M1997 = 1/845227238119395,
M1998 = 1/748703108637633,
M1999 = 1/410484896193270,
M2000 = 1/1100423064119270,
M2001 = 1/952178289546406,
M2002 = 1/547068025950453,
M2003 = 1/20846201031037,
M2004 = 1/107389498511317,
M2005 = 1/254915322490908,
M2006 = 1/1666864253229380,
M2007 = 1/279422931085,
M2008 = 1/17849123595964,
M2009 = 1/12013286985579,
M2010 = 1/10384582596751200,
M2011 = 1/2145236755036050,
M2012 = 1/10344125688724500,
M2013 = 1/135841277857209000,
M2014 = 1/2707252769182120,
M2015 = 1/121896657434965,
M2016 = 1/937887733917577
)

sort(Menzel)
plot(sort(Menzel)[1:31])  #removing 2007

ThreeYears <- Menzel[30:32]
plot(ThreeYears)

summary(Menzel)

#Menzel






################################################
#         Error Plots
################################################
x <- seq(0, 1, .001)
y <- 1-x
m <- c(x[1:500], y[501:1000], 0)



plot(x, 2*(x*y), type = "l", col = "blue", lwd = 2)
lines(x, m, col = "red", lwd = 2)
lines(x, -x*log10(x) -y*log10(y), col = "green", lwd = 2)


 #Error


################################################
#         ESPN Plots
################################################
hist(SCORES1, breaks = 30, xlim = c(0,1800), xaxt = "n", col = "burlywood3")
axis(1, at=seq(0,1800,100), labels = seq(0,1800,100))
abline(v=1147, lty = 2, col = "dodgerblue2", lwd = 3)


#ESPN

