c#########################################
# Reading in 2015 and Scoring
#########################################




######################################################################################################################
#
#                      Scoring the 2.8 million bracket ESPN entries 2015
#
######################################################################################################################


head(brackets)

#perfect bracket entry
Perfect <- c(01, 03, 05, 07, 09, 11, 13, 16,  #Midwest   Round 64
             17, 19, 21, 23, 25, 28, 30, 31,  #West
             33, 35, 37, 39, 42, 43, 45, 47,  #East
             49, 51, 53, 55, 58, 60, 61, 63,  #South
             01, 05, 11, 13,  #Round 32
             17, 23, 25, 31, 
             35, 39, 43, 45, 
             49, 53, 58, 63, 
             01, 11,   #Round 16
             17, 31, 
             39, 45, 
             49, 63, 
             01, 17, 45, 49,   #Round 8
             17, 49,   #Final 4
             49 #Champion
             )


Round.1.64 <- c(1, 3, 5, 7, 9, 11, 13, 16,  #Midwest   Round 64 
                17, 19, 21, 23, 25, 28, 30, 31,  #West      10 pts
                33, 35, 37, 39, 42, 43, 45, 47,  #East
                49, 51, 53, 55, 58, 60, 61, 63)  #South
Round.2.32 <- c(1, 5, 11, 13,  #Round 32
                17, 23, 25, 31,  #20 pts
                35, 39, 43, 45, 
                49, 53, 58, 63)
Round.3.16 <- c(1, 11,   #Round 16
                17, 31,   #40 pts
                39, 45, 
                49, 63)
Round.4.8 <- c(1, 17, 45, 49)  #80 pts
Round.5.4 <- c(17,49) #160 pts
Round.6.2 <- c(49)  #320 pts
             



set.seed(10)
split <- sample(seq_len(nrow(brackets)), size = floor(.60 * nrow(brackets)))

Data <- brackets[split,]

##### Scoring
len <- length(Data$Bracket_ID)
SCORES <- vector()
  
  
i <- 1
while(i <= len)
  {
    R1 <- length(which(Data[i, 2:33] == Round.1.64))
    R1
    score <- (R1 * 10)
    score
    
    R2 <- length(which(Data[i, 34:49] == Round.2.32))
    R2
    score2 <- score + (R2 * 20)
    if(is.na(score2)){score2 <- score}
    score2
    
    R3 <- length(which(Data[i, 50:57] == Round.3.16))
    R3
    score3 <- score2 + (R3 * 40)
    if(is.na(score3)){score3 <- score2}
    score3
    
    R4 <- length(which(Data[i, 58:61] == Round.4.8))
    R4
    score4 <- score3 + (R4 * 80)
    if(is.na(score4)){score4 <- score3}
    score4
    
    R5 <- length(which(Data[i, 62:63] == Round.5.4))
    R5
    score5 <- score4 + (R5 * 160)
    if(is.na(score5)){score5 <- score4}
    score5
    
    R6 <- length(which(Data[i, 64] == Round.6.2))
    R6
    score6 <- score5 + (R6 * 320)
    if(is.na(score6)){score6 <- score5}
    score6
    
    SCORES <- c(SCORES, score6)
    
    i <- i + 1
  }
  
SCORES
summary(SCORES)
#min  1q  med mean  3q  max
# 40  680 790 787   910 1240


i <- 1
while(i <= len)
{
  
round <- c(unname(Data[i, 2:33]) %in% Round.1.64, unname(Data[i, 34:49]) %in% Round.2.32, unname(Data[i, 50:57]) %in% Round.3.16,
           unname(Data[i, 58:61]) %in% Round.4.8, unname(Data[i, 62:63]) %in% Round.5.4, unname(Data[i, 64]) %in% Round.6.2)

R1 <- unname(na.omit(table(round[64])["TRUE"]))
R1
 score <- (R1 * 10)
score





}


######################################################################################################################
#
#                      Scoring the 2015 bracket from the Model's
#
######################################################################################################################

###############
#Tree
###############
Tree.P <- TData.2015$P.WL.Bag  #switched $ to correct model before running
Correct <- as.numeric(TData.2015$Win.Loss) -1

R1 <- Tree.P[1:32] == Correct[1:32]
R1
score <- (unname(table(R1)[2]) * 10)
score

R2 <- Tree.P[33:48] == Tree.C[33:48]
R2
score2 <- score + (unname(table(R2)[2]) * 20)
score2 <- ifelse(is.na(score2), score, score2)
score2

R3 <- Tree.P[49:56] == Correct[49:56]
R3
score3 <- score2 + (unname(table(R3)[2]) * 40)
score3 <- ifelse(is.na(score3), score2, score3)
score3

R4 <- Tree.P[57:60] == Correct[57:60]
R4
score4 <- score3 + (unname(table(R4)[2]) * 80)
score4 <- ifelse(is.na(score4), score3, score4)
score4

R5 <- Tree.P[61:62] == Correct[61:62]
R5
score5 <- score4 + (unname(table(R5)[2])) * 160
score5 <- ifelse(is.na(score5), score4, score5)
score5

R6 <- Tree.P[63] == Correct[63]
R6
score6 <- score5 + R6 * 320
score6 <- ifelse(is.na(score6), score5, score6)
score6


###############
#Tree   1250    1450
#RF     940     1180
#Bag    1250    1500
#Boost  1120    1110
###############


######################################################################################################################
#
#                      Scoring the 2014 - 2016 bracket from the Model's
#
######################################################################################################################

###############
#Tree
###############
Tree.P <- TData.SM.2015$P.WL.Bag  #switched $ to correct model before running
Correct <- as.numeric(TData.SM.2015$Win.Loss) -1

R1 <- Tree.P[1:32] == Correct[1:32]
R1
score <- (unname(table(R1)[2]) * 10)
score

R2 <- Tree.P[33:48] == Tree.C[33:48]
R2
score2 <- score + (unname(table(R2)[2]) * 20)
score2 <- ifelse(is.na(score2), score, score2)
score2

R3 <- Tree.P[49:56] == Correct[49:56]
R3
score3 <- score2 + (unname(table(R3)[2]) * 40)
score3 <- ifelse(is.na(score3), score2, score3)
score3

R4 <- Tree.P[57:60] == Correct[57:60]
R4
score4 <- score3 + (unname(table(R4)[2]) * 80)
score4 <- ifelse(is.na(score4), score3, score4)
score4

R5 <- Tree.P[61:62] == Correct[61:62]
R5
score5 <- score4 + (unname(table(R5)[2])) * 160
score5 <- ifelse(is.na(score5), score4, score5)
score5

R6 <- Tree.P[63] == Correct[63]
R6
score6 <- score5 + R6 * 320
score6 <- ifelse(is.na(score6), score5, score6)
score6














Three.Score <- vector()
RF.Scores <- list()
i <- 1
j <- 1

for(j in 1:10){
  
  set.seed(seed[j]) 
  March.RF.SM <- randomForest(Win.Loss ~ . , data = March.Train.R.SM, na.action = na.omit, control = list(maxit = 50))
  March.RF.SM
  summary(March.RF.SM)
  table(predict(March.RF.SM, newdata = March.Test.R.SM, type = "response"), March.Test.R.SM$Win.Loss)
  
  #importance
  March.RF.Import.SM <- data.frame(variable = names(March.RF.SM$importance[,1]), importance = March.RF.SM$importance[,1])
  March.RF.Import.SM <- March.RF.Import.SM[order(-March.RF.Import.SM[,2]),]
  Import.RF.SM <- barplot(March.RF.Import.SM$importance)
  text(Import.RF.SM, par("usr")[3], labels = rownames(March.RF.Import.SM), srt = 45, adj = c(1,1,1,1), xpd = TRUE, cex = .9)
  
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
  
  RF.Scores[[j]]<- Three.Score[1:3]
}

RF <- c(RF.Scores[[1]], RF.Scores[[2]], RF.Scores[[3]], RF.Scores[[4]], 
        RF.Scores[[5]], RF.Scores[[6]], RF.Scores[[7]], RF.Scores[[8]], 
        RF.Scores[[9]], RF.Scores[[10]])
RF.2014 <- c(RF.Scores[[1]][1], RF.Scores[[2]][1], RF.Scores[[3]][1], RF.Scores[[4]][1], 
             RF.Scores[[5]][1], RF.Scores[[6]][1], RF.Scores[[7]][1], RF.Scores[[8]][1], 
             RF.Scores[[9]][1], RF.Scores[[10]][1])
RF.2015 <- c(RF.Scores[[1]][2], RF.Scores[[2]][2], RF.Scores[[3]][2], RF.Scores[[4]][2], 
             RF.Scores[[5]][2], RF.Scores[[6]][2], RF.Scores[[7]][2], RF.Scores[[8]][2], 
             RF.Scores[[9]][2], RF.Scores[[10]][2])
RF.2016 <- c(RF.Scores[[1]][3], RF.Scores[[2]][3], RF.Scores[[3]][3], RF.Scores[[4]][3], 
             RF.Scores[[5]][3], RF.Scores[[6]][3], RF.Scores[[7]][3], RF.Scores[[8]][3], 
             RF.Scores[[9]][3], RF.Scores[[10]][3])
summary(RF); summary(RF.2014); summary(RF.2015); summary(RF.2016)




