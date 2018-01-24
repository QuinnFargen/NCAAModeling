# Quinn Fargen
# SDSU Math 401 S2
# 
# DataShaping


######################################################################################################################
#
#                       Reshaping and Summarizing into Model Data (TournData.R)
#
######################################################################################################################



#####################################################################################################################
Columns<- c(1:3,11,13:15)
#Conf <- gsub("\\s*\\([^\\)]+\\)","",as.character(Teamsr3))



for(i in 1){
  #2003 Season 1
  SZN3 <- SZNTeams[[1]][[1]][2:65]
  SZN3 <- Teams[(which(Teams$Team_Id %in% SZN3)),]
  Rank3 <- R3[which(R3$School %in% SZN3$Team_Name),]
  Need3 <- c(32,2,25,38,164,57,31,76,42,35,37,96,23,24,46,6
             ,165,12,40,98,7,39,188,1,3,8,20,90,18,5,34,19
             ,13,26,43,15,11,28,30,66,4,22,251,63,177,95
             ,29,33,9,10,116,48,291,64,47,108,173,70,197
             ,17,105,69,14,21)
  Rank3<- cbind(SZN3$Team_Id, R3[Need3,Columns])
  Rank3$Conf <- gsub("\\s*\\([^\\)]+\\)","",as.character(Rank3$Conf))
  colnames(Rank3)<- c("ID", "Rank", "School", "Conf", "SOS", "OSRS", "DSRS", "SRS")
  
  #2004 Season 2
  SZN4 <- SZNTeams[[2]][[1]][2:65]
  SZN4 <- Teams[(which(Teams$Team_Id %in% SZN4)),]
  Rank4 <- R4[which(R4$School %in% SZN4$Team_Name),]
  Need4 <- c(56, 28, 282, 23, 39, 34, 37, 10, 2, 79, 59, 1, 
             141, 105, 21, 265, 4, 8, 88, 19, 17, 9, 
             189, 11, 54, 18, 31, 38, 20, 213, 77, 13, 24, 6, 
             80, 3, 90, 15, 128, 25, 49, 44, 29, 33, 5, 16, 32, 12,
             26, 41, 124, 86, 244, 43, 42, 81, 150,
             22, 187, 36, 14, 53, 7, 30)
  Rank4 <- cbind(SZN4$Team_Id, R4[Need4, Columns])
  Rank4$Conf <-  gsub("\\s*\\([^\\)]+\\)","",as.character(Rank4$Conf))
  colnames(Rank4) <- c("ID", "Rank", "School", "Conf", "SOS", "OSRS", "DSRS", "SRS")
  
  #2005 Season 3
  SZN5 <- SZNTeams[[3]][[1]][2:65]
  SZN5 <- Teams[(which(Teams$Team_Id %in% SZN5)),]
  Rank5 <- R5[which(R5$School %in% SZN5$Team_Name),]
  Need5 <- c(19, 15, 26, 127, 55, 166, 16, 14, 49, 224,
             3, 182, 200, 9, 45, 17, 30, 2, 24, 41, 6, 12, 
             4, 39, 7, 36, 34, 195, 21, 61, 44, 116, 1, 50,
             181, 94, 10, 5, 85, 66, 102, 25, 40, 161, 78, 64,
             22, 23, 20, 46, 210, 63, 89, 27, 33, 56, 69, 13, 8,
             11, 38, 47, 151, 18)
  Rank5 <- cbind(SZN5$Team_Id, R5[Need5, Columns])
  Rank5$Conf <-  gsub("\\s*\\([^\\)]+\\)","",as.character(Rank5$Conf))
  colnames(Rank5) <- c("ID", "Rank", "School", "Conf", "SOS", "OSRS", "DSRS", "SRS")
  
  #2006 Season 4
  SZN6 <- SZNTeams[[4]][[1]][2:65]
  SZN6 <- Teams[(which(Teams$Team_Id %in% SZN6)),]
  Rank6 <- R5[which(R6$School %in% SZN6$Team_Name),]
  Need6 <- c(51, 46, 153, 15, 16, 186, 21, 38, 78, 32, 3, 136,
             2, 4, 58, 35, 20, 31, 12, 36, 81, 19, 5, 89, 25, 10, 42,
             7, 33, 176, 104, 110, 28, 52, 8, 40, 112, 14, 37,
             121, 80, 91, 13, 63, 55, 82, 103, 222, 39, 17, 1, 23,
             57, 11, 64, 71, 6, 9, 18, 73, 41, 88, 24, 54)
  Rank6 <- cbind(SZN6$Team_Id, R6[Need6, Columns])
  Rank6$Conf <-  gsub("\\s*\\([^\\)]+\\)","",as.character(Rank6$Conf))
  colnames(Rank6) <- c("ID", "Rank", "School", "Conf", "SOS", "OSRS", "DSRS", "SRS")
  
  #2007 Season 5
  SZN7 <- SZNTeams[[5]][[1]][2:65]
  SZN7 <- Teams[(which(Teams$Team_Id %in% SZN7)),]
  Rank7 <- R7[which(R7$School %in% SZN7$Team_Name),]
  Need7 <- c(157, 18, 38, 144, 39, 30, 52, 177, 49, 73, 11, 188,
             2, 85, 8, 12, 51, 113, 31, 16, 282, 3, 13, 128, 
             20, 34, 10, 7, 94, 17, 59, 92, 174, 1, 172, 15, 4, 
             75, 97, 21, 105, 14, 27, 40, 53, 118, 26, 22, 5, 65, 
             6, 45, 37, 67, 47, 19, 46, 29, 42, 185, 69, 9, 132, 32)
  Rank7 <- cbind(SZN7$Team_Id, R7[Need7, Columns])
  Rank7$Conf <-  gsub("\\s*\\([^\\)]+\\)","",as.character(Rank7$Conf))
  colnames(Rank7) <- c("ID", "Rank", "School", "Conf", "SOS", "OSRS", "DSRS", "SRS")
  
  #2008 Season 6
  SZN8 <- SZNTeams[[6]][[1]][2:65]
  SZN8 <- Teams[(which(Teams$Team_Id %in% SZN8)),]
  Rank8 <- R8[which(R8$School %in% SZN8$Team_Name),]
  Need8 <- c(165, 25, 35, 167, 44, 146, 129, 32, 41, 12, 23,
             136, 92, 27, 26, 5, 88, 8, 82, 24, 21, 1, 20, 77, 62,
             7, 9, 3, 36, 16, 30, 307, 180, 2, 19, 40, 90, 43, 22, 
             127, 31, 113, 106, 75, 56, 45, 17, 71, 10, 11, 18, 4, 
             142, 54, 28, 174, 49, 47, 60, 15, 13, 112, 6, 14
  )
  Rank8 <- cbind(SZN8$Team_Id, R8[Need8, Columns])
  Rank8$Conf <-  gsub("\\s*\\([^\\)]+\\)","",as.character(Rank8$Conf))
  colnames(Rank8) <- c("ID", "Rank", "School", "Conf", "SOS", "OSRS", "DSRS", "SRS")
  
  #2009 Season 7
  SZN9 <- SZNTeams[[7]][[1]][2:65]
  SZN9 <- Teams[(which(Teams$Team_Id %in% SZN9)),]
  Rank9 <- R9[which(R9$School %in% SZN9$Team_Name),]
  Need9 <- c(103, 142, 40, 17, 189, 51, 52, 21, 30, 235, 20, 79, 
             2, 109, 110, 78, 6, 104, 44, 12, 26, 11, 5, 41, 22, 48, 
             4, 39, 13, 46, 58, 7, 186, 170, 83, 1, 88, 35, 10, 28,
             3, 127, 18, 205, 151, 129, 71, 15, 59, 29, 23, 50, 8, 27,
             33, 57, 76, 14, 100, 19, 16, 9, 36, 24)             
  Rank9 <- cbind(SZN9$Team_Id, R9[Need9, Columns])
  Rank9$Conf <-  gsub("\\s*\\([^\\)]+\\)","",as.character(Rank9$Conf))
  colnames(Rank9) <- c("ID", "Rank", "School", "Conf", "SOS", "OSRS", "DSRS", "SRS")
  
  #2010 Season 8
  SZN10 <- SZNTeams[[8]][[1]][2:65]
  SZN10 <- Teams[(which(Teams$Team_Id %in% SZN10)),]
  Rank10 <- R10[which(R10$School %in% SZN10$Team_Name),]
  Need10 <- c(249, 10, 28, 8, 15, 20, 69, 1, 162, 47, 
              27, 16, 30, 45, 86, 2, 5, 4, 191, 31,
              26, 12, 23, 21, 14, 107, 165, 71, 42, 124,
              180, 46, 37, 159, 101, 6, 44, 52, 29, 13,
              60, 200, 110, 54, 153, 72, 35, 3, 43, 19,
              9, 24, 39, 33, 38, 25, 167, 17, 51, 22,
              7, 11, 102, 18)
  Rank10 <- cbind(SZN10$Team_Id, R10[Need10, Columns])
  Rank10$Conf <-  gsub("\\s*\\([^\\)]+\\)","",as.character(Rank10$Conf))
  colnames(Rank10) <- c("ID", "Rank", "School", "Conf", "SOS", "OSRS", "DSRS", "SRS")
  
  #2011 Season 9
  SZN11 <- SZNTeams[[9]][[1]][2:65]
  SZN11 <- Teams[(which(Teams$Team_Id %in% SZN11)),]
  Rank11 <- R11[which(R11$School %in% SZN11$Team_Name),]
  Need11 <- c(127, 18, 38, 188, 123, 52, 12, 21, 33,        #Removed 4 playin teams
              16, 2, 19, 35, 50, 28, 60, 31, 224, 17,
              110, 3, 29, 6, 125, 13, 25, 83, 30, 34, 26,
              120, 138, 9, 14, 74, 1, 66, 44, 4, 108,
              8, 55, 15, 140, 37, 159, 10, 42, 56, 5,
              40, 49, 164, 27, 234, 36, 70, 22, 23, 
              7, 20, 11, 99, 45)
  Rank11 <- cbind(SZN11$Team_Id, R11[Need11, Columns])
  Rank11$Conf <-  gsub("\\s*\\([^\\)]+\\)","",as.character(Rank11$Conf))
  colnames(Rank11) <- c("ID", "Rank", "School", "Conf", "SOS", "OSRS", "DSRS", "SRS")
  
  #2012 Season 10
  SZN12 <- SZNTeams[[10]][[1]][2:65]
  SZN12 <- Teams[(which(Teams$Team_Id %in% SZN12)),]
  Rank12 <- R12[which(R12$School %in% SZN12$Team_Name),]
  Need12 <- c(27, 16, 38, 40, 31, 70, 87, 35, 32,         #Removed 4 playin teams
              79, 136, 12, 10, 21, 15, 25, 72, 9,
              29, 3, 22, 1, 106, 51, 17, 173, 154, 13, 14,
              30, 5, 7, 103, 62, 37, 19, 63, 207,
              4, 41, 80, 2, 24, 76, 57, 55, 73, 69, 20,
              39, 6, 47, 23, 123, 28, 61, 18, 149, 34,
              190, 36, 11, 8, 50)
  Rank12 <- cbind(SZN12$Team_Id, R12[Need12, Columns])
  Rank12$Conf <-  gsub("\\s*\\([^\\)]+\\)","",as.character(Rank12$Conf))
  colnames(Rank12) <- c("ID", "Rank", "School", "Conf", "SOS", "OSRS", "DSRS", "SRS")
  
  #2013 Season 11
  SZN13 <- SZNTeams[[11]][[1]][2:65]
  SZN13 <- Teams[(which(Teams$Team_Id %in% SZN13)),]
  Rank13 <- R13[which(R13$School %in% SZN13$Team_Name),]
  Need13 <- c(69, 167, 13, 54, 75, 50, 53, 26, 44,         #Removed 4 playin teams
              35, 17, 73, 4, 124, 3, 18, 7, 114, 38,
              1, 113, 25, 177, 6, 36, 55, 2,
              31, 43, 14, 5, 12, 16, 27, 20, 171,
              252, 29, 37, 103, 23, 148, 34, 9, 46, 19,
              33, 133, 10, 130, 39, 193, 28, 24, 8, 59,
              42, 40, 15, 74, 51, 163, 22, 11)
  Rank13 <- cbind(SZN13$Team_Id, R13[Need13, Columns])
  Rank13$Conf <-  gsub("\\s*\\([^\\)]+\\)","",as.character(Rank13$Conf))
  colnames(Rank13) <- c("ID", "Rank", "School", "Conf", "SOS", "OSRS", "DSRS", "SRS")
  
  #2014 Season 12
  SZN14 <- SZNTeams[[12]][[1]][2:65]
  SZN14 <- Teams[(which(Teams$Team_Id %in% SZN14)),]
  Rank14 <- R14[which(R14$School %in% SZN14$Team_Name),]
  Need14 <- c(198, 153, 2, 34, 23, 46, 176, 26, 229, 53,     #Removed 4 playin teams
              20, 16, 58, 124, 6, 155, 3, 57, 25, 54,
              11, 4, 41, 15, 1, 86, 56, 35, 96, 
              12, 7, 81, 112, 66, 60, 36, 84, 27,
              17, 29, 8, 24, 18, 48, 28, 100, 65, 47,
              33, 22, 19, 37, 83, 13, 122, 30, 9,
              14, 138, 171, 180, 21, 5, 209)
  Rank14 <- cbind(SZN14$Team_Id, R14[Need14, Columns])
  Rank14$Conf <-  gsub("\\s*\\([^\\)]+\\)","",as.character(Rank14$Conf))
  colnames(Rank14) <- c("ID", "Rank", "School", "Conf", "SOS", "OSRS", "DSRS", "SRS")
  
  #2015 Season 13
  SZN15 <- SZNTeams[[13]][[1]][2:65]
  SZN15 <- Teams[(which(Teams$Team_Id %in% SZN15)),]
  Rank15 <- R15[which(R15$School %in% SZN15$Team_Name),]
  Need15 <- c(155, 4, 26, 16, 146, 72, 18, 54,               #Removed 4 playin teams
              163, 43, 52, 2, 144, 22, 37, 94, 7, 280,
              102, 32, 20, 15, 13, 1, 202, 14, 49,
              29, 17, 50, 172, 25, 109, 9, 134, 36,
              11, 10, 12, 31, 48, 27, 34, 196, 47, 63,
              33, 46, 19, 245, 136, 111, 41, 8, 35, 86,
              5, 6, 24, 21, 3, 128, 105, 23)
  Rank15 <- cbind(SZN15$Team_Id, R15[Need15, Columns])
  Rank15$Conf <-  gsub("\\s*\\([^\\)]+\\)","",as.character(Rank15$Conf))
  colnames(Rank15) <- c("ID", "Rank", "School", "Conf", "SOS", "OSRS", "DSRS", "SRS")
  
  #2016 Season 14
  SZN16 <- SZNTeams[[14]][[1]][2:65]
  SZN16 <- Teams[(which(Teams$Team_Id %in% SZN16)),]
  Rank16 <- R16[which(R16$School %in% SZN16$Team_Name),]
  Need16 <- c(13, 90, 239, 26, 138, 25, 30, 118, 31, 49,     #Removed 4 playin teams
              27, 127, 55, 12, 292, 103, 23, 269, 72,
              297, 11, 113, 21, 22, 2, 8, 24, 15, 38,
              4, 125, 3, 75, 36, 10, 18, 59, 29, 52,
              9, 100, 34, 70, 48, 112, 35, 84, 32, 
              16, 44, 141, 93, 41, 28, 37, 1,
              5, 150, 6, 140, 19, 42, 14, 71)
  Rank16 <- cbind(SZN16$Team_Id, R16[Need16, Columns])
  Rank16$Conf <-  gsub("\\s*\\([^\\)]+\\)","",as.character(Rank16$Conf))
  colnames(Rank16) <- c("ID", "Rank", "School", "Conf", "SOS", "OSRS", "DSRS", "SRS")
}




############################################################################################

############################################################################################


### Tournament games needed
TourneyGames  <- TournDetail[,c(1,3,5)]
first <- cumsum(c(1, rep(64,7)))
playins <- c(first, 513:516, 580:583, 647:650, 714:717, 781:784, 848:851)
TourneyGames <- TourneyGames[-playins, ]  #This is every match up.
row.names(TourneyGames)<- c(1:length(TourneyGames$Season))

#Look through TournDetail to make a list of tourney teams need for specific years.

TTeams0317 <- function(TournDetail)
{
  TTeamsSZN <- as.list(c(2003:2016))
  TournGames<- as.vector(table(TournDetail$Season))
  TournSZN <- c(0, cumsum(TournGames)) 
  
  yr<-1
  start<-1
  end<-2
  while(yr <= 14)
  {
    TDStart <- TournSZN[start] +1
    TDEnd <- TournSZN[end]
    CurrentYr <- TournDetail[(TDStart:TDEnd), ]
    UniqueCurrentTeams <- c(CurrentYr[TournGames[yr],3], unique(CurrentYr$Lteam))
    
    TTeamsSZN[[yr]]<- list(c(TTeamsSZN[[yr]] , UniqueCurrentTeams))
    
    yr <- yr +1
    start <- start +1
    end <- end +1
  }
  
  TTeamsSZN
}


for(i in 1){
  
  SZNTeams <- TTeams0317(TournDetail)
  
  match <- match(c(1411), c(SZNTeams[[1]][[1]]))
  SZNTeams[[1]][[1]] <- SZNTeams[[1]][[1]][-c(match)]
  match <- match(c(1250), c(SZNTeams[[2]][[1]]))
  SZNTeams[[2]][[1]] <- SZNTeams[[2]][[1]][-c(match)]
  match <- match(c(1105), c(SZNTeams[[3]][[1]]))
  SZNTeams[[3]][[1]] <- SZNTeams[[3]][[1]][-c(match)]
  match <- match(c(1214), c(SZNTeams[[4]][[1]]))
  SZNTeams[[4]][[1]] <- SZNTeams[[4]][[1]][-c(match)]
  match <- match(c(1197), c(SZNTeams[[5]][[1]]))
  SZNTeams[[5]][[1]] <- SZNTeams[[5]][[1]][-c(match)]
  match <- match(c(1164), c(SZNTeams[[6]][[1]]))
  SZNTeams[[6]][[1]] <- SZNTeams[[6]][[1]][-c(match)]
  match <- match(c(1106), c(SZNTeams[[7]][[1]]))
  SZNTeams[[7]][[1]] <- SZNTeams[[7]][[1]][-c(match)]
  match <- match(c(1457), c(SZNTeams[[8]][[1]]))
  SZNTeams[[8]][[1]] <- SZNTeams[[8]][[1]][-c(match)] #2010
  match <- match(c(1106,1412,1425,1114), c(SZNTeams[[9]][[1]])) #513:516
  SZNTeams[[9]][[1]] <- SZNTeams[[9]][[1]][-c(match)]
  match <- match(c(1233,1249,1143,1290), c(SZNTeams[[10]][[1]])) #580:583
  SZNTeams[[10]][[1]] <- SZNTeams[[10]][[1]][-c(match)] #2012
  match <- match(c(1251,1292,1254,1129), c(SZNTeams[[11]][[1]])) #647:650
  SZNTeams[[11]][[1]] <- SZNTeams[[11]][[1]][-c(match)]
  match <- match(c(1291,1411,1462,1234), c(SZNTeams[[12]][[1]])) #714:717
  SZNTeams[[12]][[1]] <- SZNTeams[[12]][[1]][-c(match)] #2014
  match <- match(c(1264,1140,1316,1129), c(SZNTeams[[13]][[1]])) #781:784
  SZNTeams[[13]][[1]] <- SZNTeams[[13]][[1]][-c(match)]
  match <- match(c(1435,1380,1409,1192), c(SZNTeams[[14]][[1]])) #848:851
  SZNTeams[[14]][[1]] <- SZNTeams[[14]][[1]][-c(match)]  #2016
}

############################################################################################
############################################################################################














#Gonna make a summary for each team needed in each years tourney
########## Gonna change cols to data desired

names <- colnames(RegDetail[, c(1,3:6,9:34)])

TeamAvgs <- as.list(c(2003:2016))


yr <- 1
R <- 3
entryYr <- 2003
while(yr <= 14)
{
  TCount <- 2 #bc first number is the year
  Rank <- paste0("Rank", R)
  Rank <- get(Rank)
  EveryYear <- data.frame()
  
  
  while(TCount <= length(SZNTeams[[yr]][[1]]))
  {
    TeamID <- SZNTeams[[yr]][[1]][TCount]
    
    TeamSZNstats <- rbind(RegDetail[which(RegDetail$Wteam == TeamID),],  #Puts all the losses at the end of the dataset
                          RegDetail[which(RegDetail$Lteam == TeamID),])
    TeamSZNstats<- TeamSZNstats[order(TeamSZNstats[,1]), c(1,3:6,9:34)] 
    row.names(TeamSZNstats)<- c(1:length(TeamSZNstats[,1]))
    TeamSZNstats <- TeamSZNstats[which(TeamSZNstats$Season == entryYr),]
    
    
    wins <- sort(unname(table(TeamSZNstats$Wteam)))
    wins <- tail(wins, 1)
    losses <- length(TeamSZNstats$Wteam) - wins
    
    statsW <- round(unname(colMeans(TeamSZNstats[(1:wins) , c(3,5:31)])), 1)                             #Stats for winning games
    statsL <- round(unname(colMeans(TeamSZNstats[(wins + 1):length(TeamSZNstats$Wteam) ,c(3,5:31)])), 1) #Stats for losing games
    
    Wfgpct <- 100 * (statsW[3]/statsW[4])
    Wfg3pct <- 100 * (statsW[5]/statsW[6])
    Wftpct <- 100 * (statsW[7]/statsW[8])
    
    Lfgpct <- 100 * (statsL[16]/statsL[17])
    Lfg3pct <- 100 * (statsL[18]/statsL[19])
    Lftpct <- 100 * (statsL[20]/statsL[21])
    
    #WWscore, WLscore, fgpct, fg3pct, ftpct, Wor, Wdr, Wast, Wto, Wstl, Wblk, Wpf
    statsW <- round(c(statsW[1:2], Wfgpct, Wfg3pct, Wftpct, statsW[9:15]), 2)
    
    #LWscore, LLscore, fgpct, fg3pct, ftpct, Lor, Ldr, Last, Lto, Lstl, Lblk, Lpf
    statsL <- round(c(statsL[1:2], Lfgpct, Lfg3pct, Lftpct, statsL[22:28]), 2)
    
    
    Rankings<- unname(Rank[which(Rank$ID == TeamID),-c(4,8)])
    
    Stats <- data.frame(c(Rankings, wins, losses, statsW, statsL))
    
    colnames(Stats) <- c("ID", "Rank", "School", "SOS", "OSRS", "DSRS",
                         "Wins", "Losses", paste0("W", names[c(3, 5, 6, 8, 10, 12:18)]), 
                         paste0("L", names[c(3, 5, 19, 21, 23, 25:31)]))
    
    EveryYear <- rbind(EveryYear, Stats)
    
    TeamAvgs[[yr]] <- EveryYear
    
    TCount <- TCount +1
  }
  
  
  
  R <- R +1
  yr <- yr + 1
  entryYr <- entryYr + 1
}


for(i in 1){
  Sum2003 <- TeamAvgs[[1]]
  row.names(Sum2003)<- Sum2003[,1]
  Sum2004 <- TeamAvgs[[2]]
  Sum2005 <- TeamAvgs[[3]]
  Sum2006 <- TeamAvgs[[4]]
  Sum2007 <- TeamAvgs[[5]]
  Sum2008 <- TeamAvgs[[6]]
  Sum2009 <- TeamAvgs[[7]]
  Sum2010 <- TeamAvgs[[8]]
  Sum2011 <- TeamAvgs[[9]]
  Sum2012 <- TeamAvgs[[10]]
  Sum2013 <- TeamAvgs[[11]]
  Sum2014 <- TeamAvgs[[12]]
  Sum2015 <- TeamAvgs[[13]]
  Sum2016 <- TeamAvgs[[14]]
}







  

############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################

#works but with random placement of T1 and T2

#This will be to make a difference for every team matchup in the tournaments.

### Tournament games needed, with no playins
TourneyGames  <- TournDetail[,c(1,3,5)]
first <- cumsum(c(1, rep(64,7)))
playins <- c(first, 513:516, 580:583, 647:650, 714:717, 781:784, 848:851)
TourneyGames <- TourneyGames[-playins, ]  #This is every match up.
row.names(TourneyGames)<- c(1:length(TourneyGames$Season))


Sum <- rbind(Sum2003, Sum2004, Sum2005, Sum2006, Sum2007, Sum2008, Sum2009,
             Sum2010, Sum2011, Sum2012, Sum2013, Sum2014, Sum2015, Sum2016)

#Wichita and Kentucky went undefeated in the reg season therefore no losing data
#going to put a average of losing teams to replace NA's for not ever losing a game
replace <- unname(colMeans(Sum[,22:33]))

#Wichita 2014
Sum2014[45, 21:32] <- replace
#Kentucky 2015
Sum2015[63, 21:32] <- replace

years <- c(rep(2003, 64), rep(2004, 64), rep(2005, 64), rep(2006, 64), rep(2007, 64), rep(2008, 64), 
           rep(2009, 64), rep(2010, 64), rep(2011, 64), rep(2012, 64), rep(2013, 64), rep(2014, 64),
           rep(2015, 64), rep(2016, 64))
Sum$lenyear.ID <- paste0(years, ".", Sum$ID)
lenyear.ID <- paste0(years, ".", Sum$ID)

#remove playing team seeds
Seed <- Seeds[Seeds$Team %in% lenyear.ID, ]
SumSeeds <- as.numeric(Seed[match(Sum$lenyear.ID, Seed$Team), 1])



#Differencing the matchups
Sum <- cbind(lenyear.ID, SumSeeds, Sum[,2:32])
rownames(Sum)<- c(1:length(Sum$lenyear.ID))



#Randomly select half to have team one win
set.seed(5)
TEAM1wins <- sample(1:882, 441)
list882 <- c(1:882)
list882 <- ifelse(list882 %in% TEAM1wins, list882 <- TRUE, list882 <- FALSE)

i <- 1
TournData.R <- data.frame()
Round <- rep(c(rep(1,32), rep(2,16), rep(3, 8), rep(4,4), 5, 5, 6)  ,14)
while(i <= 882)
{
  ifelse(list882[i] == TRUE, Team1 <- 2, Team1 <- 3 )
  ifelse(list882[i] == FALSE, Team2 <- 2, Team2 <- 3 )
  
  Team1 <- TourneyGames[i, Team1]
  Team2 <- TourneyGames[i, Team2]
  Yr <- TourneyGames[i, 1]
  
  T1 <- paste0(Yr, ".", Team1)
  T2 <- paste0(Yr, ".", Team2) 
  
  TM1 <- as.numeric(unname(Sum[Sum$lenyear.ID == T1,  c(2:3,5:33)]))
  TM2 <- as.numeric(unname(Sum[Sum$lenyear.ID == T2,  c(2:3,5:33)]))
  diff <- TM1 - TM2
  
  matchup <- c(Yr, Team1, Team2, Round[i], diff)
  
  TournData.R <- rbind(TournData.R, matchup)
  
  i <- i + 1
}

colnames(TournData.R) <- c("Year", "Team1", "Team2", "Round", "Seed", colnames(Sum)[c(3,5:33)])
TournData.R$Win.Loss <- as.factor(ifelse(list882, TournData.R$Win.Loss <- 1, TournData.R$Win.Loss <- 0))





############################################################################################
############################################################################################
































##########################################

#         Gonna try to seperate the differences of wins and losses

#         TournData.R.SM is the data for smaller number of variables no W and L just S for Season stats

##########################################
#Gonna make a summary for each team needed in each years tourney
########## Gonna change cols to data desired

names <- colnames(RegDetail[, c(1,3:6,9:34)])

TeamAvgs <- as.list(c(2003:2016))


yr <- 1
R <- 3
entryYr <- 2003
while(yr <= 14)
{
  TCount <- 2 #bc first number is the year
  Rank <- paste0("Rank", R)
  Rank <- get(Rank)
  EveryYear <- data.frame()
  
  
  while(TCount <= length(SZNTeams[[yr]][[1]]))
  {
    TeamID <- SZNTeams[[yr]][[1]][TCount]
    
    TeamSZNstats <- rbind(RegDetail[which(RegDetail$Wteam == TeamID),],  #Puts all the losses at the end of the dataset
                          RegDetail[which(RegDetail$Lteam == TeamID),])
    TeamSZNstats<- TeamSZNstats[order(TeamSZNstats[,1]), c(1,3:6,9:34)] 
    row.names(TeamSZNstats)<- c(1:length(TeamSZNstats[,1]))
    TeamSZNstats <- TeamSZNstats[which(TeamSZNstats$Season == entryYr),]
    
    
    wins <- sort(unname(table(TeamSZNstats$Wteam)))
    wins <- tail(wins, 1)
    losses <- length(TeamSZNstats$Wteam) - wins
    
    statnames <- colnames(TeamSZNstats[,c(3, 6:18)])
    statsW <- unname(colSums(TeamSZNstats[(1:wins), c(3, 6:18)]))
    statsL <- unname(colSums(TeamSZNstats[(wins + 1):length(TeamSZNstats$Wteam), c(5, 19:31)]))
    stats <- round(colSums(rbind(statsW, statsL))/length(TeamSZNstats$Wteam), 1)
    
    Sfgpct <- 100 * (stats[2]/stats[3])
    Sfg3pct <- 100 * (stats[4]/stats[5])
    Sftpct <- 100 * (stats[6]/stats[7])

    #Wins, Losses, Score, Sfgpct, Sfg3pct, Sftpct, Sor, Sdr, Sast, Sto, Sstl, Sblk, Spf
    STATS <- round(c(wins, losses, stats[1], Sfgpct, Sfg3pct, Sftpct, stats[8:14]), 2)
    

    
    Rankings<- unname(Rank[which(Rank$ID == TeamID),-c(4,8)])
    
    Stats <- data.frame(c(Rankings,STATS))
    
    colnames(Stats) <- c("ID", "Rank", "School", "SOS", "OSRS", "DSRS",
                         "Wins", "Losses", "Score", "Sfgpct", "Sfg3pct", 
                         "Sftpct", "Sor", "Sdr", "Sast", "Sto", "Sstl", 
                         "Sblk", "Spf")
    
    EveryYear <- rbind(EveryYear, Stats)
    
    TeamAvgs[[yr]] <- EveryYear
    
    TCount <- TCount +1
  }
  
  
  
  R <- R +1
  yr <- yr + 1
  entryYr <- entryYr + 1
}


for(i in 1){
  SZN2003 <- TeamAvgs[[1]]
  #row.names(Sum2003)<- Sum2003[,1]
  SZN2004 <- TeamAvgs[[2]]
  SZN2005 <- TeamAvgs[[3]]
  SZN2006 <- TeamAvgs[[4]]
  SZN2007 <- TeamAvgs[[5]]
  SZN2008 <- TeamAvgs[[6]]
  SZN2009 <- TeamAvgs[[7]]
  SZN2010 <- TeamAvgs[[8]]
  SZN2011 <- TeamAvgs[[9]]
  SZN2012 <- TeamAvgs[[10]]
  SZN2013 <- TeamAvgs[[11]]
  SZN2014 <- TeamAvgs[[12]]
  SZN2015 <- TeamAvgs[[13]]
  SZN2016 <- TeamAvgs[[14]]
}

#####################################################################################
#Wichita and Kentucky went undefeated in the reg season therefore no losing data
#going to put a average of losing teams to replace NA's for not ever losing a game

for(i in 1){
#Wichita 2014
TeamID <- SZNTeams[[12]][[1]][46]
TeamSZNstats <- rbind(RegDetail[which(RegDetail$Wteam == TeamID),],  #Puts all the losses at the end of the dataset
                      RegDetail[which(RegDetail$Lteam == TeamID),])
TeamSZNstats<- TeamSZNstats[order(TeamSZNstats[,1]), c(1,3:6,9:34)] 
row.names(TeamSZNstats)<- c(1:length(TeamSZNstats[,1]))
TeamSZNstats <- TeamSZNstats[which(TeamSZNstats$Season == 2014),]
wins <- sort(unname(table(TeamSZNstats$Wteam)))
wins <- tail(wins, 1)
losses <- length(TeamSZNstats$Wteam) - wins
statnames <- colnames(TeamSZNstats[,c(3, 6:18)])
statsW <- unname(colSums(TeamSZNstats[(1:wins), c(3, 6:18)]))
stats <- round(statsW/length(TeamSZNstats$Wteam), 1)
Sfgpct <- 100 * (stats[2]/stats[3])
Sfg3pct <- 100 * (stats[4]/stats[5])
Sftpct <- 100 * (stats[6]/stats[7])
#Score, Sfgpct, Sfg3pct, Sftpct, Sor, Sdr, Sast, Sto, Sstl, Sblk, Spf
Wichita <- round(c(stats[1], Sfgpct, Sfg3pct, Sftpct, stats[8:14]), 2)

SZN2014[45, 9:19] <- Wichita
#Kentucky 2015

TeamID <- SZNTeams[[13]][[1]][64]
TeamSZNstats <- rbind(RegDetail[which(RegDetail$Wteam == TeamID),],  #Puts all the losses at the end of the dataset
                      RegDetail[which(RegDetail$Lteam == TeamID),])
TeamSZNstats<- TeamSZNstats[order(TeamSZNstats[,1]), c(1,3:6,9:34)] 
row.names(TeamSZNstats)<- c(1:length(TeamSZNstats[,1]))
TeamSZNstats <- TeamSZNstats[which(TeamSZNstats$Season == 2015),]
wins <- sort(unname(table(TeamSZNstats$Wteam)))
wins <- tail(wins, 1)
losses <- length(TeamSZNstats$Wteam) - wins
statnames <- colnames(TeamSZNstats[,c(3, 6:18)])
statsW <- unname(colSums(TeamSZNstats[(1:wins), c(3, 6:18)]))
stats <- round(statsW/length(TeamSZNstats$Wteam), 1)
Sfgpct <- 100 * (stats[2]/stats[3])
Sfg3pct <- 100 * (stats[4]/stats[5])
Sftpct <- 100 * (stats[6]/stats[7])
#Score, Sfgpct, Sfg3pct, Sftpct, Sor, Sdr, Sast, Sto, Sstl, Sblk, Spf
Kentucky <- round(c(stats[1], Sfgpct, Sfg3pct, Sftpct, stats[8:14]), 2)

SZN2015[63, 9:19] <- Kentucky

}


#works but with random placement of T1 and T2

#This will be to make a difference for every team matchup in the tournaments.

### Tournament games needed, with no playins
TourneyGames  <- TournDetail[,c(1,3,5)]
first <- cumsum(c(1, rep(64,7)))
playins <- c(first, 513:516, 580:583, 647:650, 714:717, 781:784, 848:851)
TourneyGames <- TourneyGames[-playins, ]  #This is every match up.
row.names(TourneyGames)<- c(1:length(TourneyGames$Season))


SZN <- rbind(SZN2003, SZN2004, SZN2005, SZN2006, SZN2007, SZN2008, SZN2009,
             SZN2010, SZN2011, SZN2012, SZN2013, SZN2014, SZN2015, SZN2016)
years <- c(rep(2003, 64), rep(2004, 64), rep(2005, 64), rep(2006, 64), rep(2007, 64), rep(2008, 64), 
           rep(2009, 64), rep(2010, 64), rep(2011, 64), rep(2012, 64), rep(2013, 64), rep(2014, 64),
           rep(2015, 64), rep(2016, 64))
SZN$lenyear.ID <- paste0(years, ".", SZN$ID)
lenyear.ID <- paste0(years, ".", SZN $ID)

#remove playing team seeds
Seed <- Seeds[Seeds$Team %in% lenyear.ID, ]
SumSeeds <- as.numeric(Seed[match(SZN$lenyear.ID, Seed$Team), 1])



#Differencing the matchups
SZN <- cbind(lenyear.ID, SumSeeds, SZN[,2:19])
rownames(SZN)<- c(1:length(SZN$lenyear.ID))



#Randomly select half to have team one win
set.seed(5)
TEAM1wins <- sample(1:882, 441)
list882 <- c(1:882)
list882 <- ifelse(list882 %in% TEAM1wins, list882 <- TRUE, list882 <- FALSE)

i <- 1
TournData.R.SM <- data.frame()
Round <- rep(c(rep(1,32), rep(2,16), rep(3, 8), rep(4,4), 5, 5, 6)  ,14)
while(i <= 882)
{
  ifelse(list882[i] == TRUE, Team1 <- 2, Team1 <- 3 )
  ifelse(list882[i] == FALSE, Team2 <- 2, Team2 <- 3 )
  
  Team1 <- TourneyGames[i, Team1]
  Team2 <- TourneyGames[i, Team2]
  Yr <- TourneyGames[i, 1]
  
  T1 <- paste0(Yr, ".", Team1)
  T2 <- paste0(Yr, ".", Team2) 
  
  TM1 <- as.numeric(unname(SZN[SZN$lenyear.ID == T1,  c(2:3,5:20)]))
  TM2 <- as.numeric(unname(SZN[SZN$lenyear.ID == T2,  c(2:3,5:20)]))
  diff <- TM1 - TM2
  
  matchup <- c(Yr, Team1, Team2, Round[i], diff)
  
  TournData.R.SM <- rbind(TournData.R.SM, matchup)
  
  i <- i + 1
}

colnames(TournData.R.SM) <- c("Year", "Team1", "Team2", "Round", "Seed", colnames(SZN)[c(3,5:20)])
TournData.R.SM$Win.Loss <- as.factor(ifelse(list882, TournData.R.SM$Win.Loss <- 1, TournData.R.SM$Win.Loss <- 0))







