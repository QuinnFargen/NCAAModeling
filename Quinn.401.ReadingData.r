#####
#Quinn Fargen
#SDSU Math 401 S2
#
# Reading in Data

library(readr)



######################################################################################################################
#
#                                                   Reading in Data
#
######################################################################################################################

#                           Regular Season COMPACT Results

# This file identifies the game-by-game results for 32 seasons of historical data, from 1985 to 2015. Each year, 
# it includes all games played from daynum 0 through 132 (which by definition is "Selection Sunday," the day that 
# tournament pairings are announced). Each row in the file represents a single game played.


RegComp <- as.data.frame(read_csv("C:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/401 Data/Regula  rSeasonCompactResults.csv", 
                                  col_types = cols(Wloc = col_factor(levels = c("N", "H", "A")))))
RegComp <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/401 Data/RegularSeasonCompactResults.csv", 
                                  col_types = cols(Wloc = col_factor(levels = c("N", "H", "A")))))

#                           Tourney COMPACT Results

# "Season" "Daynum" "Wteam"  "Wscore" "Lteam"  "Lscore" "Wloc"   "Numot"

# This file identifies the game-by-game NCAA tournament results for all seasons of historical data. The data is formatted 
# exactly like the regular_season_compact_results.csv data. Note that these games also include the play-in games (which always 
# occurred on day 134/135) for those years that had play-in games.

TournComp <- as.data.frame(read_csv("C:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/401 Data/TourneyCompactResults.csv", 
                                    col_types = cols(Wloc = col_factor(levels = c("N")))))

TournComp <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/401 Data/TourneyCompactResults.csv", 
                                    col_types = cols(Wloc = col_factor(levels = c("N")))))



#                           Regular Season DETAILED Results

# This file is a more detailed set of game results, covering seasons 2003-2016. This includes team-level total statistics 
# for each game (total field goals attempted, offensive rebounds, etc.) 

RegDetail <- as.data.frame(read_csv("C:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/401 Data/RegularSeasonDetailedResults.csv", 
                                    col_types = cols(Wloc = col_factor(levels = c("N", "H", "A")))))

RegDetail <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/401 Data/RegularSeasonDetailedResults.csv", 
                                    col_types = cols(Wloc = col_factor(levels = c("N", "H", "A")))))

#                           Tourney DETAILED Results

#"Season" "Daynum" "Wteam"  "Wscore" "Lteam"  "Lscore" "Wloc"   "Numot"  "Wfgm"   "Wfga"   "Wfgm3"  
#"Wfga3" "Wftm"   "Wfta"   "Wor"    "Wdr"    "Wast"   "Wto" "Wstl"   "Wblk"   "Wpf"    "Lfgm"   "Lfga"   
#"Lfgm3" "Lfga3"  "Lftm"   "Lfta"   "Lor"    "Ldr"    "Last"  "Lto"    "Lstl"   "Lblk"   "Lpf"   

# This file contains the more detailed results for tournament games from 2003 onward.

TournDetail <- as.data.frame(read_csv("C:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/401 Data/TourneyDetailedResults.csv", 
                                      col_types = cols(Wloc = col_factor(levels = c("N")))))

TournDetail<- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/401 Data/TourneyDetailedResults.csv", 
                                     col_types = cols(Wloc = col_factor(levels = c("N")))))




#                           Tourney Seeds

# Season  Seed  Team

# This file identifies the seeds for all teams in each NCAA tournament, for all seasons of historical data. Thus, there are between
# 64-68 rows for each year, depending on the bracket structure.

TourneySeeds <- as.data.frame(read_csv("C:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/401 Data/TourneySeeds.csv"))

TourneySeeds <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/401 Data/TourneySeeds.csv"))


#                           Tourney Slots

# Slot StrongSeed Weakseed

# This file identifies the mechanism by which teams are paired against each other, depending upon their seeds. Because of the 
# existence of play-in games for particular seed numbers, the pairings have small differences from year to year. If there were 
# N teams in the tournament during a particular year, there were N-1 teams eliminated (leaving one champion) and therefore N-1 
# games played, as well as N-1 slots in the tournament bracket, and thus there will be N-1 records in this file for that season.

TourneySlots <- as.data.frame(read_csv("C:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/401 Data/TourneySlots.csv"))

TourneySlots <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/401 Data/TourneySlots.csv"))


#                           Teams

# teamId Teamname

Teams <- as.data.frame(read_csv("C:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/401 Data/Teams.csv"))

Teams <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/401 Data/Teams.csv"))

#Turn team id's into row id's and then NULL first row
#   rownames(Teams) <- Teams[,1]
#   Teams[,1] <- NULL


#                           Seasons

#Dayzero Region names

Seasons <- as.data.frame(read_csv("C:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/401 Data/Seasons.csv"))

Seasons <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/401 Data/Seasons.csv"))


#                           Rankings
R3 <- as.data.frame(read_csv("c:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/Rankings/Rankings2003.csv"))
R4 <- as.data.frame(read_csv("c:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/Rankings/Rankings2004.csv"))
R5 <- as.data.frame(read_csv("c:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/Rankings/Rankings2005.csv"))
R6 <- as.data.frame(read_csv("c:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/Rankings/Rankings2006.csv"))
R7 <- as.data.frame(read_csv("c:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/Rankings/Rankings2007.csv"))
R8 <- as.data.frame(read_csv("c:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/Rankings/Rankings2008.csv"))
R9 <- as.data.frame(read_csv("c:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/Rankings/Rankings2009.csv"))
R10 <- as.data.frame(read_csv("c:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/Rankings/Rankings2010.csv"))
R11 <- as.data.frame(read_csv("c:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/Rankings/Rankings2011.csv"))
R12 <- as.data.frame(read_csv("c:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/Rankings/Rankings2012.csv"))
R13 <- as.data.frame(read_csv("c:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/Rankings/Rankings2013.csv"))
R14 <- as.data.frame(read_csv("c:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/Rankings/Rankings2014.csv"))
R15 <- as.data.frame(read_csv("c:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/Rankings/Rankings2015.csv"))
R16 <- as.data.frame(read_csv("c:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/Rankings/Rankings2016.csv"))
R17 <- as.data.frame(read_csv("c:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/Rankings/Rankings2017.csv"))

R3 <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/Rankings/Rankings2003.csv"))
R4 <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/Rankings/Rankings2004.csv"))
R5 <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/Rankings/Rankings2005.csv"))
R6 <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/Rankings/Rankings2006.csv"))
R7 <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/Rankings/Rankings2007.csv"))
R8 <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/Rankings/Rankings2008.csv"))
R9 <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/Rankings/Rankings2009.csv"))
R10 <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/Rankings/Rankings2010.csv"))
R11 <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/Rankings/Rankings2011.csv"))
R12 <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/Rankings/Rankings2012.csv"))
R13 <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/Rankings/Rankings2013.csv"))
R14 <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/Rankings/Rankings2014.csv"))
R15 <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/Rankings/Rankings2015.csv"))
R16 <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/Rankings/Rankings2016.csv"))
R17 <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/Rankings/Rankings2017.csv"))


#                           Conference 
Conf <- as.data.frame(read_csv("C:/Users/Quinn Fargen/Desktop/Quinn.Fargen.401/401 Data/ConfData.csv"))

Conf <- as.data.frame(read_csv("~/Desktop/Quinn.Fargen.401/401 Data/ConfData.csv"))














