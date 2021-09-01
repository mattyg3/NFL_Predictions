#Merge  Clean_Offense_Weekly, Clean_Salary_Weekly, Clean_Snap_Count_Weekly, Clean_Targets_Weekly, Clean_Odds_Weekly
#Split by position
{
    #Only Offense,DST from Salary dataset
    Clean_Salary_Weekly <- Clean_Salary_Weekly[Clean_Salary_Weekly$Position=="QB" | 
                                                   Clean_Salary_Weekly$Position=="RB" |
                                                   Clean_Salary_Weekly$Position=="WR" |
                                                   Clean_Salary_Weekly$Position=="TE" |
                                                   Clean_Salary_Weekly$Position=="DST",]
    #Remove Rank and duplicated columns
    Clean_Salary_Weekly <- within(Clean_Salary_Weekly, rm(Rank))
    Clean_Offense_Weekly <- within(Clean_Offense_Weekly, rm(Rank, Player.Name, TeamABV, Position, Opponent))
    Clean_Snap_Count_Weekly <- within(Clean_Snap_Count_Weekly, rm(Rank, Player.Name, TeamABV, Position, Opponent))
    Clean_Targets_Weekly <- within(Clean_Targets_Weekly, rm(Player.Name, SKIP))
    
    Merge_Offense <- Clean_Salary_Weekly %>% 
        left_join(Clean_Offense_Weekly, by=c("PlayerID", "Year", "Week")) %>%
        left_join(Clean_Snap_Count_Weekly, by=c("PlayerID", "Year", "Week")) %>%
        left_join(Clean_Targets_Weekly, by=c("PlayerID", "Year", "Week")) %>%
        left_join(Clean_Odds_Weekly, by=c("TeamABV", "Year", "Week"))
    
    Merge_QB <- Merge_Offense[Merge_Offense$Position=="QB",]
    Merge_RB <- Merge_Offense[Merge_Offense$Position=="RB",]
    Merge_WR <- Merge_Offense[Merge_Offense$Position=="WR",]
    Merge_TE <- Merge_Offense[Merge_Offense$Position=="TE",]
    
    #DEF Points Allowed by position
    DEFvsQB <- Clean_DEF_Allowed_Weekly[,c('TeamABV', 'Year', 'Week', 'Opponent', 'o_QB.Pts', 'o_Total.Avg.Pts', 
                                           'o_QB.Pts.3', 'o_QB.Pts.16', 'o_Total.Avg.Pts.Pts.3', 'o_Total.Avg.Pts.Pts.16')]
    DEFvsRB <- Clean_DEF_Allowed_Weekly[,c('TeamABV', 'Year', 'Week', 'Opponent', 'o_RB.Pts', 'o_Total.Avg.Pts', 
                                           'o_RB.Pts.3', 'o_RB.Pts.16', 'o_Total.Avg.Pts.Pts.3', 'o_Total.Avg.Pts.Pts.16')]
    DEFvsWR <- Clean_DEF_Allowed_Weekly[,c('TeamABV', 'Year', 'Week', 'Opponent', 'o_WR.Pts', 'o_Total.Avg.Pts', 
                                           'o_WR.Pts.3', 'o_WR.Pts.16', 'o_Total.Avg.Pts.Pts.3', 'o_Total.Avg.Pts.Pts.16')]
    DEFvsTE <- Clean_DEF_Allowed_Weekly[,c('TeamABV', 'Year', 'Week', 'Opponent', 'o_TE.Pts', 'o_Total.Avg.Pts', 
                                            'o_TE.Pts.3', 'o_TE.Pts.16', 'o_Total.Avg.Pts.Pts.3', 'o_Total.Avg.Pts.Pts.16')]
    
    #Merge DEFvs... and Merge_...
    Merge_QB <- left_join(Merge_QB, DEFvsQB, by=c("TeamABV"="Opponent", "Year", "Week"))
    Merge_RB <- left_join(Merge_RB, DEFvsRB, by=c("TeamABV"="Opponent", "Year", "Week"))
    Merge_WR <- left_join(Merge_WR, DEFvsWR, by=c("TeamABV"="Opponent", "Year", "Week"))
    Merge_TE <- left_join(Merge_TE, DEFvsTE, by=c("TeamABV"="Opponent", "Year", "Week"))
    
    
    #Merge Clean_DST_Weekly, Clean_Salary_Weekly, Clean_Odds_Weekly
    Clean_DST_Weekly <- within(Clean_DST_Weekly, rm(Rank, TeamName, Position, Opponent))
    Merge_DST <- Clean_Salary_Weekly[Clean_Salary_Weekly$Position=="DST",] %>%
        left_join(Clean_DST_Weekly, by=c("TeamABV", "Year", "Week")) %>%
        left_join(Clean_Odds_Weekly, by=c("TeamABV", "Year", "Week"))
    
    remove(Clean_DEF_Allowed_Weekly, Clean_DST_Weekly, Clean_Odds_Weekly, Clean_Offense_Weekly, Clean_Salary_Weekly, 
    Clean_Snap_Count_Weekly, Clean_Targets_Weekly, DEFvsQB, DEFvsRB, DEFvsWR, DEFvsTE, Merge_Offense)
}

