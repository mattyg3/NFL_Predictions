{
    #DEF Allowed
    #Split in Team specific data sets
    #Add 16 game avg and 3 game avg for all stats
    ####NO Trimming Needed
    {
        TEAM_ID <- unique(DEF_Allowed_Weekly$TeamABV)
        TEAM_DEF_list <- as.list(NULL)
        for(i in 1:length(TEAM_ID)){
            df <- DEF_Allowed_Weekly[DEF_Allowed_Weekly$TeamABV %in% TEAM_ID[i],]
            df <- df[order(df$Week),]
            df <- df[order(df$Year),]
            
            #OFFSET DATA
            len <- nrow(df)
            
            df$o_QB.Pts <- as.numeric(c("NA",df$QB.Pts[1:len-1]))
            df$o_RB.Pts <- as.numeric(c("NA",df$RB.Pts[1:len-1]))
            df$o_WR.Pts <- as.numeric(c("NA",df$WR.Pts[1:len-1]))
            df$o_TE.Pts <- as.numeric(c("NA",df$TE.Pts[1:len-1]))
            df$o_Total.Avg.Pts <- as.numeric(c("NA",df$Total.Avg.Pts[1:len-1]))
            
            #Remove Kicker Stats and old stats
            df <- within(df, rm(K.Pts, QB.Pts, RB.Pts, WR.Pts, TE.Pts, Total.Avg.Pts))
            
            #Calculate moving averages
            x <- ts(df$o_QB.Pts)
            df$o_QB.Pts.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
            df$o_QB.Pts.16 <- as.vector(stats::filter(x, rep(1/16,16), sides=1))
            
            x <- ts(df$o_RB.Pts)
            df$o_RB.Pts.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
            df$o_RB.Pts.16 <- as.vector(stats::filter(x, rep(1/16,16), sides=1))
            
            x <- ts(df$o_WR.Pts)
            df$o_WR.Pts.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
            df$o_WR.Pts.16 <- as.vector(stats::filter(x, rep(1/16,16), sides=1))
            
            x <- ts(df$o_TE.Pts)
            df$o_TE.Pts.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
            df$o_TE.Pts.16 <- as.vector(stats::filter(x, rep(1/16,16), sides=1))
            
            x <- ts(df$o_Total.Avg.Pts)
            df$o_Total.Avg.Pts.Pts.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
            df$o_Total.Avg.Pts.Pts.16 <- as.vector(stats::filter(x, rep(1/16,16), sides=1))
            
            #Load into list to rejoin later
            TEAM_DEF_list[[i]] <- df
        }
        
        Clean_DEF_Allowed_Weekly <- do.call("rbind", TEAM_DEF_list)
        remove(TEAM_ID, df, x, i, TEAM_DEF_list)
    }
    
    #DST Weekly
    #Split in Team specific data sets
    #Add 16 game avg and 3 game avg for all stats
    ####NO Trimming Needed
    {
        DST_ID <- unique(DST_Weekly$TeamABV)
        DST_list <- as.list(NULL)
        for(i in 1:length(DST_ID)){
            df <- DST_Weekly[DST_Weekly$TeamABV %in% DST_ID[i],]
            df <- df[order(df$Week),]
            df <- df[order(df$Year),]
            
            #OFFSET DATA
            len <- nrow(df)
            
            df$o_TFL <- as.numeric(c("NA",df$TFL[1:len-1]))
            df$o_Sacks <- as.numeric(c("NA",df$Sacks[1:len-1]))
            df$o_QB.Hit <- as.numeric(c("NA",df$QB.Hit[1:len-1]))
            df$o_INT <- as.numeric(c("NA",df$INT[1:len-1]))
            df$o_Fumble.Rec <- as.numeric(c("NA",df$Fumble.Rec[1:len-1]))
            df$o_Safety <- as.numeric(c("NA",df$Safety[1:len-1]))
            df$o_Def.TD <- as.numeric(c("NA",df$Def.TD[1:len-1]))
            df$o_ST.TD <- as.numeric(c("NA",df$ST.TD[1:len-1]))
            df$o_Pts.Allowed <- as.numeric(c("NA",df$Pts.Allowed[1:len-1]))
            
            
            #Remove Fantasy Pt Stats
            df <- within(df, rm(Fantasy.Pts, Fantasy.Pts.Week))
            
            
            #Calculate moving averages
            #change type to time-series for filter, then back to vector for rbind
            x <- ts(df$o_TFL)
            df$o_TFL.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
            df$o_TFL.16 <- as.vector(stats::filter(x, rep(1/16,16), sides=1))
            
            x <- ts(df$o_Sacks)
            df$o_Sacks.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
            df$o_Sacks.16 <- as.vector(stats::filter(x, rep(1/16,16), sides=1))
            
            x <- ts(df$o_QB.Hit)
            df$o_QB.Hit.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
            df$o_QB.Hit.16 <- as.vector(stats::filter(x, rep(1/16,16), sides=1))
            
            x <- ts(df$o_INT)
            df$o_INT.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
            df$o_INT.16 <- as.vector(stats::filter(x, rep(1/16,16), sides=1))
            
            x <- ts(df$o_Fumble.Rec)
            df$o_Fumble.Rec.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
            df$o_Fumble.Rec.16 <- as.vector(stats::filter(x, rep(1/16,16), sides=1))
            
            x <- ts(df$o_Safety)
            df$o_Safety.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
            df$o_Safety.16 <- as.vector(stats::filter(x, rep(1/16,16), sides=1))
            
            x <- ts(df$o_Def.TD)
            df$o_Def.TD.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
            df$o_Def.TD.16 <- as.vector(stats::filter(x, rep(1/16,16), sides=1))
            
            x <- ts(df$o_ST.TD)
            df$o_ST.TD.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
            df$o_ST.TD.16 <- as.vector(stats::filter(x, rep(1/16,16), sides=1))
            
            x <- ts(df$o_Pts.Allowed)
            df$o_Pts.Allowed.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
            df$o_Pts.Allowed.16 <- as.vector(stats::filter(x, rep(1/16,16), sides=1))
            

            
            #Load into list to rejoin later
            DST_list[[i]] <- df
        }
        
        Clean_DST_Weekly <- do.call("rbind", DST_list)
        remove(DST_ID, df, x, i, DST_list)
        
    }
    
    #Odds data set
    #Keep only key variables for now
    ####NO Trimming Needed
    {
        Clean_Odds_Weekly <- select(Odds_Weekly, c("TeamABV", "Year", "Week", "Total", "Favorite.ABV", "Spread"))
        Clean_Odds_Weekly$Fav.Ind <- ifelse(Clean_Odds_Weekly$Favorite.ABV==Clean_Odds_Weekly$TeamABV,-1,1)
        Clean_Odds_Weekly$Spread.Adj <- Clean_Odds_Weekly$Spread * Clean_Odds_Weekly$Fav.Ind
    }
    
    #Offense Data set
    #split by playerID
    #Add cum career avg and 3 game avg for all stats
    ####Trim first 3 games for each playerID
    {
        PLAYER_ID <- unique(Offense_Weekly$PlayerID)
        Player_list <- as.list(NULL)
        #J is the counter for players with atleast 3 games
        j <- 1
        for(i in 1:length(PLAYER_ID)){
            df <- Offense_Weekly[Offense_Weekly$PlayerID %in% PLAYER_ID[i],]
            df <- df[order(df$Week),]
            df <- df[order(df$Year),]
            if(nrow(df) < 4){
                #remove players with less than 3 games played
            } else{
                #OFFSET DATA
                len <- nrow(df)
                
                df$o_Pass.Yds <- as.numeric(c("NA",df$Pass.Yds[1:len-1]))
                df$o_Pass.TD <- as.numeric(c("NA",df$Pass.TD[1:len-1]))
                df$o_Pass.INT <- as.numeric(c("NA",df$Pass.INT[1:len-1]))
                df$o_Rush.Yds <- as.numeric(c("NA",df$Rush.Yds[1:len-1]))
                df$o_Rush.TD <- as.numeric(c("NA",df$Rush.TD[1:len-1]))
                df$o_Receptions <- as.numeric(c("NA",df$Receptions[1:len-1]))
                df$o_Rec.Yds <- as.numeric(c("NA",df$Rec.Yds[1:len-1]))
                df$o_Rec.TD <- as.numeric(c("NA",df$Rec.TD[1:len-1]))
                df$o_Fumble.Lost <- as.numeric(c("NA",df$Fumble.Lost[1:len-1]))
            
                #Remove Fantasy Pt Stats
                df <- within(df, rm(Fantasy.Points, Fantasy.Pts.Game))
                
                #Remove NA from first game for cummean func
                df <- df[2:nrow(df),]
                #Calculate moving averages
                #change type to time-series for filter, then back to vector for rbind
                x <- ts(df$o_Pass.Yds)
                df$o_Pass.Yds.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
                df$o_Pass.Yds.Career <- cummean(df$o_Pass.Yds)
                
                x <- ts(df$o_Pass.TD)
                df$o_Pass.TD.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
                df$o_Pass.TD.Career <- cummean(df$o_Pass.TD)
                
                x <- ts(df$o_Pass.INT)
                df$o_Pass.INT.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
                df$o_Pass.INT.Career <- cummean(df$o_Pass.INT)
                
                x <- ts(df$o_Rush.Yds)
                df$o_Rush.Yds.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
                df$o_Rush.Yds.Career <- cummean(df$o_Rush.Yds)
                
                x <- ts(df$o_Rush.TD)
                df$o_Rush.TD.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
                df$o_Rush.TD.Career <- cummean(df$o_Rush.TD)
                
                x <- ts(df$o_Receptions)
                df$o_Receptions.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
                df$o_Receptions.Career <- cummean(df$o_Receptions)
                
                x <- ts(df$o_Rec.Yds)
                df$o_Rec.Yds.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
                df$o_Rec.Yds.Career <- cummean(df$o_Rec.Yds)
                
                x <- ts(df$o_Rec.TD)
                df$o_Rec.TD.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
                df$o_Rec.TD.Career <- cummean(df$o_Rec.TD)
                
                x <- ts(df$o_Fumble.Lost)
                df$o_Fumble.Lost.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
                df$o_Fumble.Lost.Career <- cummean(df$o_Fumble.Lost)
                
                #Trim second and third games for each playerID
                df <- df[3:nrow(df),]
                

                
                #Load into list to rejoin later
                Player_list[[j]] <- df
                j <- j+1
            }
            
        }
        
        Clean_Offense_Weekly <- do.call("rbind", Player_list)
        remove(PLAYER_ID, df, x, i, Player_list, j)
        
    }
    
    #Salary data set
    #Salary_Weekly nulls change datatype
    ####NO Trimming Needed
    {
        Salary_Weekly$Opp.Rank <- as.numeric(Salary_Weekly$Opp.Rank)
        Salary_Weekly$Opp.Pos.Rank <- as.numeric(Salary_Weekly$Opp.Pos.Rank)
        Clean_Salary_Weekly <- Salary_Weekly[Salary_Weekly$Year>2014,]
    }
    
    #Snap Count data set
    #split by playerID
    #add cum career avg and 3 game avg for all stats
    ####Trim first 3 games for each playerID
    {
        PLAYER_ID <- unique(Snap_Count_Weekly$PlayerID)
        Player_list <- as.list(NULL)
        #J is the counter for players with atleast 3 games
        j <- 1
        for(i in 1:length(PLAYER_ID)){
            df <- Snap_Count_Weekly[Snap_Count_Weekly$PlayerID %in% PLAYER_ID[i],]
            df <- df[order(df$Week),]
            df <- df[order(df$Year),]
            if(nrow(df) < 4){
                #remove players with less than 3 games played
            } else{
                
                #OFFSET DATA
                len <- nrow(df)
                
                df$o_Snaps.Played <- as.numeric(c("NA",df$Snaps.Played[1:len-1]))
                df$o_Snap.Percent <- as.numeric(c("NA",df$Snap.Percent[1:len-1]))
                df$o_Rush.Snap.Percent <- as.numeric(c("NA",df$Rush.Snap.Percent[1:len-1]))
                df$o_Target.Snap.Percent <- as.numeric(c("NA",df$Target.Snap.Percent[1:len-1]))
                df$o_Touch.Snap.Percent <- as.numeric(c("NA",df$Touch.Snap.Percent[1:len-1]))
                df$o_Intend.Touch.Snap.Percent <- as.numeric(c("NA",df$Intend.Touch.Snap.Percent[1:len-1]))
                
                
                #Remove Fantasy Pt Stats
                df <- within(df, rm(Fantasy.Pts, Fantasy.Pts.Snap.Percent, Snaps.Played, Snap.Percent, Rush.Snap.Percent,
                                    Target.Snap.Percent, Touch.Snap.Percent, Intend.Touch.Snap.Percent))
                
                #Remove NA from first game for cummean func
                df <- df[2:nrow(df),]
                
                #Calculate moving averages
                #change type to time-series for filter, then back to vector for rbind
                x <- ts(df$o_Snaps.Played)
                df$o_Snaps.Played.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
                df$o_Snaps.Played.Career <- cummean(df$o_Snaps.Played)
                
                x <- ts(df$o_Snap.Percent)
                df$o_Snap.Percent.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
                df$o_Snap.Percent.Career <- cummean(df$o_Snap.Percent)
                
                x <- ts(df$o_Rush.Snap.Percent)
                df$o_Rush.Snap.Percent.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
                df$o_Rush.Snap.Percent.Career <- cummean(df$o_Rush.Snap.Percent)
                
                x <- ts(df$o_Target.Snap.Percent)
                df$o_Target.Snap.Percent.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
                df$o_Target.Snap.Percent.Career <- cummean(df$o_Target.Snap.Percent)
                
                x <- ts(df$o_Touch.Snap.Percent)
                df$o_Touch.Snap.Percent.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
                df$o_Touch.Snap.Percent.Career <- cummean(df$o_Touch.Snap.Percent)
                
                x <- ts(df$o_Intend.Touch.Snap.Percent)
                df$o_Intend.Touch.Snap.Percent.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
                df$o_Intend.Touch.Snap.Percent.Career <- cummean(df$o_Intend.Touch.Snap.Percent)
                
                #Trim second and third games for each playerID
                df <- df[3:nrow(df),]
                

                
                #Load into list to rejoin later
                Player_list[[j]] <- df
                j <- j+1
            }
            
        }
        
        Clean_Snap_Count_Weekly <- do.call("rbind", Player_list)
        remove(PLAYER_ID, df, x, i, Player_list, j)
    }
    
    #Targets data set
    #split by playerID
    #add cum career avg and 3 game avg for all stats
    ####Trim first 3 games for each playerID
    {
        PLAYER_ID <- unique(Targets_Weekly$PlayerID)
        Player_list <- as.list(NULL)
        #J is the counter for players with atleast 3 games
        j <- 1
        for(i in 1:length(PLAYER_ID)){
            df <- Targets_Weekly[Targets_Weekly$PlayerID %in% PLAYER_ID[i],]
            df <- df[order(df$Week),]
            df <- df[order(df$Year),]
            #Remove no-played games
            df <- df[df$SKIP=="Good",]
            if(nrow(df) < 4){
                #remove players with less than 3 games played
            } else{
                
                #OFFSET DATA
                len <- nrow(df)
                
                df$o_Targets<- as.numeric(c("NA",df$Targets[1:len-1]))
              
                #Remove Fantasy Pt Stats
                df <- within(df, rm(Targets))
                
                #Remove NA from first game for cummean func
                df <- df[2:nrow(df),]
                
                #Calculate moving averages
                #change type to time-series for filter, then back to vector for rbind
                x <- ts(df$o_Targets)
                df$o_Targets.3 <- as.vector(stats::filter(x, rep(1/3,3), sides=1))
                df$o_Targets.Career <- cummean(df$o_Targets)
                
                
                
                #Trim second and third games for each playerID
                df <- df[3:nrow(df),]
                
                #Load into list to rejoin later
                Player_list[[j]] <- df
                j <- j+1
            }
            
        }
        
        Clean_Targets_Weekly <- do.call("rbind", Player_list)
        remove(PLAYER_ID, df, x, i, Player_list, j)
    }
    
    remove(DEF_Allowed_Weekly, DST_Weekly, Odds_Weekly, Offense_Weekly, Salary_Weekly, Snap_Count_Weekly, Targets_Weekly)
    
}
