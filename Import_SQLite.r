{
    library(DBI)
    library(dplyr)
    library(RSQLite)
    
}

{
    #Connect to DB
    setwd("D:/surff/Desktop/NFL Data/")
    con <- dbConnect(SQLite(), "NFL_Data_DB.db")
    
    #Load NFL Data Tables
    DEF_Allowed_Weekly <- dbReadTable(con, "Def_Allowed_Weekly")
    
    DST_Weekly <- dbReadTable(con, "DST_Weekly")
    
    Odds_Weekly <- dbReadTable(con, "Odds_Weekly")
    
    Offense_Weekly <- dbReadTable(con, "Offense_Weekly")
    
    Salary_Weekly <- dbReadTable(con, "Salary_Weekly")
    
    Snap_Count_Weekly <- dbReadTable(con, "Snap_Count_Weekly")
    
    Targets_Weekly <- dbReadTable(con, "Targets_Weekly")
    
    #Disconnect from DB
    dbDisconnect(con)
    
}
