library("RSQLite")
# connect to the sqlite file
# con = dbConnect(drv="SQLite", dbname="database.sqlite")
con <- dbConnect(RSQLite::SQLite(), dbname="data/database.sqlite")
# get a list of all tables
alltables <- dbListTables(con)
# get the populationtable as a data.frame
# p1 = dbGetQuery( con,'select * from populationtable' )
p1  <- dbGetQuery( con, 'PRAGMA table_info(NationalNames)')
# count the entries in the SQLite table
p2 <- dbGetQuery( con,'select count(*) from NationalNames' )
# get the first 5 rows
p3a <- dbGetQuery( con, 'SELECT * FROM NationalNames LIMIT 100')
p3a
# get the name "Mary" from all years
p4a <- dbGetQuery( con, "SELECT * FROM NationalNames WHERE Name='Mary' AND Gender='F'")
p4a

library(ggplot2)
qplot(data=p4a, x=Year, y=Count)
#####
## from example


# count the areas in the SQLite table
p2 = dbGetQuery( con,'select count(*) from areastable' )
# find entries of the DB from the last week
p3 = dbGetQuery(con, "SELECT population WHERE DATE(timeStamp) < DATE('now', 'weekday 0', '-7 days')")
#Clear the results of the last query
dbClearResult(p3)
#Select population with managerial type of job
p4 = dbGetQuery(con, "select * from populationtable where jobdescription like '%manager%'")