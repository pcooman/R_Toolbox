library(RMySQL)

ucscDb <- dbConnect(MySQL(),user="genome",                      # creates a connection object
                    host="genome-mysql.cse.ucsc.edu")

result <- dbGetQuery(ucscDb,"show databases;")                  # note the ';' at the end of the command!
                                                                # show a list of all available databases on the genome server
                                                                # Combines dbSendQuery() and fetch()

dbDisconnect(ucscDb)                                            # important to close the connection

head(result)

# hg19 is one of the available databases
hg19 <- dbConnect(MySQL(),user="genome",                        # create a connection object to directly tap into the hg19 database on the genome server
                  host="genome-mysql.cse.ucsc.edu",
                  db = "hg19")

allTables <- dbListTables(hg19)                                 # List all available tables in the database hg19 (these may be linked to each other!)
length(allTables)
allTables[1:5]

# affyU133Plus2 is a specific table in the database hg19
dbListFields(hg19,"affyU133Plus2")                              # similar to column names of a data frame
dbGetQuery(hg19,"select count(*) from affyU133Plus2")           

affyData <- dbReadTable(hg19,"affyU133Plus2")                   # read in a table as a data frame in R
head(affyData)

query <- dbSendQuery(hg19, " select * from affyU133Plus2 where misMatches between 1 and 3")       # create a query
affyMis <- fetch(query)                                                                          # execute the query (same as GetQuery?)
quantile(affyMis$misMatches)

affyMisSmall <- fetch(query,n=10)      # returns the NEXT ten lines corresponding to the previous query (misMatches between 1 and 3)
dbClearResult(query)                   # Clear the query

dim(affyMisSmall)

# trying out some stuff on my own
query <- dbSendQuery(hg19, " select qName from affyU133Plus2")       # create a query
x <- fetch(query,n=10)                                               # fetch() is a deprecated function --> should use dbFetch instead!
dbClearResult(query)                                                 # make sure to clear the old query before defining a new one

summary(hg19)                    # returns user name, host name, db name and connection type
dbGetInfo(hg19)                  # returns info in a list (host name, server name, ...)
dbListResults(hg19)              # ? returns empty list
dbListTables(hg19)

dbNextResult(hg19)  # returns next result if there is one, NULL if there isn't
dbMoreResults(hg19) # results a logical whether there are more results available

# check https://dev.mysql.com/doc/refman/5.0/en/select.html for SELECT syntax

# LIMIT        # return only this many rows
# OFFSET       # if defining a limit, offset defines where to start counting
# HAVING       # can only be called after a GROUP BY (otherwise you should WHERE), as such it helps to adjust the GROUP BY command 

# SELECT column name 1, count(column name 2), column name 3 ... FROM Table
# SELECT * FROM table WHERE <condition> GROUP BY <column name> HAVING <condition> ORDER BY <col name> LIMIT num OFFSET

dbDisconnect(hg19)