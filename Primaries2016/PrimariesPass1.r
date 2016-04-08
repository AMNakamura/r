# Presidential primaries
# A look at races in different states and whether or not 
#      potential candidates won by a little or or blew out their competition
# This program will use the JSON files created by the hardworking CNN staff
#      to create visual representations of the margins of victory for 
#      democratic and republican hopefuls in the New Hampshire, South Carolina,
#      and Arizona primaries. 

library(curl,lib.loc="c:/r/packages")         # needed for jsonlite
library(jsonlite,lib.loc="c:/r/packages")

# The links to some of the races were provided by Bob Ruds (see http://www.r-bloggers.com/primary-plotting/)

# New Hampshire
nhR <- fromJSON("http://data.cnn.com/ELECTION/2016primary/NH/county/R.json",
                flatten=TRUE ) 
nhD <- fromJSON("http://data.cnn.com/ELECTION/2016primary/NH/county/D.json",
                flatten=TRUE) 
# South Carlina
scR <- fromJSON("http://data.cnn.com/ELECTION/2016primary/SC/county/R.json",
                flatten=TRUE ) # Creates a list, must be lowercase flatten
scD <- fromJSON("http://data.cnn.com/ELECTION/2016primary/SC/county/D.json",
                flatten=TRUE) # creates a list

# Arizona
azR <- fromJSON("http://data.cnn.com/ELECTION/2016primary/AZ/county/R.json",
                flatten=TRUE ) # Creates a list, must be lowercase flatten
azD <- fromJSON("http://data.cnn.com/ELECTION/2016primary/AZ/county/D.json",
                flatten=TRUE) # creates a list

# Get some basic information about the JSON structure using the 
# New Hampshire file. 

summary(nhR) 
summary(nhD)
# Three of the lists (pages, candidates, and counties) are wrapped in data frames
# Use str() to display the internal structure of the race list.

str(nhD$race)
str(nhR$race)

str(nhD$counties)
#  nhD$counties is a data frame with 29 obs. and  9 variables  
#  and a nested JSON element
#  The first three, co_id, name, and countycode are 
#  vectors containing the county name and identifiers. 

# Counties contains an object "race", which represents the second 
# level of information in the $counties object. 
# The fourth member of $race is "candidates".

str(nhD$counties$race.candidates)
# The third level (counties.race.candidates) is represented by 
# the output of str() above as 
#..$ :'data.frame':  2 obs. of  15 variables
# Take a look at the column names. Identify any differences
# in naming conventions. 
candnames1<-unique(unlist(lapply(nhD$counties$race.candidates,names)))
candnames2<-unique(unlist(lapply(azD$counties$race.candidates,names)))
candnames3<-unique(unlist(lapply(scD$counties$race.candidates,names)))

######################### flattening democrats ############################

#New Hampshire
library(reshape2,lib.loc="c:/r/packages")    # for melt (0)
nhd1 <-melt(nhD$counties$race.candidates, id=1:15)  
nhd2 <-nhD$counties[1:2] # co_id and county name
nhd2$L1<-seq.int(nrow(nhd2)) # Add a sequence number. Will merge with
# The L1 variable is created by melt() . Will then perform a 1:many
# join.
nhdem <- merge(x = nhd1, y = nhd2, by = "L1", all.x = TRUE) 
# Use dcast to transpose by the county name, code, and candidate
nhdemVpct1 <-dcast(nhdem, L1 + name + co_id ~ lname , value.var="pctDecimal")
nhdemVnum1 <-dcast(nhdem, co_id ~ lname , value.var="votes")
nhdemVpct1<-transform(nhdemVpct1, Clinton = as.numeric(Clinton)) 
nhdemVpct1<-transform(nhdemVpct1, Sanders = as.numeric(Sanders)) 

nhdem2 <- merge(x = nhdemVpct1, y = nhdemVnum1, by = "co_id", all.x = TRUE) 

#South Carolina 
scd1 <-melt(scD$counties$race.candidates, id=1:15) 
scd2 <-scD$counties[1:2] 
scd2$L1<-seq.int(nrow(scd2))
scdem <- merge(x = scd1, y = scd2, by = "L1", all.x = TRUE) 
# Use dcast to transpose by the county name, code, and candidate
scdemVpct1 <-dcast(scdem, L1 + name + co_id ~ lname , value.var="pctDecimal")
scdemVnum1 <-dcast(scdem, co_id ~ lname , value.var="votes")
scdemVpct1<-transform(scdemVpct1, Clinton = as.numeric(Clinton)) 
scdemVpct1<-transform(scdemVpct1, Sanders = as.numeric(Sanders)) 

scdem2 <- merge(x = scdemVpct1, y = scdemVnum1, by = "co_id", all.x = TRUE) 

#Arizona
azd1 <-melt(azD$counties$race.candidates, id=1:15) 
azd2 <-azD$counties[1:2] # co_id and (county) name
azd2$L1<-seq.int(nrow(azd2)) 
azdem <- merge(x = azd1, y = azd2, by = "L1", all.x = TRUE) #Left outer join.

# Use dcast to transpose by the county name, code, and candidate
azdemVpct1 <-dcast(azdem, L1 + name + co_id ~ lname , value.var="pctDecimal")
azdemVnum1 <-dcast(azdem, co_id ~ lname , value.var="votes")
azdemVpct1<-transform(azdemVpct1, Clinton = as.numeric(Clinton)) #Coazert to numeric
azdemVpct1<-transform(azdemVpct1, Sanders = as.numeric(Sanders)) #Coazert to numeric

azdem2 <- merge(x = azdemVpct1, y = azdemVnum1, by = "co_id", all.x = TRUE) #Left outer join.


