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
