# Presidential primaries
# What does it mean to say dominate a competition?  Not just winning the competition.  But winning by a lot -- in other words "crushing" or "demolishing" the field.

By how much did the winner win?  Was it a close race?  Or was it a blowout?  Here people often look at the margin of victory -- the difference (in score or time) between the winner and the loser.

That absolute difference only makes sense when put into context:  Winning by 1 second is much more meaningful in a 30 second race than in a 15 minute race; Winning by 50 points in a dual meet is much more significant than winning by 50 points in a championship meet



# delete R.dat from default working directory if loading.
getwd()
setwd('C:\\!mom\\_Rproject\\Primaries')  

library(curl,lib.loc="c:/r/packages")  # needed for jsonlite
library(jsonlite,lib.loc="c:/r/packages")
#jsonlite

# http://www.r-bloggers.com/primary-plotting/ (Thanks to Bob Rudis)
# for sharing many of the links to the early primaries data.  

# New Hampshire
nhR <- fromJSON("http://data.cnn.com/ELECTION/2016primary/NH/county/R.json",
                flatten=TRUE ) # Creates a list, must be lowercase flatten
nhD <- fromJSON("http://data.cnn.com/ELECTION/2016primary/NH/county/D.json",
                flatten=TRUE) # creates a list
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
# Summary() is usually meant for statistical summaries, but it 
# is a generic function, so you can pass it different kinds of objects.
# Each object in R has a length, a class, and a mode.
# The mode shows how the object is stored in memory. In this case, the mode 
# shows whether or not the object is numeric, character, or a nested list.
# Length shows the number of elements in each object. 
# The mode shows that there are two objects with one character element each (ose and raceID) 
#-- there are no scalars in R, so these are vectors of length 1
# -- see: http://cran.at.r-project.org/doc/manuals/R-lang.html#Basic-types
# There is one object with a single numeric element (lts), and four lists (pages, candidates, race, and counties)
# Three of the lists (pages, candidates, and counties) are wrapped in data frames
# Use str() to display the internal structure of the race list.
str(nhD$race)
str(nhR$race)
# Shows that $race contains 14 elements, 7 character strings (e.g., "Republican Primary-President")
# -- five logicals (e.g., cresults), and one numeric (pollclose)

str(nhR$pages)
# "pages":[{"firstCounty":"Acworth","lastCounty":"Woodstock"}] (how it looks in the original JSON)
#  str() returns: 'data.frame' : 1 obs. of 2 variables. 
#   $ firstCounty: chr "Beaver"
#   $ lastCounty : chr "Weber"
#  Meaning one row and two columns of character type, named firstCounty and lastCounty
#  One observation, with values of 'Beaver' and 'Weber'

str(nhD$candidates) 
# Four variables (lname (chr), party (chr), pctDecima (chr)l, and winner (logical))
# 2 observations (one each for Sanders and Clinton)

str(nhD$counties)
ck <- as.data.frame(nhD$counties)
# 'data.frame':  29 obs. of  9 variables  
#  Each variable is listed with the $ prepend.
#  This variable contains nested JSON elements
#  The first three, co_id, name, and countycode are 
#  vectors containing the county name and identifiers. 
#  
# $ co_id          : int  49001 49003 49005 ...
# $ name           : chr  "Beaver" "Box Elder" "Cache" ...
# $ countycode     : int  49001 49003 49005 ...

# Counties contains an object "race", which represents the second 
# level of information in the $counties object. It is indicated by 
# the path name $race.object. 

# The fourth member of $race is "candidates".

str(nhD$counties$race.candidates)

# The third level (counties.race.candidates) is represented by 
# the output of str() above as 
#..$ :'data.frame':  2 obs. of  15 variables:
# You can get the 15 variable names by passing the list to 
# the unlist function. Use unique() to return only one column name per 
# observation, otherwise you will have a list with 435 *29x15 elements
# (29 observatsion and 15 variables)

candnames1<-unique(unlist(lapply(nhD$counties$race.candidates,names)))
candnames2<-unique(unlist(lapply(azD$counties$race.candidates,names)))
candnames3<-unique(unlist(lapply(scD$counties$race.candidates,names)))
#  c("id", "fname", "mname", "lname", "suffix", "usesuffix", "party", 
#    "inrace", "nominee", "winner", "vpct", "pctDecimal", "inc", "votes", 
#     "cvotes")
# The JSON has a relational structure. It has an object called "candidates" 
# that contains a variable "lname" (e.g., "Sanders"), which link in a 
# one-to-many relationship with observations in race.candidates.  

######################### flattening democrats ############################

#New Hampshire
library(reshape2,lib.loc="c:/r/packages")    # for melt (0)

nhd1 <-melt(nhD$counties$race.candidates, id=1:15) ##Yes!474 obs. 16 variables. Yes! What does ID mean? 
nhd2 <-nhD$counties[1:2] # co_id and county name

nhd2$L1<-seq.int(nrow(nhd2)) # Add a sequence number. Will merge with
# the L1 variable created by the melt() . Will then perform a 1:many
# join.

nhdem <- merge(x = nhd1, y = nhd2, by = "L1", all.x = TRUE) #Left outer join.

# Use dcast to transpose by the county name, code, and candidate
nhdemVpct1 <-dcast(nhdem, L1 + name + co_id ~ lname , value.var="pctDecimal")
nhdemVnum1 <-dcast(nhdem, co_id ~ lname , value.var="votes")

nhdemVpct1<-transform(nhdemVpct1, Clinton = as.numeric(Clinton)) #Coazert to numeric
nhdemVpct1<-transform(nhdemVpct1, Sanders = as.numeric(Sanders)) #Coazert to numeric

nhdem2 <- merge(x = nhdemVpct1, y = nhdemVnum1, by = "co_id", all.x = TRUE) #Left outer join.

#South Carolina 
scd1 <-melt(scD$counties$race.candidates, id=1:15) ##Yes!474 obs. 16 variables. Yes! What does ID mean? 
scd2 <-scD$counties[1:2] # co_id and (county) name

scd2$L1<-seq.int(nrow(scd2)) # Add a sequence number. Will merge with
# the L1 variable created by the melt() . Will then perform a 1:many
# join.

scdem <- merge(x = scd1, y = scd2, by = "L1", all.x = TRUE) #Left outer join.

# Use dcast to transpose by the county name, code, and candidate
scdemVpct1 <-dcast(scdem, L1 + name + co_id ~ lname , value.var="pctDecimal")
scdemVnum1 <-dcast(scdem, co_id ~ lname , value.var="votes")

scdemVpct1<-transform(scdemVpct1, Clinton = as.numeric(Clinton)) #Coazert to numeric
scdemVpct1<-transform(scdemVpct1, Sanders = as.numeric(Sanders)) #Coazert to numeric

scdem2 <- merge(x = scdemVpct1, y = scdemVnum1, by = "co_id", all.x = TRUE) #Left outer join.

#Arizona
azd1 <-melt(azD$counties$race.candidates, id=1:15) ##Yes!85 obs. 16 variables. Yes! What does ID mean? 
azd2 <-azD$counties[1:2] # co_id and (county) name

azd2$L1<-seq.int(nrow(azd2)) # Add a sequence number. Will merge with
# the L1 variable created by the melt() . Will then perform a 1:many
# join.

azdem <- merge(x = azd1, y = azd2, by = "L1", all.x = TRUE) #Left outer join.

# Use dcast to transpose by the county name, code, and candidate
azdemVpct1 <-dcast(azdem, L1 + name + co_id ~ lname , value.var="pctDecimal")
azdemVnum1 <-dcast(azdem, co_id ~ lname , value.var="votes")

azdemVpct1<-transform(azdemVpct1, Clinton = as.numeric(Clinton)) #Coazert to numeric
azdemVpct1<-transform(azdemVpct1, Sanders = as.numeric(Sanders)) #Coazert to numeric

azdem2 <- merge(x = azdemVpct1, y = azdemVnum1, by = "co_id", all.x = TRUE) #Left outer join.

######################### flattening republicans ############################

#New Hampshire
nhr1 <-melt(nhR$counties$race.candidates, id=1:15) ##Yes!1896 obs. 16 variables. Yes! What does ID mean? 
nhr2 <-nhR$counties[1:2] # co_id and county name

nhr2$L1<-seq.int(nrow(nhr2)) # Add a sequence number. Will merge with
# the L1 variable created by the melt() . Will then perform a 1:many
# join.

nhrep <- merge(x = nhr1, y = nhr2, by = "L1", all.x = TRUE) #Left outer join.

# Use dcast to transpose by the county name, code, and candidate
nhrepVpct1 <-dcast(nhrep, L1 + name + co_id ~ lname , value.var="pctDecimal")
nhrepVnum1 <-dcast(nhrep, co_id ~ lname , value.var="votes")

nhrepVpct1<-transform(nhrepVpct1, Fiorina = as.numeric(Christie)) #Coazert to numeric
nhrepVpct1<-transform(nhrepVpct1, Cruz = as.numeric(Cruz)) #Coazert to numeric
nhrepVpct1<-transform(nhrepVpct1, Kasich = as.numeric(Kasich)) #Coazert to numeric
nhrepVpct1<-transform(nhrepVpct1, Trump = as.numeric(Trump)) #Coazert to numeric
nhrepVpct1<-transform(nhrepVpct1, Bush = as.numeric(Bush)) #Coazert to numeric
nhrepVpct1<-transform(nhrepVpct1, Carson = as.numeric(Carson)) #Coazert to numeric
nhrepVpct1<-transform(nhrepVpct1, Christie = as.numeric(Christie)) #Coazert to numeric
nhrepVpct1<-transform(nhrepVpct1, Rubio = as.numeric(Rubio)) #Coazert to numeric

# Create a file with the percentages and vote counts in one file.
nhrep2 <- merge(x = nhrepVpct1, y = nhrepVnum1, by = "co_id", all.x = TRUE) #Left outer join.

#South Carolina
scr1 <-melt(scR$counties$race.candidates, id=1:15) ##Yes!276 obs. 16 variables. Yes! What does ID mean? 
scr2 <-scR$counties[1:2] # co_id and county name

scr2$L1<-seq.int(nrow(scr2)) # Add a sequence number. Will merge with
# the L1 variable created by the melt() . Will then perform a 1:many
# join.

screp <- merge(x = scr1, y = scr2, by = "L1", all.x = TRUE) #Left outer join.

# Use dcast to transpose by the county name, code, and candidate
screpVpct1 <-dcast(screp, L1 + name + co_id ~ lname , value.var="pctDecimal")
screpVnum1 <-dcast(screp, co_id ~ lname , value.var="votes")

# Christie and Fiorina dropped
screpVpct1<-transform(screpVpct1, Cruz = as.numeric(Cruz)) # to numeric
screpVpct1<-transform(screpVpct1, Kasich = as.numeric(Kasich)) # to numeric
screpVpct1<-transform(screpVpct1, Trump = as.numeric(Trump)) # to numeric
screpVpct1<-transform(screpVpct1, Bush = as.numeric(Bush)) # numeric
screpVpct1<-transform(screpVpct1, Carson = as.numeric(Carson)) # to numeric
screpVpct1<-transform(screpVpct1, Rubio = as.numeric(Rubio)) # to numeric

# Create a file with the percentages and vote counts in one file.
screp2 <- merge(x = screpVpct1, y = screpVnum1, by = "co_id", all.x = TRUE) #Left outer join.

#Arizona
azr1 <-melt(azR$counties$race.candidates, id=1:15) ##Yes!30 obs. 16 variables. Yes! What does ID mean? 
azr2 <-azR$counties[1:2] # co_id and county name

azr2$L1<-seq.int(nrow(azr2)) # Add a sequence number. Will merge with
# the L1 variable created by the melt() . Will then perform a 1:many
# join.

azrep <- merge(x = azr1, y = azr2, by = "L1", all.x = TRUE) #Left outer join.

# Use dcast to transpose by the county name, code, and candidate
azrepVpct1 <-dcast(azrep, L1 + name + co_id ~ lname , value.var="pctDecimal")
azrepVnum1 <-dcast(azrep, co_id ~ lname , value.var="votes")

# Bush, Carson, and Rubio gone. Down to Trump, Cruz, and kasich
azrepVpct1<-transform(azrepVpct1, Cruz = as.numeric(Cruz)) # to numeric
azrepVpct1<-transform(azrepVpct1, Kasich = as.numeric(Kasich)) # to numeric
azrepVpct1<-transform(azrepVpct1, Trump = as.numeric(Trump)) # to numeric

# Create a file with the percentages and vote counts in one file.
azrep2 <- merge(x = azrepVpct1, y = azrepVnum1, by = "co_id", all.x = TRUE) #Left outer join.

################### calculate the margins  ######################
#https://www.usenix.org/legacy/event/evtwote11/tech/final_files/Cary8-2-11.pdf
#Conference Paper: Estimating the margin of victory for instant-runoff voting
#Conference: Proceedings of the 2011 conference on Electronic voting technology/workshop on trustworthy elections 
#David Cary

#Deﬁnition1. The margin of victory for a single election contest is 
#the minimum total number of ballots that must in some combination 
#be added and removed in order for the set of contest winner(s) to 
#change with some positive probability.
#For a single-winner plurality contest, the margin of victory is 
#the difference of the vote totals of two candidates with the most 
#votes, the winner and a runner-up. 

#To put the absolute number into context, I'll divide it by the 
#total number of votes. 


#Use this one.
#https://www.usenix.org/legacy/event/evtwote11/tech/final_files/Magrino.pdf: 
#  it is half the difference in votes between the winner and the runnerup, 
#  rounded up if necessary. (For our purposes we assume that ties always 
#  break in a manner so as to produce achangedoutcome; 
#  otherwisethe“tie-free”marginmay be one vote larger.)  
#  the minimum number of ballots that, if they were different, would 
#  change the winner of the election. 

# percentage of total votes 
# that separated the winner and the second-place finisher
# Margin = (Votes for Winner - Votes for second place)/ total votes.

# Democrats first.

# New Hampshire
nhdem2$tvotes <- rowSums(nhdem2[c(6:7)], na.rm = TRUE)
nhdem2$first <- do.call(pmax, nhdem2[6:7])
nhdem2$min <- do.call(pmin, nhdem2[6:7])
nhdem2$second <- nhdem2$tvotes - (nhdem2$first + nhdem2$min)
nhdem2$minVotes <- ceiling((nhdem2$first - nhdem2$second)/2)
nhdem2$margin <- round((nhdem2$minVotes)/nhdem2$tvotes,digits=2)

ties <-subset(nhdem2, nhdem2$first==nhdem2$second) 
ties # <0 rows> 

# Find the name of the winner. This is simple when only two candidates are running.
nhdem2$Winner <- ifelse(nhdem2$Sanders.y > nhdem2$Clinton.y,'Sanders','Clinton')

# South Carolina
scdem2$tvotes <- rowSums(scdem2[c(6:7)], na.rm = TRUE)
scdem2$first <- do.call(pmax, scdem2[6:7])
scdem2$min <- do.call(pmin, scdem2[6:7])
scdem2$second <- scdem2$tvotes - (scdem2$first + scdem2$min)
scdem2$minVotes <- ceiling((scdem2$first - scdem2$second)/2)
scdem2$margin <- round((scdem2$minVotes)/scdem2$tvotes,digits=2)

ties <-subset(scdem2, scdem2$first==scdem2$second) 
ties # <0 rows> 

# Find the name of the winner. This is simple when only two candidates are running.
scdem2$Winner <- ifelse(scdem2$Sanders.y > scdem2$Clinton.y,'Sanders','Clinton')

# New Hampshire
azdem2$tvotes <- rowSums(azdem2[c(6:7)], na.rm = TRUE)
azdem2$first <- do.call(pmax, azdem2[6:7])
azdem2$min <- do.call(pmin, azdem2[6:7])
azdem2$second <- azdem2$tvotes - (azdem2$first + azdem2$min)
azdem2$minVotes <- ceiling((azdem2$first - azdem2$second)/2)
azdem2$margin <- round((azdem2$minVotes)/azdem2$tvotes,digits=2)

ties <-subset(azdem2, azdem2$first==azdem2$second) 
ties # <0 rows> 

# Find the name of the winner. This is simple when only two candidates are running.
azdem2$Winner <- ifelse(azdem2$Sanders.y > azdem2$Clinton.y,'Sanders','Clinton')


################# Now, republicans. 

# New Hampshire
# Modified version since voters had more than two choices.
nhrep2$tvotes <- rowSums(nhrep2[c(12:19)], na.rm = TRUE)
nhrep2$first <- do.call(pmax, nhrep2[12:19])
nhrvotes <-subset(nhrep2,select=c(12:19)) #237 obs 8 vars.

# Find the name of the winner;
ties <-subset(nhrep2, nhrep2$first==nhrep2$second) 
ties # <0 rows> 

nhrWin <-as.data.frame(cbind(row.names(nhrvotes),apply(nhrvotes,1,function(x)
  names(nhrvotes)[which(x==max(x))])))
# V1 is a factor, so convertert to numeric before trying to merge.
#if you don't convert to character first, you'll get the underlying levels.
nhrWin$L1 = as.numeric(as.character(nhrWin$V1)) 
names(nhrWin)[names(nhrWin)=="V2"] <- "Winner"
nhrWin <-subset(nhrWin,select=c(Winner,L1))
nhrWin$Winner <-as.character(nhrWin$Winner)

# find the second highest number of votes;
nhr2nd <- as.data.frame(cbind(row.names(nhrvotes),apply(nhrvotes, 1, function(x) sort(x, decreasing=TRUE)[2]) 
))
nhr2nd$L1 = as.numeric(as.character(nhr2nd$V1)) #convert to numeric. 

names(nhr2nd)[names(nhr2nd)=="V2"] <- "second"
# Convert second from a factor to numeric. 
nhr2nd$second <- as.numeric(as.character(nhr2nd$second))

nhrk <- merge(x = nhr2nd, y = nhrWin, by = "L1") #winner and 2nd highest vote count.
nhrk$Winner<-sub(pattern = ".y", replacement = "",  x = nhrk$Winner) 

nhrep3 <- merge(x = nhrk, y = nhrep2, by = "L1") #Merge with source
nhrep3$minVotes <- ceiling((nhrep3$first - nhrep3$second)/2)
nhrep3$margin <- round((nhrep3$minVotes)/nhrep3$tvotes,digits=2)

# South Carolina
screp2$tvotes <- rowSums(screp2[c(10:15)], na.rm = TRUE)
screp2$first <- do.call(pmax, screp2[10:15])
scrvotes <-subset(screp2,select=c(10:15)) 

# Find the name of the winner;
ties <-subset(screp2, screp2$first==screp2$second) 
ties # <0 rows> 

scrWin <-as.data.frame(cbind(row.names(scrvotes),apply(scrvotes,1,function(x)
  names(scrvotes)[which(x==max(x))])))
# V1 is a factor, so convertert to numeric before trying to merge.
#if you don't convert to character first, you'll get the underlying levels.
scrWin$L1 = as.numeric(as.character(scrWin$V1)) 
names(scrWin)[names(scrWin)=="V2"] <- "Winner"
scrWin <-subset(scrWin,select=c(Winner,L1))
scrWin$Winner <-as.character(scrWin$Winner)

# find the second highest number of votes;
scr2nd <- as.data.frame(cbind(row.names(scrvotes),apply(scrvotes, 1, function(x) sort(x, decreasing=TRUE)[2]) 
))
scr2nd$L1 = as.numeric(as.character(scr2nd$V1)) #convert to numeric. 

names(scr2nd)[names(scr2nd)=="V2"] <- "second"
# Convert second from a factor to numeric. 
scr2nd$second <- as.numeric(as.character(scr2nd$second))

scrk <- merge(x = scr2nd, y = scrWin, by = "L1") #winner and 2nd highest vote count.
scrk$Winner<-sub(pattern = ".y", replacement = "",  x = scrk$Winner) 

screp3 <- merge(x = scrk, y = screp2, by = "L1") #Merge with source
screp3$minVotes <- ceiling((screp3$first - screp3$second)/2)
screp3$margin <- round((screp3$minVotes)/screp3$tvotes,digits=2)

# South Carolina
azrep2$tvotes <- rowSums(azrep2[c(7:9)], na.rm = TRUE)
azrep2$first <- do.call(pmax, azrep2[7:9])
azrvotes <-subset(azrep2,select=c(7:9)) 

# Find the name of the winner;
ties <-subset(azrep2, azrep2$first==azrep2$second) 
ties # <0 rows> 

azrWin <-as.data.frame(cbind(row.names(azrvotes),apply(azrvotes,1,function(x)
  names(azrvotes)[which(x==max(x))])))
# V1 is a factor, so convertert to numeric before trying to merge.
#if you don't convert to character first, you'll get the underlying levels.
azrWin$L1 = as.numeric(as.character(azrWin$V1)) 
names(azrWin)[names(azrWin)=="V2"] <- "Winner"
azrWin <-subset(azrWin,select=c(Winner,L1))
azrWin$Winner <-as.character(azrWin$Winner)

# find the second highest number of votes;
azr2nd <- as.data.frame(cbind(row.names(azrvotes),apply(azrvotes, 1, function(x) sort(x, decreasing=TRUE)[2]) 
))
azr2nd$L1 = as.numeric(as.character(azr2nd$V1)) #convert to numeric. 

names(azr2nd)[names(azr2nd)=="V2"] <- "second"
# Convert second from a factor to numeric. 
azr2nd$second <- as.numeric(as.character(azr2nd$second))

azrk <- merge(x = azr2nd, y = azrWin, by = "L1") #winner and 2nd highest vote count.
azrk$Winner<-sub(pattern = ".y", replacement = "",  x = azrk$Winner) 

azrep3 <- merge(x = azrk, y = azrep2, by = "L1") #Merge with source
azrep3$minVotes <- ceiling((azrep3$first - azrep3$second)/2)
azrep3$margin <- round((azrep3$minVotes)/azrep3$tvotes,digits=2)

############################# Aggregating #########################

# Republicans - New Hampshire
nhrtabl <-aggregate(first~margin+Winner, data=nhrep3, sum, na.rm=TRUE)
nhrbubble=aggregate(tvotes~margin+Winner, data=nhrep3, sum, na.rm=TRUE)
nhrtabl<- merge(x = nhrtabl, y = nhrbubble, by = c("margin","Winner"))
nhrtabl$radius <- round(sqrt( nhrtabl$tvotes/ pi ),digits=1) 

# Republicans - South Carolina
scrtabl <-aggregate(first~margin+Winner, data=screp3, sum, na.rm=TRUE)
scrbubble=aggregate(tvotes~margin+Winner, data=screp3, sum, na.rm=TRUE)
scrtabl<- merge(x = scrtabl, y = scrbubble, by = c("margin","Winner"))
scrtabl$radius <- sqrt( scrtabl$tvotes/ pi ) 

# Republicans - Arizona
azrtabl <-aggregate(first~margin+Winner, data=azrep3, sum, na.rm=TRUE)
azrbubble=aggregate(tvotes~margin+Winner, data=azrep3, sum, na.rm=TRUE)
azrtabl<- merge(x = azrtabl, y = azrbubble, by = c("margin","Winner"))
azrtabl$radius <- sqrt( azrtabl$tvotes/ pi ) 

# Democrats - New Hampshire
nhdtabl <-aggregate(first~margin+Winner, data=nhdem2, sum, na.rm=TRUE)
nhdbubble=aggregate(tvotes~margin+Winner, data=nhdem2, sum, na.rm=TRUE)
nhdtabl<- merge(x = nhdtabl, y = nhdbubble, by = c("margin","Winner"))
nhdtabl$radius <- sqrt( nhdtabl$tvotes/ pi ) 

# Democrats - South Carolina
scdtabl <-aggregate(first~margin+Winner, data=scdem2, sum, na.rm=TRUE)
scdbubble=aggregate(tvotes~margin+Winner, data=scdem2, sum, na.rm=TRUE)
scdtabl<- merge(x = scdtabl, y = scdbubble, by = c("margin","Winner"))
scdtabl$radius <- sqrt( scdtabl$tvotes/ pi ) 

# Democrats - Arizona
azdtabl <-aggregate(first~margin+Winner, data=azdem2, sum, na.rm=TRUE)
azdbubble=aggregate(tvotes~margin+Winner, data=azdem2, sum, na.rm=TRUE)
azdtabl<- merge(x = azdtabl, y = azdbubble, by = c("margin","Winner"))
azdtabl$radius <- sqrt( azdtabl$tvotes/ pi ) 


############## A quick look at the distributions ####################
# Will eventually want to map these, so graph the margins 
# to see where reasonable cut-points will be. 

library(ggplot2,lib.loc='c:/r/packages')
library(proto,lib.loc='c:/r/packages')
library(labeling,lib.loc='c:/r/packages')
library(digest,lib.loc='c:/r/packages')

# Bar-Charts New Hampshire
nhrepBar<-ggplot(nhrtabl, aes(x=margin, y=first,fill=Winner)) + 
  geom_bar(stat="identity") + scale_x_continuous(limits=c(0,.5)) +
  xlab("Margin of Victory") +
  ylab("Votes Cast for Winner") +
  ggtitle("New Hampshire Republican Primary Presidential Race\nMargins of Victory Across Counties")

nhdemBar <-ggplot(nhdtabl, aes(x=margin, y=first,fill=Winner)) + 
  geom_bar(stat="identity") + scale_x_continuous(limits=c(0,.5)) +
  xlab("Margin of Victory") +
  ylab("Votes Cast for Winner") +
  ggtitle("New Hampshire Democratic Primary Presidential Race\nMargins of Victory Across Counties")

# Bar-Charts South Carolina
screpBar<-ggplot(scrtabl, aes(x=margin, y=first,fill=Winner)) + 
  geom_bar(stat="identity") + scale_x_continuous(limits=c(0,.5)) +
  xlab("Margin of Victory") +
  ylab("Votes Cast for Winner") +
  ggtitle("South Carolina Republican Primary Presidential Race\nMargins of Victory Across Counties")

scdemBar <-ggplot(scdtabl, aes(x=margin, y=first,fill=Winner)) + 
  geom_bar(stat="identity") + scale_x_continuous(limits=c(0,.5)) +
  xlab("Margin of Victory") +
  ylab("Votes Cast for Winner") +
  ggtitle("South Carolina Democratic Primary Presidential Race\nMargins of Victory Across Counties")

# Bar-Charts Arizona
azrepBar<-ggplot(azrtabl, aes(x=margin, y=first,fill=Winner)) + 
  geom_bar(stat="identity") + scale_x_continuous(limits=c(0,.5)) +
  xlab("Margin of Victory") +
  ylab("Votes Cast for Winner") +
  ggtitle("Arizona Republican Primary Presidential Race\nMargins of Victory Across Counties")

azdemBar <-ggplot(azdtabl, aes(x=margin, y=first,fill=Winner)) + 
  geom_bar(stat="identity") + scale_x_continuous(limits=c(0,.5)) +
  xlab("Margin of Victory") +
  ylab("Votes Cast for Winner") +
  ggtitle("Arizona Democratic Primary Presidential Race\nMargins of Victory Across Counties")


library(gridExtra,lib.loc='c:/r/packages')

grid.arrange(nhdemBar, nhrepBar,ncol = 2) 
grid.arrange(scdemBar, screpBar,ncol = 2) 
grid.arrange(azdemBar, azrepBar,ncol = 2) 



# Bubble plots

ggplot(data=nhrtabl, aes(x=margin, y=Counties)) +
  geom_point(aes(size=radius,color=Winner)) + 
  scale_x_continuous(limits=c(0,.5)) +
  xlab("Margin of Victory") +
  ggtitle("New Hampshire Republican Primary Presidential Race\nMargins of Victory Across Counties")

## Plotly? 


library('ggplot2',lib.loc="c:/r/packages")
library('plotly',lib.loc='c:/r/packages')
library('yaml',lib.loc="c:/r/packages")
library('RColorBrewer',lib.loc="c:/r/packages")

nhdxr <- merge(x = nhrtabl, y = nhdtabl, by = "margin",all.x = TRUE,all.y=TRUE) 


plot_ly(nhrtabl, x = margin, y = first, size = radius, 
        color=Winner,mode = "markers",text = paste("Votes: ", tvotes))

plot_ly(nhdxr, x = margin, y=first.x, opacity = 0.6, type = "histogram") %>%
  add_trace(x = margin, y=first.y) %>%
  layout(barmode="overlay")
