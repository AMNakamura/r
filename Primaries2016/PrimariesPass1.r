# A Nakamura 
# PrimariesPass1.r
# April 9, 2016
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
                
# Nevada
nvD <- fromJSON('C:/_Rproject/Primaries/NV_E.txt',
                flatten=TRUE) 
nvR <- fromJSON('C:/_Rproject/Primaries/NV_S.txt',
                flatten=TRUE) 
                

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

#Nevada
nvd1 <-melt(nvD$counties$race.candidates, id=1:15) ##Yes!85 obs. 16 variables. Yes! What does ID mean? 
nvd2 <-nvD$counties[1:2] # co_id and (county) name

nvd2$L1<-seq.int(nrow(nvd2)) # Add a sequence number. Will merge with
# the L1 variable created by the melt() . Will then perform a 1:many
# join.

nvdem <- merge(x = nvd1, y = nvd2, by = "L1", all.x = TRUE) #Left outer join.

# Use dcast to transpose by the county name, code, and candidate
nvdemVpct1 <-dcast(nvdem, L1 + name + co_id ~ lname , value.var="pctDecimal")
nvdemVnum1 <-dcast(nvdem, co_id ~ lname , value.var="votes")

nvdemVpct1<-transform(nvdemVpct1, Clinton = as.numeric(Clinton)) #Convert to numeric
nvdemVpct1<-transform(nvdemVpct1, Sanders = as.numeric(Sanders)) #Convert to numeric

nvdem2 <- merge(x = nvdemVpct1, y = nvdemVnum1, by = "co_id", all.x = TRUE) #Left outer join.######################### flattening republicans ############################

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


#Nevada
nvr1 <-melt(nvR$counties$race.candidates, id=1:15) ##Yes!30 obs. 16 variables. Yes! What does ID mean? 
nvr2 <-nvR$counties[1:2] # co_id and county name

nvr2$L1<-seq.int(nrow(nvr2)) # Add a sequence number. Will merge with
# the L1 variable created by the melt() . Will then perform a 1:many
# join.

nvrep <- merge(x = nvr1, y = nvr2, by = "L1", all.x = TRUE) #Left outer join.

# Use dcast to transpose by the county name, code, and candidate
nvrepVpct1 <-dcast(nvrep, L1 + name + co_id ~ lname , value.var="pctDecimal")
nvrepVnum1 <-dcast(nvrep, co_id ~ lname , value.var="votes")

# Bush gone. Down to Trump, Carson, Cruz, Rubio, and Kasich
nvrepVpct1<-transform(nvrepVpct1, Cruz = as.numeric(Cruz)) # to numeric
nvrepVpct1<-transform(nvrepVpct1, Kasich = as.numeric(Kasich)) # to numeric
nvrepVpct1<-transform(nvrepVpct1, Trump = as.numeric(Trump)) # to numeric
nvrepVpct1<-transform(nvrepVpct1, Carson = as.numeric(Carson)) # to numeric
nvrepVpct1<-transform(nvrepVpct1, Rubio = as.numeric(Rubio)) # to numeric

# Create a file with the percentages and vote counts in one file.
nvrep2 <- merge(x = nvrepVpct1, y = nvrepVnum1, by = "co_id", all.x = TRUE) #Left outer join.

################### calculate the margins  ######################
# Margin = (Votes for Winner - Votes for second place)/ total votes.

# Democrats first.

# New Hampshire
nhdem2 <- nhdem2[order(nhdem2$L1),]
nhdem2$tvotes <- rowSums(nhdem2[c(6:7)], na.rm = TRUE)
nhdem2$first <- do.call(pmax, nhdem2[6:7])
nhdem2$second <- do.call(pmin, nhdem2[6:7])

nhdem2$minVotes <- ceiling((nhdem2$first - nhdem2$second)/2)
nhdem2$margin <- round((nhdem2$minVotes)/nhdem2$tvotes,digits=2)
summary(nhdem2$margin)

ties <-subset(nhdem2, nhdem2$first==nhdem2$second) 
ties 

# Find the name of the winner. This is simple when only two candidates are running.
nhdem2$Winner <- ifelse(nhdem2$Sanders.y > nhdem2$Clinton.y,'Sanders','Clinton')

# South Carolina
scdem2$tvotes <- rowSums(scdem2[c(6:7)], na.rm = TRUE)
scdem2$first <- do.call(pmax, scdem2[6:7])
scdem2$second <- do.call(pmin, scdem2[6:7])
scdem2$minVotes <- ceiling((scdem2$first - scdem2$second)/2)
scdem2$margin <- round((scdem2$minVotes)/scdem2$tvotes,digits=2)
summary(scdem2$margin)

ties <-subset(scdem2, scdem2$first==scdem2$second) 
ties  

# Find the name of the winner. This is simple when only two candidates are running.
scdem2$Winner <- ifelse(scdem2$Sanders.y > scdem2$Clinton.y,'Sanders','Clinton')

# Arizona
azdem2 <- azdem2[order(azdem2$L1),]
azdem2$tvotes <- rowSums(azdem2[c(6:7)], na.rm = TRUE)
azdem2$first <- do.call(pmax, azdem2[6:7])
azdem2$second <- do.call(pmin, azdem2[6:7])
azdem2$minVotes <- ceiling((azdem2$first - azdem2$second)/2)
azdem2$margin <- round((azdem2$minVotes)/azdem2$tvotes,digits=2)
summary(azdem2$margin)

ties <-subset(azdem2, azdem2$first==azdem2$second) 
ties 

# Find the name of the winner. This is simple when only two candidates are running.
azdem2$Winner <- ifelse(azdem2$Sanders.y > azdem2$Clinton.y,'Sanders','Clinton')


# Nevada
nvdem2 <- azdem2[order(nvdem2$L1),]
nvdem2$tvotes <- rowSums(nvdem2[c(6:7)], na.rm = TRUE)
nvdem2$first <- do.call(pmax, nvdem2[6:7])
nvdem2$second <- do.call(pmin, nvdem2[6:7])
nvdem2$minVotes <- ceiling((nvdem2$first - nvdem2$second)/2)
nvdem2$margin <- round((nvdem2$minVotes)/nvdem2$tvotes,digits=2)
summary(nvdem2$margin)

ties <-subset(nvdem2, nvdem2$first==nvdem2$second) 
ties 

# Find the name of the winner. This is simple when only two candidates are running.
nvdem2$Winner <- ifelse(nvdem2$Sanders.y > nvdem2$Clinton.y,'Sanders','Clinton')
# ties
nvdem2$Winner <- ifelse(nvdem2$first==nvdem2$second,'Tied',nvdem2$Winner)


################# Now, republicans ########################################### 

# New Hampshire
# Modified version since voters had more than two choices.
nhrep2 <- nhrep2[order(nhrep2$L1),]
rownames(nhrep2) <- NULL
nhrep2$tvotes <- rowSums(nhrep2[,grepl(".y", names(nhrep2))], na.rm = TRUE)
nhrep2$first <- do.call(pmax, nhrep2[,grepl(".y", names(nhrep2))])
nhrvotes <-nhrep2[,grepl(".y", names(nhrep2))]

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
summary(nhrep3$margin)

# Check for ties and change the winner to "Tied" if necessary
ties <-subset(nhrep3, nhrep3$first==nhrep3$second) 
ties

# South Carolina
screp2 <- screp2[order(screp2$L1),]
rownames(screp2) <- NULL
screp2$tvotes <- rowSums(screp2[,grepl(".y", names(screp2))], na.rm = TRUE)
screp2$first <- do.call(pmax, screp2[,grepl(".y", names(screp2))])
scrvotes <-screp2[,grepl(".y", names(screp2))]

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
summary(screp3$margin)

# Check for ties and change the winner to "Tied" if necessary
ties <-subset(screp3, screp3$first==screp3$second) 
ties 

# Arizona
azrep2 <- azrep2[order(azrep2$L1),]
rownames(azrep2) <- NULL
azrep2$tvotes <- rowSums(azrep2[,grepl(".y", names(azrep2))], na.rm = TRUE)
azrep2$first <- do.call(pmax, azrep2[,grepl(".y", names(azrep2))])
azrvotes <-azrep2[,grepl(".y", names(azrep2))]

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
summary(azrep3$margin)

# Check for ties and change the winner to "Tied" if necessary
ties <-subset(azrep3, azrep3$first==azrep3$second) 
ties # <0 rows> 

# Nevada
nvrep2 <- nvrep2[order(nvrep2$L1),] # in case JSON had counties out of id order
rownames(nvrep2) <- NULL
nvrep2$tvotes <- rowSums(nvrep2[,grepl(".y", names(nvrep2))], na.rm = TRUE)
nvrep2$first <- do.call(pmax, nvrep2[,grepl(".y", names(nvrep2))])
nvrvotes <-nvrep2[,grepl(".y", names(nvrep2))]

nvrWin <-as.data.frame(cbind(row.names(nvrvotes),apply(nvrvotes,1,function(x)
  names(nvrvotes)[which(x==max(x))])))
# V1 is a factor, so convertert to numeric before trying to merge.
#if you don't convert to character first, you'll get the underlying levels.
nvrWin$L1 = as.numeric(as.character(nvrWin$V1)) 
names(nvrWin)[names(nvrWin)=="V2"] <- "Winner"
nvrWin <-subset(nvrWin,select=c(Winner,L1))
nvrWin$Winner <-as.character(nvrWin$Winner)

# find the second highest number of votes;
nvr2nd <- as.data.frame(cbind(row.names(nvrvotes),apply(nvrvotes, 1, function(x) sort(x, decreasing=TRUE)[2]) 
))
nvr2nd$L1 = as.numeric(as.character(nvr2nd$V1)) #convert to numeric. 

names(nvr2nd)[names(nvr2nd)=="V2"] <- "second"

# Convert second from a factor to numeric. 
nvr2nd$second <- as.numeric(as.character(nvr2nd$second))

nvrk <- merge(x = nvr2nd, y = nvrWin, by = "L1") #winner and 2nd highest vote count.
nvrk$Winner<-sub(pattern = ".y", replacement = "",  x = nvrk$Winner) 

nvrep3 <- merge(x = nvrk, y = nvrep2, by = "L1") #Merge with source
nvrep3$minVotes <- ceiling((nvrep3$first - nvrep3$second)/2)
nvrep3$margin <- round((nvrep3$minVotes)/nvrep3$tvotes,digits=2)
summary(nvrep3$margin)

# Check for ties and change the winner to "Tied" if necessary
ties <-subset(nvrep2, nvrep2$first==nvrep2$second) 
ties # <0 rows> 

################### calculate the margins  ######################
# Margin = (Votes for Winner - Votes for second place)/ total votes.

# Democrats first.

# New Hampshire
nhdem2 <- nhdem2[order(nhdem2$L1),]
nhdem2$tvotes <- rowSums(nhdem2[c(6:7)], na.rm = TRUE)
nhdem2$first <- do.call(pmax, nhdem2[6:7])
nhdem2$second <- do.call(pmin, nhdem2[6:7])

nhdem2$minVotes <- ceiling((nhdem2$first - nhdem2$second)/2)
nhdem2$margin <- round((nhdem2$minVotes)/nhdem2$tvotes,digits=2)
summary(nhdem2$margin)

ties <-subset(nhdem2, nhdem2$first==nhdem2$second) 
ties 

# Find the name of the winner. This is simple when only two candidates are running.
nhdem2$Winner <- ifelse(nhdem2$Sanders.y > nhdem2$Clinton.y,'Sanders','Clinton')

# South Carolina
scdem2$tvotes <- rowSums(scdem2[c(6:7)], na.rm = TRUE)
scdem2$first <- do.call(pmax, scdem2[6:7])
scdem2$second <- do.call(pmin, scdem2[6:7])
scdem2$minVotes <- ceiling((scdem2$first - scdem2$second)/2)
scdem2$margin <- round((scdem2$minVotes)/scdem2$tvotes,digits=2)
summary(scdem2$margin)

ties <-subset(scdem2, scdem2$first==scdem2$second) 
ties  

# Find the name of the winner. This is simple when only two candidates are running.
scdem2$Winner <- ifelse(scdem2$Sanders.y > scdem2$Clinton.y,'Sanders','Clinton')

# Arizona
azdem2 <- azdem2[order(azdem2$L1),]
azdem2$tvotes <- rowSums(azdem2[c(6:7)], na.rm = TRUE)
azdem2$first <- do.call(pmax, azdem2[6:7])
azdem2$second <- do.call(pmin, azdem2[6:7])
azdem2$minVotes <- ceiling((azdem2$first - azdem2$second)/2)
azdem2$margin <- round((azdem2$minVotes)/azdem2$tvotes,digits=2)
summary(azdem2$margin)

ties <-subset(azdem2, azdem2$first==azdem2$second) 
ties 

# Find the name of the winner. This is simple when only two candidates are running.
azdem2$Winner <- ifelse(azdem2$Sanders.y > azdem2$Clinton.y,'Sanders','Clinton')


# Nevada
nvdem2 <- azdem2[order(nvdem2$L1),]
nvdem2$tvotes <- rowSums(nvdem2[c(6:7)], na.rm = TRUE)
nvdem2$first <- do.call(pmax, nvdem2[6:7])
nvdem2$second <- do.call(pmin, nvdem2[6:7])
nvdem2$minVotes <- ceiling((nvdem2$first - nvdem2$second)/2)
nvdem2$margin <- round((nvdem2$minVotes)/nvdem2$tvotes,digits=2)
summary(nvdem2$margin)

ties <-subset(nvdem2, nvdem2$first==nvdem2$second) 
ties 

# Find the name of the winner. This is simple when only two candidates are running.
nvdem2$Winner <- ifelse(nvdem2$Sanders.y > nvdem2$Clinton.y,'Sanders','Clinton')
# ties
nvdem2$Winner <- ifelse(nvdem2$first==nvdem2$second,'Tied',nvdem2$Winner)


################# Now, republicans. 

# New Hampshire
# Modified version since voters had more than two choices.
nhrep2 <- nhrep2[order(nhrep2$L1),]
rownames(nhrep2) <- NULL
nhrep2$tvotes <- rowSums(nhrep2[,grepl(".y", names(nhrep2))], na.rm = TRUE)
nhrep2$first <- do.call(pmax, nhrep2[,grepl(".y", names(nhrep2))])
nhrvotes <-nhrep2[,grepl(".y", names(nhrep2))]


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
summary(nhrep3$margin)

# Find the name of the winner;
ties <-subset(nhrep3, nhrep3$first==nhrep3$second) 
ties

# South Carolina
screp2 <- screp2[order(screp2$L1),]
rownames(screp2) <- NULL
screp2$tvotes <- rowSums(screp2[,grepl(".y", names(screp2))], na.rm = TRUE)
screp2$first <- do.call(pmax, screp2[,grepl(".y", names(screp2))])
scrvotes <-screp2[,grepl(".y", names(screp2))]


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
summary(screp3$margin)

ties <-subset(screp3, screp3$first==screp3$second) 
ties 

# Arizona
azrep2 <- azrep2[order(azrep2$L1),]
rownames(azrep2) <- NULL
azrep2$tvotes <- rowSums(azrep2[,grepl(".y", names(azrep2))], na.rm = TRUE)
azrep2$first <- do.call(pmax, azrep2[,grepl(".y", names(azrep2))])
azrvotes <-azrep2[,grepl(".y", names(azrep2))]

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
summary(azrep3$margin)

# Find the name of the winner;
ties <-subset(azrep3, azrep3$first==azrep3$second) 
ties # <0 rows> 


# Nevada
nvrep2 <- nvrep2[order(nvrep2$L1),] # in case JSON had counties out of id order
rownames(nvrep2) <- NULL
nvrep2$tvotes <- rowSums(nvrep2[,grepl(".y", names(nvrep2))], na.rm = TRUE)
nvrep2$first <- do.call(pmax, nvrep2[,grepl(".y", names(nvrep2))])
nvrvotes <-nvrep2[,grepl(".y", names(nvrep2))]

nvrWin <-as.data.frame(cbind(row.names(nvrvotes),apply(nvrvotes,1,function(x)
  names(nvrvotes)[which(x==max(x))])))
# V1 is a factor, so convertert to numeric before trying to merge.
#if you don't convert to character first, you'll get the underlying levels.
nvrWin$L1 = as.numeric(as.character(nvrWin$V1)) 
names(nvrWin)[names(nvrWin)=="V2"] <- "Winner"
nvrWin <-subset(nvrWin,select=c(Winner,L1))
nvrWin$Winner <-as.character(nvrWin$Winner)

# find the second highest number of votes;
nvr2nd <- as.data.frame(cbind(row.names(nvrvotes),apply(nvrvotes, 1, function(x) sort(x, decreasing=TRUE)[2]) 
))
nvr2nd$L1 = as.numeric(as.character(nvr2nd$V1)) #convert to numeric. 

names(nvr2nd)[names(nvr2nd)=="V2"] <- "second"

# Convert second from a factor to numeric. 
nvr2nd$second <- as.numeric(as.character(nvr2nd$second))

nvrk <- merge(x = nvr2nd, y = nvrWin, by = "L1") #winner and 2nd highest vote count.
nvrk$Winner<-sub(pattern = ".y", replacement = "",  x = nvrk$Winner) 

nvrep3 <- merge(x = nvrk, y = nvrep2, by = "L1") #Merge with source
nvrep3$minVotes <- ceiling((nvrep3$first - nvrep3$second)/2)
nvrep3$margin <- round((nvrep3$minVotes)/nvrep3$tvotes,digits=2)
summary(nvrep3$margin)

# Find the name of the winner;
ties <-subset(nvrep2, nvrep2$first==nvrep2$second) 
ties # <0 rows> 

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

# Republicans - Nevada
nvrtabl <-aggregate(first~margin+Winner, data=nvrep3, sum, na.rm=TRUE)
nvrbubble=aggregate(tvotes~margin+Winner, data=nvrep3, sum, na.rm=TRUE)
nvrtabl<- merge(x = nvrtabl, y = nvrbubble, by = c("margin","Winner"))
nvrtabl$radius <- sqrt( nvrtabl$tvotes/ pi ) 

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

# Democrats - Nevada
nvdtabl <-aggregate(first~margin+Winner, data=nvdem2, sum, na.rm=TRUE)
nvdbubble=aggregate(tvotes~margin+Winner, data=nvdem2, sum, na.rm=TRUE)
nvdtabl<- merge(x = nvdtabl, y = nvdbubble, by = c("margin","Winner"))
nvdtabl$radius <- sqrt( nvdtabl$tvotes/ pi ) 

############## A quick look at the distributions ####################
# Will eventually want to map these, so graph the margins 
# to see where reasonable cut-points will be. 

library(ggplot2,lib.loc='c:/r/packages')
library(proto,lib.loc='c:/r/packages')
library(labeling,lib.loc='c:/r/packages')
library(digest,lib.loc='c:/r/packages')

library(scales,lib.loc='c:/r/packages')

# Bar-Charts New Hampshire
# Used the solution provided in Stack Overflow for creating
# a subtitle with a global variable. See: http://stackoverflow.com/questions/19957536/add-dynamic-subtitle-using-ggplot
votes<-as.character(format(sum(nhrep3$tvotes),big.mark=","))
plot.title <-"New Hampshire Republican Primary Presidential Race"
plot.subtitle <-paste("Total votes: ", votes)

nhrepBar<-ggplot(nhrtabl, aes(x=margin, y=first,fill=Winner)) + 
  geom_bar(stat="identity") + scale_x_continuous(limits=c(0,.5)) +
  scale_y_continuous(labels = comma, limits=c(0,25000)) +
  xlab("Margin of Victory") +
  ylab("Votes Cast for Winner") +
  ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) 
  
votes<-as.character(format(sum(nhdem2$tvotes),big.mark=","))
plot.title <-"New Hampshire Democratic Primary Presidential Race"
plot.subtitle <-paste("Total votes: ", votes)

nhdemBar <-ggplot(nhdtabl, aes(x=margin, y=first,fill=Winner)) + 
  geom_bar(stat="identity") + scale_x_continuous(limits=c(0,.5)) +
  scale_y_continuous(labels = comma, limits=c(0,25000)) +
  xlab("Margin of Victory") +
  ylab("Votes Cast for Winner") +
  ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) 

# Bar-Charts South Carolina
votes<-as.character(format(sum(screp3$tvotes),big.mark=","))
plot.title <-"South Carolina Republican Primary Presidential Race"
plot.subtitle <-paste("Total votes: ", votes)

screpBar<-ggplot(scrtabl, aes(x=margin, y=first,fill=Winner)) + 
  geom_bar(stat="identity") + scale_x_continuous(limits=c(0,.5)) +
  scale_y_continuous(labels = comma, limits=c(0,25000)) +
  xlab("Margin of Victory") +
  ylab("Votes Cast for Winner") +
  ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) 

votes<-as.character(format(sum(scdem2$tvotes),big.mark=","))
plot.title <-"South Carolina Democratic Primary Presidential Race"
plot.subtitle <-paste("Total votes: ", votes)

scdemBar <-ggplot(scdtabl, aes(x=margin, y=first,fill=Winner)) + 
  geom_bar(stat="identity") + scale_x_continuous(limits=c(0,.5)) +
  scale_y_continuous(labels = comma, limits=c(0,25000)) +
  xlab("Margin of Victory") +
  ylab("Votes Cast for Winner") +
  ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) 

# Bar-Charts Arizona
votes<-as.character(format(sum(azrep3$tvotes),big.mark=","))
plot.title <-"Arizona Republican Primary Presidential Race"
plot.subtitle <-paste("Total votes: ", votes)

azrepBar<-ggplot(azrtabl, aes(x=margin, y=first,fill=Winner)) + 
  geom_bar(stat="identity") + scale_x_continuous(limits=c(0,.5)) +
  scale_y_continuous(labels = comma, limits=c(0,150000)) +
  xlab("Margin of Victory") +
  ylab("Votes Cast for Winner") +
  ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) 

votes<-as.character(format(sum(azdem2$tvotes),big.mark=","))
plot.title <-"Arizona Democratic Primary Presidential Race"
plot.subtitle <-paste("Total votes: ", votes)

azdemBar <-ggplot(azdtabl, aes(x=margin, y=first,fill=Winner)) + 
  geom_bar(stat="identity") + scale_x_continuous(limits=c(0,.5)) +
  scale_y_continuous(labels = comma, limits=c(0,150000)) +
  xlab("Margin of Victory") +
  ylab("Votes Cast for Winner") +
  ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) 

# Bar-Charts Nevada
votes<-as.character(format(sum(nvrep3$tvotes),big.mark=","))
plot.title <-"Nevada Republican Caucus Presidential Race"
plot.subtitle <-paste("Total votes: ", votes)

nvrepBar<-ggplot(nvrtabl, aes(x=margin, y=first,fill=Winner)) + 
  geom_bar(stat="identity") + scale_x_continuous(limits=c(0,.5)) +
  scale_y_continuous(labels = comma, limits=c(0,25000)) +
  xlab("Margin of Victory") +
  ylab("Votes Cast for Winner") +
  ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) 

votes<-as.character(format(sum(nvdem2$tvotes),big.mark=","))
plot.title <-"Nevada Democratic Caucus Presidential Race"
plot.subtitle <-paste("Total votes: ", votes)

nvdemBar <-ggplot(nvdtabl, aes(x=margin, y=first,fill=Winner)) + 
  geom_bar(stat="identity") + scale_x_continuous(limits=c(0,.5)) +
  scale_y_continuous(labels = comma, limits=c(0,25000)) +
  xlab("Margin of Victory") +
  ylab("Votes Cast for Winner") +
  ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) 

library(gridExtra,lib.loc='c:/r/packages')

grid.arrange(nhdemBar, nhrepBar,ncol = 2) 
grid.arrange(scdemBar, screpBar,ncol = 2) 
grid.arrange(azdemBar, azrepBar,ncol = 2) 
grid.arrange(nvdemBar, nvrepBar,ncol = 2)

save(nhR,file="nhR.Rda")
save(nhD,file="nhD.Rda")
save(scR,file="scR.Rda")
save(scD,file="scD.Rda")
save(azR,file="azR.Rda")
save(azD,file="azD.Rda")
save(nvR,file="nvR.Rda")
save(nvD,file="nvD.Rda")
save(nhrtabl,file="nhrtabl.Rda")
save(nhdtabl,file="nhdtabl.Rda")
save(scrtabl,file="scrtabl.Rda")
save(scdtabl,file="scdtabl.Rda")
save(azrtabl,file="azrtabl.Rda")
save(azdtabl,file="azdtabl.Rda")
save(nvrtabl,file="nvrtabl.Rda")
save(nvdtabl,file="nvdtabl.Rda")
