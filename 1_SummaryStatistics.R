##############################
### 1   Do Summary Statistics on Data obtained
##
##  1-1 Clean Check and Complie
##  1-1.1 Combine with 26 new people
##  1-1.2 Deal with user_10097 and user_10099 weekly table
##  1-1.3 Check data
##  1-1.4 Combine User features with looger table, same user order = list.train
##  1-1.5 Compile Data Sets
##
##  1-2 Summary on Users (Nodes)
##  1-2.1 Find Components of Users
##  1-2.2 How Old
##  1-2.3 Which Country
##  1-2.4 Which Continent
##  1-2.5 What Language - core / Prime users
##
##  1-3 Summary on Training Reports
##  1-3.1 Total Time / Counts on Training - Year
##  1-3.2 Total Time on Training - Weekly
##  1-3.3 How many hours By training category
##
##  1-4 Summary on Comments
##  1-4.1 Check Users with Messages and Comments
##  1-4.2 Comments On Messages
##  1-4.3 #Messages having Comments For each User
##  1-4.4 #Comments Users Made
##  1-4.5 Median #Comments On Messages For each User
##  1-4.6 #Messages Sent vs #Comments Received
##
##  1-5 Summary on Races
##
##  1-6 Summary on Groups
##############################


##############################
##  1-1 Clean Check and Complie
##  1-1.1 Combine with 26 new people
##############################
table.logger <- rbind(table.logger, table.logger.new.v2)
list.train365.weekly <- c(list.train365.weekly,list.train365.weekly.new)
list.train365.year <- c(list.train365.year, list.train365.year.new)
activities <- rbind(activities,activities.new)

# do precise version of table.logger
# due to some ruplicates on name variable, table.logger will not contain it
table.logger <- unique(subset(table.logger[,-2]))
# compile and save as alldata9file.RData

##############################
##  1-1.2 Deal with user_10097 and user_10099 weekly table
##############################
# weekly table of user_10097 has been scraped in 0_scrapedata_patch
list.train365.weekly$user_10097 <- list.train365.10097[[1]]
list.train365.weekly$user_10099 <- NA
# actually, weekly table of 10099 is of 10097. 10099 has no records.
# user_10097's activities are included in the activites table

##############################
##  1-1.3 Check data
##############################
# activities several changes on categories
activities$activity.cat[activities$activity.name =="trx"] <- "s"
activities$activity.cat[activities$activity.name =="turnen"] <- "s"
activities$activity.cat[activities$activity.name =="morning communte"] <- "b"

# new 26 people who are NA in weekly is now NULL, correct this mistake
nuweek <- names(list.train365.weekly)[unlist(lapply(list.train365.weekly,is.null))]
nuyear <- names(list.train365.year)[unlist(lapply(list.train365.year,is.null))]
nametochange <- setdiff(nuweek, nuyear)[-1] # 4 deleted and 6260 remain NULL
list.train365.weekly[match(nametochange, names(list.train365.weekly))] <- NA
# for user_6260 it's public, not deleted should be NA not NULL
list.train365.weekly$user_6260  <- NA

# check na from weekly table and yearly table, both have 774 na
naweek <- names(list.train365.weekly)[is.na(list.train365.weekly)]
nayear <- names(list.train365.year)[is.na(list.train365.year)]
identical(naweek, nayear) #True

# check user with 0 rows in weekly table
weekrow <- unlist(lapply(list.train365.weekly, nrow))
weekrow[weekrow ==0] # user_11657 and user_9612
# we know that 11657 only have one record that is not in 52*7 weeks
# But for user_9612, having 202 records, should scrape weekly table again
list.train365.weekly$user_9612 <- list.train365.9612[[1]]
# user_9612's activities are included in the activites table

# check total training counts
countweek <- unlist(lapply(match(names(weekrow),names(list.train365.weekly)), function(x)return(sum(as.numeric(as.character(list.train365.weekly[[x]]$count))))))
countyear <- unlist(lapply(match(names(weekrow),names(list.train365.weekly)), function(x){xx <- as.numeric(as.character(list.train365.year[[x]]$count));return(sum(xx[-length(xx)]))}))
countdiff <- countweek - countyear; countdiff[is.na(countdiff)] <- 0
# names(weekrow)[abs(countdiff) > 5]
# maximum diff is 7, so the data with row >1 are regarded as correct

##############################
##  1-1.4 Combine User features with looger table, same user order = list.train
##############################
# construct table in user order = list.train's order
# based on table.logger in alldata9file.RData
nameorder <- names(list.train365.weekly)

# save old version and change table.logger order
table.logger.old <- table.logger
table.logger <- table.logger[match(nameorder,as.character(table.logger$user)),]
identical(as.character(table.logger$user), nameorder) # True

# userfeature in alldata9file has something incorrect
# leave the names of new 26 people and remove this table
userf26 <- userfeature[2795:2820,]
# do again based on usernames and hidden training

## hidden: users who are public, public to members, need permissions
hidden <- c(hidden.training,rep("public",26))

## deleted: users who have deleted their accounts
userdelete <- c("user_5075","user_9597","user_10086","user_6259")
deleted <- nameorder %in% userdelete

## name1 and name2: name2 = previous names and NA
usernames <- unique(usernames)
nametable <- table(usernames$user)
user2name <- names(nametable)[nametable>1]
user1name <- names(nametable)[nametable==1]

# find previous names
name2line <- unlist(lapply(user2name, function(x)return(as.character(usernames[usernames$user == x,2])[2])))
# draw names to be name1
name1table <- subset(usernames, !(as.character(usernames$name) %in% name2line))
name1 <- as.character(name1table$name[match(nameorder,as.character(name1table$user))])
name1[2795:2820] <- as.character(userf26$name)

# now create name2 only 10 people have previous names
name2 <- rep(NA, length(nameorder))
name2[match(user2name, nameorder)] <- name2line
# new 26 users do not have previous names

## core users: who had records on training logs - core 2042; 774 NA; 4 NULL
core  <- !is.na(list.train365.year)
core[match(nuyear, nameorder)] <- F

## prime users: who had records on training logs for >26 weeks.
prime <- rep(F, length(nameorder))
prime[core] <- unlist(lapply(list.train365.weekly[core],function(x)return(length(unique(x$week))>26)))
# 1430 prime user

## combine them all together to be a new table.logger
table.logger <- data.frame(table.logger, name1, name2, hidden, deleted,
                           core, prime, stringsAsFactors = F)

##############################
##  1-1.5 Compile Data Sets
##############################
# All files below have been obtained.
# Loggers: table.logger (with country, language, name1&2, hidden/permission,
#                          delete, core user, prime user);
# Traning: list.train365.year, list.train365.weekly,
#           activities (training categories);
# Comment: table.comment365; 
# Groups:  table.actgroups, table.member;
# Races:   list.race;

# Now, Compile and Save nine datesets together.
save(table.logger,
     list.train365.year, list.train365.weekly, activities,
     table.comment365, 
     table.actgroups, table.member,
     list.race,
     file = "alldata8file_new.Rdata")
rm(list = ls())


##############################
##  1-2 Summary on Users (Nodes)
##  1-2.1 Find Components of Users
##############################
load("alldata8file_new.RData")

## get user's ids:
####################################################################
# active users:                                                    #
# An account/user was considered active if that user               #
# did any of the following during the 365 days to 2015/6/16:       #
# logged training, logged illness/injury, participated in races    #
# for which results were published on AP, made comments on an      #
# active user’s log.                                               #
####################################################################
people.ap <- unique(as.character(table.logger$user))
# core users: who had records on training logs
people.core <- as.character(table.logger$user[table.logger$core])
# prime users: who had records on training logs for >26 weeks.
people.prime <- as.character(table.logger$user[table.logger$prime])
# people who commented on messages
people.comment <- unique(as.character(table.comment365$user_reply))
people.send <- unique(as.character(table.comment365$user_send))

# people who participated in races
people.race  <- unique(unlist(lapply(1:length(list.race),function(i){return(
  as.character(list.race[[i]]$performance$user))})))
# people who are group members
people.grpmember <- unique(as.character(table.member$id))

## Plot VennDiagram by VennDiagram package
# Notice: there are some users who reported illness/injury only
# that will not shown on Venngram
library(VennDiagram)
venn.diagram(list(Commented = people.comment,
                  CoreUsers = people.core,
                  GroupMember = people.grpmember,
                  InRace = people.race),
             fill=c("red","green","blue","yellow"),
             alpha=c(0.5,0.5,0.5,0.5), cex=2.5,
             main = "VennDiagram for Core Users",
             filename="1-2-1-1 VennDiagram-Core.png")
venn.diagram(list(Commented = people.comment,
                  PrimeUsers = people.prime,
                  GroupMember = people.grpmember,
                  InRace = people.race),
             fill=c("red","skyblue","blue","yellow"),
             alpha=c(0.5,0.5,0.5,0.5), cex=2.5,
             main = "VennDiagram for Prime Users",
             filename="1-2-1-2 VennDiagram-Prime.png")
venn.diagram(list(CoreUsers = people.core,
                  PrimeUsers = people.prime,
                  ActiveUsers = people.ap),
             fill=c("green","skyblue","blue"),
             alpha=c(0.5,0.5,0.5), cex=1,
             main = "VennDiagram for Active Users",
             filename="1-2-1-3 VennDiagram-Active.png")


##############################
##  1-2.2 How Old
##############################

# Count APs don't show their birthyear = 1006 APs
sum(is.na(table.logger$birthyear))

# get user's birth years, without one write by mistake eg:1900 or 1
birth <- subset(table.logger, (is.na(table.logger$birthyear) ==F) &
                  (as.numeric(as.character(table.logger$birthyear))> 1900))
years <- as.numeric(as.character(as.vector(birth$birthyear)))

# find core user's birth years
birthact <- subset(birth, as.character(birth$user) %in% people.core)
yearsact <- as.numeric(as.character(as.vector(birthact$birthyear)))

# find prime user's birth years
birthpri <- subset(birth, as.character(birth$user) %in% people.prime)
yearspri <- as.numeric(as.character(as.vector(birthpri$birthyear)))

# histogram on birth years
library(httr)
par(mar = c(4,4,2,1))
hist(years, breaks = 30,col='skyblue',border=F, 
     main = "Histogram: Year of Birth", xlab = "")
hist(yearsact,add=T,breaks = 30,col=scales::alpha('red',.5),border=F)
hist(yearspri,add=T,breaks = 30,col=scales::alpha('green',.5),border=F)

legend("topleft", c("ActiveUser", "CoreUser","PrimeUser"), 
       col=c("skyblue", scales::alpha('red',.5)
             ,scales::alpha('green',.5)), pch = 15,pt.cex = 2)
#dev.copy(png,'1-2-2-1 HistogramBirthyear.png');dev.off()

# Boxplot on birth years
par(mar = c(4,4,2,1))
boxplot(years, yearsact,yearspri, varwidth = T,notch = T,  pch = 20,
        names = c("Active Users","Core Users", "Prime Users"), 
        col = threecolor,
        main = "Boxplots: Year of Birth")
#dev.copy(png,'1-2-2-2 BoxplotBirthyear.png');dev.off()


##############################
##  1-2.3 Which Country
##############################

# Count APs who don't show their country = 966
sum(is.na(table.logger$country))
# table of the counts at each country levels
table.country <- table(table.logger$country)

# match abbreviations with full name
#"at" "au" "be" "bg" "by" "ca" "ch" "cn" "cz" "de" "dk" "es" "fi" "fr" "gr"
#"hu" "ie" "il" "in" "it" "jp" "kg" "kr" "li" "lt" "lu" "lv" "ml" "ng" "nl"
#"no" "nz" "pl" "pr" "ro" "rs" "se" "si" "th" "tm" "tr" "tz" "uk" "us" "za"
ctryfullname <- c("Austria", "Australia", "Belgium", "Bulgaria", "Belarus",
                 "Canada", "Switzerland", "China", "Czech",
                 "Germany","Denmark", "Spain", "Finland", "France", "Greece",
                 "Hungary", "Ireland", "Israel", "India", "Italy", "Japan",
                 "Kyrgyzstan", "Korea", "Liechtenstein", "Lithuania",
                 "Luxembourg", "Latvia", "Mali", "Nigeria", "Netherlands",
                 "Norway", "New Zealand", "Poland", "Puerto Rico",
                 "Romania", "Serbia", "Sweden", "Slovenia", "Thailand",
                 "Turkmenistan", "Turkey", "Tanzania", "United Kingdom",
                 "United States", "South Africa")
# combine to table.country
table.country <- data.frame(table.country, ctryfullname)

# plot a wordcloud for countries
library(wordcloud)
wordcloud(table.country$ctryfullname, table.country$Freq, scale=c(4,0.8), min.freq = 1,
          random.color = T,
          rot.per = .35, use.r.layout=FALSE, colors=brewer.pal(8, 'Dark2'))
#dev.copy(png,'1-2-3-1 WordCloudCountry.png');dev.off()

## define colors for active/ core/ prime uses
threecolor <- c("skyblue",scales::alpha('red',.7),scales::alpha('green',.7))
##

# find top 10 countries have most active users
table.country10 <- table.country[order(table.country$Freq, decreasing = T)[1:10],]
# barplot on Top 10 countries
par(las=2) # make label text perpendicular to axis
par(mar=c(5,6,4,2)) # increase y-axis margin.
barplot(table.country10$Freq, names.arg = table.country10$ctryfullname, 
        main="Top10 Countries of Active Users",cex.names=0.8, horiz=TRUE,
        col=threecolor[1],xlab = "Number of Active Users",border = NA)
#dev.copy(png,'1-2-3-2 BarplotCountry.png');dev.off()

# find out core/prime users' countries
table.corelogger <- table.logger[table.logger$core,]
table.corecountry <- table(table.corelogger$country)
table.primelogger <- table.logger[table.logger$prime,]
table.primecountry <- table(table.primelogger$country)
# combine frequency for core/prime users with active users
FreqCore <- table.corecountry[match(as.character(table.country$Var1),
                                    names(table.corecountry))]
FreqPrime <- table.primecountry[match(as.character(table.country$Var1),
                                    names(table.primecountry))]
table.country <- data.frame(table.country,FreqCore, FreqPrime)

# find top 10 countries have most active users - again
table.country10 <- table.country[order(table.country$Freq, decreasing = T)[1:10],]

# grouped barplot on Top 10 countries
par(las=2) # make label text perpendicular to axis
par(mar=c(5,6,4,2)) # increase y-axis margin.
barplot(height = rbind(Freq = as.numeric(table.country10$Freq),
                       FreqCore = as.numeric(table.country10$FreqCore),
                       FreqPrime = as.numeric(table.country10$FreqPrime)),
        names.arg = table.country10$ctryfullname, beside = T, 
        main="Top10 Countries of Users",cex.names=0.8, horiz=TRUE, border = NA,
        col=threecolor, xlab = "Number of Users",
        legend = c("Active Users", "Core Users", "Prime Users"),
        args.legend = list(border = NA))
#dev.copy(png,'1-2-3-3 BarplotCountryACP.png');dev.off()


##############################
##  1-2.4 Which Continent
##############################
# match continent with country full names
print(ctryfullname)
continent <-  c("Asia","Europe","North America","South America",
                "Africa","Oceania","Antarctica")
ctrycontinent <- continent[c(2,6,2,2,2,3,2,1,2,2,2,2,2,2,2,2,
                             2,1,1,2,1,1,1,2,2,2,2,5,5,2,2,6,
                             2,3,2,2,2,2,1,1,2,5,2,3,5)]
# combine with table of countries
table.country <- data.frame(table.country, ctrycontinent)
# have freqency by continents
table.continent <- aggregate(x = subset(table.country, select = c(Freq, FreqCore, FreqPrime)),
                     by = list(table.country$ctrycontinent), FUN = sum, na.rm = T)

# grouped barplot on Continents
par(las=2) # make label text perpendicular to axis
par(mar=c(5,6,4,2)) # increase y-axis margin.
barplot(height = rbind(Freq = as.numeric(table.continent$Freq),
                       FreqCore = as.numeric(table.continent$FreqCore),
                       FreqPrime = as.numeric(table.continent$FreqPrime)),
        names.arg = table.continent$Group.1, beside = T, 
        main="Continents of Users",cex.names=0.8, horiz=TRUE,
        col=threecolor, xlab = "Number of Users", border = NA)
legend("bottomright",pch = 15, pt.cex = 2,  col= threecolor, border = NA,
       c("Active Users", "Core Users", "Prime Users"))
#dev.copy(png,'1-2-4-1 BarplotContinent.png');dev.off()

library(RColorBrewer)
# pie chart on continents - Active Users
slices <- as.matrix(table.continent$Freq)
lbls <- as.character(table.continent$Group.1)
pct <- round(slices/sum(slices)*100)
lbls <- paste0(lbls," ",pct, "%") # add percents% to labels 
par(mar = c(0,0,2,0))
pie(slices,labels = lbls, col=brewer.pal(5, "Set1"),
    main="Pie Chart of Active Users' Continents", border = NA)
#dev.copy(png,'1-2-4-2 PiechartContinentActive.png');dev.off()

# pie chart on continents - Core Users
slices <- as.matrix(table.continent$FreqCore)
lbls <- as.character(table.continent$Group.1)
pct <- round(slices/sum(slices)*100)
lbls <- paste0(lbls," ",pct, "%") # add percents% to labels 
par(mar = c(0,0,2,0))
pie(slices,labels = lbls, col=brewer.pal(5, "Set3"),
    main="Pie Chart of Core Users' Continents", border = NA)
#dev.copy(png,'1-2-4-3 PiechartContinentCore.png');dev.off()

# pie chart on continents - Prime Users
slices <- as.matrix(table.continent$FreqPrime)
lbls <- as.character(table.continent$Group.1)
pct <- round(slices/sum(slices)*100)
lbls <- paste0(lbls," ",pct, "%") # add percents% to labels 
par(mar = c(0,0,2,0))
pie(slices,labels = lbls, col=brewer.pal(5, "Set2"),
    main="Pie Chart of Prime Users' Continents", border = NA)
#dev.copy(png,'1-2-4-4 PiechartContinentPrime.png');dev.off()


##############################
##  1-2.5 What Language - core / Prime users
##############################
# Language are identified by their training logs
# only core / prime users have languages

# draw coreuser's languages - only 2042 people have language
table.langcore <- table.logger[table.logger$core,]
table.language <- table(table.langcore$language)

# match abbreviations with full name
# de   en   es   fi   fr   hu   it   lt   nl   no   se   sv 
langfullname <- c("German", "English", "Spanish", "Finnish", "French",
                   "Hungarian", "Italian", "Lithuanian", "Dutch","Norwegian",
                  "Swedish","Slovene" )
# combine to table.language
table.language <- data.frame(table.language, langfullname)

# plot a wordcloud for language
library(wordcloud)
wordcloud(table.language$langfullname, log(table.country$Freq), scale=c(5,1), min.freq = 0,
          random.color = T,
          rot.per = .35, use.r.layout=FALSE, colors=brewer.pal(12, 'Set1'))
#dev.copy(png,'1-2-5-1 WordCloudLanguageCore.png');dev.off()


# find out prime users' language
table.primelogger <- table.logger[table.logger$prime,]
table.primelanguage <- table(table.primelogger$language)
# combine frequency for prime users with active users
FreqPrime <- table.primelanguage[match(as.character(table.language$Var1),
                                      names(table.primelanguage))]
table.language <- data.frame(table.language,FreqPrime)

# grouped barplot on languages
par(las=2) # make label text perpendicular to axis
par(mar=c(5,6,4,2)) # increase y-axis margin.
barplot(height = rbind(FreqCore = as.numeric(table.language$Freq),
                       FreqPrime = as.numeric(table.language$FreqPrime)),
        names.arg = table.language$langfullname, beside = T, 
        main="Languages of Users",cex.names=0.8, horiz=TRUE, border = NA,
        col=threecolor[2:3], xlab = "Number of Users",
        legend = c("Core Users", "Prime Users"),
        args.legend = list(border = NA))
#dev.copy(png,'1-2-5-2 BarplotLanguageCP.png');dev.off()

# group on small languages as other
table.languageother <- data.frame(table.language,
                                  group2 = ifelse(table.language$Freq<20,
                                                  "Other",langfullname))
table.languageother <- aggregate(x = subset(table.languageother, 
                                            select = c(Freq, FreqPrime)),
                                 by = list(table.languageother$group2),
                                 FUN = sum)
# pie chart on Language - Core Users
slices <- as.matrix(table.languageother$Freq)
lbls <- as.character(table.languageother$Group.1)
pct <- round(slices/sum(slices)*100,2)
lbls <- paste0(lbls," ",pct, "%") # add percents% to labels 
par(mar = c(0,0,2,0))
pie(slices,labels = lbls, col=brewer.pal(9, "Set1"),
    main="Pie Chart of Core Users' Language", border = NA, cex = 1)
#dev.copy(png,'1-2-5-3 PiechartLanguageCore.png');dev.off()

# pie chart on Language - Prime Users
slices <- as.matrix(table.languageother$FreqPrime)
lbls <- as.character(table.languageother$Group.1)
pct <- round(slices/sum(slices)*100,2)
lbls <- paste0(lbls," ",pct, "%") # add percents% to labels 
par(mar = c(0,0,2,0))
pie(slices,labels = lbls, col=brewer.pal(8, "Dark2"),
    main="Pie Chart of Prime Users' Language", border = NA, cex = 1)
#dev.copy(png,'1-2-5-4 PiechartLanguagePrime.png');dev.off()


##############################
##  1-3 Summary on Training Reports
##  1-3.1 Total Time / Counts on Training - Year
##############################

# obtain total training time in a year for each core user
# Notice that only core users have training log records
tottimebyyear <- unlist(lapply(list.train365.year[table.logger$core],
                               function(x)return(x$time[nrow(x)])))
tottimeyname <- names(tottimebyyear)
tottimebyyear <- as.character(tottimebyyear)

# create function to transform hours in character to numeric eg:3.45 hours
hourtran <- function(hourchar){
  if(hourchar =="" | hourchar =="#"){
    hournum <- 0
  }else{
  hhmmss <- as.numeric(unlist(strsplit(hourchar,":")))
  if(length(hhmmss)==3) hournum <- hhmmss[1] + hhmmss[2]/60 + hhmmss[3]/3600
  if(length(hhmmss)==2) hournum <- hhmmss[1]/60 + hhmmss[2]/3600
  if(length(hhmmss)==1) hournum <- hhmmss[1]/3600
  }
  return(hournum)    
}
# apply hourtran function on tottimebyyear vector
tottimeyhours <- unlist(lapply(tottimebyyear, hourtran))
# draw prime users' training time by year
tottimeyhoursprime <- tottimeyhours[tottimeyname %in% people.prime]

# histogram on total training time by year
library(httr)
par(mar = c(4,4,2,1))
hist(tottimeyhours, breaks = 30,col=threecolor[2],border=F, 
     main = "Histogram of Annual Training Hours", xlab = "Hours")
hist(tottimeyhoursprime,add=T,breaks = 30,col=threecolor[3],border=F)
legend("topright", c("CoreUser","PrimeUser"), 
       col= threecolor[2:3], pch = 15,pt.cex = 2)
#dev.copy(png,'1-3-1-1 HistogramTrainHourYear.png');dev.off()

# Boxplot on total training time by year
par(mar = c(4,6,2,1))
boxplot(tottimeyhours, tottimeyhoursprime, varwidth = T,notch = T,  pch = 20,
        names = c("Core Users", "Prime Users"), horizontal = T,
        main = "Boxplots of Annual Training Hours")
#dev.copy(png,'1-3-1-2 BoxplotTrainHourYear.png');dev.off()

# obtain total training counts for core users
totcountbyyear <- unlist(lapply(list.train365.year[table.logger$core],
                               function(x)return(x$count[nrow(x)])))
totcountyname <- names(totcountbyyear)
totcountbyyear <- as.numeric(as.character(totcountbyyear))
# draw prime users' training counts by year
totcountbyyearprime <- totcountbyyear[tottimeyname %in% people.prime]

# histogram on total training counts by year
par(mar = c(4,4,2,1))
hist(totcountbyyear, breaks = 30,col=threecolor[2],border=F, 
     main = "Histogram of Annual Training Counts", xlab = "Counts")
hist(totcountbyyearprime,add=T,breaks = 30,col=threecolor[3],border=F)
legend("topright", c("CoreUser","PrimeUser"), 
       col= threecolor[2:3], pch = 15,pt.cex = 2)
#dev.copy(png,'1-3-1-3 HistogramTrainCountYear.png');dev.off()

# Boxplot on total training time by year
par(mar = c(4,6,2,1))
boxplot(totcountbyyear, totcountbyyearprime, varwidth = T,notch = T,  pch = 20,
        names = c("Core Users", "Prime Users"), horizontal = T,
        main = "Boxplots of Annual Training Counts")
#dev.copy(png,'1-3-1-4 BoxplotTrainCountYear.png');dev.off()

##############################
##  1-3.2 Total Time on Training - Weekly
##############################

# get core user's weekly training hours, return a vector, length = 52
tableweekhour <- function(listtable){
  if(nrow(listtable)==0) return(rep(0,52))
  weeks <- listtable$week
  hours <- unlist(lapply(as.character(listtable$time),hourtran))
  aggtime <- aggregate(x = as.data.frame(hours), by = list(weeks), FUN = sum)
  hourvec <- rep(0,52)
  hourvec[aggtime$Group.1] <- aggtime$hours
  return(hourvec)
}
tottimebyweek <- lapply(list.train365.weekly[table.logger$core],tableweekhour)
tottimewname <- names(tottimebyweek)
tottimebyweek <- unlist(tottimebyweek, recursive = T)
tottimewmat <- matrix(tottimebyweek,byrow = T, ncol = 52)
rownames(tottimewmat) <- tottimewname


# do Boxplot first - not a nice plot, most of them are around 0-10
par(mar = c(4,4,2,1))
boxplot(tottimewmat, pch = 20, xlab = "Weeks",
        ylab = "Hours", main = "Boxplot of Core User Weekly Training Hours",
        xaxt = "n")
axis(1, at = seq(1, 52, by = 5), las=2)
#dev.copy(png,'1-3-2-1 BoxplotTrainHourWeek.png');dev.off()

# check weekly training > 100
trainweek100 <- apply(tottimewmat>100,1,sum)
names(trainweek100[trainweek100>0])
round(tottimewmat[trainweek100>0,])
# "user_14268" - week 22 - 112h Adventure Racing - end in 11/13
#      112h depend on endday of week, 24h/day for a week
# "user_2655" - week 22 - 111h AP Competition
# "user_4445" - week 6 - 140h ride bike  
# "user_5715" - week 22 - 112h Adventure Race
# "user_7798" - week 51 107h Adventure Race
# Three of them are attending ARWC 2014
# See details: http://www.attackpoint.org/viewlog.jsp/user_2655/period-7/enddate-2014-11-22

# Lines of people - plot 50 people for a look
plot(tottimewmat[2000,], ylim = c(0,20),type = "l",
     xlab = "52 Weeks", ylab = "Hours",
     main = "Weekly Training Hours - 25 Samples")
for(i in 1:24){
  lines(tottimewmat[i*80,],type = "l", col = i)
}
#dev.copy(png,'1-3-2-2 TrainHourWeek25Sample.png');dev.off()

# Lines of people - plot 10 people for a look
plot(tottimewmat[2000,], ylim = c(0,20),type = "l", lwd = 2,
     xlab = "52 Weeks", ylab = "Hours",
     main = "Weekly Training Hours - 10 Samples")
for(i in 1:9){
  lines(tottimewmat[i*200,],type = "o", pch = 20,
        col = brewer.pal(9, "Set1")[i], lwd = 2)
}
#dev.copy(png,'1-3-2-3 TrainHourWeek10Sample.png');dev.off()

# draw prime users' total time 
tottimewmatprime <- tottimewmat[rownames(tottimewmat) %in% people.prime,]
# draw core\prime users' total time (core but not prime) 
tottimewmatNprime <- tottimewmat[!(rownames(tottimewmat) %in% people.prime),]

# Lines of people - plot 10 people for a look - prime
plot(tottimewmatprime[1400,], ylim = c(0,20),type = "l", lwd = 2,
     xlab = "52 Weeks", ylab = "Hours",
     main = "Weekly Training Hours - Prime User- 10 Samples")
for(i in 1:9){
  lines(tottimewmatprime[i*140,],type = "l", pch = 20,
        col = brewer.pal(9, "Set1")[i], lwd = 2)
}
#dev.copy(png,'1-3-2-4 TrainHourWeek10SamplePrime.png');dev.off()

# calculate the median of three groups
totweekcoremed <- apply(tottimewmat,2,median)
totweekprimemed <- apply(tottimewmatprime,2,median)
totweekNprimemed <- apply(tottimewmatNprime,2,median)

# plot median of core users and prime users
par(mar = c(4,4,4,1))
plot(totweekcoremed, type = "l", lwd = 2, col = 1,
     xlab = "52 Weeks", ylab = "Hours", ylim = c(min(totweekcoremed), max(totweekprimemed)),
     main = "Weekly Training Hours - Median")
lines(totweekprimemed, type = "l", lwd = 2, col = 2, lty = 2)
legend("topleft",c("CoreUser", "PrimeUser"), 
       pch = c(20,20), lty = c(1,2), col = c(1,2))
#dev.copy(png,'1-3-2-5 TrainHourWeekMedian.png');dev.off()

# plot median of core users and prime users and core\ prime users
par(mar = c(4,4,4,1))
plot(totweekcoremed, type = "l", lwd = 2, col = threecolor[2],
     xlab = "52 Weeks", ylab = "Hours", ylim = c(min(totweekNprimemed), max(totweekprimemed)),
     main = "Weekly Training Hours - Median")
lines(totweekprimemed, type = "l", lwd = 2, col = threecolor[3], lty = 2)
lines(totweekNprimemed, type = "l", lwd = 2, col = threecolor[1], lty = 3)

legend(x =30, y = 2.5,c("CoreUser", "PrimeUser", "Core[NotPrime]User"), 
       pch = c(20,20), lty = c(1,2,3), col = threecolor[c(2,3,1)])
#dev.copy(png,'1-3-2-6 TrainHourWeekMedianCore-Prime.png');dev.off()

##############################
##  1-3.3 How many hours By training category
##############################

## get aggregated table for training categories - by Year
# write a function: return a vector, length of 18, each one is hours
tablecathour <- function(listtable){
  listtable <- listtable[-nrow(listtable),]
  hours <- unlist(lapply(as.character(listtable$time),hourtran))
  cats <- activities$activity.cat[match(str_trim(tolower(listtable$activities)), activities$activity.name)]
  aggcat <- aggregate(x = as.data.frame(hours), by = list(cats), FUN = sum)
  hourvec <- rep(0,18)
  hourvec[aggcat$Group.1] <- aggcat$hours
  return(hourvec)  
}
# apply tablecathour function on each core users
cathours <- lapply(list.train365.year[table.logger$core], tablecathour)
cathoursname <- names(cathours)
cathours <- unlist(cathours, recursive = T)
cathours <- matrix(cathours, ncol = 18, byrow = T)
rownames(cathours) <- cathoursname
# draw prime users hours by categories 
cathoursprime <- cathours[cathoursname %in% people.prime,]

# pie chart on core users - by hours on each categories
library(RColorBrewer)
cathoursavg <- apply(cathours, 2,mean)
catlevels <- levels(activities$activity.cat)
slices <- as.matrix(cathoursavg)
pct <- round(slices/sum(slices)*100,1)
catgroup <- ifelse(pct<5,"Other",catlevels)
table.cathour <- aggregate(x = pct,by = list(catgroup),
                           FUN = sum, na.rm = T)
# Group.1: "b"     "fo"    "Other" "r"     "s"     "w"   
catgroupfullname <- c("Bike","Foot Orienteering","Others","Run",
                      "Strength","Walk")
lbls <- paste0(catgroupfullname," ",table.cathour$V1, "%") # add percents% to labels 
par(mar = c(0,0,2,0))
pie(table.cathour$V1,labels = lbls, col=brewer.pal(9, "Set1"),
    main="Pie Chart of Core User's Training Hours by Category", border = NA)
#dev.copy(png,'1-3-3-1 PiechartTrainCateCore.png');dev.off()

# pie chart on prime users - by hours on each categories
cathoursavg <- apply(cathoursprime, 2,mean)
catlevels <- levels(activities$activity.cat)
slices <- as.matrix(cathoursavg)
pct <- round(slices/sum(slices)*100,1)
catgroup <- ifelse(pct<5,"Other",catlevels)
table.cathour <- aggregate(x = pct,by = list(catgroup),
                           FUN = sum, na.rm = T)
# Group.1: "b"     "fo"    "Other" "r"     "s"     "w"   
catgroupfullname <- c("Bike","Foot Orienteering","Others","Run",
                      "Strength","Walk")
lbls <- paste0(catgroupfullname," ",table.cathour$V1, "%") # add percents% to labels 
par(mar = c(0,0,2,0))
pie(table.cathour$V1,labels = lbls, col=brewer.pal(9, "Set1"),
    main="Pie Chart of Prime User's Training Hours by Category", border = NA)
#dev.copy(png,'1-3-3-2 PiechartTrainCatePrime.png');dev.off()

## *get weekly hours by categories, write a function
catlevels <- levels(activities$activity.cat)
library(stringr) # str_trim()
tablecathour <- function(listtable){
  if(nrow(listtable)>0){
  listtable <- subset(listtable, listtable$time != ""&listtable$time != "#")
  hourarray <- array(0,dim = c(52,18))
  for(wk in 1:52){
    subtable <- subset(listtable, listtable$week == wk)
    if(nrow(subtable)>0){
      hours <- unlist(lapply(as.character(subtable$time),hourtran))
      abbact <- str_trim(tolower(subtable$activities))
      cats <- activities$activity.cat[match(abbact, activities$activity.name)]
      if(sum(is.na(cats))>0) cats[is.na(cats)] <- "o" # there are 30 activities NA
      aggcat <- aggregate(x = as.data.frame(hours), by = list(cats), FUN = sum)      
      groupnum <- match(aggcat$Group.1,catlevels)
      hourarray[wk,groupnum] <-  aggcat$hours
    }
  }
  return(hourarray)  
}}
# apply function on list of weekly training 
cathourweek <- sapply(list.train365.weekly[table.logger$core], tablecathour,simplify = "array")

# calculate the median/ 75%/ 25% hours of each week each category
cathourweekmed <- array(NA, c(52,18))
cathourweekuf <- array(NA, c(52,18))
cathourweeklf <- array(NA, c(52,18))

for (x in 1:52){
  for(y in 1:18){
    xy <- unlist(lapply(cathourweek, function(t){return(t[x,y])}))
    cathourweekmed[x,y] <- median(xy)
    cathourweekuf[x,y] <- quantile(xy,0.75)
    cathourweeklf[x,y] <- quantile(xy,0.25)    
  }
}
# lower fourth are all 0, median only have run category larger than zero, 0.5-0.7
par(mar = c(4,4,4,1))
plot(totweekcoremed, type = "l", lwd = 2, col = threecolor[2],
     xlab = "52 Weeks", ylab = "Hours", ylim = c(0, max(totweekprimemed)),
     main = "Weekly Training Hours - Median")
# plot run weekly median to have a look. 
lines(cathourweekmed[,11], type = "l", lwd = 2, col = threecolor[1])
# no need to save this plot

##############################
##  1-4 Summary on Comments
##  1-4.1 Check Users with Messages and Comments
##############################

## check how many core / prime users do not reply or send messages
# core users: who had records on training logs
people.core <- as.character(table.logger$user[table.logger$core])
# prime users: who had records on training logs for >26 weeks.
people.prime <- as.character(table.logger$user[table.logger$prime])
# people who commented on messages
people.comment <- unique(as.character(table.comment365$user_reply))
# people who sent messages that receive replies
people.receive <- unique(as.character(table.comment365$user_send))
# Plot VennDiagram 
library(VennDiagram)
venn.diagram(list(CoreUsers = people.core,
                  PrimeUsers = people.prime,
                  ReceivedReply = people.receive,
                  Commented = people.comment),
             fill=c("red","green","blue","yellow"),
             alpha=c(0.5,0.5,0.5,0.5), cex=2.5,
             main = "VennDiagram for Messages and Comments",
             filename="1-4-1 VennDiagram-Comment.png")
# From Venn Plot, all but two who received replies are core users,
# 163 who commented on other's logs are not core users.

##############################
##  1-4.2 Comments On Messages
##############################

# In total, 24183 messages have comments
length(unique(table.comment365$message_id))
# comments below active users' messages(24183 msgs)
message.ap <- table(as.character(table.comment365$message_id))
# comments below core users' messages(24151 msgs)
table.comment365core <- subset(table.comment365, table.comment365$user_send %in% people.core)
message.core <- table(as.character(table.comment365core$message_id))
# comments below prime users' messages(22773 msgs)
table.comment365prime <- subset(table.comment365, table.comment365$user_send %in% people.prime)
message.prime <- table(as.character(table.comment365prime$message_id))

## Boxplot on Sqrt comments On messages
par(mar = c(2,7,2,1))
boxplot(sqrt(as.vector(message.ap)),
        sqrt(as.vector(message.core)),
        sqrt(as.vector(message.prime)),
        varwidth = T,  pch = 20, horizontal = T, col = threecolor, las =1,
        names = c("On Active Users'\n Messages",
                  "On Core Users'\n Messages",
                  "On Prime Users'\n Messages"), 
        main = "Boxplots of Sqrt(#Comments>0) On Messages")
#dev.copy(png,'1-4-2-1 BoxplotSqrtCommentOnMsg.png');dev.off()

## Boxplot on Log comments On messages
par(mar = c(2,7,2,1))
boxplot(log(as.vector(message.ap)),
        log(as.vector(message.core)),
        log(as.vector(message.prime)),
        varwidth = T,  pch = 20, horizontal = T, col = threecolor, las =1,
        names = c("On Active Users'\n Messages",
                  "On Core Users'\n Messages",
                  "On Prime Users'\n Messages"), 
        main = "Boxplots of Log(#Comments>0) On Messages")
#dev.copy(png,'1-4-2-2 BoxplotLogCommentOnMsg.png');dev.off()

## Boxplot on comments On messages
par(mar = c(2,7,2,1))
boxplot((as.vector(message.ap)),
        (as.vector(message.core)),
        (as.vector(message.prime)),
        varwidth = T,  pch = 20, horizontal = T, col = threecolor, las =1,
        names = c("On Active Users'\n Messages",
                  "On Core Users'\n Messages",
                  "On Prime Users'\n Messages"), 
        main = "Boxplots of #Comments>0 On Messages")
#dev.copy(png,'1-4-2-3 BoxplotCommentOnMsg.png');dev.off()

## Biggest Outliers:
# message_1058666 has 219 comments: about memberships fee etc
# message_1054701 has 84 comments: about a race

## histogram on how many comments on messages
library(httr)
par(mar = c(4,4,2,1))
hist(as.vector(message.ap), breaks = 200,col=threecolor[1],border=F, xlim = c(0,20),
     main = "Histogram of #Comments>0 On Messages", xlab = "#Comments(>0)",
     ylab = "Frequency of Messages")
hist(as.vector(message.core),add=T,breaks = 200,col=threecolor[2],border=F)
hist(as.vector(message.prime),add=T,breaks = 200,col=threecolor[3],border=F)
legend("topright", c("Active Users","Core Users", "Prime Users"), 
       title = "Messages Sent By:",
       col = threecolor, pch = 15,pt.cex = 2)
#dev.copy(png,'1-4-2-4 HistogramCommentOnMsg.png');dev.off()

##############################
##  1-4.3 #Messages having Comments For each User
##############################
# calculate the different messages that have comments for users
# that is: number of unique message names relevant to a user_id 
table.msgsend <- subset(table.comment365, select = c(user_send, message_id))
table.msgsend <- unique(table.msgsend)
# draw ap, core and prime users' data
nummsg.ap <- table(as.character(table.msgsend$user_send))
nummsg.core <- nummsg.ap[names(nummsg.ap) %in% people.core]
nummsg.prime <- nummsg.ap[names(nummsg.ap) %in% people.prime]

## Boxplot on #Messages having Comments
par(mar = c(4,4,4,1))
boxplot((as.vector(nummsg.ap)),
        (as.vector(nummsg.core)),
        (as.vector(nummsg.prime)),
        xlab = "#Messages(>0) For Each User",
        varwidth = T,  pch = 20, horizontal = T, col = threecolor, las =1,
        names = c("Active\nUsers","Core\nUsers", "Prime\nUsers"), 
        main = "Boxplots of #Messages Received Comment(s)")
#dev.copy(png,'1-4-3-1 BoxplotMsgRcvCmts.png');dev.off()

## Boxplot on sqrt(#Messages) having Comments
par(mar = c(4,4,4,1))
boxplot(sqrt(as.vector(nummsg.ap)),
        sqrt(as.vector(nummsg.core)),
        sqrt(as.vector(nummsg.prime)),
        xlab = "Sqrt(#Messages>0) For Each User",
        varwidth = T,  pch = 20, horizontal = T, col = threecolor, las =1,
        names = c("Active\nUsers","Core\nUsers", "Prime\nUsers"), 
        main = "Boxplots of Sqrt(#Messages) Received Comment(s)")
#dev.copy(png,'1-4-3-2 BoxplotSqrtMsgRcvCmts.png');dev.off()

## Boxplot on Log(#Messages) having Comments
par(mar = c(4,4,4,1))
boxplot(log(as.vector(nummsg.ap)),
        log(as.vector(nummsg.core)),
        log(as.vector(nummsg.prime)),
        xlab = "Log(#Messages>0) For Each User",
        varwidth = T,  pch = 20, horizontal = T, col = threecolor, las =1,
        names = c("Active\nUsers","Core\nUsers", "Prime\nUsers"), 
        main = "Boxplots of Sqrt(#Messages) Received Comment(s)")
#dev.copy(png,'1-4-3-3 BoxplotLogMsgRcvCmts.png');dev.off()

# Plot histogram on #Messages having Comments
par(mar = c(4,4,2,1))
hist((as.vector(nummsg.ap)), breaks = 120,col=threecolor[1],border=F, xlim = c(0,100),
     main = "Histogram of #Messages Received Comment(s)", xlab = "#Messages(>0) For Each User",
     ylab = "#Users")
hist((as.vector(nummsg.core)),add=T,breaks = 120,col=threecolor[2],border=F)
hist((as.vector(nummsg.prime)),add=T,breaks = 120,col=threecolor[3],border=F)
legend("topright", c("Active Users","Core Users", "Prime Users"), 
       col = threecolor, pch = 15,pt.cex = 2)
#dev.copy(png,'1-4-3-4 HistogramMsgRcvCmts.png');dev.off()

# Plot histogram on Log(#Messages) having Comments
par(mar = c(4,4,2,1))
hist(log(as.vector(nummsg.ap)), breaks = 20,col=threecolor[1],border=F, 
     main = "Histogram of Log(#Messages) Received Comment(s)", xlab = "Log(#Messages>0) For Each User",
     ylab = "#Users")
hist(log(as.vector(nummsg.core)),add=T,breaks = 20,col=threecolor[2],border=F)
hist(log(as.vector(nummsg.prime)),add=T,breaks = 20,col=threecolor[3],border=F)
legend("topright", c("Active Users","Core Users", "Prime Users"), 
       col = threecolor, pch = 15,pt.cex = 2)
#dev.copy(png,'1-4-3-5 HistogramLogMsgRcvCmts.png');dev.off()

##############################
##  1-4.4 #Comments Users Made
##############################

# calculate the comments each user replies
# we don't care who do not replies
numreply.ap <- table(as.character(table.comment365$user_reply))
numreply.core <- numreply.ap[names(numreply.ap) %in% people.core]
numreply.prime <- numreply.ap[names(numreply.ap) %in% people.prime]

## Boxplot on #Comments Users Made
par(mar = c(4,4,4,1))
boxplot((as.vector(numreply.ap)),
        (as.vector(numreply.core)),
        (as.vector(numreply.prime)),
        xlab = "#Comments For User (Who Made At Least One)",
        varwidth = T,  pch = 20, horizontal = T, col = threecolor, las =1,
        names = c("Active\nUsers","Core\nUsers", "Prime\nUsers"), 
        main = "Boxplots of #Comments Users Made")
#dev.copy(png,'1-4-4-1 BoxplotCmtsUserMade.png');dev.off()

## Biggest Outliers:
# user_920 made 2643 comments Barb
# user_4380 made 1562 comments tRicky

## Boxplot on log(#Comments) Users Made
par(mar = c(4,4,4,1))
boxplot(log(as.vector(numreply.ap)),
        log(as.vector(numreply.core)),
        log(as.vector(numreply.prime)),
        xlab = "Log(#Comments) For User (Who Made At Least One)",
        varwidth = T,  pch = 20, horizontal = T, col = threecolor, las =1,
        names = c("Active\nUsers","Core\nUsers", "Prime\nUsers"), 
        main = "Boxplots of Log(#Comments) Users Made")
#dev.copy(png,'1-4-4-2 BoxplotLogCmtsUserMade.png');dev.off()

## Plot histogram on #Comments Users Made
par(mar = c(4,4,2,1))
hist((as.vector(numreply.ap)), breaks = 600,col=threecolor[1],border=F, xlim = c(0,100),
     main = "Histogram of #Comments Users Made",
     xlab = "#Comments For User (Who Made At Least One)",
     ylab = "#Users")
hist((as.vector(numreply.core)),add=T,breaks = 600,col=threecolor[2],border=F)
hist((as.vector(numreply.prime)),add=T,breaks = 600,col=threecolor[3],border=F)
legend("topright", c("Active Users","Core Users", "Prime Users"), 
       col = threecolor, pch = 15,pt.cex = 2)
#dev.copy(png,'1-4-4-3 HistogramCmtsUserMade.png');dev.off()

## Plot histogram on Log(#Comments) Users Made
par(mar = c(4,4,2,1))
hist(log(as.vector(numreply.ap)), breaks = 20,col=threecolor[1],border=F,
     main = "Histogram of Log(#Comments) Users Made",
     xlab = "Log(#Comments) For User (Who Made At Least One)",
     ylab = "#Users")
hist(log(as.vector(numreply.core)),add=T,breaks = 20,col=threecolor[2],border=F)
hist(log(as.vector(numreply.prime)),add=T,breaks = 20,col=threecolor[3],border=F)
legend("topright", c("Active Users","Core Users", "Prime Users"), 
       col = threecolor, pch = 15,pt.cex = 2)
#dev.copy(png,'1-4-4-4 HistogramLogCmtsUserMade.png');dev.off()

##############################
##  1-4.5 Median #Comments On Messages For each User
##############################

# combine table.messagesend: #messages for each user
# message.ap: each message has #reply
cmtcount <- message.ap[match(as.character(table.msgsend$message_id),names(message.ap))]
table.usermsgcmt <- data.frame(table.msgsend, cmtcount, stringsAsFactors = F)
table.aggusermedcmt <- aggregate(table.usermsgcmt[,3],
                                 by = list(as.factor(table.usermsgcmt$user_send)),
                                 FUN = median)
numcmtmsgrate.ap <- table.aggusermedcmt$x[table.aggusermedcmt$Group.1 %in% people.ap]
numcmtmsgrate.core <- table.aggusermedcmt$x[table.aggusermedcmt$Group.1 %in% people.core]
numcmtmsgrate.prime <- table.aggusermedcmt$x[table.aggusermedcmt$Group.1 %in% people.prime]

## Boxplot on Median #Comments On Messages For each User
par(mar = c(4,4,4,1))
boxplot((as.vector(numcmtmsgrate.ap)),
        (as.vector(numcmtmsgrate.core)),
        (as.vector(numcmtmsgrate.prime)),
        xlab = "Median #Comments(>0) on Messages For Each User",
        varwidth = T,  pch = 20, horizontal = T, col = threecolor, las =1,
        names = c("Active\nUsers","Core\nUsers", "Prime\nUsers"), 
        main = "Boxplots of Median #Comments(>0) On Messages")
#dev.copy(png,'1-4-5-1 BoxplotCmtsMsgRate.png');dev.off()

## Outlier:
# user_7034 16 & user_8418 11

## Plot histogram on Median #Comments On Messages For each User
par(mar = c(4,4,2,1))
hist((as.vector(numcmtmsgrate.ap)), breaks = 30,col=threecolor[1],border=F,xlim = c(1,5),
     main = "Histogram of Median #Comments(>0) On Messages",
     xlab = "Median #Comments(>0) on Messages For Each User",
     ylab = "#Users")
hist((as.vector(numcmtmsgrate.core)),add=T,breaks = 25,col=threecolor[2],border=F)
hist((as.vector(numcmtmsgrate.prime)),add=T,breaks = 25,col=threecolor[3],border=F)
legend("topright", c("Active Users","Core Users", "Prime Users"), 
       col = threecolor, pch = 15,pt.cex = 2)
#dev.copy(png,'1-4-5-2 HistogramCmtsMsgRate.png');dev.off()

##############################
##  1-4.6 #Messages Sent vs #Comments Received
##############################

# obtain all active user's #messages and #comments, incluede 0
nummsg.all <- rep(0,2820)
nummsg.all[match(names(nummsg.ap), people.ap)] <- nummsg.ap
numcmt.all <- rep(0,2820)
numcmt.all[match(names(numreply.ap), people.ap)] <- numreply.ap
nummsgvscmt <- data.frame(people.ap, nummsg.all, numcmt.all, stringsAsFactors = F)

# Now Scatter Plot on #Msg VS #Cmt
par(mar = c(4,4,4,1))
plot(log(nummsgvscmt[,2:3]), pch = 20, xlab = "#Messages", ylab = "#Comments",
     col = threecolor[1],
     main = "ScatterPlot: Log(#Comments) VS Log(#Messages)")
points(log(nummsgvscmt[people.ap %in% people.core,2:3]), pch = 20,col = threecolor[2])
points(log(nummsgvscmt[people.ap %in% people.prime,2:3]), pch = 20,col = threecolor[3])
legend("bottomright", c("Active Users","Core Users", "Prime Users"), 
       col = threecolor, pch = 20,pt.cex = 2)
#dev.copy(png,'1-4-6 ScatterMsgVSCmt.png');dev.off()

##############################
##  1-5 Summary on Races
##############################
length(list.race) #2953 race in 365 days

counts <- 0
for(i in 1:length(list.race)){
  if(list.race[[i]]$num_user>=2) counts <- counts + 1
} # 1275 races have more than 2 core users 365 days

##############################
##  1-6 Summary on Groups
##############################
numgrp.ap <- table(as.character(table.member$id)) # 652 user attend groups
numgrp.core <- numgrp.ap[names(numgrp.ap) %in% people.core] # 651
numgrp.prime <- numgrp.ap[names(numgrp.ap) %in% people.prime] # 588

# Now plot histogram 
par(mar = c(4,4,2,1))
hist((as.vector(numgrp.ap)), breaks = 9,col=threecolor[1],border=F,
     main = "Histogram of #Groups Users Attend",
     xlab = "#Groups", ylab = "#Users")
hist((as.vector(numgrp.core)),add=T,breaks = 9,col=threecolor[2],border=F)
hist((as.vector(numgrp.prime)),add=T,breaks = 9,col=threecolor[3],border=F)
legend("topright", c("Active Users","Core Users", "Prime Users"), 
       col = threecolor, pch = 15,pt.cex = 2)
#dev.copy(png,'1-6 HistogramMembership.png');dev.off()


##############################
##    End of Session 1      ##
##############################
rm(list = ls())
