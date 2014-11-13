library(Amelia)
library(plyr)
library(PerformanceAnalytics)
library(lubridate)

######################################################################
###
### Create a DF with vertical information, i.e. for each person in each grant
###

### Create list in global environment
temp.list <- list(length=15)

for(i in 1:15){
  # Get data for all columns for one person, e.g. "RFCD.Code.1", "RFCD.Percentage.1", etc.
  # for temp DF
  temp <- rawdata[, c("Grant.Application.ID", grep(paste("\\.", i, "$", sep=""), names(rawdata), value=T))]
  # Clean name of columns of temp
  names(temp) <- gsub(paste("\\.", i, "$", sep=""), "", names(temp))
  
  # Initial step
  if(i == 1) temp.names <- names(temp)
  
  # This is needed as certain information is available only for the first five
  # researchers
  if(i > 5) {
    temp$RFCD.Code <- NA
    temp$RFCD.Percentage <- NA
    temp$SEO.Code <- NA
    temp$SEO.Percentage <- NA
  }
  # Put resulting DF in list
  temp.list[[i]] <- temp[, temp.names]
  rm(temp)
}

### Create vertical in global environment
vertical <- data.frame()

### Loop through temp.list and create a DF through rbind
for (i in 1:15) {
  vertical <- rbind(vertical, temp.list[[i]])
}

### Delete
rm(temp.list)

### Look only at subset with existing role, i.e. actual researchers
vertical <- subset(vertical, Role != "")


### Look at missmap
missmap(vertical)

### Remove unwanted level "" from Role column
vertical$Role <- factor(as.character(vertical$Role))

### YOB as factor
vertical$Year.of.Birth <- factor(vertical$Year.of.Birth)

######################################################################
###
### Remove spaces and set missing values to NA
###

vertical$Country.of.Birth <- gsub(" ", ".", as.character(vertical$Country.of.Birth))
vertical$Country.of.Birth[vertical$Country.of.Birth == ""] <- NA

### Previous step made column a character vector, but we need a factor
vertical$Country.of.Birth <- factor(vertical$Country.of.Birth)

### For Home.Language
vertical$Home.Language[vertical$Home.Language == ""] <- NA

### Previous step made column a character vector, but we need a factor
vertical$Home.Language <- factor(vertical$Home.Language)

### Replace integers with characters, set NAs and the vector as a factor
vertical$Dept.No. <- paste("Dept", vertical$Dept.No., sep = "")
vertical$Dept.No.[vertical$Dept.No. == "DeptNA"] <- NA
vertical$Dept.No. <- factor(vertical$Dept.No.)

### Same as last step
vertical$Faculty.No. <- paste("Faculty", vertical$Faculty.No., sep = "")
vertical$Faculty.No.[vertical$Faculty.No. == "FacultyNA"] <- NA
vertical$Faculty.No. <- factor(vertical$Faculty.No.)

### Same as last two steps. Replace frequent codes with NA
vertical$RFCD.Code <- paste("RFCD", vertical$RFCD.Code, sep = "")
vertical$RFCD.Percentage[vertical$RFCD.Code == "RFCDNA"] <- NA
vertical$RFCD.Code[vertical$RFCD.Code == "RFCDNA"] <- NA
vertical$RFCD.Percentage[vertical$RFCD.Code == "RFCD0"] <- NA
vertical$RFCD.Code[vertical$RFCD.Code == "RFCD0"] <- NA
vertical$RFCD.Percentage[vertical$RFCD.Code == "RFCD999999"] <- NA
vertical$RFCD.Code[vertical$RFCD.Code == "RFCD999999"] <- NA
vertical$RFCD.Code <- factor(vertical$RFCD.Code)

### As the last step
vertical$SEO.Code <- paste("SEO", vertical$SEO.Code, sep = "")
vertical$SEO.Percentage[vertical$SEO.Code == "SEONA"] <- NA
vertical$SEO.Code[vertical$SEO.Code == "SEONA"] <- NA
vertical$SEO.Percentage[vertical$SEO.Code == "SEO0"] <- NA
vertical$SEO.Code[vertical$SEO.Code  == "SEO0"] <- NA
vertical$SEO.Percentage[vertical$SEO.Code == "SEO999999"] <- NA
vertical$SEO.Code[vertical$SEO.Code== "SEO999999"] <- NA
vertical$SEO.Code <- factor(vertical$SEO.Code)

### The levels used here full of space and hence unwieldy
### Change to more appropiate levels
vertical$No..of.Years.in.Uni.at.Time.of.Grant <- as.character(vertical$No..of.Years.in.Uni.at.Time.of.Grant)
vertical$No..of.Years.in.Uni.at.Time.of.Grant[vertical$No..of.Years.in.Uni.at.Time.of.Grant == ""] <- "DurationUnk"
vertical$No..of.Years.in.Uni.at.Time.of.Grant[vertical$No..of.Years.in.Uni.at.Time.of.Grant == ">=0 to 5"] <- "Duration0to5"
vertical$No..of.Years.in.Uni.at.Time.of.Grant[vertical$No..of.Years.in.Uni.at.Time.of.Grant == ">5 to 10"] <- "Duration5to10"
vertical$No..of.Years.in.Uni.at.Time.of.Grant[vertical$No..of.Years.in.Uni.at.Time.of.Grant == ">10 to 15"] <- "Duration10to15"
vertical$No..of.Years.in.Uni.at.Time.of.Grant[vertical$No..of.Years.in.Uni.at.Time.of.Grant == "more than 15"] <- "DurationGT15"
vertical$No..of.Years.in.Uni.at.Time.of.Grant[vertical$No..of.Years.in.Uni.at.Time.of.Grant == "Less than 0"] <- "DurationLT0"

### Set vector to factors again
vertical$No..of.Years.in.Uni.at.Time.of.Grant <- factor(vertical$No..of.Years.in.Uni.at.Time.of.Grant)

### Again, NAs and factor
vertical$With.PHD[vertical$With.PHD==""] <- NA
vertical$With.PHD <- factor(vertical$With.PHD)

### A short function to replace role titles
shortNames <- function(x, pre = "")
{
  x <- gsub("EXT_CHIEF_INVESTIGATOR",  "ECI", x)
  x <- gsub("STUD_CHIEF_INVESTIGATOR", "SCI", x)
  x <- gsub("CHIEF_INVESTIGATOR",      "CI", x)
  x <- gsub("DELEGATED_RESEARCHER",    "DR", x)
  x <- gsub("EXTERNAL_ADVISOR",        "EA", x)
  x <- gsub("HONVISIT",                "HV", x)
  x <- gsub("PRINCIPAL_SUPERVISOR",    "PS", x)
  x <- gsub("STUDRES",                 "SR", x)
  x <- gsub("Unk",                     "UNK", x)
  # Contains all names that are not Grant.Application.ID
  other <- x[x != "Grant.Application.ID"]
  # Returns all replaced Names
  c("Grant.Application.ID", paste(pre, other, sep = ""))
}

### Look at the missmap again
missmap(vertical)

######################################################################
#####
##### Engineering of additional feature of existing data non-grant specific data
#####

library(reshape2) 
### Functions for the reshape2 package, i.e. "dcast", produce certain warning
### messages. These can be ignored.


### Role ###
### Calculate how many people worked on each grant
people <- ddply(vertical, .(Grant.Application.ID), function(x) nrow(x))

### Calculate how many people of each role participated in grant request
role.count <- dcast(ddply(vertical, .(Grant.Application.ID, Role), nrow), formula = Grant.Application.ID ~ Role)
### Shorten the names
names(role.count) <- shortNames(names(role.count))
### Set NAs to 0
role.count[is.na(role.count)] <- 0

### Year.of.Birth ###

### Calcuate amount of people on grant according to role and DOB
role.DOB <- dcast(ddply(vertical, .(Grant.Application.ID, Role, Year.of.Birth), 
                        nrow), formula = Grant.Application.ID ~ Role + Year.of.Birth)

### Shorten names and set NAs to 0
names(role.DOB) <- shortNames(names(role.DOB))
role.DOB[is.na(role.DOB)] <- 0

### Calcuate amount of people on grant according to (only) DOB
people.DOB <- dcast(ddply(vertical, .(Grant.Application.ID, Year.of.Birth), 
                          nrow), formula = Grant.Application.ID ~ Year.of.Birth)

### NAs and names
people.DOB[is.na(people.DOB)] <- 0
names(people.DOB)[-1] <- paste("DOB", shortNames(names(people.DOB))[-1], sep=".")

### Country.of.Birth ###

### Calculate amount of people on grant according to role and Country of birth
role.COB <- dcast(ddply(vertical, .(Grant.Application.ID, Role, Country.of.Birth), 
                        nrow), formula = Grant.Application.ID ~ Role + Country.of.Birth)

# Names and NAs
names(role.COB) <- shortNames(names(role.COB))
role.COB[is.na(role.COB)] <- 0

### Calculate amount of people on grant according to (only) the Country of birth
people.COB <- dcast(ddply(vertical, .(Grant.Application.ID, Country.of.Birth), 
                          nrow), formula = Grant.Application.ID ~ Country.of.Birth)

### NAs and names
people.COB[is.na(people.COB)] <- 0
names(people.COB)[length(people.COB)] <- "UnkCountry"

### Home.Language ###

### Calculate amount of people on grant according to role and Home.Language
role.lang <- dcast(ddply(vertical, .(Grant.Application.ID, Role, Home.Language), 
                         nrow), formula = Grant.Application.ID ~ Role + Home.Language)

### Names and NAs
names(role.lang) <- shortNames(names(role.lang))
role.lang[is.na(role.lang)] <- 0

### Calculate amount of people on grant according to (only) Home.Language
people.lang <- dcast(ddply(vertical, .(Grant.Application.ID, Home.Language), 
                           nrow), formula = Grant.Application.ID ~ Home.Language)

### NAs
people.lang[is.na(people.lang)] <- 0

### Dept.No. ###

### Calculate amount of people on grant according to role and Dept.No.
role.dept <- dcast(ddply(vertical, .(Grant.Application.ID, Role, Dept.No.), 
                         nrow), formula = Grant.Application.ID ~ Role + Dept.No.)

### Names and NAs
names(role.dept) <- shortNames(names(role.dept))
role.dept[is.na(role.dept)] <- 0

### Calculate amount of people on grant according to (only) Dept.No.
people.dept <- dcast(ddply(vertical, .(Grant.Application.ID, Dept.No.), 
                           nrow), formula = Grant.Application.ID ~ Dept.No.)

### NAs
people.dept[is.na(people.dept)] <- 0


### Faculty.No. ###

### Calculate amount of people on grant according to role and Faculty.No.
role.faculty <- dcast(ddply(vertical, .(Grant.Application.ID, Role, Faculty.No.), 
                            nrow), formula = Grant.Application.ID ~ Role + Faculty.No.)

### Names and NAs
names(role.faculty) <- shortNames(names(role.faculty))
role.faculty[is.na(role.faculty)] <- 0

### Calculate amount of people on grant according to (only) Faculty.No.
people.faculty <- dcast(ddply(vertical, .(Grant.Application.ID, Faculty.No.), 
                              nrow), formula = Grant.Application.ID ~ Faculty.No.)

### NAs
people.faculty[is.na(people.faculty)] <- 0


### With.PHD ###

### Calculate amount of people on grant according to role and With.PHD
role.PHD <- dcast(ddply(vertical, .(Grant.Application.ID, Role, With.PHD), 
                        nrow), formula = Grant.Application.ID ~ Role + With.PHD)

### Names and NAs
names(role.PHD) <- shortNames(names(role.PHD))
role.PHD[is.na(role.PHD)] <- 0

### Remove columns with NAs as names
role.PHD  <- role.PHD[, c(1, 2, 4, 6, 9, 11, 14)]

### Calculate amount of people on grant according to (only) with.PHD
people.With.PHD <- dcast(ddply(vertical, .(Grant.Application.ID, With.PHD), 
                               nrow), formula = Grant.Application.ID ~ With.PHD)

### NAs in rows and columns
people.With.PHD[is.na(people.With.PHD)] <- 0
people.With.PHD  <- people.With.PHD[, c(1, 2)]

### Set "Yes " to "PHD"
names(people.With.PHD)[2]  <- "PHD"


### Number.of.Successful.Grant + Number.of.Unsuccessful.Grant ###

### Calculate amount of successfull grants per grant ID
people.grants1 <- ddply(vertical, .(Grant.Application.ID, Number.of.Successful.Grant), nrow)

### NAs
people.grants1$Number.of.Successful.Grant[is.na(people.grants1$Number.of.Successful.Grant)] <- 0

### Calculate product of successful grants and participating people
people.grants1$Product <- people.grants1$Number.of.Successful.Grant * people.grants1$V1

### Sum up the successes (from the product) for each grant
people.grants.success <- ddply(people.grants1, .(Grant.Application.ID), transform, successes = sum(Product))

### Only take: Grant.Application.ID + successes
people.grants.success <- people.grants.success[, c(1, 5)]
### Remove duplicate records
people.grants.success <- unique(people.grants.success)

### Calculate amount of unsuccessfull grants per grant ID (same as before)
people.grants1 <- ddply(vertical, .(Grant.Application.ID, Number.of.Unsuccessful.Grant), nrow)
people.grants1$Number.of.Unsuccessful.Grant[is.na(people.grants1$Number.of.Unsuccessful.Grant)] <- 0
people.grants1$Product <- people.grants1$Number.of.Unsuccessful.Grant * people.grants1$V1

people.grants.fail <- ddply(people.grants1, .(Grant.Application.ID), transform, fails = sum(Product))
people.grants.fail <- people.grants.fail[, c(1, 5)]
people.grants.fail <- unique(people.grants.fail)

### Calculate amount of successfull grants per grant ID and role (as before but with role)
role.grants1 <- ddply(vertical, .(Grant.Application.ID, Role, Number.of.Successful.Grant), nrow)
role.grants1$Number.of.Successful.Grant[is.na(role.grants1$Number.of.Successful.Grant)] <- 0
role.grants1$Product <- role.grants1$Number.of.Successful.Grant * role.grants1$V1

role.grants.success <- ddply(role.grants1, .(Grant.Application.ID, Role), transform, successes = sum(Product))
role.grants.success <- role.grants.success[, c(1, 2, 6)]
role.grants.success <- unique(role.grants.success)

### Recast previous DF with information in the columns for each grant
role.grants.success <- dcast(role.grants.success, formula = Grant.Application.ID ~ Role)

### Names and NAs
names(role.grants.success) <- shortNames(names(role.grants.success))
role.grants.success[is.na(role.grants.success)] <- 0

### Calculate amount of unsuccessfull grants per grant ID and role (same as before)
role.grants1 <- ddply(vertical, .(Grant.Application.ID, Role, Number.of.Unsuccessful.Grant), nrow)
role.grants1$Number.of.Unsuccessful.Grant[is.na(role.grants1$Number.of.Unsuccessful.Grant)] <- 0
role.grants1$Product <- role.grants1$Number.of.Unsuccessful.Grant * role.grants1$V1

role.grants.fail <- ddply(role.grants1, .(Grant.Application.ID, Role), transform, fail = sum(Product))
role.grants.fail <- role.grants.fail[, c(1, 2, 6)]
role.grants.fail <- unique(role.grants.fail)

role.grants.fail <- dcast(role.grants.fail, formula = Grant.Application.ID ~ Role)
names(role.grants.fail) <- shortNames(names(role.grants.fail))
role.grants.fail[is.na(role.grants.fail)] <- 0

### No..of.Years.in.Uni.at.Time.of.Grant ###

### Calculate amount of successfull grants per role AND No..of.Years.in.Uni.at.Time.of.Grant
role.uniyears <- dcast(ddply(vertical, .(Grant.Application.ID, Role, No..of.Years.in.Uni.at.Time.of.Grant)
                             , nrow), formula = Grant.Application.ID ~ Role + No..of.Years.in.Uni.at.Time.of.Grant)

### Names and NAs
names(role.uniyears) <- shortNames(names(role.uniyears))
role.uniyears[is.na(role.uniyears)] <- 0

### Calculate amount of successfull grants per No..of.Years.in.Uni.at.Time.of.Grant
### (as before)
people.uniyears <- dcast(ddply(vertical, .(Grant.Application.ID, No..of.Years.in.Uni.at.Time.of.Grant)
                               , nrow), formula = Grant.Application.ID ~ No..of.Years.in.Uni.at.Time.of.Grant)

names(people.uniyears) <- shortNames(names(people.uniyears))
people.uniyears[is.na(people.uniyears)] <- 0

### RFCD.Code ###

### Calculate amount of successfull grants per role AND RFDC Code
role.rfcd.code <- dcast(ddply(vertical, .(Grant.Application.ID, Role, RFCD.Code)
                              , nrow), formula = Grant.Application.ID ~ Role + substr(RFCD.Code, 1, 6))
names(role.rfcd.code) <- shortNames(names(role.rfcd.code))
role.rfcd.code[is.na(role.rfcd.code)] <- 0

### Calculate amount of successfull grants per RFDC Code
people.rfcd.code <- dcast(ddply(vertical, .(Grant.Application.ID, RFCD.Code)
                                , nrow), formula = Grant.Application.ID ~ substr(RFCD.Code, 1, 6))
names(people.rfcd.code) <- shortNames(names(people.rfcd.code))
people.rfcd.code[is.na(people.rfcd.code)] <- 0

### SEO.Code ###

### Calculate amount of successfull grants per role AND SEO Code (as before)
role.seo.code <- dcast(ddply(vertical, .(Grant.Application.ID, Role, SEO.Code)
                             , nrow), formula = Grant.Application.ID ~ Role + substr(SEO.Code, 1, 5))

names(role.seo.code) <- shortNames(names(role.seo.code))
role.seo.code[is.na(role.seo.code)] <- 0

### Calculate amount of successfull grants per SEO Code
people.seo.code <- dcast(ddply(vertical, .(Grant.Application.ID, SEO.Code)
                               , nrow), formula = Grant.Application.ID ~ substr(SEO.Code, 1, 5))

names(people.seo.code) <- shortNames(names(people.seo.code))
people.seo.code[is.na(people.seo.code)] <- 0


### Number of publications per journal ###

### Role + type
role.pub.A1 <- dcast(ddply(vertical, .(Grant.Application.ID, Role, A.), nrow)
                     , formula = Grant.Application.ID ~ Role)

role.pub.A <- dcast(ddply(vertical, .(Grant.Application.ID, Role, A), nrow)
                    , formula = Grant.Application.ID ~ Role)

role.pub.B <- dcast(ddply(vertical, .(Grant.Application.ID, Role, B), nrow)
                    , formula = Grant.Application.ID ~ Role)

role.pub.C <- dcast(ddply(vertical, .(Grant.Application.ID, Role, C), nrow)
                    , formula = Grant.Application.ID ~ Role)

### Shorten names and add prefix of publication type
names(role.pub.A1) <- shortNames(names(role.pub.A1))
names(role.pub.A1)[-1] <- paste("Astar", names(role.pub.A1)[-1], sep=".")

names(role.pub.A) <- shortNames(names(role.pub.A))
names(role.pub.A)[-1] <- paste("A", names(role.pub.A)[-1], sep=".")

names(role.pub.B) <- shortNames(names(role.pub.B))
names(role.pub.B)[-1] <- paste("B", names(role.pub.B)[-1], sep=".")

names(role.pub.C) <- shortNames(names(role.pub.C))
names(role.pub.C)[-1] <- paste("C", names(role.pub.C)[-1], sep=".")


### Number of publication just per type and grant
people.pub.A1 <- data.frame(Grant.Application.ID = role.pub.A1$Grant.Application.ID, A1 = rowSums(role.pub.A1[, -1]))
people.pub.A <- data.frame(Grant.Application.ID = role.pub.A$Grant.Application.ID, A = rowSums(role.pub.A[, -1]))
people.pub.B <- data.frame(Grant.Application.ID = role.pub.B$Grant.Application.ID, B = rowSums(role.pub.B[, -1]))
people.pub.C <- data.frame(Grant.Application.ID = role.pub.C$Grant.Application.ID, C = rowSums(role.pub.C[, -1]))

### Create a DF with all publication data
people.pub <- join(people.pub.A1, people.pub.A, by="Grant.Application.ID")
people.pub <- join(people.pub, people.pub.B, by="Grant.Application.ID")
people.pub <- join(people.pub, people.pub.C, by="Grant.Application.ID")


### RFCD & SEO percentages ###

### Calculate sum of percentages
RFCD.Percentage <- ddply(vertical, .(Grant.Application.ID), transform, RFCD.Percentage2 = sum(RFCD.Percentage))

### Only take relevant columns, remove duplicates and NAs
RFCD.Percentage <- RFCD.Percentage[, c(1, 21)]
RFCD.Percentage <- unique(RFCD.Percentage)
RFCD.Percentage[is.na(RFCD.Percentage)] <- 0

### Set correct name
names(RFCD.Percentage)[2] <- "RFCD.Percentage"

### Same as RFCF for SEO percentage
SEO.Percentage <- ddply(vertical, .(Grant.Application.ID), transform, SEO.Percentage2 = sum(SEO.Percentage))

SEO.Percentage <- SEO.Percentage[, c(1, 21)]
SEO.Percentage <- unique(SEO.Percentage)
SEO.Percentage[is.na(SEO.Percentage)] <- 0

names(SEO.Percentage)[2] <- "SEO.Percentage"


######################################################################
#####
##### Engineering of features for grant specific data
#####


### Create grant data
grantData <- rawdata[, c("Contract.Value.Band", "Grant.Category.Code")]

### Make a lubridate object for the time, then derive the day, week, month and year info
startTime <- dmy(rawdata$Start.date)

grantData$Month <- factor(as.character(month(startTime, label = TRUE)))
grantData$Weekday <- factor(as.character(wday(startTime, label = TRUE)))
grantData$Day <- day(startTime)
grantData$Year <- year(startTime)

### Create dummies
dummies <- dummyVars(~., data = grantData, levelsOnly = TRUE)

### Recreate grant data with dummies
grantData <- as.data.frame(predict(dummies, grantData))

### Remove spaces from column names
names(grantData) <- gsub(" ", "", names(grantData))

### Create class deping on grant status
grantData$Class <- factor(ifelse(rawdata$Grant.Status, "successful", "unsuccessful"))

### Get Application.ID from rawdata
grantData$Grant.Application.ID <- rawdata$Grant.Application.ID

## Create function to find and remove zero-variance ("ZV") predictors
noZV <- function(x)
{
  keepers <- unlist(lapply(x, function(x) length(unique(x)) > 1))
  x[,keepers,drop = FALSE]
}

### Remove zero variance predictors from grantData
grantData <- noZV(grantData)



######################################################################
#####
##### Merge predictors together
#####

### Merge predictors
summarized <- join(people, people.DOB[, -length(people.DOB)], by="Grant.Application.ID")
names(summarized)[2] <- "Members"
#summarized <- join(summarized, role.count, by="Grant.Application.ID")
summarized <- join(summarized, people.COB, by="Grant.Application.ID")
summarized <- join(summarized, people.lang[, -length(people.lang)], by="Grant.Application.ID")
summarized <- join(summarized, people.With.PHD, by="Grant.Application.ID")
summarized <- join(summarized, people.grants.success, by="Grant.Application.ID")
summarized <- join(summarized, people.grants.fail, by="Grant.Application.ID")
summarized <- join(summarized, people.dept[, -length(people.dept)], by="Grant.Application.ID")
#summarized <- join(summarized, people.faculty[, -length(people.faculty)], by="Grant.Application.ID")
summarized <- join(summarized, people.uniyears, by="Grant.Application.ID")
summarized <- join(summarized, people.pub, by="Grant.Application.ID")
summarized <- join(summarized, people.rfcd.code[, -length(people.rfcd.code)], by="Grant.Application.ID")
summarized <- join(summarized, people.seo.code[, -length(people.seo.code)], by="Grant.Application.ID")
summarized <- join(summarized, RFCD.Percentage, by="Grant.Application.ID")
summarized <- join(summarized, SEO.Percentage, by="Grant.Application.ID")
summarized <- join(summarized, grantData, by="Grant.Application.ID")

### Remove Grant.Application.ID (not needed anymore)
summarized$Grant.Application.ID <- NULL
