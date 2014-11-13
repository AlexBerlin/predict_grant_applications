### Load Data
rawdata <- read.csv("unimelb_training.csv")

### Look at roles (in columns of rawdata)
roles  <- vector("character")
for (i in 1:15){
  colstr = paste("Role.", as.character(i), sep="")
  roles <- c(roles, as.vector(unique(rawdata[, colstr])))
}
### Get unique roles
roles <- unique(roles)

######################################################################
###
### Initial data cleaning of rawdata
###

### Change long column name
names(rawdata)[5]  <- "Contract.Value.Band"

### Features to characters
rawdata$Sponsor.Code <- as.character(rawdata$Sponsor.Code)
rawdata$Grant.Category.Code <- as.character(rawdata$Grant.Category.Code)
rawdata$Contract.Value.Band <- as.character(rawdata$Contract.Value.Band)

### Deal with missing values
rawdata$Sponsor.Code[rawdata[, "Sponsor.Code"] == ""] <- "Unk"
rawdata$Sponsor.Code <- factor(paste("Sponsor", rawdata$Sponsor.Code, sep=""))

rawdata$Grant.Category.Code[rawdata[, "Grant.Category.Code"] == ""] <- "Unk"
rawdata$Grant.Category.Code <- factor(paste("Grant.Category",rawdata$Grant.Category.Code, sep=""))

rawdata$Contract.Value.Band[rawdata[, "Contract.Value.Band"] == ""] <- "Unk"
rawdata$Contract.Value.Band<- factor(paste("Contract.Value.Band",rawdata$Contract.Value.Band, sep=""))
