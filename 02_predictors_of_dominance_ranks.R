##########################################################################################################################
# Calculating the predictors of the hierarchies of Sulphur-crested cockatoos at 3 study locations (CG, BA, NB)


##########################################################################################################################


# LOAD PACKAGES
library(beepr)
library(aniDom)
library(lme4)
library(lmerTest)
library(plyr)
library(texreg)

# LOAD DATA
roost<- read.csv("roost_sites_NORTH.csv")
roost_2022 <- read.csv("roost_2022.csv")

agg <- read.csv('soc_data_2019.csv', stringsAsFactors = FALSE,header = T,row.names = 1)
agg_2022 <- read.csv('agg_data_cleaned.csv',stringsAsFactors = F)

vector<-read.csv2("relatedness_metadata.csv",stringsAsFactors = FALSE, header=T)
marking <- read.csv2("Marking_sheet_MASTER_corrected.csv")
sexing_2022 <- read.csv('sexing_2022.csv', stringsAsFactors = FALSE,header = T,row.names = 1)

IDs <- as.data.frame(unique(c(agg$WINNER,agg$WINNER)))

# CGLMORAL
### Data july
CG <- agg[which(agg$LOCATION=="CG"),]

winners.CG <- as.vector(CG$WINNER)
losers.CG <- as.vector(CG$LOSER)

####### Checking number interactions per indiv (july)
nbr_win <- as.data.frame(table(winners.CG))
nbr_loss <- as.data.frame(table(losers.CG))

nbr_win_CG <- as.data.frame(table(winners.CG))
nbr_loss_CG <- as.data.frame(table(losers.CG))
names(nbr_win_CG) <- c("ID", "wins")
names(nbr_loss_CG) <- c("ID", "loss")


int <- rbind.fill(nbr_win_CG[c("wins","ID")], nbr_loss_CG[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
nbr.int.CG <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(nbr.int.CG) <- c("ID", "wins", "los")

for (i in 1:nrow(nbr.int.CG)){ 
  nbr.int.CG$total[i] <- sum(nbr.int.CG[i,2]+nbr.int.CG[i,3])
}

nbr.int.CG$over10 <- nbr.int.CG$total>=10


### DOES INDIVIDUAL HAVE >10 int ?
CG.ids.over10 <- nbr.int.CG[which(nbr.int.CG$over10==TRUE),]


######## calculate scores
CG$areBoth = ((CG$WINNER %in% CG.ids.over10$ID) & (CG$LOSER %in% CG.ids.over10$ID))
CG.over10 <- CG[which(CG$areBoth==TRUE),]

winners.CG <- as.vector(CG.over10$WINNER)
losers.CG <- as.vector(CG.over10$LOSER)

scores.CG <- elo_scores(winners=winners.CG,
                        losers=losers.CG,
                        randomise=TRUE,
                        sigmoid.param=1/300,
                        K=200,
                        n.rands=10000,
                        return.as.ranks = TRUE
)

scores.CG <- as.data.frame(scores.CG)
######## calculate ranks
for (i in 1:nrow(scores.CG)) { 
  scores.CG$Med.rank [i] <- rowMeans(scores.CG)[i]
}

rank.CG <- as.data.frame(scores.CG$Med.rank)
rank.CG$rank <- rank(rank.CG, na.last = TRUE)
rank.CG$ID <- rownames(scores.CG)

rank.CG$st_rank <-rank.CG$rank/max(rank.CG$rank)
rank.CG$group <-"CG"




# CG 2022
CG_2022 <- agg_2022[which(agg_2022$Location=="CG"),]

winners.CG.2022 <- as.vector(CG_2022$Winner)
losers.CG.2022 <- as.vector(CG_2022$Loser)

####### Checking number interactions per individual
nbr_win_2022 <- as.data.frame(table(winners.CG.2022))
nbr_loss_2022 <- as.data.frame(table(losers.CG.2022))

nbr_win_CG_2022 <- as.data.frame(table(winners.CG.2022))
nbr_loss_CG_2022 <- as.data.frame(table(losers.CG.2022))
names(nbr_win_CG_2022) <- c("ID", "wins")
names(nbr_loss_CG_2022) <- c("ID", "loss")


int <- rbind.fill(nbr_win_CG_2022[c("wins","ID")], nbr_loss_CG_2022[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
nbr.int.CG_2022 <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(nbr.int.CG_2022) <- c("ID", "wins", "los")

for (i in 1:nrow(nbr.int.CG_2022)){ 
  nbr.int.CG_2022$total[i] <- sum(nbr.int.CG_2022[i,2]+nbr.int.CG_2022[i,3])
}

nbr.int.CG_2022$over10 <- nbr.int.CG_2022$total>=10


### Subsetting to individuals with >10 interactions at the site (CG)
CG.ids.over10_2022 <- nbr.int.CG_2022[which(nbr.int.CG_2022$over10==TRUE),]

######## calculate scores
CG_2022$areBoth = ((CG_2022$Winner %in% CG.ids.over10_2022$ID) & (CG_2022$Loser %in% CG.ids.over10_2022$ID))
CG.over10_2022 <- CG_2022[which(CG_2022$areBoth==TRUE),]

winners.CG_2022 <- as.vector(CG.over10_2022$Winner)
losers.CG_2022 <- as.vector(CG.over10_2022$Loser)

scores.CG.2022 <- elo_scores(winners=winners.CG_2022,
                             losers=losers.CG_2022,
                             randomise=TRUE,
                             sigmoid.param=1/300,
                             K=200,
                             n.rands=10000,
                             return.as.ranks = TRUE
)

scores.CG.2022 <- as.data.frame(scores.CG.2022)

######## calculate ranks
for (i in 1:nrow(scores.CG.2022)) { 
  scores.CG.2022$Med.rank [i] <- rowMeans(scores.CG.2022)[i]
}

rank.CG.2022 <- as.data.frame(scores.CG.2022$Med.rank)
rank.CG.2022$rank <- rank(rank.CG.2022, na.last = TRUE)
rank.CG.2022$ID <- rownames(scores.CG.2022)
rank.CG.2022$st_rank <-rank.CG.2022$rank/max(rank.CG.2022$rank)

rank.CG.2022$group <-"CG 2022"



## BA
BA <- agg[which(agg$LOCATION=="BA"),]

winners.BA <- as.vector(BA$WINNER)
losers.BA <- as.vector(BA$LOSER)

####### Checking number interactions per indiv (july)
BAr_win <- as.data.frame(table(winners.BA))
BAr_loss <- as.data.frame(table(losers.BA))

BAr_win_BA <- as.data.frame(table(winners.BA))
BAr_loss_BA <- as.data.frame(table(losers.BA))
names(BAr_win_BA) <- c("ID", "wins")
names(BAr_loss_BA) <- c("ID", "loss")


int <- rbind.fill(BAr_win_BA[c("wins","ID")], BAr_loss_BA[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
BAr.int.BA <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(BAr.int.BA) <- c("ID", "wins", "los")

for (i in 1:nrow(BAr.int.BA)){ 
  BAr.int.BA$total[i] <- sum(BAr.int.BA[i,2]+BAr.int.BA[i,3])
}

BAr.int.BA$over10 <- BAr.int.BA$total>=10


### DOES INDIVIDUAL HAVE >10 int ?
BA.ids.over10 <- BAr.int.BA[which(BAr.int.BA$over10==TRUE),]


######## calculate scores
BA$areBoth = ((BA$WINNER %in% BA.ids.over10$ID) & (BA$LOSER %in% BA.ids.over10$ID))
BA.over10 <- BA[which(BA$areBoth==TRUE),]

winners.BA <- as.vector(BA.over10$WINNER)
losers.BA <- as.vector(BA.over10$LOSER)

scores.BA <- elo_scores(winners=winners.BA,
                        losers=losers.BA,
                        randomise=TRUE,
                        sigmoid.param=1/300,
                        K=200,
                        n.rands=10000,
                        return.as.ranks = TRUE
)

beep(1)
scores.BA <- as.data.frame(scores.BA)
######## calculate ranks
for (i in 1:nrow(scores.BA)) { 
  scores.BA$Med.rank [i] <- rowMeans(scores.BA)[i]
}

rank.BA <- as.data.frame(scores.BA$Med.rank)
rank.BA$rank <- rank(rank.BA, na.last = TRUE)
rank.BA$ID <- rownames(scores.BA)

rank.BA$st_rank <-rank.BA$rank/max(rank.BA$rank)
rank.BA$group <-"BA"

## NB
NB <- agg[which(agg$LOCATION=="NB"),]

winners.NB <- as.vector(NB$WINNER)
losers.NB <- as.vector(NB$LOSER)

####### Checking number interactions per indiv (july)
nbr_win <- as.data.frame(table(winners.NB))
nbr_loss <- as.data.frame(table(losers.NB))

nbr_win_NB <- as.data.frame(table(winners.NB))
nbr_loss_NB <- as.data.frame(table(losers.NB))
names(nbr_win_NB) <- c("ID", "wins")
names(nbr_loss_NB) <- c("ID", "loss")


int <- rbind.fill(nbr_win_NB[c("wins","ID")], nbr_loss_NB[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
nbr.int.NB <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(nbr.int.NB) <- c("ID", "wins", "los")

for (i in 1:nrow(nbr.int.NB)){ 
  nbr.int.NB$total[i] <- sum(nbr.int.NB[i,2]+nbr.int.NB[i,3])
}

nbr.int.NB$over10 <- nbr.int.NB$total>=10


### DOES INDIVIDUAL HAVE >10 int ?
NB.ids.over10 <- nbr.int.NB[which(nbr.int.NB$over10==TRUE),]


######## calculate scores
NB$areBoth = ((NB$WINNER %in% NB.ids.over10$ID) & (NB$LOSER %in% NB.ids.over10$ID))
NB.over10 <- NB[which(NB$areBoth==TRUE),]

winners.NB <- as.vector(NB.over10$WINNER)
losers.NB <- as.vector(NB.over10$LOSER)

scores.NB <- elo_scores(winners=winners.NB,
                        losers=losers.NB,
                        randomise=TRUE,
                        sigmoid.param=1/300,
                        K=200,
                        n.rands=10000,
                        return.as.ranks = TRUE
)

beep(5)
scores.NB <- as.data.frame(scores.NB)
######## calculate ranks
for (i in 1:nrow(scores.NB)) { 
  scores.NB$Med.rank [i] <- rowMeans(scores.NB)[i]
}

rank.NB <- as.data.frame(scores.NB$Med.rank)
rank.NB$rank <- rank(rank.NB, na.last = TRUE)
rank.NB$ID <- rownames(scores.NB)

rank.NB$st_rank <-rank.NB$rank/max(rank.NB$rank)
rank.NB$group <-"NB"

rank <- rbind(rank.BA[,2:5],rank.CG[,2:5],rank.NB[,2:5])

rank$roost <- roost$ROOST[match(rank$ID,roost$ID)]

rank$at.roost <- rank$group ==rank$roost
rank$roost[rank$roost=="CG"] <- "CG 2019"


rank$age <- vector$Age[match(rank$ID,vector$Social_ID)]
rank$age[is.na(rank$age)] <- marking$Assigned_Age[match(rank$ID[is.na(rank$age)], marking$ID_Site)]
#rank$age[rank$age==""] <- NA

rank$sex <- vector$Sex[match(rank$ID,vector$Social_ID)]
rank$sex[is.na(rank$sex)] <- marking$Assigned_Sex[match(rank$ID[is.na(rank$sex)], marking$ID_Site)]


#write.csv(rank,"ranks_2019_all_groups.csv")


roost_2022$roost[roost_2022$roost=="CG"] <- "CG 2022"
rank.CG.2022$roost <- roost_2022$roost[match(rank.CG.2022$ID,roost_2022$ID)]
rank.CG.2022$at.roost <- rank.CG.2022$group ==rank.CG.2022$roost
rank.CG.2022$age <- sexing_2022$assigned_age[match(rank.CG.2022$ID,sexing_2022$ID)]

rank.CG.2022$sex <- sexing_2022$assigned_sex[match(rank.CG.2022$ID,sexing_2022$ID)]


rank_tot <- rbind(rank,rank.CG.2022[,2:ncol(rank.CG.2022)])
rank_tot$age[rank_tot$age==""] <- NA
predict.rank <- lmer(st_rank ~ age+at.roost+
                               sex+
                              (1|group),
                     data=rank_tot,
                     na.action="na.omit")
summary(predict.rank)

texreg(predict.rank)

rank$age[rank$age==""] <- NA

predict.rank.2019 <- lmer(st_rank ~ age+at.roost +
                       sex+
                       (1|group),
                     data=rank,
                     na.action="na.omit")
summary(predict.rank.2019)

### MALES ONLY
males1 <- vector[which(vector$Sex=="M"),]
males2 <- marking[which(marking$SAssigned_Sex=="M"),]

males <- c(males1$Bird_ID,males2$ID_Site)
agg$both.males <- agg$WINNER %in%males &agg$LOSER %in%males


CG <- agg[which(agg$LOCATION=="CG" & (agg$both.males==TRUE)),]

winners.CG <- as.vector(CG$WINNER)
losers.CG <- as.vector(CG$LOSER)

####### Checking number interactions per indiv (july)
nbr_win <- as.data.frame(table(winners.CG))
nbr_loss <- as.data.frame(table(losers.CG))

nbr_win_CG <- as.data.frame(table(winners.CG))
nbr_loss_CG <- as.data.frame(table(losers.CG))
names(nbr_win_CG) <- c("ID", "wins")
names(nbr_loss_CG) <- c("ID", "loss")


int <- rbind.fill(nbr_win_CG[c("wins","ID")], nbr_loss_CG[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
nbr.int.CG <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(nbr.int.CG) <- c("ID", "wins", "los")

for (i in 1:nrow(nbr.int.CG)){ 
  nbr.int.CG$total[i] <- sum(nbr.int.CG[i,2]+nbr.int.CG[i,3])
}

nbr.int.CG$over10 <- nbr.int.CG$total>=10


### DOES INDIVIDUAL HAVE >10 int ?
CG.ids.over10 <- nbr.int.CG[which(nbr.int.CG$over10==TRUE),]


######## calculate scores
CG$areBoth = ((CG$WINNER %in% CG.ids.over10$ID) & (CG$LOSER %in% CG.ids.over10$ID))
CG.over10 <- CG[which(CG$areBoth==TRUE),]

winners.CG <- as.vector(CG.over10$WINNER)
losers.CG <- as.vector(CG.over10$LOSER)

scores.CG <- elo_scores(winners=winners.CG,
                        losers=losers.CG,
                        randomise=TRUE,
                        sigmoid.param=1/300,
                        K=200,
                        n.rands=10000,
                        return.as.ranks = TRUE
)


beep(5)
scores.CG <- as.data.frame(scores.CG)
######## calculate ranks
for (i in 1:nrow(scores.CG)) { 
  scores.CG$Med.rank [i] <- rowMeans(scores.CG)[i]
}

rank.CG <- as.data.frame(scores.CG$Med.rank)
rank.CG$rank <- rank(rank.CG, na.last = TRUE)
rank.CG$ID <- rownames(scores.CG)

rank.CG$st_rank <-rank.CG$rank/max(rank.CG$rank)
rank.CG$group <-"CG"

## BA
BA <- agg[which(agg$LOCATION=="BA"& agg$both.males==TRUE),]

winners.BA <- as.vector(BA$WINNER)
losers.BA <- as.vector(BA$LOSER)

####### Checking number interactions per indiv (july)
BAr_win <- as.data.frame(table(winners.BA))
BAr_loss <- as.data.frame(table(losers.BA))

BAr_win_BA <- as.data.frame(table(winners.BA))
BAr_loss_BA <- as.data.frame(table(losers.BA))
names(BAr_win_BA) <- c("ID", "wins")
names(BAr_loss_BA) <- c("ID", "loss")


int <- rbind.fill(BAr_win_BA[c("wins","ID")], BAr_loss_BA[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
BAr.int.BA <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(BAr.int.BA) <- c("ID", "wins", "los")

for (i in 1:nrow(BAr.int.BA)){ 
  BAr.int.BA$total[i] <- sum(BAr.int.BA[i,2]+BAr.int.BA[i,3])
}

BAr.int.BA$over10 <- BAr.int.BA$total>=10


### DOES INDIVIDUAL HAVE >10 int ?
BA.ids.over10 <- BAr.int.BA[which(BAr.int.BA$over10==TRUE),]


######## calculate scores
BA$areBoth = ((BA$WINNER %in% BA.ids.over10$ID) & (BA$LOSER %in% BA.ids.over10$ID))
BA.over10 <- BA[which(BA$areBoth==TRUE),]

winners.BA <- as.vector(BA.over10$WINNER)
losers.BA <- as.vector(BA.over10$LOSER)

scores.BA <- elo_scores(winners=winners.BA,
                        losers=losers.BA,
                        randomise=TRUE,
                        sigmoid.param=1/300,
                        K=200,
                        n.rands=10000,
                        return.as.ranks = TRUE
)

beep(3)
scores.BA <- as.data.frame(scores.BA)
######## calculate ranks
for (i in 1:nrow(scores.BA)) { 
  scores.BA$Med.rank [i] <- rowMeans(scores.BA)[i]
}

rank.BA <- as.data.frame(scores.BA$Med.rank)
rank.BA$rank <- rank(rank.BA, na.last = TRUE)
rank.BA$ID <- rownames(scores.BA)

rank.BA$st_rank <-rank.BA$rank/max(rank.BA$rank)
rank.BA$group <-"BA"

## NB
NB <- agg[which(agg$LOCATION=="NB"& agg$both.males==TRUE),]

winners.NB <- as.vector(NB$WINNER)
losers.NB <- as.vector(NB$LOSER)

####### Checking number interactions per indiv (july)
nbr_win <- as.data.frame(table(winners.NB))
nbr_loss <- as.data.frame(table(losers.NB))

nbr_win_NB <- as.data.frame(table(winners.NB))
nbr_loss_NB <- as.data.frame(table(losers.NB))
names(nbr_win_NB) <- c("ID", "wins")
names(nbr_loss_NB) <- c("ID", "loss")


int <- rbind.fill(nbr_win_NB[c("wins","ID")], nbr_loss_NB[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
nbr.int.NB <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(nbr.int.NB) <- c("ID", "wins", "los")

for (i in 1:nrow(nbr.int.NB)){ 
  nbr.int.NB$total[i] <- sum(nbr.int.NB[i,2]+nbr.int.NB[i,3])
}

nbr.int.NB$over10 <- nbr.int.NB$total>=10


### DOES INDIVIDUAL HAVE >10 int ?
NB.ids.over10 <- nbr.int.NB[which(nbr.int.NB$over10==TRUE),]


######## calculate scores
NB$areBoth = ((NB$WINNER %in% NB.ids.over10$ID) & (NB$LOSER %in% NB.ids.over10$ID))
NB.over10 <- NB[which(NB$areBoth==TRUE),]

winners.NB <- as.vector(NB.over10$WINNER)
losers.NB <- as.vector(NB.over10$LOSER)

scores.NB <- elo_scores(winners=winners.NB,
                        losers=losers.NB,
                        randomise=TRUE,
                        sigmoid.param=1/300,
                        K=200,
                        n.rands=10000,
                        return.as.ranks = TRUE
)

beep(2)
scores.NB <- as.data.frame(scores.NB)
######## calculate ranks
for (i in 1:nrow(scores.NB)) { 
  scores.NB$Med.rank [i] <- rowMeans(scores.NB)[i]
}

rank.NB <- as.data.frame(scores.NB$Med.rank)
rank.NB$rank <- rank(rank.NB, na.last = TRUE)
rank.NB$ID <- rownames(scores.NB)

rank.NB$st_rank <-rank.NB$rank/max(rank.NB$rank)
rank.NB$group <-"NB"




## CG 2022
males_2022 <- sexing_2022$ID[sexing_2022$assigned_sex=="M"]
CG_2022_m <- agg_2022[which(agg_2022$Location=="CG"& 
                            agg_2022$Winner %in%males_2022 &
                              agg_2022$Loser%in%males_2022),]

winners.CG.m <- as.vector(CG_2022_m$Winner)
losers.CG.m <- as.vector(CG_2022_m$Loser)

####### Checking number interactions per indiv (july)
CGr_win_CG <- as.data.frame(table(winners.CG.m))
CGr_loss_CG <- as.data.frame(table(losers.CG.m))
names(CGr_win_CG) <- c("ID", "wins")
names(CGr_loss_CG) <- c("ID", "loss")


int <- rbind.fill(CGr_win_CG[c("wins","ID")], CGr_loss_CG[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
CGr.int.CG <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(CGr.int.CG) <- c("ID", "wins", "los")

for (i in 1:nrow(CGr.int.CG)){ 
  CGr.int.CG$total[i] <- sum(CGr.int.CG[i,2]+CGr.int.CG[i,3])
}

CGr.int.CG$over10 <- CGr.int.CG$total>=10


### DOES INDIVIDUAL HAVE >10 int ?
CG.ids.over10 <- CGr.int.CG[which(CGr.int.CG$over10==TRUE),]


######## calculate scores
CG_2022_m$areBoth = ((CG_2022_m$Winner %in% CG.ids.over10$ID) & (CG_2022_m$Loser %in% CG.ids.over10$ID))
CG.over10_m <- CG_2022_m[which(CG_2022_m$areBoth==TRUE),]

winners.CG.m.2022 <- as.vector(CG.over10_m$Winner)
losers.CG.m.2022 <- as.vector(CG.over10_m$Loser)

scores.CG.m.2022 <- elo_scores(winners=winners.CG.m.2022,
                        losers=losers.CG.m.2022,
                        randomise=TRUE,
                        sigmoid.param=1/300,
                        K=200,
                        n.rands=10000,
                        return.as.ranks = TRUE
)

beep(2)
scores.CG.m.2022 <- as.data.frame(scores.CG.m.2022)
######## calculate ranks
for (i in 1:nrow(scores.CG.m.2022)) { 
  scores.CG.m.2022$Med.rank [i] <- rowMeans(scores.CG.m.2022)[i]
}

rank.CG.m.2022 <- as.data.frame(scores.CG.m.2022$Med.rank)
rank.CG.m.2022$rank <- rank(rank.CG.m.2022, na.last = TRUE)
rank.CG.m.2022$ID <- rownames(scores.CG.m.2022)

rank.CG.m.2022$st_rank <-rank.CG.m.2022$rank/max(rank.CG.m.2022$rank)
rank.CG.m.2022$group <-"CG"





rank <- rbind(rank.BA[,2:5],rank.CG[,2:5],rank.NB[,2:5])

rank$roost <- roost$ROOST[match(rank$ID,roost$ID)]

rank$at.roost <- rank$group ==rank$roost

rank$age <- vector$Corrected_Age[match(rank$ID,vector$Bird_ID)]
rank$age[is.na(rank$age)] <- marking$Assigned_Age[match(rank$ID[is.na(rank$age)],marking$ID_Site)]



predict.rank.m.2019 <- lmer(st_rank ~ age+at.roost +
                       (1|ID) +
                       (1|group),
                     data=rank,
                     na.action="na.omit",
                     control=lmerControl(optimizer="Nelder_Mead",
                                        optCtrl=list(maxfun=2e5)),)
summary(predict.rank.m.2019)
afurl <- "https://raw.githubusercontent.com/lme4/lme4/master/misc/issues/allFit.R"
eval(parse(text=getURL(afurl)))
aa <- allFit(predict.rank)

is.OK <- sapply(aa,is,"merMod")  
aa.OK <- aa[is.OK]
lapply(aa.OK,function(x) x@optinfo$conv$lme4$messages)

rank.CG.m.2022$roost <- roost_2022$roost[match(rank.CG.m.2022$ID,roost_2022$ID)]
rank.CG.m.2022$group <-"CG 2022"
rank.CG.m.2022$at.roost <- rank.CG.m.2022$group ==rank.CG.m.2022$roost


rank.CG.m.2022$age <- sexing_2022$assigned_age[match(rank.CG.m.2022$ID,sexing_2022$ID)]


rank_tot_m <- rbind(rank,rank.CG.m.2022[,2:ncol(rank.CG.m.2022)])

predict.rank.m.tot <- lmer(st_rank ~ age*at.roost +
                              (1|ID) +
                              (1|group),
                            data=rank_tot_m,
                            na.action="na.omit",
                            control=lmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=2e5)),)
summary(predict.rank.m.tot)
afurl <- "https://raw.githubusercontent.com/lme4/lme4/master/misc/issues/allFit.R"
eval(parse(text=getURL(afurl)))
aa <- allFit(predict.rank.m.tot)

is.OK <- sapply(aa,is,"merMod")  
aa.OK <- aa[is.OK]
lapply(aa.OK,function(x) x@optinfo$conv$lme4$messages)
