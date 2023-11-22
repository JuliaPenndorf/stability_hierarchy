##########################################################################################################################
# Calculating the dominance hierarchies of Sulphur-crested cockatoos at 3 study locations (CG, BA, NB)
#                             repeatability & figures (shape & hierarchy)

##########################################################################################################################



#load packages
library (aniDom)
library(reshape2)
library(plyr)

#load data
agg <- read.csv('soc_data_2019.csv', stringsAsFactors = FALSE,header = T,row.names = 1)
agg_2022 <- read.csv('agg_data_cleaned.csv',stringsAsFactors = F)

gen_sexing <- read.csv2('genetics_sexings_20210419.csv')
sexing <- read.csv2('Marking_sheet_MASTER_corrected.csv')
vector<-read.csv("Vector_IDs_15July.csv")
sexing_2022 <- read.csv("sexing_2022.csv")
roost<- read.csv("roost_sites_NORTH.csv")
roost_2022 <- read.csv("roost_2022.csv")


IDs <- as.data.frame(unique(c(agg$WINNER,agg$WINNER)))


# CG 2019
CG <- agg[which(agg$LOCATION=="CG"),]

winners.CG <- as.vector(CG$WINNER)
losers.CG <- as.vector(CG$LOSER)

####### Checking number interactions per individual
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


### Subsetting to individuals with >10 interactions at the site (CG)
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

cg_certainty_repeat <- estimate_uncertainty_by_repeatability(winners=winners.CG,
                                                             losers=losers.CG,
                                                             sigmoid.param=1/300,
                                                             K=200,
                                                             n.rands=10000)
cg_certainty_split <- estimate_uncertainty_by_splitting(winners=winners.CG,
                                                        losers=losers.CG,
                                                        sigmoid.param=1/300,
                                                        K=200,
                                                        n.rands=10000,
                                                        randomise=TRUE)

scores.CG <- as.data.frame(scores.CG)

######## calculate ranks
for (i in 1:nrow(scores.CG)) { 
  scores.CG$Med.rank [i] <- rowMeans(scores.CG)[i]
}

rank.CG <- as.data.frame(scores.CG$Med.rank)
rank.CG$rank <- rank(rank.CG, na.last = TRUE)
rank.CG$ID <- rownames(scores.CG)


rank.CG$sexing <- gen_sexing$sexing[match(rank.CG$ID,gen_sexing$ID)]

rank.CG$sexing[is.na(rank.CG$sexing)] <- as.character(vector$Sex[match(rank.CG$ID[is.na(rank.CG$sexing)], vector$Social_ID)])
rank.CG$sexing[is.na(rank.CG$sexing)] <- as.character(sexing$Assigned_Sex[match(rank.CG$ID[is.na(rank.CG$sexing)], sexing$ID_Site)])

rank.CG$age <- as.character(sexing$Assigned_Age[match(rank.CG$ID, sexing$ID_Site)])


# CG 2022
CG_2022 <- agg_2022[which(agg_2022$Location=="CG"),]

winners.CG.2022 <- as.vector(CG_2022$Winner)
losers.CG.2022 <- as.vector(CG_2022$Loser)

####### Checking number interactions per individual
nbr_win <- as.data.frame(table(winners.CG.2022))
nbr_loss <- as.data.frame(table(losers.CG.2022))

nbr_win_CG <- as.data.frame(table(winners.CG.2022))
nbr_loss_CG <- as.data.frame(table(losers.CG.2022))
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


### Subsetting to individuals with >10 interactions at the site (CG)
CG.ids.over10.2022 <- nbr.int.CG[which(nbr.int.CG$over10==TRUE),]

######## calculate scores
CG_2022$areBoth = ((CG_2022$Winner %in% CG.ids.over10.2022$ID) & (CG_2022$Loser %in% CG.ids.over10.2022$ID))
CG.over10.2022 <- CG_2022[which(CG_2022$areBoth==TRUE),]

winners.CG.2022 <- as.vector(CG.over10.2022$Winner)
losers.CG.2022 <- as.vector(CG.over10.2022$Loser)

scores.CG.2022 <- elo_scores(winners=winners.CG.2022,
                             losers=losers.CG.2022,
                             randomise=TRUE,
                             sigmoid.param=1/300,
                             K=200,
                             n.rands=10000,
                             return.as.ranks = TRUE
)

cg_certainty_repeat_2022 <- estimate_uncertainty_by_repeatability(winners=winners.CG.2022,
                                                                  losers=losers.CG.2022,
                                                                  sigmoid.param=1/300,
                                                                  K=200,
                                                                  n.rands=10000)
cg_certainty_split_2022 <- estimate_uncertainty_by_splitting(winners=winners.CG.2022,
                                                             losers=losers.CG.2022,
                                                             sigmoid.param=1/300,
                                                             K=200,
                                                             n.rands=10000,
                                                             randomise=TRUE)

scores.CG.2022 <- as.data.frame(scores.CG.2022)

######## calculate ranks
for (i in 1:nrow(scores.CG.2022)) { 
  scores.CG.2022$Med.rank [i] <- rowMeans(scores.CG.2022)[i]
}

rank.CG.2022 <- as.data.frame(scores.CG.2022$Med.rank)
rank.CG.2022$rank <- rank(rank.CG.2022, na.last = TRUE)
rank.CG.2022$ID <- rownames(scores.CG.2022)


rank.CG.2022$sexing <- sexing_2022$sex[match(rank.CG.2022$ID,sexing_2022$ID)]

rank.CG.2022$age <- rank.CG.2022$age[match(rank.CG.2022$ID, sexing_2022$ID)]



# BA
BA <- agg[which(agg$LOCATION=="BA"),]

winners.BA <- as.vector(BA$WINNER)
losers.BA <- as.vector(BA$LOSER)

####### Checking number interactions per individual
nbr_win <- as.data.frame(table(winners.BA))
nbr_loss <- as.data.frame(table(losers.BA))

nbr_win_BA <- as.data.frame(table(winners.BA))
nbr_loss_BA <- as.data.frame(table(losers.BA))
names(nbr_win_BA) <- c("ID", "wins")
names(nbr_loss_BA) <- c("ID", "loss")


int <- rbind.fill(nbr_win_BA[c("wins","ID")], nbr_loss_BA[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
nbr.int.BA <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(nbr.int.BA) <- c("ID", "wins", "los")

for (i in 1:nrow(nbr.int.BA)){ 
  nbr.int.BA$total[i] <- sum(nbr.int.BA[i,2]+nbr.int.BA[i,3])
}

nbr.int.BA$over10 <- nbr.int.BA$total>=10


### Subsetting to individuals with >10 interactions at the site (BA)
BA.ids.over10 <- nbr.int.BA[which(nbr.int.BA$over10==TRUE),]


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
ba_certainty_repeat <- estimate_uncertainty_by_repeatability(winners=winners.BA,
                                                             losers=losers.BA,
                                                             sigmoid.param=1/300,
                                                             K=200,
                                                             n.rands=10000)
ba_certainty_split <- estimate_uncertainty_by_splitting(winners=winners.BA,
                                                        losers=losers.BA,
                                                        sigmoid.param=1/300,
                                                        K=200,
                                                        n.rands=10000,
                                                        randomise=TRUE)

scores.BA <- as.data.frame(scores.BA)

######## calculate ranks
for (i in 1:nrow(scores.BA)) { 
  scores.BA$Med.rank [i] <- rowMeans(scores.BA)[i]
}

rank.BA <- as.data.frame(scores.BA$Med.rank)
rank.BA$rank <- rank(rank.BA, na.last = TRUE)
rank.BA$ID <- rownames(scores.BA)

rank.BA$sexing <- gen_sexing$sexing[match(rank.BA$ID,gen_sexing$ID)]

rank.BA$sexing[is.na(rank.BA$sexing)] <- as.character(vector$Sex[match(rank.BA$ID[is.na(rank.BA$sexing)], vector$Social_ID)])
rank.BA$sexing[is.na(rank.BA$sexing)] <- as.character(sexing$Assigned_Sex[match(rank.BA$ID[is.na(rank.BA$sexing)], sexing$ID_Site)])

rank.BA$age <- as.character(sexing$Assigned_Age[match(rank.BA$ID, sexing$ID_Site)])




# NB
NB <- agg[which(agg$LOCATION=="NB"),]

winners.NB <- as.vector(NB$WINNER)
losers.NB <- as.vector(NB$LOSER)

####### Checking number interactions per individual 
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


### Subsetting to individuals with >10 interactions at the site (NB)
NB.ids.over10 <- nbr.int.NB[which(nbr.int.NB$over10==TRUE),]


######## calculate elo-scores
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
nb_certainty_repeat <- estimate_uncertainty_by_repeatability(winners=winners.NB,
                                                             losers=losers.NB,
                                                             sigmoid.param=1/300,
                                                             K=200,
                                                             n.rands=10000)
nb_certainty_split <- estimate_uncertainty_by_splitting(winners=winners.NB,
                                                        losers=losers.NB,
                                                        sigmoid.param=1/300,
                                                        K=200,
                                                        n.rands=10000,
                                                        randomise=TRUE)
scores.NB <- as.data.frame(scores.NB)

######## get dominance ranks
for (i in 1:nrow(scores.NB)) { 
  scores.NB$Med.rank [i] <- rowMeans(scores.NB)[i]
}

rank.NB <- as.data.frame(scores.NB$Med.rank)
rank.NB$rank <- rank(rank.NB, na.last = TRUE)
rank.NB$ID <- rownames(scores.NB)


rank.NB$sexing <- gen_sexing$sexing[match(rank.NB$ID,gen_sexing$ID)]

rank.NB$sexing[is.na(rank.NB$sexing)] <- as.character(vector$Sex[match(rank.NB$ID[is.na(rank.NB$sexing)], vector$Social_ID)])
rank.NB$sexing[is.na(rank.NB$sexing)] <- as.character(sexing$Assigned_Sex[match(rank.NB$ID[is.na(rank.NB$sexing)], sexing$ID_Site)])

rank.NB$age <- as.character(sexing$Assigned_Age[match(rank.NB$ID, sexing$ID_Site)])






# colouring hierarchy plot by sex
## CG
rank2 <- rank.CG
identities <- as.data.frame(cbind(rank2$ID,as.numeric(rank2$rank)))
colnames(identities) <- c("ID", "rank")
identities$age <- sexing$Assigned_Age[match(identities$ID,sexing$ID_Site)]
identities$sex <- sexing$Assigned_Sex[match(identities$ID,sexing$ID_Site)]
identities$age[is.na(identities$age)] <- vector$Age[match(identities$ID[is.na(identities$age)],vector$ID)]
identities$sex[is.na(identities$sex)] <- vector$Sex[match(identities$ID[is.na(identities$sex)],vector$ID)]

identities$age[identities$ID=="X78"] <- "A"
identities$sex[identities$ID=="X78"] <- "M"

identities$age[identities$ID=="X11"] <- "A"
identities$sex[identities$ID=="X11"] <- "M"

identities$age[identities$ID=="X93"] <- "A"
identities$sex[identities$ID=="X93"] <- "M"

identities$age[identities$ID=="X5"] <- "A"
identities$sex[identities$ID=="X5"] <- "M"

identities$age[identities$ID=="X50"] <- "A"
identities$sex[identities$ID=="X50"] <- "F"

identities$age[identities$ID=="X31"] <- "A"
identities$sex[identities$ID=="X31"] <- "F"

identities$age[identities$ID=="X42"] <- "A"
identities$sex[identities$ID=="X42"] <- "M"

identities$age[identities$ID=="X40"] <- "A"
identities$sex[identities$ID=="X40"] <- "F"

identities$age[identities$ID=="X39"] <- "A"
identities$sex[identities$ID=="X39"] <- "F"

identities$age[identities$ID=="X35"] <- "A"
identities$sex[identities$ID=="X35"] <- "M"

identities$age[identities$ID=="X27"] <- "A"
identities$sex[identities$ID=="X27"] <- "M"

identities$age[identities$ID=="X124"] <- "A"
identities$sex[identities$ID=="X124"] <- "M"

identities$age[identities$ID=="X107"] <- "A"
identities$sex[identities$ID=="X107"] <- "F"

identities$age[identities$ID=="bigP_H_CG"] <- "A"
identities$sex[identities$ID=="bigP_H_CG"] <- "M"

rank.CG <- identities
rank.CG$age_sex <- paste(rank.CG$age,rank.CG$sex)
rank.CG$COLOUR <- "lightgrey"
rank.CG$COLOUR [which(rank.CG$age_sex == "A M")] <- "darkcyan"
rank.CG$COLOUR [which(rank.CG$age_sex == "J M")] <- "turquoise"


rank.CG$COLOUR [which(rank.CG$age_sex == "A F")] <- "darkred"
rank.CG$COLOUR [which(rank.CG$age_sex == "J F")] <- "tomato"


rank.CG$roost <- roost$ROOST[match(rank.CG$ID,roost$ID)]
rank.CG$roost_same <- rank.CG$roost=="CG"

rank.CG$COLOUR_r <- "#B6AD90"
rank.CG$COLOUR_r[which(rank.CG$roost_same ==T)] <- "#79B791"

plot_ranks(-scores.CG,plot.CIs=TRUE, colors=rank.CG$COLOUR_r,plot.identities=F)

legend(0, 75,
       legend=c("resident", "visitor" ),
       col=c("#79B791","#B6AD90"), 
       pch=20, cex=1,
       text.width=11.5)


## CG 2022
rank.CG.2022$age <- sexing_2022$assigned_age[match(rank.CG.2022$ID,sexing_2022$ID)]
rank.CG.2022$sex <- sexing_2022$assigned_sex[match(rank.CG.2022$ID,sexing_2022$ID)]

rank.CG.2022$age_sex <- paste(rank.CG.2022$age,rank.CG.2022$sex)
rank.CG.2022$COLOUR <- "lightgrey"
rank.CG.2022$COLOUR [which(rank.CG.2022$age_sex == "A M")] <- "darkcyan"
rank.CG.2022$COLOUR [which(rank.CG.2022$age_sex == "J M")] <- "turquoise"

rank.CG.2022$COLOUR [which(rank.CG.2022$age_sex == "A F")] <- "darkred"
rank.CG.2022$COLOUR [which(rank.CG.2022$age_sex == "J F")] <- "tomato"


rank.CG.2022.sub <- na.omit(rank.CG.2022)

scores.CG2.2022 <- scores.CG.2022[which(rownames(scores.CG.2022)%in%rank.CG.2022.sub$ID),]

plot_ranks(-scores.CG.2022,plot.CIs=TRUE, colors=rank.CG.2022$COLOUR,plot.identities=F)

legend(0, 41,
       legend=c("A M", "J M", "A F", "J F", "unknown age and/or sex" ),
       col=c("darkcyan","turquoise", "darkred","tomato","lightgrey"), 
       pch=20, cex=1,
       text.width=27.5)


rank.CG.2022$roost <- roost_2022$roost[match(rank.CG.2022$ID,roost_2022$ID)]
rank.CG.2022$roost_same <- rank.CG.2022$roost=="CG"

rank.CG.2022$COLOUR_r <- "#B6AD90"
rank.CG.2022$COLOUR_r[which(rank.CG.2022$roost_same ==T)] <- "#79B791"

plot_ranks(-scores.CG.2022,plot.CIs=TRUE, colors=rank.CG.2022$COLOUR_r,plot.identities=F)

legend(0, 50,
       legend=c("resident", "visitor" ),
       col=c("#79B791","#B6AD90"), 
       pch=20, cex=1,
       text.width=11.5)




## BA
rank2 <- rank.BA
identities <- as.data.frame(cbind(rank2$ID,as.numeric(rank2$rank)))
colnames(identities) <- c("ID", "rank")
identities$age <- sexing$Assigned_Age[match(identities$ID,sexing$ID_Site)]
identities$sex <- sexing$Assigned_Sex[match(identities$ID,sexing$ID_Site)]
identities$age[is.na(identities$age)] <- vector$Age[match(identities$ID[is.na(identities$age)],vector$ID)]
identities$sex[is.na(identities$sex)] <- vector$Sex[match(identities$ID[is.na(identities$sex)],vector$ID)]

identities$age[identities$ID=="X78"] <- "A"
identities$sex[identities$ID=="X78"] <- "M"

identities$age[identities$ID=="X11"] <- "A"
identities$sex[identities$ID=="X11"] <- "M"

identities$age[identities$ID=="X93"] <- "A"
identities$sex[identities$ID=="X93"] <- "M"

identities$age[identities$ID=="X5"] <- "A"
identities$sex[identities$ID=="X5"] <- "M"

identities$age[identities$ID=="X50"] <- "A"
identities$sex[identities$ID=="X50"] <- "F"

identities$age[identities$ID=="X31"] <- "A"
identities$sex[identities$ID=="X31"] <- "F"

identities$age[identities$ID=="X42"] <- "A"
identities$sex[identities$ID=="X42"] <- "M"

identities$age[identities$ID=="X40"] <- "A"
identities$sex[identities$ID=="X40"] <- "F"

identities$age[identities$ID=="X39"] <- "A"
identities$sex[identities$ID=="X39"] <- "F"

identities$age[identities$ID=="X35"] <- "A"
identities$sex[identities$ID=="X35"] <- "M"

identities$age[identities$ID=="X27"] <- "A"
identities$sex[identities$ID=="X27"] <- "M"

identities$age[identities$ID=="X124"] <- "A"
identities$sex[identities$ID=="X124"] <- "M"

identities$age[identities$ID=="X107"] <- "A"
identities$sex[identities$ID=="X107"] <- "F"

identities$age[identities$ID=="bigP_H_CG"] <- "A"
identities$sex[identities$ID=="bigP_H_CG"] <- "M"

identities$age[identities$ID=="X15"] <- "A"
identities$sex[identities$ID=="X15"] <- "F"

identities$age[identities$ID=="X118"] <- "A"
identities$sex[identities$ID=="X118"] <- "M"

identities$age[identities$ID=="X88"] <- "A"
identities$sex[identities$ID=="X88"] <- "M"

rank.BA <- identities
rank.BA$age_sex <- paste(rank.BA$age,rank.BA$sex)
rank.BA$COLOUR <- "lightgrey"
rank.BA$COLOUR [which(rank.BA$age_sex == "A M")] <- "darkcyan"
rank.BA$COLOUR [which(rank.BA$age_sex == "J M")] <- "turquoise"


rank.BA$COLOUR [which(rank.BA$age_sex == "A F")] <- "darkred"
rank.BA$COLOUR [which(rank.BA$age_sex == "J F")] <- "tomato"


rank.BA.sub <- na.omit(rank.BA)
rank.BA.sub <- rank.BA.sub[which(rank.BA.sub$ID!="BOB_V_BA"),]


scores.BA2 <- scores.BA[which(rownames(scores.BA)%in%rank.BA.sub$ID),]

plot_ranks(-scores.BA,plot.CIs=TRUE, colors=rank.BA$COLOUR,plot.identities=F)

legend(-1, 88,
       legend=c("A M", "J M", "A F", "J F", "unknown age and/or sex" ),
       col=c("darkcyan","turquoise", "darkred","tomato","lightgrey"), 
       pch=20, cex=1,
       text.width=37.5)


rank.BA$roost <- roost$ROOST[match(rank.BA$ID,roost$ID)]
rank.BA$roost_same <- rank.BA$roost=="BA"

rank.BA$COLOUR_r <- "#B6AD90"
rank.BA$COLOUR_r[which(rank.BA$roost_same ==T)] <- "#79B791"

plot_ranks(-scores.BA,plot.CIs=TRUE, colors=rank.BA$COLOUR_r,plot.identities=F)

legend(-1, 105,
       legend=c("resident", "visitor" ),
       col=c("#79B791","#B6AD90"), 
       pch=20, cex=1,
       text.width=11.5)





## NB
rank2 <- rank.NB
identities <- as.data.frame(cbind(rank2$ID,as.numeric(rank2$rank)))
colnames(identities) <- c("ID", "rank")
identities$age <- sexing$Assigned_Age[match(identities$ID,sexing$ID_Site)]
identities$sex <- sexing$Assigned_Sex[match(identities$ID,sexing$ID_Site)]
identities$age[is.na(identities$age)] <- vector$Age[match(identities$ID[is.na(identities$age)],vector$ID)]
identities$sex[is.na(identities$sex)] <- vector$Sex[match(identities$ID[is.na(identities$sex)],vector$ID)]

identities$age[identities$ID=="X78"] <- "A"
identities$sex[identities$ID=="X78"] <- "M"

identities$age[identities$ID=="X11"] <- "A"
identities$sex[identities$ID=="X11"] <- "M"

identities$age[identities$ID=="X93"] <- "A"
identities$sex[identities$ID=="X93"] <- "M"

identities$age[identities$ID=="X5"] <- "A"
identities$sex[identities$ID=="X5"] <- "M"

identities$age[identities$ID=="X50"] <- "A"
identities$sex[identities$ID=="X50"] <- "F"

identities$age[identities$ID=="X31"] <- "A"
identities$sex[identities$ID=="X31"] <- "F"

identities$age[identities$ID=="X42"] <- "A"
identities$sex[identities$ID=="X42"] <- "M"

identities$age[identities$ID=="X40"] <- "A"
identities$sex[identities$ID=="X40"] <- "F"

identities$age[identities$ID=="X39"] <- "A"
identities$sex[identities$ID=="X39"] <- "F"

identities$age[identities$ID=="X35"] <- "A"
identities$sex[identities$ID=="X35"] <- "M"

identities$age[identities$ID=="X27"] <- "A"
identities$sex[identities$ID=="X27"] <- "M"

identities$age[identities$ID=="X124"] <- "A"
identities$sex[identities$ID=="X124"] <- "M"

identities$age[identities$ID=="X107"] <- "A"
identities$sex[identities$ID=="X107"] <- "F"

identities$age[identities$ID=="bigP_H_CG"] <- "A"
identities$sex[identities$ID=="bigP_H_CG"] <- "M"

identities$age[identities$ID=="X15"] <- "A"
identities$sex[identities$ID=="X15"] <- "F"

identities$age[identities$ID=="X118"] <- "A"
identities$sex[identities$ID=="X118"] <- "M"

identities$age[identities$ID=="X88"] <- "A"
identities$sex[identities$ID=="X88"] <- "M"

identities$age[identities$ID=="X71"] <- "A"
identities$sex[identities$ID=="X71"] <- "F"

identities$age[identities$ID=="X115"] <- "A"
identities$sex[identities$ID=="X115"] <- "M"
rank.NB <- identities
rank.NB$age_sex <- paste(rank.NB$age,rank.NB$sex)
rank.NB$COLOUR <- "lightgrey"
rank.NB$COLOUR [which(rank.NB$age_sex == "A M")] <- "darkcyan"
rank.NB$COLOUR [which(rank.NB$age_sex == "J M")] <- "turquoise"


rank.NB$COLOUR [which(rank.NB$age_sex == "A F")] <- "darkred"
rank.NB$COLOUR [which(rank.NB$age_sex == "J F")] <- "tomato"


rank.NB.sub <- na.omit(rank.NB)
scores.NB2 <- scores.NB[which(rownames(scores.NB)%in%rank.NB.sub$ID),]

plot_ranks(-scores.NB,plot.CIs=TRUE, colors=rank.NB$COLOUR,plot.identities=F)

legend(-1, 51,
       legend=c("A M", "J M", "A F", "J F", "unknown age and/or sex" ),
       col=c("darkcyan","turquoise", "darkred","tomato","lightgrey"), 
       pch=20, cex=1,
       text.width=27.5)


rank.NB$roost <- roost$ROOST[match(rank.NB$ID,roost$ID)]
rank.NB$roost_same <- rank.NB$roost=="NB"

rank.NB$COLOUR_r <- "#B6AD90"
rank.NB$COLOUR_r[which(rank.NB$roost_same ==T)] <- "#79B791"

plot_ranks(-scores.NB,plot.CIs=TRUE, colors=rank.NB$COLOUR_r,plot.identities=F)

legend(-1, 63,
       legend=c("resident", "visitor" ),
       col=c("#79B791","#B6AD90"), 
       pch=20, cex=1,
       text.width=11.5)






# Plotting the likelihood of winning as function of the rank difference
## CG 2019
cg <- CG[which(CG$WINNER %in%rank.CG.sub$ID &
                 CG$LOSER %in%rank.CG.sub$ID ),]
plot_hierarchy_shape(identity=rank.CG$ID,
                     rank=as.numeric(rank.CG$rank), 
                     winners=cg$WINNER, 
                     losers=cg$LOSER, 
                     fitted=T
)
## CG 2022
plot_hierarchy_shape(identity=rank.CG.2022$ID,
                     rank=rank.CG.2022$rank, 
                     winners=winners.CG.2022, 
                     losers=losers.CG.2022, 
                     fitted=T
)
## BA
plot_hierarchy_shape(identity=rank.BA$ID,
                     rank=rank.BA$rank, 
                     winners=winners.BA, 
                     losers=losers.BA, 
                     fitted=T
)
## NB
plot_hierarchy_shape(identity=rank.NB$ID,
                     rank=rank.NB$rank, 
                     winners=winners.NB, 
                     losers=losers.NB, 
                     fitted=T
)


### Shape hierarchy males
###### CG 2019

# CG 2019
males.2019 <- unique(c(vector$Social_ID[vector$Sex=="M"],sexing$ID_Site[sexing$Assigned_Sex=="M"]))
CG.2019.m <- agg[which(agg$LOCATION=="CG" &
                      agg$WINNER%in%males.2019 &
                      agg$LOSER %in%males.2019),]

winners.CG.2019.m <- as.vector(CG.2019.m$WINNER)
losers.CG.2019.m <- as.vector(CG.2019.m$LOSER)

####### Checking number interactions per individual
nbr_win <- as.data.frame(table(winners.CG.2019.m))
nbr_loss <- as.data.frame(table(losers.CG.2019.m))

nbr_win_CG.2019.m <- as.data.frame(table(winners.CG.2019.m))
nbr_loss_CG.2019.m <- as.data.frame(table(losers.CG.2019.m))
names(nbr_win_CG.2019.m) <- c("ID", "wins")
names(nbr_loss_CG.2019.m) <- c("ID", "loss")


int <- rbind.fill(nbr_win_CG.2019.m[c("wins","ID")], nbr_loss_CG.2019.m[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
nbr.int.CG.2019.m <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(nbr.int.CG.2019.m) <- c("ID", "wins", "los")

for (i in 1:nrow(nbr.int.CG.2019.m)){ 
  nbr.int.CG.2019.m$total[i] <- sum(nbr.int.CG.2019.m[i,2]+nbr.int.CG.2019.m[i,3])
}

nbr.int.CG.2019.m$over10 <- nbr.int.CG.2019.m$total>=10


### Subsetting to individuals with >10 interactions at the site (CG.2019.m)
CG.2019.m.ids.over10 <- nbr.int.CG.2019.m[which(nbr.int.CG.2019.m$over10==TRUE),]

######## calculate scores
CG.2019.m$areBoth = ((CG.2019.m$WINNER %in% CG.2019.m.ids.over10$ID) & (CG.2019.m$LOSER %in% CG.2019.m.ids.over10$ID))
CG.2019.m.over10 <- CG.2019.m[which(CG.2019.m$areBoth==TRUE),]

winners.CG.2019.m <- as.vector(CG.2019.m.over10$WINNER)
losers.CG.2019.m <- as.vector(CG.2019.m.over10$LOSER)

scores.CG.2019.m <- elo_scores(winners=winners.CG.2019.m,
                        losers=losers.CG.2019.m,
                        randomise=TRUE,
                        sigmoid.param=1/300,
                        K=200,
                        n.rands=10000,
                        return.as.ranks = TRUE
)

scores.CG.2019.m <- as.data.frame(scores.CG.2019.m)

######## calculate ranks
for (i in 1:nrow(scores.CG.2019.m)) { 
  scores.CG.2019.m$Med.rank [i] <- rowMeans(scores.CG.2019.m)[i]
}

rank.CG.2019.m <- as.data.frame(scores.CG.2019.m$Med.rank)
rank.CG.2019.m$rank <- rank(rank.CG.2019.m, na.last = TRUE)
rank.CG.2019.m$ID <- rownames(scores.CG.2019.m)


rank.CG.2019.m$sexing <- gen_sexing$sexing[match(rank.CG.2019.m$ID,gen_sexing$ID)]

rank.CG.2019.m$sexing[is.na(rank.CG.2019.m$sexing)] <- as.character(vector$Sex[match(rank.CG.2019.m$ID[is.na(rank.CG.2019.m$sexing)], vector$Social_ID)])
rank.CG.2019.m$sexing[is.na(rank.CG.2019.m$sexing)] <- as.character(sexing$Assigned_Sex[match(rank.CG.2019.m$ID[is.na(rank.CG.2019.m$sexing)], sexing$ID_Site)])

rank.CG.2019.m$age <- as.character(sexing$Assigned_Age[match(rank.CG.2019.m$ID, sexing$ID_Site)])

plot_hierarchy_shape(identity=rank.CG.2019.m$ID,
                     rank=as.numeric(rank.CG.2019.m$rank), 
                     winners=winners.CG.2019.m, 
                     losers=losers.CG.2019.m, 
                     fitted=T
)





females.2019 <- unique(c(vector$Social_ID[vector$Sex=="F"],sexing$ID_Site[sexing$Assigned_Sex=="F"]))
CG.2019.f <- agg[which(agg$LOCATION=="CG" &
                         agg$WINNER%in%females.2019 &
                         agg$LOSER %in%females.2019),]

winners.CG.2019.f <- as.vector(CG.2019.f$WINNER)
losers.CG.2019.f <- as.vector(CG.2019.f$LOSER)

####### Checking number interactions per individual
nbr_win <- as.data.frame(table(winners.CG.2019.f))
nbr_loss <- as.data.frame(table(losers.CG.2019.f))

nbr_win_CG.2019.f <- as.data.frame(table(winners.CG.2019.f))
nbr_loss_CG.2019.f <- as.data.frame(table(losers.CG.2019.f))
names(nbr_win_CG.2019.f) <- c("ID", "wins")
names(nbr_loss_CG.2019.f) <- c("ID", "loss")


int <- rbind.fill(nbr_win_CG.2019.f[c("wins","ID")], nbr_loss_CG.2019.f[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
nbr.int.CG.2019.f <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(nbr.int.CG.2019.f) <- c("ID", "wins", "los")

for (i in 1:nrow(nbr.int.CG.2019.f)){ 
  nbr.int.CG.2019.f$total[i] <- sum(nbr.int.CG.2019.f[i,2]+nbr.int.CG.2019.f[i,3])
}

nbr.int.CG.2019.f$over10 <- nbr.int.CG.2019.f$total>=10


### Subsetting to individuals with >10 interactions at the site (CG.2019.f)
CG.2019.f.ids.over10 <- nbr.int.CG.2019.f[which(nbr.int.CG.2019.f$over10==TRUE),]

######## calculate scores
CG.2019.f$areBoth = ((CG.2019.f$WINNER %in% CG.2019.f.ids.over10$ID) & (CG.2019.f$LOSER %in% CG.2019.f.ids.over10$ID))
CG.2019.f.over10 <- CG.2019.f[which(CG.2019.f$areBoth==TRUE),]

winners.CG.2019.f <- as.vector(CG.2019.f.over10$WINNER)
losers.CG.2019.f <- as.vector(CG.2019.f.over10$LOSER)

scores.CG.2019.f <- elo_scores(winners=winners.CG.2019.f,
                               losers=losers.CG.2019.f,
                               randomise=TRUE,
                               sigmoid.param=1/300,
                               K=200,
                               n.rands=10000,
                               return.as.ranks = TRUE
)

scores.CG.2019.f <- as.data.frame(scores.CG.2019.f)

######## calculate ranks
for (i in 1:nrow(scores.CG.2019.f)) { 
  scores.CG.2019.f$Med.rank [i] <- rowMeans(scores.CG.2019.f)[i]
}

rank.CG.2019.f <- as.data.frame(scores.CG.2019.f$Med.rank)
rank.CG.2019.f$rank <- rank(rank.CG.2019.f, na.last = TRUE)
rank.CG.2019.f$ID <- rownames(scores.CG.2019.f)


rank.CG.2019.f$sexing <- gen_sexing$sexing[match(rank.CG.2019.f$ID,gen_sexing$ID)]

rank.CG.2019.f$sexing[is.na(rank.CG.2019.f$sexing)] <- as.character(vector$Sex[match(rank.CG.2019.f$ID[is.na(rank.CG.2019.f$sexing)], vector$Social_ID)])
rank.CG.2019.f$sexing[is.na(rank.CG.2019.f$sexing)] <- as.character(sexing$Assigned_Sex[match(rank.CG.2019.f$ID[is.na(rank.CG.2019.f$sexing)], sexing$ID_Site)])

rank.CG.2019.f$age <- as.character(sexing$Assigned_Age[match(rank.CG.2019.f$ID, sexing$ID_Site)])

plot_hierarchy_shape(identity=rank.CG.2019.f$ID,
                     rank=as.numeric(rank.CG.2019.f$rank), 
                     winners=winners.CG.2019.f, 
                     losers=losers.CG.2019.f, 
                     fitted=T
)

# BA 2019
BA.2019.m <- agg[which(agg$LOCATION=="BA" &
                         agg$WINNER%in%males.2019 &
                         agg$LOSER %in%males.2019),]

winners.BA.2019.m <- as.vector(BA.2019.m$WINNER)
losers.BA.2019.m <- as.vector(BA.2019.m$LOSER)

####### Checking number interactions per individual
nbr_win <- as.data.frame(table(winners.BA.2019.m))
nbr_loss <- as.data.frame(table(losers.BA.2019.m))

nbr_win_BA.2019.m <- as.data.frame(table(winners.BA.2019.m))
nbr_loss_BA.2019.m <- as.data.frame(table(losers.BA.2019.m))
names(nbr_win_BA.2019.m) <- c("ID", "wins")
names(nbr_loss_BA.2019.m) <- c("ID", "loss")


int <- rbind.fill(nbr_win_BA.2019.m[c("wins","ID")], nbr_loss_BA.2019.m[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
nbr.int.BA.2019.m <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(nbr.int.BA.2019.m) <- c("ID", "wins", "los")

for (i in 1:nrow(nbr.int.BA.2019.m)){ 
  nbr.int.BA.2019.m$total[i] <- sum(nbr.int.BA.2019.m[i,2]+nbr.int.BA.2019.m[i,3])
}

nbr.int.BA.2019.m$over10 <- nbr.int.BA.2019.m$total>=10


### Subsetting to individuals with >10 interactions at the site (BA.2019.m)
BA.2019.m.ids.over10 <- nbr.int.BA.2019.m[which(nbr.int.BA.2019.m$over10==TRUE),]

######## calculate scores
BA.2019.m$areBoth = ((BA.2019.m$WINNER %in% BA.2019.m.ids.over10$ID) & (BA.2019.m$LOSER %in% BA.2019.m.ids.over10$ID))
BA.2019.m.over10 <- BA.2019.m[which(BA.2019.m$areBoth==TRUE),]

winners.BA.2019.m <- as.vector(BA.2019.m.over10$WINNER)
losers.BA.2019.m <- as.vector(BA.2019.m.over10$LOSER)

scores.BA.2019.m <- elo_scores(winners=winners.BA.2019.m,
                               losers=losers.BA.2019.m,
                               randomise=TRUE,
                               sigmoid.param=1/300,
                               K=200,
                               n.rands=10000,
                               return.as.ranks = TRUE
)

scores.BA.2019.m <- as.data.frame(scores.BA.2019.m)

######## calculate ranks
for (i in 1:nrow(scores.BA.2019.m)) { 
  scores.BA.2019.m$Med.rank [i] <- rowMeans(scores.BA.2019.m)[i]
}

rank.BA.2019.m <- as.data.frame(scores.BA.2019.m$Med.rank)
rank.BA.2019.m$rank <- rank(rank.BA.2019.m, na.last = TRUE)
rank.BA.2019.m$ID <- rownames(scores.BA.2019.m)


rank.BA.2019.m$sexing <- gen_sexing$sexing[match(rank.BA.2019.m$ID,gen_sexing$ID)]

rank.BA.2019.m$sexing[is.na(rank.BA.2019.m$sexing)] <- as.character(vector$Sex[match(rank.BA.2019.m$ID[is.na(rank.BA.2019.m$sexing)], vector$Social_ID)])
rank.BA.2019.m$sexing[is.na(rank.BA.2019.m$sexing)] <- as.character(sexing$Assigned_Sex[match(rank.BA.2019.m$ID[is.na(rank.BA.2019.m$sexing)], sexing$ID_Site)])

rank.BA.2019.m$age <- as.character(sexing$Assigned_Age[match(rank.BA.2019.m$ID, sexing$ID_Site)])

plot_hierarchy_shape(identity=rank.BA.2019.m$ID,
                     rank=as.numeric(rank.BA.2019.m$rank), 
                     winners=winners.BA.2019.m, 
                     losers=losers.BA.2019.m, 
                     fitted=T
)



# NB 2019
NB.2019.m <- agg[which(agg$LOCATION=="NB" &
                         agg$WINNER%in%males.2019 &
                         agg$LOSER %in%males.2019),]

winners.NB.2019.m <- as.vector(NB.2019.m$WINNER)
losers.NB.2019.m <- as.vector(NB.2019.m$LOSER)

####### Checking number interactions per individual
nbr_win <- as.data.frame(table(winners.NB.2019.m))
nbr_loss <- as.data.frame(table(losers.NB.2019.m))

nbr_win_NB.2019.m <- as.data.frame(table(winners.NB.2019.m))
nbr_loss_NB.2019.m <- as.data.frame(table(losers.NB.2019.m))
names(nbr_win_NB.2019.m) <- c("ID", "wins")
names(nbr_loss_NB.2019.m) <- c("ID", "loss")


int <- rbind.fill(nbr_win_NB.2019.m[c("wins","ID")], nbr_loss_NB.2019.m[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
nbr.int.NB.2019.m <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(nbr.int.NB.2019.m) <- c("ID", "wins", "los")

for (i in 1:nrow(nbr.int.NB.2019.m)){ 
  nbr.int.NB.2019.m$total[i] <- sum(nbr.int.NB.2019.m[i,2]+nbr.int.NB.2019.m[i,3])
}

nbr.int.NB.2019.m$over10 <- nbr.int.NB.2019.m$total>=10


### Subsetting to individuals with >10 interactions at the site (NB.2019.m)
NB.2019.m.ids.over10 <- nbr.int.NB.2019.m[which(nbr.int.NB.2019.m$over10==TRUE),]

######## calculate scores
NB.2019.m$areBoth = ((NB.2019.m$WINNER %in% NB.2019.m.ids.over10$ID) & (NB.2019.m$LOSER %in% NB.2019.m.ids.over10$ID))
NB.2019.m.over10 <- NB.2019.m[which(NB.2019.m$areBoth==TRUE),]

winners.NB.2019.m <- as.vector(NB.2019.m.over10$WINNER)
losers.NB.2019.m <- as.vector(NB.2019.m.over10$LOSER)

scores.NB.2019.m <- elo_scores(winners=winners.NB.2019.m,
                               losers=losers.NB.2019.m,
                               randomise=TRUE,
                               sigmoid.param=1/300,
                               K=200,
                               n.rands=10000,
                               return.as.ranks = TRUE
)

scores.NB.2019.m <- as.data.frame(scores.NB.2019.m)

######## calculate ranks
for (i in 1:nrow(scores.NB.2019.m)) { 
  scores.NB.2019.m$Med.rank [i] <- rowMeans(scores.NB.2019.m)[i]
}

rank.NB.2019.m <- as.data.frame(scores.NB.2019.m$Med.rank)
rank.NB.2019.m$rank <- rank(rank.NB.2019.m, na.last = TRUE)
rank.NB.2019.m$ID <- rownames(scores.NB.2019.m)


rank.NB.2019.m$sexing <- gen_sexing$sexing[match(rank.NB.2019.m$ID,gen_sexing$ID)]

rank.NB.2019.m$sexing[is.na(rank.NB.2019.m$sexing)] <- as.character(vector$Sex[match(rank.NB.2019.m$ID[is.na(rank.NB.2019.m$sexing)], vector$Social_ID)])
rank.NB.2019.m$sexing[is.na(rank.NB.2019.m$sexing)] <- as.character(sexing$Assigned_Sex[match(rank.NB.2019.m$ID[is.na(rank.NB.2019.m$sexing)], sexing$ID_Site)])

rank.NB.2019.m$age <- as.character(sexing$Assigned_Age[match(rank.NB.2019.m$ID, sexing$ID_Site)])

plot_hierarchy_shape(identity=rank.NB.2019.m$ID,
                     rank=as.numeric(rank.NB.2019.m$rank), 
                     winners=winners.NB.2019.m, 
                     losers=losers.NB.2019.m, 
                     fitted=T
)


# CG 2022
males.2022 <- sexing_2022$ID[sexing_2022$assigned_sex=="M"]
CG.2022.m <- agg_2022[which(agg_2022$Location=="CG" &
                            agg_2022$Winner%in%males.2022 &
                            agg_2022$Loser %in%males.2022),]

winners.CG.2022.m <- as.vector(CG.2022.m$Winner)
losers.CG.2022.m <- as.vector(CG.2022.m$Loser)

####### Checking number interactions per individual
nbr_win <- as.data.frame(table(winners.CG.2022.m))
nbr_loss <- as.data.frame(table(losers.CG.2022.m))

nbr_win_CG.2022.m <- as.data.frame(table(winners.CG.2022.m))
nbr_loss_CG.2022.m <- as.data.frame(table(losers.CG.2022.m))
names(nbr_win_CG.2022.m) <- c("ID", "wins")
names(nbr_loss_CG.2022.m) <- c("ID", "loss")


int <- rbind.fill(nbr_win_CG.2022.m[c("wins","ID")], nbr_loss_CG.2022.m[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
nbr.int.CG.2022.m <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(nbr.int.CG.2022.m) <- c("ID", "wins", "los")

for (i in 1:nrow(nbr.int.CG.2022.m)){ 
  nbr.int.CG.2022.m$total[i] <- sum(nbr.int.CG.2022.m[i,2]+nbr.int.CG.2022.m[i,3])
}

nbr.int.CG.2022.m$over10 <- nbr.int.CG.2022.m$total>=10


### Subsetting to individuals with >10 interactions at the site (CG.2022.m)
CG.2022.m.ids.over10 <- nbr.int.CG.2022.m[which(nbr.int.CG.2022.m$over10==TRUE),]

######## calculate scores
CG.2022.m$areBoth = ((CG.2022.m$Winner %in% CG.2022.m.ids.over10$ID) & 
                       (CG.2022.m$Loser %in% CG.2022.m.ids.over10$ID))
CG.2022.m.over10 <- CG.2022.m[which(CG.2022.m$areBoth==TRUE),]

winners.CG.2022.m <- as.vector(CG.2022.m.over10$Winner)
losers.CG.2022.m <- as.vector(CG.2022.m.over10$Loser)

scores.CG.2022.m <- elo_scores(winners=winners.CG.2022.m,
                               losers=losers.CG.2022.m,
                               randomise=TRUE,
                               sigmoid.param=1/300,
                               K=200,
                               n.rands=10000,
                               return.as.ranks = TRUE
)

scores.CG.2022.m <- as.data.frame(scores.CG.2022.m)

######## calculate ranks
for (i in 1:nrow(scores.CG.2022.m)) { 
  scores.CG.2022.m$Med.rank [i] <- rowMeans(scores.CG.2022.m)[i]
}

rank.CG.2022.m <- as.data.frame(scores.CG.2022.m$Med.rank)
rank.CG.2022.m$rank <- rank(rank.CG.2022.m, na.last = TRUE)
rank.CG.2022.m$ID <- rownames(scores.CG.2022.m)


rank.CG.2022.m$sexing <- gen_sexing$sexing[match(rank.CG.2022.m$ID,gen_sexing$ID)]

rank.CG.2022.m$sexing[is.na(rank.CG.2022.m$sexing)] <- as.character(vector$Sex[match(rank.CG.2022.m$ID[is.na(rank.CG.2022.m$sexing)], vector$Social_ID)])
rank.CG.2022.m$sexing[is.na(rank.CG.2022.m$sexing)] <- as.character(sexing$Assigned_Sex[match(rank.CG.2022.m$ID[is.na(rank.CG.2022.m$sexing)], sexing$ID_Site)])

rank.CG.2022.m$age <- as.character(sexing$Assigned_Age[match(rank.CG.2022.m$ID, sexing$ID_Site)])

plot_hierarchy_shape(identity=rank.CG.2022.m$ID,
                     rank=as.numeric(rank.CG.2022.m$rank), 
                     winners=winners.CG.2022.m, 
                     losers=losers.CG.2022.m, 
                     fitted=T
)

females.2022 <- sexing_2022$ID[sexing_2022$assigned_sex=="F"]
CG.2022.f <- agg_2022[which(agg_2022$Location=="CG" &
                              agg_2022$Winner%in%females.2022 &
                              agg_2022$Loser %in%females.2022),]

winners.CG.2022.f <- as.vector(CG.2022.f$Winner)
losers.CG.2022.f <- as.vector(CG.2022.f$Loser)

####### Checking number interactions per individual
nbr_win <- as.data.frame(table(winners.CG.2022.f))
nbr_loss <- as.data.frame(table(losers.CG.2022.f))

nbr_win_CG.2022.f <- as.data.frame(table(winners.CG.2022.f))
nbr_loss_CG.2022.f <- as.data.frame(table(losers.CG.2022.f))
names(nbr_win_CG.2022.f) <- c("ID", "wins")
names(nbr_loss_CG.2022.f) <- c("ID", "loss")


int <- rbind.fill(nbr_win_CG.2022.f[c("wins","ID")], nbr_loss_CG.2022.f[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
nbr.int.CG.2022.f <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(nbr.int.CG.2022.f) <- c("ID", "wins", "los")

for (i in 1:nrow(nbr.int.CG.2022.f)){ 
  nbr.int.CG.2022.f$total[i] <- sum(nbr.int.CG.2022.f[i,2]+nbr.int.CG.2022.f[i,3])
}

nbr.int.CG.2022.f$over10 <- nbr.int.CG.2022.f$total>=10


### Subsetting to individuals with >10 interactions at the site (CG.2022.f)
CG.2022.f.ids.over10 <- nbr.int.CG.2022.f[which(nbr.int.CG.2022.f$over10==TRUE),]

######## calculate scores
CG.2022.f$areBoth = ((CG.2022.f$Winner %in% CG.2022.f.ids.over10$ID) & 
                       (CG.2022.f$Loser %in% CG.2022.f.ids.over10$ID))
CG.2022.f.over10 <- CG.2022.f[which(CG.2022.f$areBoth==TRUE),]

winners.CG.2022.f <- as.vector(CG.2022.f.over10$Winner)
losers.CG.2022.f <- as.vector(CG.2022.f.over10$Loser)

scores.CG.2022.f <- elo_scores(winners=winners.CG.2022.f,
                               losers=losers.CG.2022.f,
                               randomise=TRUE,
                               sigmoid.param=1/300,
                               K=200,
                               n.rands=10000,
                               return.as.ranks = TRUE
)

scores.CG.2022.f <- as.data.frame(scores.CG.2022.f)

######## calculate ranks
for (i in 1:nrow(scores.CG.2022.f)) { 
  scores.CG.2022.f$Med.rank [i] <- rowMeans(scores.CG.2022.f)[i]
}

rank.CG.2022.f <- as.data.frame(scores.CG.2022.f$Med.rank)
rank.CG.2022.f$rank <- rank(rank.CG.2022.f, na.last = TRUE)
rank.CG.2022.f$ID <- rownames(scores.CG.2022.f)


rank.CG.2022.f$sexing <- gen_sexing$sexing[match(rank.CG.2022.f$ID,gen_sexing$ID)]

rank.CG.2022.f$sexing[is.na(rank.CG.2022.f$sexing)] <- as.character(vector$Sex[match(rank.CG.2022.f$ID[is.na(rank.CG.2022.f$sexing)], vector$Social_ID)])
rank.CG.2022.f$sexing[is.na(rank.CG.2022.f$sexing)] <- as.character(sexing$Assigned_Sex[match(rank.CG.2022.f$ID[is.na(rank.CG.2022.f$sexing)], sexing$ID_Site)])

rank.CG.2022.f$age <- as.character(sexing$Assigned_Age[match(rank.CG.2022.f$ID, sexing$ID_Site)])

plot_hierarchy_shape(identity=rank.CG.2022.f$ID,
                     rank=as.numeric(rank.CG.2022.f$rank), 
                     winners=winners.CG.2022.f, 
                     losers=losers.CG.2022.f, 
                     fitted=T
)
