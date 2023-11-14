##########################################################################################################################
# Calculating the dominance hierarchies of Sulphur-crested cockatoos at 3 study locations (CG, BA, NB)


##########################################################################################################################



#load packages
library (aniDom)
library(reshape2)
library(plyr)

#load data
agg <- read.csv('soc_data_2019.csv', stringsAsFactors = FALSE,header = T,row.names = 1)
agg$WINNER[agg$WINNER=="VPTsm_H_CG"] <- "VPT_H_CG" #correcting typo
agg$LOSER[agg$LOSER=="VPTsm_H_CG"] <- "VPT_H_CG" #correcting typo

sexing_gen_2019 <-read.csv('Vector_IDs_15July.csv',stringsAsFactors = F)
sexing_eye_2019 <-read.csv2('Marking_sheet_MASTER_corrected.csv',stringsAsFactors = F)

IDs <- as.data.frame(unique(c(agg$WINNER,agg$WINNER)))

# CG
CG_jul <- agg[which(agg$LOCATION=="CG" &
                    agg$DATE <20190720),]
CG_sept <- agg[which(agg$LOCATION=="CG" &
                      agg$DATE>20190919),]

### subsetting individuals with >= 10 interactions at the site 
inds_CG_jul <- c(CG_jul$WINNER,CG_jul$LOSER)
inds_CG_jul_summary <-as.data.frame(table(inds_CG_jul))

inds_CG_jul <- inds_CG_jul_summary[which(inds_CG_jul_summary$Freq>=10),]

inds_CG_sept <- c(CG_sept$WINNER,CG_sept$LOSER)
inds_CG_sept_summary <-as.data.frame(table(inds_CG_sept))

inds_CG_sept <- inds_CG_sept_summary[which(inds_CG_sept_summary$Freq>=10),]


### Subsetting to individuals with >=10 interactions in each period
inds_CG_both <- Reduce(intersect, list(inds_CG_jul$inds_CG_jul,
                                       inds_CG_sept$inds_CG_sept))
######## calculate scores for each period
CG_jul_sub <- CG_jul[which(CG_jul$WINNER %in% inds_CG_both &
                           CG_jul$LOSER %in% inds_CG_both),]
CG_winners_jul <- as.vector(CG_jul_sub$WINNER)
CG_losers_jul <- as.vector(CG_jul_sub$LOSER)

CG_scores_jul <- elo_scores(winners=CG_winners_jul,
                        losers=CG_losers_jul,
                        randomise=TRUE,
                        sigmoid.param=1/300,
                        K=200,
                        n.rands=10000,
                        return.as.ranks = TRUE
)



CG_scores_jul <- as.data.frame(CG_scores_jul)

######## calculate ranks
for (i in 1:nrow(CG_scores_jul)) { 
  CG_scores_jul$Med.rank [i] <- rowMeans(CG_scores_jul)[i]
}

CG_rank_jul <- as.data.frame(CG_scores_jul$Med.rank)
CG_rank_jul$rank <- rank(CG_rank_jul, na.last = TRUE)
CG_rank_jul$ID <- rownames(CG_scores_jul)
CG_rank_jul$period <- "July"
CG_rank_jul$site <- "CG"
colnames(CG_rank_jul)[1] <- "meanRank"

CG_sept_sub <- CG_sept[which(CG_sept$WINNER %in% inds_CG_both&
                           CG_sept$LOSER %in% inds_CG_both),]
CG_winners_sept <- as.vector(CG_sept_sub$WINNER)
CG_losers_sept <- as.vector(CG_sept_sub$LOSER)

CG_scores_sept <- elo_scores(winners=CG_winners_sept,
                            losers=CG_losers_sept,
                            randomise=TRUE,
                            sigmoid.param=1/300,
                            K=200,
                            n.rands=10000,
                            return.as.ranks = TRUE
)

CG_scores_sept <- as.data.frame(CG_scores_sept)


######## calculate ranks
for (i in 1:nrow(CG_scores_sept)) { 
  CG_scores_sept$Med.rank [i] <- rowMeans(CG_scores_sept)[i]
}

CG_rank_sept <- as.data.frame(CG_scores_sept$Med.rank)
CG_rank_sept$rank <- rank(CG_rank_sept, na.last = TRUE)
CG_rank_sept$ID <- rownames(CG_scores_sept)
CG_rank_sept$period <- "September"
CG_rank_sept$site <- "CG"
colnames(CG_rank_sept)[1] <- "meanRank"



# BA
BA_jul <- agg[which(agg$LOCATION=="BA" &
                      agg$DATE <20190720),]
BA_sept <- agg[which(agg$LOCATION=="BA" &
                       agg$DATE>20190919),]

### subsetting individuals with >= 10 interactions at the site 
inds_BA_jul <- c(BA_jul$WINNER,BA_jul$LOSER)
inds_BA_jul_summary <-as.data.frame(table(inds_BA_jul))

inds_BA_jul <- inds_BA_jul_summary[which(inds_BA_jul_summary$Freq>=10),]

inds_BA_sept <- c(BA_sept$WINNER,BA_sept$LOSER)
inds_BA_sept_summary <-as.data.frame(table(inds_BA_sept))

inds_BA_sept <- inds_BA_sept_summary[which(inds_BA_sept_summary$Freq>=10),]


### Subsetting to individuals with >=10 interactions in each period
inds_BA_both <- Reduce(intersect, list(inds_BA_jul$inds_BA_jul,
                                       inds_BA_sept$inds_BA_sept))
######## calculate scores for each period
BA_jul_sub <- BA_jul[which(BA_jul$WINNER %in% inds_BA_both &
                           BA_jul$LOSER %in% inds_BA_both),]
BA_winners_jul <- as.vector(BA_jul_sub$WINNER)
BA_losers_jul <- as.vector(BA_jul_sub$LOSER)

BA_scores_jul <- elo_scores(winners=BA_winners_jul,
                            losers=BA_losers_jul,
                            randomise=TRUE,
                            sigmoid.param=1/300,
                            K=200,
                            n.rands=10000,
                            return.as.ranks = TRUE
)



BA_scores_jul <- as.data.frame(BA_scores_jul)

######## calculate ranks
for (i in 1:nrow(BA_scores_jul)) { 
  BA_scores_jul$Med.rank [i] <- rowMeans(BA_scores_jul)[i]
}

BA_rank_jul <- as.data.frame(BA_scores_jul$Med.rank)
BA_rank_jul$rank <- rank(BA_rank_jul, na.last = TRUE)
BA_rank_jul$ID <- rownames(BA_scores_jul)
BA_rank_jul$period <- "July"
BA_rank_jul$site <- "BA"
colnames(BA_rank_jul)[1] <- "meanRank"

BA_sept_sub <- BA_sept[which(BA_sept$WINNER %in% inds_BA_both &
                             BA_sept$LOSER %in% inds_BA_both),]
BA_winners_sept <- as.vector(BA_sept_sub$WINNER)
BA_losers_sept <- as.vector(BA_sept_sub$LOSER)

BA_scores_sept <- elo_scores(winners=BA_winners_sept,
                             losers=BA_losers_sept,
                             randomise=TRUE,
                             sigmoid.param=1/300,
                             K=200,
                             n.rands=10000,
                             return.as.ranks = TRUE
)

BA_scores_sept <- as.data.frame(BA_scores_sept)


######## calculate ranks
for (i in 1:nrow(BA_scores_sept)) { 
  BA_scores_sept$Med.rank [i] <- rowMeans(BA_scores_sept)[i]
}

BA_rank_sept <- as.data.frame(BA_scores_sept$Med.rank)
BA_rank_sept$rank <- rank(BA_rank_sept, na.last = TRUE)
BA_rank_sept$ID <- rownames(BA_scores_sept)
BA_rank_sept$period <- "September"
BA_rank_sept$site <- "BA"
colnames(BA_rank_sept)[1] <- "meanRank"




# NB
NB_jul <- agg[which(agg$LOCATION=="NB" &
                      agg$DATE <20190720),]
NB_sept <- agg[which(agg$LOCATION=="NB" &
                       agg$DATE>20190919),]

### subsetting individuals with >= 10 interactions at the site 
inds_NB_jul <- c(NB_jul$WINNER,NB_jul$LOSER)
inds_NB_jul_summary <-as.data.frame(table(inds_NB_jul))

inds_NB_jul <- inds_NB_jul_summary[which(inds_NB_jul_summary$Freq>=10),]

inds_NB_sept <- c(NB_sept$WINNER,NB_sept$LOSER)
inds_NB_sept_summary <-as.data.frame(table(inds_NB_sept))

inds_NB_sept <- inds_NB_sept_summary[which(inds_NB_sept_summary$Freq>=10),]


### Subsetting to individuals with >=10 interactions in each period
inds_NB_both <- Reduce(intersect, list(inds_NB_jul$inds_NB_jul,
                                       inds_NB_sept$inds_NB_sept))
######## calculate scores for each period
NB_jul_sub <- NB_jul[which(NB_jul$WINNER %in% inds_NB_both &
                           NB_jul$LOSER %in% inds_NB_both),]
NB_winners_jul <- as.vector(NB_jul_sub$WINNER)
NB_losers_jul <- as.vector(NB_jul_sub$LOSER)

NB_scores_jul <- elo_scores(winners=NB_winners_jul,
                            losers=NB_losers_jul,
                            randomise=TRUE,
                            sigmoid.param=1/300,
                            K=200,
                            n.rands=10000,
                            return.as.ranks = TRUE
)



NB_scores_jul <- as.data.frame(NB_scores_jul)

######## calculate ranks
for (i in 1:nrow(NB_scores_jul)) { 
  NB_scores_jul$Med.rank [i] <- rowMeans(NB_scores_jul)[i]
}

NB_rank_jul <- as.data.frame(NB_scores_jul$Med.rank)
NB_rank_jul$rank <- rank(NB_rank_jul, na.last = TRUE)
NB_rank_jul$ID <- rownames(NB_scores_jul)
NB_rank_jul$period <- "July"
NB_rank_jul$site <- "NB"
colnames(NB_rank_jul)[1] <- "meanRank"

NB_sept_sub <- NB_sept[which(NB_sept$WINNER %in% inds_NB_both &
                             NB_sept$LOSER %in% inds_NB_both),]
NB_winners_sept <- as.vector(NB_sept_sub$WINNER)
NB_losers_sept <- as.vector(NB_sept_sub$LOSER)

NB_scores_sept <- elo_scores(winners=NB_winners_sept,
                             losers=NB_losers_sept,
                             randomise=TRUE,
                             sigmoid.param=1/300,
                             K=200,
                             n.rands=10000,
                             return.as.ranks = TRUE
)

NB_scores_sept <- as.data.frame(NB_scores_sept)


######## calculate ranks
for (i in 1:nrow(NB_scores_sept)) { 
  NB_scores_sept$Med.rank [i] <- rowMeans(NB_scores_sept)[i]
}

NB_rank_sept <- as.data.frame(NB_scores_sept$Med.rank)
NB_rank_sept$rank <- rank(NB_rank_sept, na.last = TRUE)
NB_rank_sept$ID <- rownames(NB_scores_sept)
NB_rank_sept$period <- "September"
NB_rank_sept$site <- "NB"
colnames(NB_rank_sept)[1] <- "meanRank"

ranks_complete <- rbind(CG_rank_jul,
                        CG_rank_sept,
                        BA_rank_jul,
                        BA_rank_sept,
                        NB_rank_jul,
                        NB_rank_sept)

ranks_complete$sex <- sexing_gen_2019$Sex[match(ranks_complete$ID,sexing_gen_2019$Social_ID)]

sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="5"] <-"X5"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="11"] <-"X11"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="31"] <-"X31"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="35"] <-"X35"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="40"] <-"X40"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="42"] <-"X42"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="31"] <-"X31"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="78"] <-"X78"

ranks_complete$sex[is.na(ranks_complete$sex)] <- sexing_eye_2019$Assigned_Sex[match(ranks_complete$ID[is.na(ranks_complete$sex)],
                                                                                    sexing_eye_2019$ID_Site)]

ranks_complete$sex[ranks_complete$ID=="X71"] <- "F"
ranks_complete$sex[ranks_complete$ID=="X53"] <- "M"
ranks_complete$sex[ranks_complete$ID=="X31"] <- "F"
ranks_complete$sex[ranks_complete$ID=="X27"] <- "M"
ranks_complete$sex[ranks_complete$ID=="X23"] <- "M"
ranks_complete$sex[ranks_complete$ID=="X2"] <- "M"
ranks_complete$sex[ranks_complete$ID=="X15"] <- "F"
ranks_complete$sex[ranks_complete$ID=="X139"] <- "F"
ranks_complete$sex[ranks_complete$ID=="X124"] <- "M"
ranks_complete$sex[ranks_complete$ID=="X115"] <- "M"
ranks_complete$sex[ranks_complete$ID=="X108"] <- "M"
ranks_complete$sex[ranks_complete$ID=="X103"] <- "M"

ranks_complete$sex[ranks_complete$ID=="BGN_V_BA"]<-"M"
ranks_complete$sex[ranks_complete$ID=="VPT_V_CG"]<-"M"

Rpt_model <- rptGaussian(rank ~ period + site + sex +(1|ID), 
                         grname=c("ID"), 
                         data = ranks_complete, 
                         nboot=1000, 
                         npermut=0,
                         adjusted = TRUE)
print(Rpt_model)
summary(Rpt_model)
summary(Rpt_model$mod)


# repeatability CG
ranks_complete_CG <- ranks_complete[which(ranks_complete$site=="CG"),]
Rpt_model_CG <- rptGaussian(rank ~ period + sex+ (1|ID), 
                         grname=c("ID"), 
                         data = ranks_complete_CG, 
                         nboot=1000, 
                         npermut=0,
                         adjusted = TRUE)
print(Rpt_model_CG)
summary(Rpt_model_CG)
summary(Rpt_model_CG$mod)


# repeatability BA
ranks_complete_BA <- ranks_complete[which(ranks_complete$site=="BA"),]
Rpt_model_BA <- rptGaussian(rank ~ period + sex+  (1|ID), 
                            grname=c("ID"), 
                            data = ranks_complete_BA, 
                            nboot=1000, 
                            npermut=0,
                            adjusted = TRUE)
print(Rpt_model_BA)
summary(Rpt_model_BA)
summary(Rpt_model_BA$mod)


# repeatability NB
ranks_complete_NB <- ranks_complete[which(ranks_complete$site=="NB"),]
Rpt_model_NB <- rptGaussian(rank ~ period + sex+  (1|ID), 
                            grname=c("ID"), 
                            data = ranks_complete_NB, 
                            nboot=1000, 
                            npermut=0,
                            adjusted = TRUE)
print(Rpt_model_NB)
summary(Rpt_model_NB)
summary(Rpt_model_NB$mod)



# repeatability males
ranks_complete_M <- ranks_complete[which(ranks_complete$sex=="M"),]
Rpt_model_M <- rptGaussian(rank ~ period +  (1|ID), 
                            grname=c("ID"), 
                            data = ranks_complete_M, 
                            nboot=1000, 
                            npermut=0,
                            adjusted = TRUE)
print(Rpt_model_M)
summary(Rpt_model_M)
summary(Rpt_model_M$mod)

# repeatability females
ranks_complete_F <- ranks_complete[which(ranks_complete$sex=="F"),]
Rpt_model_F <- rptGaussian(rank ~ period +  (1|ID), 
                           grname=c("ID"), 
                           data = ranks_complete_F, 
                           nboot=1000, 
                           npermut=0,
                           adjusted = TRUE)
print(Rpt_model_M)
summary(Rpt_model_M)
summary(Rpt_model_M$mod)