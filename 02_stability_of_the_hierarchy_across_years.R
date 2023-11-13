################################################################################
###               STABILITY HIERARCHY OF SCC (2019-2022)                     ###


# LOAD PACKAGES
library(stringr)
library(reshape2)
library(aniDom)
require(plyr)
library(lme4)
library(tidyverse)
library(ggbump)
library(rptR)

# LOAD DATA
agg_2019 <- read.csv('soc_data_2019.csv',stringsAsFactors = F)
agg_2022 <- read.csv('agg_data_cleaned.csv',stringsAsFactors = F)
sexing_gen_2019 <-read.csv('Vector_IDs_15July.csv',stringsAsFactors = F)
sexing_eye_2019 <-read.csv2('Marking_sheet_MASTER_corrected.csv',stringsAsFactors = F)

relatedness <- read.csv('self_relatedness.csv',stringsAsFactors = F)
ids <- make.unique(relatedness$X)
rownames(relatedness) <- ids
relatedness <- relatedness[,2:121]

# re-identifying individuals across datasets
melted <- melt(as.matrix(relatedness))
match_id <- na.omit(melted)
match_id <- match_id[which(str_detect(match_id$Var1, "_2019") |
                           str_detect(match_id$Var1, "WT" )),]
match_id <- match_id[which(str_detect(match_id$Var2, "_2022") |
                           str_detect(match_id$Var2, "WT" )),]
colnames(match_id) <- c("ID_2019", "ID_2022","Value")
match_id$ID_2019 <- str_replace_all(match_id$ID_2019, "_2019", "")
match_id$ID_2019 <- str_replace_all(match_id$ID_2019, ".1", "")

match_id$ID_2022 <- str_replace_all(match_id$ID_2022, "_2022", "")

match_id$ID_2022 <- str_replace_all(match_id$ID_2022, ".1", "")

ids <- as.data.frame(match_id$ID_2022)

match_id$ID_2022[match_id$ID_2022=="BPN_H_CG"] <- "BPN_V_CG"
match_id$ID_2022[match_id$ID_2022=="PMmiddle_CG"] <- "PM_H_CG"
match_id$ID_2022[match_id$ID_2022=="BNwing_CG"] <- "BNw_CG"
match_id$ID_2022[match_id$ID_2022=="PVBsm_CG"] <- "PVB_H_CG"
match_id$ID_2022[match_id$ID_2022=="PVNBsm_CG"] <- "PVBN_H_CG"

match_id$ID_2019[match_id$ID_2019=="VBP_H_CG"] <- "VBP_V_CG"
match_id$ID_2019[match_id$ID_2019=="VPT_H_CG"] <- "VPT_V_CG"
match_id$ID_2019[match_id$ID_2019=="P_H_CG"] <- "bigP_H_CG"

# adding wingtagged individuals across years
wing_2019 <-c("X42","X78","X11","X35","X118","X93","X124","X5","X115","X15","X2","X77","X88",
              "X23","X136","X39","X107","X71","X40","X108","X27","X135","X31","X50","X53",
              "X103","X138","X139","X133")
wing_2022 <- c("X35","X11","X143","X51","X5","X78","X42","X2","X52","X4","X39","X1","X135",
               "X12","X31","X40","X53","X142","X85","X48","X136","X105","X108","X133")
wing_both <- Reduce(intersect, list(wing_2019,wing_2022))

wing_both_m <- as.data.frame(matrix(data = NA, nrow = 14, ncol = 3))
wing_both_m$V1 <- wing_both
wing_both_m$V2 <- wing_both
colnames(wing_both_m) <- c("ID_2019", "ID_2022","Value")

ids <- rbind(match_id[,1:3],wing_both_m)

ids$ID_2019 <- str_replace_all(ids$ID_2019, "_2019", "")
ids$ID_2022 <- str_replace_all(ids$ID_2022, "_2022", "")

ids$ID_2022[ids$ID_2019=="X133"]<- "TNP_H_CG"

#subset aggressive data to include only individuals included in both years
agg_2019_sub <- agg_2019[which(agg_2019$WINNER%in%ids$ID_2019 &
                               agg_2019$LOSER%in%ids$ID_2019),]

agg_2022_sub <- agg_2022[which(agg_2022$Winner %in%ids$ID_2022 &
                               agg_2022$Loser %in%ids$ID_2022),]


# subsetting to CG
### subsetting individuals with >= 5 interactions at the site in 2019
agg_CG_2019 <- agg_2019_sub[which(agg_2019_sub$LOCATION=="CG"),]

inds_CG_2019 <- c(agg_CG_2019$WINNER,agg_CG_2019$LOSER)
inds_CG_2019_summary <-as.data.frame(table(inds_CG_2019))

inds_CG_2019_sub <- inds_CG_2019_summary[which(inds_CG_2019_summary$Freq>=5),]

### subsetting individuals with >= 5 interactions at the site in 2022
agg_CG_2022 <- agg_2022_sub[which(agg_2022_sub$Location=="CG"),]

inds_CG_2022 <- c(agg_CG_2022$Winner,agg_CG_2022$Loser)
inds_CG_2022_summary <-as.data.frame(table(inds_CG_2022))

inds_CG_2022_sub <- inds_CG_2022_summary[which(inds_CG_2022_summary$Freq>=5),]
inds_CG_2022_sub$ID2019 <- ids$ID_2019[match(inds_CG_2022_sub$inds_CG_2022,
                                             ids$ID_2022)]

agg_both <- as.data.frame(Reduce(intersect, list(inds_CG_2019_sub$inds_CG_2019,
                                                 inds_CG_2022_sub$ID2019 )))
colnames(agg_both) <- "ID_2019"
agg_both$ID_2022 <- ids$ID_2022[match(agg_both$ID_2019,
                                      ids$ID_2019)]

#####  calculating rank 2019
agg_CG_2019_sub <- agg_CG_2019[which(agg_CG_2019$WINNER%in%agg_both$ID_2019 & 
                                     agg_CG_2019$LOSER%in%agg_both$ID_2019),]

elo_CG_2019 <- elo_scores(winners=agg_CG_2019_sub$WINNER, 
                          losers=agg_CG_2019_sub$LOSER, 
                          identities = NULL, 
                          sigmoid.param = 1/300, 
                          K = 200, 
                          init.score = 0, 
                          randomise = TRUE, 
                          n.rands = 10000, 
                          return.as.ranks = FALSE, 
                          return.trajectories = FALSE, 
                          dates = NULL)
mean_2019 <- rowMeans(as.matrix(elo_CG_2019))
names(mean_2019) <- rownames(elo_CG_2019)
ranks_CG_2019 <-rank(-mean_2019)
ranks_CG_2019 <- as.data.frame(ranks_CG_2019)

### 2022
#####  calculating rank 2022
agg_CG_2022_sub <- agg_CG_2022[which(agg_CG_2022$Winner %in% agg_both$ID_2022 & 
                                     agg_CG_2022$Loser %in% agg_both$ID_2022),]

elo_CG_2022 <- elo_scores(winners=agg_CG_2022_sub$Winner, 
                          losers=agg_CG_2022_sub$Loser, 
                          identities = NULL, 
                          sigmoid.param = 1/300, 
                          K = 200, 
                          init.score = 0, 
                          randomise = TRUE, 
                          n.rands = 10000, 
                          return.as.ranks = FALSE, 
                          return.trajectories = FALSE, 
                          dates = NULL)
mean_2022 <- rowMeans(as.matrix(elo_CG_2022))
names(mean_2022) <- rownames(elo_CG_2022)
ranks_CG_2022 <-rank(-mean_2022)
ranks_CG_2022<-as.data.frame(ranks_CG_2022)


# # replace ids depending on years$
# ranks_CG_2022$ID_2019 <- ids$ID_2019[match(rownames(ranks_CG_2022),
#                                            ids$ID_2022)]
# ranks_CG_2022$rank_2019 <- ranks_CG_2019$ranks_CG_2019[match(ranks_CG_2022$ID_2019,
#                                                       rownames(ranks_CG_2019))]
# cor.test(ranks_CG_2022$ranks_CG_2022,ranks_CG_2022$rank_2019)
# 
# lmm <- lm(ranks_CG_2022$ranks_CG_2022~ ranks_CG_2022$rank_2019)
# summary(lmm)
# plot(ranks_CG_2022$ranks_CG_2022~ ranks_CG_2022$rank_2019,
#      xlab="Rank 2019",
#      ylab="Rank 2022")
# abline(lmm, col="red")

sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="5"] <-"X5"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="11"] <-"X11"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="31"] <-"X31"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="35"] <-"X35"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="40"] <-"X40"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="42"] <-"X42"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="31"] <-"X31"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="78"] <-"X78"
# ranks_CG_2022$sex <- sexing_eye_2019$Assigned_Sex[match(ranks_CG_2022$ID_2019,
#                                                         sexing_eye_2019$ID_Site)]
# ranks_CG_2022$sex[ranks_CG_2022$ID_2019=="BGN_V_BA"] <- "F"
# ranks_CG_2022$sex[ranks_CG_2022$ID_2019=="X31"] <- "F"
# 
# ## males 
# ranks_CG_males <- ranks_CG_2022[which(ranks_CG_2022$sex=="M"),]
# lmm_m <- lm(ranks_CG_males$ranks_CG_2022~ ranks_CG_males$rank_2019)
# summary(lmm_m)
# 
# ## females 
# ranks_CG_females <- ranks_CG_2022[which(ranks_CG_2022$sex=="F"),]
# lmm_f <- lm(ranks_CG_females$ranks_CG_2022~ ranks_CG_females$rank_2019)
# summary(lmm_f)

ranks_CG_2019$Year <- "2019"
ranks_CG_2019$ID <- rownames(ranks_CG_2019)
ranks_CG_2019$Sex <- sexing_gen_2019$Sex[match(ranks_CG_2019$ID,
                                               sexing_gen_2019$Social_ID)]
ranks_CG_2019$Sex[is.na(ranks_CG_2019$Sex)] <- sexing_eye_2019$Assigned_Sex[match(ranks_CG_2019$ID[is.na(ranks_CG_2019$Sex)],
                                                                                  sexing_eye_2019$ID_Site )]
colnames(ranks_CG_2019)[1] <- "Rank"
ranks_CG_2019$Sex[ranks_CG_2019$ID=="X31"]<-"F"
ranks_CG_2019$Sex[ranks_CG_2019$ID=="BGN_V_BA"]<-"M"
ranks_CG_2019$Sex[ranks_CG_2019$ID=="VPT_V_CG"]<-"M"
ranks_CG_2019$Sex[ranks_CG_2019$ID=="VBP_V_CG"]<-"M"

ranks_CG_2022$Year <- "2022"
ranks_CG_2022$ID <- ids$ID_2019[match(rownames(ranks_CG_2022),ids$ID_2022)]
ranks_CG_2022$ID <- ids$ID_2019[match(rownames(ranks_CG_2022),ids$ID_2022)]


ranks_CG_2022$Sex <- sexing_gen_2019$Sex[match(ranks_CG_2022$ID,
                                               sexing_gen_2019$Social_ID)]
ranks_CG_2022$Sex[is.na(ranks_CG_2022$Sex)] <- sexing_eye_2019$Assigned_Sex[match(ranks_CG_2022$ID[is.na(ranks_CG_2022$Sex)],
                                                                                  sexing_eye_2019$ID_Site )]
ranks_CG_2022$Sex[ranks_CG_2022$ID=="BGN_V_BA"]<-"M"
ranks_CG_2022$Sex[ranks_CG_2022$ID=="X31"]<-"F"
ranks_CG_2022$Sex[ranks_CG_2022$ID=="VPT_V_CG"]<-"M"
ranks_CG_2022$Sex[ranks_CG_2022$ID=="VBP_V_CG"]<-"M"
colnames(ranks_CG_2022)[1] <- "Rank"

ranks_CG_2022_sub <- ranks_CG_2022[,which(colnames(ranks_CG_2022)%in%colnames(ranks_CG_2019))]
DominanceScore<-rbind(ranks_CG_2019,ranks_CG_2022_sub)

Rpt_model <- rptGaussian(Rank ~ Year + Sex + (1|ID), 
                         grname=c("ID"), 
                         data = DominanceScore, 
                         nboot=1000, 
                         npermut=0,
                         adjusted = TRUE)
print(Rpt_model)
summary(Rpt_model)
summary(Rpt_model$mod)

DominanceScore_M <- DominanceScore[which(DominanceScore$Sex=="M"),]
Rpt_model_m <- rptGaussian(Rank ~ Year + (1|ID), 
                         grname=c("ID"), 
                         data = DominanceScore_M, 
                         nboot=1000, 
                         npermut=0,
                         adjusted = TRUE)
print(Rpt_model_m)
summary(Rpt_model_m)
summary(Rpt_model_m$mod)

DominanceScore_F <- DominanceScore[which(DominanceScore$Sex=="F"),]
Rpt_model_f <- rptGaussian(Rank ~ Year + (1|ID), 
                           grname=c("ID"), 
                           data = DominanceScore_F, 
                           nboot=1000, 
                           npermut=0,
                           adjusted = TRUE)
print(Rpt_model_f)
summary(Rpt_model_f)
summary(Rpt_model_f$mod)
