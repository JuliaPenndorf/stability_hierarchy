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
library(DynaRankR)

# LOAD DATA
agg_2019 <- read.csv('soc_data_2019.csv',stringsAsFactors = F)
agg_2022 <- read.csv('agg_data_cleaned.csv',stringsAsFactors = F)
sexing_gen_2019 <-read.csv('Vector_IDs_15July.csv',stringsAsFactors = F)
sexing_eye_2019 <-read.csv('Marking_sheet_MASTER_corrected.csv',stringsAsFactors = F)

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

# correcting typos
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
agg_2019_sub <- agg_2019#[which(agg_2019$WINNER%in%ids$ID_2019 & agg_2019$LOSER%in%ids$ID_2019),]

agg_2022_sub <- agg_2022#[which(agg_2022$Winner %in%ids$ID_2022 & agg_2022$Loser %in%ids$ID_2022),]


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

agg_both <- as.data.frame(Reduce(intersect, list(inds_CG_2019_sub$inds_CG_2019,inds_CG_2022_sub$ID2019 )))
colnames(agg_both) <- "ID_2019"
agg_both$ID_2022 <- ids$ID_2022[match(agg_both$ID_2019,ids$ID_2019)]

#####  calculating rank 2019
#agg_CG_2019_sub <- agg_CG_2019[which(agg_CG_2019$WINNER%in%agg_both$ID_2019 & agg_CG_2019$LOSER%in%agg_both$ID_2019),]

elo_CG_2019 <- elo_scores(winners=agg_CG_2019$WINNER, 
                          losers=agg_CG_2019$LOSER, 
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
agg_CG_2022_sub <- agg_CG_2022#[which(agg_CG_2022$Winner %in% agg_both$ID_2022 & 
#                                      agg_CG_2022$Loser %in% agg_both$ID_2022),]
# agg_CG_2022_sub$Winner_ID2019 <- agg_both$ID_2019[match(agg_CG_2022_sub$Winner,
#                                                         agg_both$ID_2022)]
# agg_CG_2022_sub$Loser_ID2019 <- agg_both$ID_2019[match(agg_CG_2022_sub$Loser,
#                                                         agg_both$ID_2022)]

# elo_CG_2022 <- elo_scores(winners=agg_CG_2022_sub$Winner_ID2019, 
#                           losers=agg_CG_2022_sub$Loser_ID2019, 
#                           identities = NULL, 
#                           sigmoid.param = 1/300, 
#                           K = 200, 
#                           #init.score = mean_2019, 
#                           randomise = TRUE, 
#                           n.rands = 10000, 
#                           return.as.ranks = FALSE, 
#                           return.trajectories = FALSE, 
#                           dates = NULL)
elo_CG_2022 <- elo_scores(winners=agg_CG_2022_sub$Winner, 
                          losers=agg_CG_2022_sub$Loser, 
                          identities = NULL, 
                          sigmoid.param = 1/300, 
                          K = 200, 
                          #init.score = mean_2019, 
                          randomise = TRUE, 
                          n.rands = 10000, 
                          return.as.ranks = FALSE, 
                          return.trajectories = FALSE, 
                          dates = NULL)
mean_2022 <- rowMeans(as.matrix(elo_CG_2022))
names(mean_2022) <- rownames(elo_CG_2022)
ranks_CG_2022 <-rank(-mean_2022)
ranks_CG_2022<-as.data.frame(ranks_CG_2022)



#subsetting to individuals present in both years
ranks_CG_2019$ID <- rownames(ranks_CG_2019)
CG_2019_sub <- as.data.frame(ranks_CG_2019[which(rownames(ranks_CG_2019) %in% agg_both$ID_2019),])
colnames(CG_2019_sub)[1] <- "rank"

ranks_CG_2022$ID <- rownames(ranks_CG_2022)
CG_2022_sub <- as.data.frame(ranks_CG_2022[which(rownames(ranks_CG_2022) %in% agg_both$ID_2022),])
colnames(CG_2022_sub)[1] <- "rank"

CG_2019_sub$rerank <- rank(CG_2019_sub$rank)
CG_2022_sub$rerank <- rank(CG_2022_sub$rank)

sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="5"] <-"X5"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="11"] <-"X11"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="31"] <-"X31"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="35"] <-"X35"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="40"] <-"X40"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="42"] <-"X42"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="31"] <-"X31"
sexing_eye_2019$ID_Site[sexing_eye_2019$ID_Site=="78"] <-"X78"


CG_2019_sub$Year <- "2019"
CG_2019_sub$ID <- rownames(CG_2019_sub)
CG_2019_sub$Sex <- sexing_gen_2019$Sex[match(CG_2019_sub$ID,
                                               sexing_gen_2019$Social_ID)]
CG_2019_sub$Sex[is.na(CG_2019_sub$Sex)] <- sexing_eye_2019$Assigned_Sex[match(CG_2019_sub$ID[is.na(CG_2019_sub$Sex)],
                                                                               sexing_eye_2019$ID_Site )]
CG_2019_sub$Age <- sexing_gen_2019$Age[match(CG_2019_sub$ID,
                                               sexing_gen_2019$Social_ID)]
CG_2019_sub$Age[is.na(CG_2019_sub$Age)] <- sexing_eye_2019$Assigned_Age[match(CG_2019_sub$ID[is.na(CG_2019_sub$Age)],
                                                                                  sexing_eye_2019$ID_Site )]
colnames(CG_2019_sub)[1] <- "Rank"
CG_2019_sub$Sex[CG_2019_sub$ID=="BGN_V_BA"]<-"M"
CG_2019_sub$Age[CG_2019_sub$ID=="BGN_V_BA"]<-"A"

CG_2019_sub$Sex[CG_2019_sub$ID=="X31"]<-"F"
CG_2019_sub$Age[CG_2019_sub$ID=="X31"]<-"A"

CG_2019_sub$Sex[CG_2019_sub$ID=="VPT_V_CG"]<-"M"
CG_2019_sub$Age[CG_2019_sub$ID=="VPT_V_CG"]<-"A"

CG_2019_sub$Sex[CG_2019_sub$ID=="VBP_V_CG"]<-"M"
CG_2019_sub$Age[CG_2019_sub$ID=="VBP_V_CG"]<-"A"

CG_2019_sub$Sex[CG_2019_sub$ID=="X2"]<-"M"
CG_2019_sub$Age[CG_2019_sub$ID=="X2"]<-"A"

CG_2019_sub$Sex[CG_2019_sub$ID=="X39"]<-"M"
CG_2019_sub$Age[CG_2019_sub$ID=="X39"]<-"A"

CG_2022_sub$Year <- "2022"
CG_2022_sub$ID <- agg_both$ID_2019[match(rownames(CG_2022_sub),
                                         agg_both$ID_2022)]


CG_2022_sub$Sex <- sexing_gen_2019$Sex[match(CG_2022_sub$ID,
                                               sexing_gen_2019$Social_ID)]
CG_2022_sub$Sex[is.na(CG_2022_sub$Sex)] <- sexing_eye_2019$Assigned_Sex[match(CG_2022_sub$ID[is.na(CG_2022_sub$Sex)],
                                                                                  sexing_eye_2019$ID_Site )]

CG_2022_sub$Age <- sexing_gen_2019$Age[match(CG_2022_sub$ID,
                                               sexing_gen_2019$Social_ID)]
CG_2022_sub$Age[is.na(CG_2022_sub$Age)] <- sexing_eye_2019$Assigned_Age[match(CG_2022_sub$ID[is.na(CG_2022_sub$Age)],
                                                                                  sexing_eye_2019$ID_Site )]
colnames(CG_2022_sub)[1] <- "Rank"

CG_2022_sub$Sex[CG_2022_sub$ID=="BGN_V_BA"]<-"M"
CG_2022_sub$Age[CG_2022_sub$ID=="BGN_V_BA"]<-"A"

CG_2022_sub$Sex[CG_2022_sub$ID=="X31"]<-"F"
CG_2022_sub$Age[CG_2022_sub$ID=="X31"]<-"A"

CG_2022_sub$Sex[CG_2022_sub$ID=="VPT_V_CG"]<-"M"
CG_2022_sub$Age[CG_2022_sub$ID=="VPT_V_CG"]<-"A"

CG_2022_sub$Sex[CG_2022_sub$ID=="VBP_V_CG"]<-"M"
CG_2022_sub$Age[CG_2022_sub$ID=="VBP_V_CG"]<-"A"

CG_2022_sub$Sex[CG_2022_sub$ID=="X2"]<-"M"
CG_2022_sub$Age[CG_2022_sub$ID=="X2"]<-"A"

CG_2022_sub$Sex[CG_2022_sub$ID=="X39"]<-"M"
CG_2022_sub$Age[CG_2022_sub$ID=="X39"]<-"A"


similarity_years <- NA

elo_CG_2019_sub <- elo_CG_2019[which(rownames(elo_CG_2019) %in% agg_both$ID_2019),]
elo_CG_2022_sub <- elo_CG_2022[which(rownames(elo_CG_2022) %in% agg_both$ID_2022),]

x <- agg_both$ID_2019[match(rownames(elo_CG_2022_sub),agg_both$ID_2022)]
rownames(elo_CG_2022_sub) <- x

for (i in 1:ncol(elo_CG_2019)) {
  rank_CG_2019_ov <- as.data.frame(elo_CG_2019_sub[,i])
  rank_CG_2019_ov$id <- rownames(elo_CG_2019_sub)
  rank_CG_2019_ov$rank <- rank(-rank_CG_2019_ov[,1])
  rank_CG_2019 <- rank_CG_2019_ov %>% arrange(desc(-rank_CG_2019_ov$rank),decreasing=F)
  
  rank_CG_2022_ov <- as.data.frame(elo_CG_2022_sub[,i])
  rank_CG_2022_ov$id <- rownames(elo_CG_2022_sub)
  rank_CG_2022_ov$rank <- rank(-rank_CG_2022_ov[,1])
  rank_CG_2022 <- rank_CG_2022_ov %>% arrange(desc(-rank_CG_2022_ov$rank),decreasing=F)

  
  similarity_years[i] <- dyadic_similarity(rank_CG_2019$id, rank_CG_2022$id)
  
}

similarity_random <- NA
for (i in 1:10000) {
      random_2019<- sample(rank_CG_2019$id,size=nrow(rank_CG_2019),replace=F)
      random_2022 <- sample(rank_CG_2022$id,size=nrow(rank_CG_2022),replace=F)
      similarity_random[i] <- dyadic_similarity(random_2019, random_2022)
}

mean(similarity_years)
sd(similarity_years)

mean(similarity_random)
sd(similarity_random)


# subsetting to adult males 
CG_2019_males <- CG_2019_sub[which(CG_2019_sub$Age=="A" &
                                     CG_2019_sub$Sex=="M"),]
CG_2022_males <- CG_2022_sub[which(CG_2022_sub$Age=="A" &
                                     CG_2022_sub$Sex=="M"),]
similarity_years_males <- NA

elo_CG_2019_males <- elo_CG_2019[which(rownames(elo_CG_2019) %in% CG_2019_males$ID),]
elo_CG_2022_males <- elo_CG_2022[which(rownames(elo_CG_2022) %in% rownames(CG_2022_males)),]

x <- agg_both$ID_2019[match(rownames(elo_CG_2022_males),agg_both$ID_2022)]
rownames(elo_CG_2022_males) <- x

for (i in 1:ncol(elo_CG_2019_males)) {
  rank_CG_2019_ov <- as.data.frame(elo_CG_2019_males[,i])
  rank_CG_2019_ov$id <- rownames(elo_CG_2019_males)
  rank_CG_2019_ov$rank <- rank(-rank_CG_2019_ov[,1])
  rank_CG_2019 <- rank_CG_2019_ov %>% arrange(desc(-rank_CG_2019_ov$rank),decreasing=F)
  
  rank_CG_2022_ov <- as.data.frame(elo_CG_2022_males[,i])
  rank_CG_2022_ov$id <- rownames(elo_CG_2022_males)
  rank_CG_2022_ov$rank <- rank(-rank_CG_2022_ov[,1])
  rank_CG_2022 <- rank_CG_2022_ov %>% arrange(desc(-rank_CG_2022_ov$rank),decreasing=F)
  
  
  similarity_years_males[i] <- dyadic_similarity(rank_CG_2019$id, rank_CG_2022$id)
  
}

similarity_random_males <- NA
for (i in 1:10000) {
  random_2019<- sample(rank_CG_2019$id,size=nrow(rank_CG_2019),replace=F)
  random_2022 <- sample(rank_CG_2022$id,size=nrow(rank_CG_2022),replace=F)
  similarity_random_males[i] <- dyadic_similarity(random_2019, random_2022)
}

mean(similarity_years_males)
sd(similarity_years_males)

mean(similarity_random_males)
sd(similarity_random_males)



# subsetting to adult females 
CG_2019_females <- CG_2019_sub[which(CG_2019_sub$Age=="A" &
                                     CG_2019_sub$Sex=="F"),]
CG_2022_females <- CG_2022_sub[which(CG_2022_sub$Age=="A" &
                                     CG_2022_sub$Sex=="F"),]
similarity_years_females <- NA

elo_CG_2019_females <- elo_CG_2019[which(rownames(elo_CG_2019) %in% CG_2019_females$ID),]
elo_CG_2022_females <- elo_CG_2022[which(rownames(elo_CG_2022) %in% rownames(CG_2022_females)),]

x <- agg_both$ID_2019[match(rownames(elo_CG_2022_females),agg_both$ID_2022)]
rownames(elo_CG_2022_females) <- x

for (i in 1:ncol(elo_CG_2019_females)) {
  rank_CG_2019_ov <- as.data.frame(elo_CG_2019_females[,i])
  rank_CG_2019_ov$id <- rownames(elo_CG_2019_females)
  rank_CG_2019_ov$rank <- rank(-rank_CG_2019_ov[,1])
  rank_CG_2019 <- rank_CG_2019_ov %>% arrange(desc(-rank_CG_2019_ov$rank),decreasing=F)
  
  rank_CG_2022_ov <- as.data.frame(elo_CG_2022_females[,i])
  rank_CG_2022_ov$id <- rownames(elo_CG_2022_females)
  rank_CG_2022_ov$rank <- rank(-rank_CG_2022_ov[,1])
  rank_CG_2022 <- rank_CG_2022_ov %>% arrange(desc(-rank_CG_2022_ov$rank),decreasing=F)
  
  
  similarity_years_females[i] <- dyadic_similarity(rank_CG_2019$id, rank_CG_2022$id)
  
}

similarity_random_females <- NA
for (i in 1:10000) {
  random_2019<- sample(rank_CG_2019$id,size=nrow(rank_CG_2019),replace=F)
  random_2022 <- sample(rank_CG_2022$id,size=nrow(rank_CG_2022),replace=F)
  similarity_random_females[i] <- dyadic_similarity(random_2019, random_2022)
}

mean(similarity_years_females)
sd(similarity_years_females)

mean(similarity_random_females)
sd(similarity_random_females)



# subsetting to adults 
CG_2019_ad <- CG_2019_sub[which(CG_2019_sub$Age=="A"),]
CG_2022_ad <- CG_2022_sub[which(CG_2022_sub$Age=="A"),]
similarity_years_ad <- NA

elo_CG_2019_ad <- elo_CG_2019[which(rownames(elo_CG_2019) %in% CG_2019_ad$ID),]
elo_CG_2022_ad <- elo_CG_2022[which(rownames(elo_CG_2022) %in% rownames(CG_2022_ad)),]

x <- agg_both$ID_2019[match(rownames(elo_CG_2022_ad),agg_both$ID_2022)]
rownames(elo_CG_2022_ad) <- x

for (i in 1:ncol(elo_CG_2019_ad)) {
  rank_CG_2019_ov <- as.data.frame(elo_CG_2019_ad[,i])
  rank_CG_2019_ov$id <- rownames(elo_CG_2019_ad)
  rank_CG_2019_ov$rank <- rank(-rank_CG_2019_ov[,1])
  rank_CG_2019 <- rank_CG_2019_ov %>% arrange(desc(-rank_CG_2019_ov$rank),decreasing=F)
  
  rank_CG_2022_ov <- as.data.frame(elo_CG_2022_ad[,i])
  rank_CG_2022_ov$id <- rownames(elo_CG_2022_ad)
  rank_CG_2022_ov$rank <- rank(-rank_CG_2022_ov[,1])
  rank_CG_2022 <- rank_CG_2022_ov %>% arrange(desc(-rank_CG_2022_ov$rank),decreasing=F)
  
  
  similarity_years_ad[i] <- dyadic_similarity(rank_CG_2019$id, rank_CG_2022$id)
  
}

similarity_random_ad <- NA
for (i in 1:10000) {
  random_2019<- sample(rank_CG_2019$id,size=nrow(rank_CG_2019),replace=F)
  random_2022 <- sample(rank_CG_2022$id,size=nrow(rank_CG_2022),replace=F)
  similarity_random_ad[i] <- dyadic_similarity(random_2019, random_2022)
}

mean(similarity_years_ad)
sd(similarity_years_ad)

mean(similarity_random_ad)
sd(similarity_random_ad)
