##########################################################################################################################
# Calculating the dominance hierarchies of Sulphur-crested cockatoos at 3 study locations (CG, BA, NB)


##########################################################################################################################



#load packages
library(aniDom)
library(reshape2)
library(plyr)
library(dplyr)
library(DynaRankR)

#load data
agg <- read.csv('soc_data_2019.csv', stringsAsFactors = FALSE,header = T,row.names = 1)
agg$WINNER[agg$WINNER=="VPTsm_H_CG"] <- "VPT_H_CG" #correcting typo
agg$LOSER[agg$LOSER=="VPTsm_H_CG"] <- "VPT_H_CG" #correcting typo

sexing_gen_2019 <-read.csv('Vector_IDs_15July.csv',stringsAsFactors = F)
sexing_eye_2019 <-read.csv('Marking_sheet_MASTER_corrected.csv',stringsAsFactors = F)

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
                        return.as.ranks = F
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
init_scores_CG_for_sept <- CG_scores_jul$Med.rank
names(init_scores_CG_for_sept) <- rownames(CG_scores_jul)

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
                            init.score = init_scores_CG_for_sept,
                            return.as.ranks = F
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


similarity_CG <- NA
dynamics_CG <-list()

for (i in 1:ncol(CG_scores_jul)) {
  # DYNAMIC SIMILARITY
      rank_CG_jul_ov <- as.data.frame(CG_scores_jul[,i])
      rank_CG_jul_ov$id <- rownames(CG_scores_jul)
      rank_CG_jul_ov$rank <- rank(-rank_CG_jul_ov[,1])
      rank_CG_jul <- rank_CG_jul_ov %>% arrange(desc(-rank_CG_jul_ov$rank),decreasing=F)
  
      rank_CG_sept_ov <- as.data.frame(CG_scores_sept[,i])
      rank_CG_sept_ov$id <- rownames(CG_scores_sept)
      rank_CG_sept_ov$rank <- rank(-rank_CG_sept_ov[,1])
      rank_CG_sept <- rank_CG_sept_ov %>% arrange(desc(-rank_CG_sept_ov$rank),decreasing=F)
      
      similarity_CG[i] <- dyadic_similarity(rank_CG_jul$id, rank_CG_sept$id)
      
  # HIERACHY DYNAMICS
      # period_1 <- rep(2019,times=nrow(CG_scores_jul))
      # period_2 <- rep(2020,times=nrow(CG_scores_sept))
      # period <- c(period_1,period_2)
      # jul_df <- CG_scores_jul[order(CG_scores_jul[,i],decreasing = T),]
      # sept_df <- CG_scores_jul[order(CG_scores_jul[,i],decreasing = T),]
      # jul <- rank(-jul_df[,i])
      # sept <- rank(-sept_df[,i])
      # ids <- c(rownames(jul_df),rownames(sept_df))
      # scores <- c(jul,sept)
      # df <- as.data.frame(cbind(period,ids,scores))
      # colnames(df) <- c("period","id","rank")
      # df$rank <- as.numeric(df$rank)
      # dynamics_CG[[i]] <- get_dynamics(ranks = df, 
      #                                  type = 'rank')

}


similarityCG_random <- NA
for (i in 1:10000) {
  random_CG_jul<- sample(rank_CG_jul$id,size=nrow(rank_CG_jul),replace=F)
  random_CG_sept <- sample(rank_CG_sept$id,size=nrow(rank_CG_sept),replace=F)
  similarityCG_random[i] <- dyadic_similarity(random_CG_jul, random_CG_sept)
}


# CALCULATE SIMILARITY WITHIN AGE-GROUPS 
adults <- c(unique(sexing_gen_2019$Social_ID[which(sexing_gen_2019$Age=="A")],
                   sexing_eye_2019$ID_Site[which(sexing_eye_2019$Assigned_Age=="A")]))
juveniles <- c(unique(sexing_gen_2019$Social_ID[which(sexing_gen_2019$Age=="J")],
                   sexing_eye_2019$ID_Site[which(sexing_eye_2019$Assigned_Age=="J")]))

males <- c(unique(sexing_gen_2019$Social_ID[which(sexing_gen_2019$Sex=="M")],
                      sexing_eye_2019$ID_Site[which(sexing_eye_2019$Assigned_Sex=="M")]))
females <- c(unique(sexing_gen_2019$Social_ID[which(sexing_gen_2019$Sex=="F")],
                  sexing_eye_2019$ID_Site[which(sexing_eye_2019$Assigned_Sex=="F")]))


## ADULTS
similarity_CG_ad <- NA
#dynamics_CG_ad <-list()

elo_CG_jul_adults <- CG_scores_jul[which(rownames(CG_scores_jul) %in% adults),1:10000]
elo_CG_2022_adults <- CG_scores_sept[which(rownames(CG_scores_sept) %in% adults),1:10000]



for (i in 1:ncol(elo_CG_jul_adults)) {
  # DYNAMIC SIMILARITY
  rank_CG_jul_ov <- as.data.frame(elo_CG_jul_adults[,i])
  rank_CG_jul_ov$id <- rownames(elo_CG_jul_adults)
  rank_CG_jul_ov$rank <- rank(-rank_CG_jul_ov[,1])
  rank_CG_jul <- rank_CG_jul_ov %>% arrange(desc(-rank_CG_jul_ov$rank),decreasing=F)
  
  rank_CG_sept_ov <- as.data.frame(elo_CG_2022_adults[,i])
  rank_CG_sept_ov$id <- rownames(elo_CG_2022_adults)
  rank_CG_sept_ov$rank <- rank(-rank_CG_sept_ov[,1])
  rank_CG_sept <- rank_CG_sept_ov %>% arrange(desc(-rank_CG_sept_ov$rank),decreasing=F)
  
  similarity_CG_ad[i] <- dyadic_similarity(rank_CG_jul$id, rank_CG_sept$id)
}


similarityCG_adults_random <- NA
for (i in 1:10000) {
  random_CG_jul<- sample(rank_CG_jul$id,size=nrow(rank_CG_jul),replace=F)
  random_CG_sept <- sample(rank_CG_sept$id,size=nrow(rank_CG_sept),replace=F)
  similarityCG_adults_random[i] <- dyadic_similarity(random_CG_jul, random_CG_sept)
}

mean(similarity_CG_ad)
sd(similarity_CG_ad)

mean(similarityCG_adults_random)
sd(similarityCG_adults_random)

## JUVENILES
similarity_CG_juv <- NA
#dynamics_CG_ad <-list()

elo_CG_jul_juv <- CG_scores_jul[which(rownames(CG_scores_jul) %in% juveniles),1:10000]
elo_CG_sept_juv <- CG_scores_sept[which(rownames(CG_scores_sept) %in% juveniles),1:10000]



for (i in 1:ncol(elo_CG_jul_juv)) {
  # DYNAMIC SIMILARITY
  rank_CG_jul_ov <- as.data.frame(elo_CG_jul_juv[,i])
  rank_CG_jul_ov$id <- rownames(elo_CG_jul_juv)
  rank_CG_jul_ov$rank <- rank(-rank_CG_jul_ov[,1])
  rank_CG_jul <- rank_CG_jul_ov %>% arrange(desc(-rank_CG_jul_ov$rank),decreasing=F)
  
  rank_CG_sept_ov <- as.data.frame(elo_CG_sept_juv[,i])
  rank_CG_sept_ov$id <- rownames(elo_CG_sept_juv)
  rank_CG_sept_ov$rank <- rank(-rank_CG_sept_ov[,1])
  rank_CG_sept <- rank_CG_sept_ov %>% arrange(desc(-rank_CG_sept_ov$rank),decreasing=F)
  
  similarity_CG_juv[i] <- dyadic_similarity(rank_CG_jul$id, rank_CG_sept$id)
}


similarityCG_juv_random <- NA
for (i in 1:10000) {
  random_CG_jul<- sample(rank_CG_jul$id,size=nrow(rank_CG_jul),replace=F)
  random_CG_sept <- sample(rank_CG_sept$id,size=nrow(rank_CG_sept),replace=F)
  similarityCG_juv_random[i] <- dyadic_similarity(random_CG_jul, random_CG_sept)
}

mean(similarity_CG_juv)
sd(similarity_CG_juv)

mean(similarityCG_juv_random)
sd(similarityCG_juv_random)


## Males
similarity_CG_males <- NA
#dynamics_CG_ad <-list()

elo_CG_jul_males <- CG_scores_jul[which(rownames(CG_scores_jul) %in% males),1:10000]
elo_CG_sept_males <- CG_scores_sept[which(rownames(CG_scores_sept) %in% males),1:10000]



for (i in 1:ncol(elo_CG_jul_males)) {
  # DYNAMIC SIMILARITY
  rank_CG_jul_ov <- as.data.frame(elo_CG_jul_males[,i])
  rank_CG_jul_ov$id <- rownames(elo_CG_jul_males)
  rank_CG_jul_ov$rank <- rank(-rank_CG_jul_ov[,1])
  rank_CG_jul <- rank_CG_jul_ov %>% arrange(desc(-rank_CG_jul_ov$rank),decreasing=F)
  
  rank_CG_sept_ov <- as.data.frame(elo_CG_sept_males[,i])
  rank_CG_sept_ov$id <- rownames(elo_CG_sept_males)
  rank_CG_sept_ov$rank <- rank(-rank_CG_sept_ov[,1])
  rank_CG_sept <- rank_CG_sept_ov %>% arrange(desc(-rank_CG_sept_ov$rank),decreasing=F)
  
  similarity_CG_males[i] <- dyadic_similarity(rank_CG_jul$id, rank_CG_sept$id)
}


similarityCG_males_random <- NA
for (i in 1:10000) {
  random_CG_jul<- sample(rank_CG_jul$id,size=nrow(rank_CG_jul),replace=F)
  random_CG_sept <- sample(rank_CG_sept$id,size=nrow(rank_CG_sept),replace=F)
  similarityCG_males_random[i] <- dyadic_similarity(random_CG_jul, random_CG_sept)
}

mean(similarity_CG_males)
sd(similarity_CG_males)

mean(similarityCG_males_random)
sd(similarityCG_males_random)


## females
similarity_CG_females <- NA
#dynamics_CG_ad <-list()

elo_CG_jul_females <- CG_scores_jul[which(rownames(CG_scores_jul) %in% females),1:10000]
elo_CG_sept_females <- CG_scores_sept[which(rownames(CG_scores_sept) %in% females),1:10000]



for (i in 1:ncol(elo_CG_jul_females)) {
  # DYNAMIC SIMILARITY
  rank_CG_jul_ov <- as.data.frame(elo_CG_jul_females[,i])
  rank_CG_jul_ov$id <- rownames(elo_CG_jul_females)
  rank_CG_jul_ov$rank <- rank(-rank_CG_jul_ov[,1])
  rank_CG_jul <- rank_CG_jul_ov %>% arrange(desc(-rank_CG_jul_ov$rank),decreasing=F)
  
  rank_CG_sept_ov <- as.data.frame(elo_CG_sept_females[,i])
  rank_CG_sept_ov$id <- rownames(elo_CG_sept_females)
  rank_CG_sept_ov$rank <- rank(-rank_CG_sept_ov[,1])
  rank_CG_sept <- rank_CG_sept_ov %>% arrange(desc(-rank_CG_sept_ov$rank),decreasing=F)
  
  similarity_CG_females[i] <- dyadic_similarity(rank_CG_jul$id, rank_CG_sept$id)
}


similarityCG_females_random <- NA
for (i in 1:10000) {
  random_CG_jul<- sample(rank_CG_jul$id,size=nrow(rank_CG_jul),replace=F)
  random_CG_sept <- sample(rank_CG_sept$id,size=nrow(rank_CG_sept),replace=F)
  similarityCG_females_random[i] <- dyadic_similarity(random_CG_jul, random_CG_sept)
}

mean(similarity_CG_females)
sd(similarity_CG_females)

mean(similarityCG_females_random)
sd(similarityCG_females_random)




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
                            return.as.ranks = F
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
init_scores_BA_for_sept <- BA_scores_jul$Med.rank
names(init_scores_BA_for_sept) <- rownames(BA_scores_jul)


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
                             init.score = init_scores_BA_for_sept,
                             return.as.ranks = F
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


similarity_BA <- NA

for (i in 1:ncol(BA_scores_jul)) {
  rank_BA_jul_ov <- as.data.frame(BA_scores_jul[,i])
  rank_BA_jul_ov$id <- rownames(BA_scores_jul)
  rank_BA_jul_ov$rank <- rank(-rank_BA_jul_ov[,1])
  rank_BA_jul <- rank_BA_jul_ov %>% arrange(desc(-rank_BA_jul_ov$rank),decreasing=F)
  
  rank_BA_sept_ov <- as.data.frame(BA_scores_sept[,i])
  rank_BA_sept_ov$id <- rownames(BA_scores_sept)
  rank_BA_sept_ov$rank <- rank(-rank_BA_sept_ov[,1])
  rank_BA_sept <- rank_BA_sept_ov %>% arrange(desc(-rank_BA_sept_ov$rank),decreasing=F)
  
  similarity_BA[i] <- dyadic_similarity(rank_BA_jul$id, rank_BA_sept$id)
  
}

similarityBA_random <- NA
for (i in 1:10000) {
  random_BA_jul<- sample(rank_BA_jul$id,size=nrow(rank_BA_jul),replace=F)
  random_BA_sept <- sample(rank_BA_sept$id,size=nrow(rank_BA_sept),replace=F)
  similarityBA_random[i] <- dyadic_similarity(random_BA_jul, random_BA_sept)
}



## ADULTS
similarity_BA_ad <- NA
#dynamics_BA_ad <-list()

elo_BA_jul_adults <- BA_scores_jul[which(rownames(BA_scores_jul) %in% adults),1:10000]
elo_BA_2022_adults <- BA_scores_sept[which(rownames(BA_scores_sept) %in% adults),1:10000]



for (i in 1:ncol(elo_BA_jul_adults)) {
  # DYNAMIC SIMILARITY
  rank_BA_jul_ov <- as.data.frame(elo_BA_jul_adults[,i])
  rank_BA_jul_ov$id <- rownames(elo_BA_jul_adults)
  rank_BA_jul_ov$rank <- rank(-rank_BA_jul_ov[,1])
  rank_BA_jul <- rank_BA_jul_ov %>% arrange(desc(-rank_BA_jul_ov$rank),decreasing=F)
  
  rank_BA_sept_ov <- as.data.frame(elo_BA_2022_adults[,i])
  rank_BA_sept_ov$id <- rownames(elo_BA_2022_adults)
  rank_BA_sept_ov$rank <- rank(-rank_BA_sept_ov[,1])
  rank_BA_sept <- rank_BA_sept_ov %>% arrange(desc(-rank_BA_sept_ov$rank),decreasing=F)
  
  similarity_BA_ad[i] <- dyadic_similarity(rank_BA_jul$id, rank_BA_sept$id)
}


similarityBA_adults_random <- NA
for (i in 1:10000) {
  random_BA_jul<- sample(rank_BA_jul$id,size=nrow(rank_BA_jul),replace=F)
  random_BA_sept <- sample(rank_BA_sept$id,size=nrow(rank_BA_sept),replace=F)
  similarityBA_adults_random[i] <- dyadic_similarity(random_BA_jul, random_BA_sept)
}

mean(similarity_BA_ad)
sd(similarity_BA_ad)

mean(similarityBA_adults_random)
sd(similarityBA_adults_random)

## JUVENILES
similarity_BA_juv <- NA
#dynamics_BA_ad <-list()

elo_BA_jul_juv <- BA_scores_jul[which(rownames(BA_scores_jul) %in% juveniles),1:10000]
elo_BA_sept_juv <- BA_scores_sept[which(rownames(BA_scores_sept) %in% juveniles),1:10000]



for (i in 1:ncol(elo_BA_jul_juv)) {
  # DYNAMIC SIMILARITY
  rank_BA_jul_ov <- as.data.frame(elo_BA_jul_juv[,i])
  rank_BA_jul_ov$id <- rownames(elo_BA_jul_juv)
  rank_BA_jul_ov$rank <- rank(-rank_BA_jul_ov[,1])
  rank_BA_jul <- rank_BA_jul_ov %>% arrange(desc(-rank_BA_jul_ov$rank),decreasing=F)
  
  rank_BA_sept_ov <- as.data.frame(elo_BA_sept_juv[,i])
  rank_BA_sept_ov$id <- rownames(elo_BA_sept_juv)
  rank_BA_sept_ov$rank <- rank(-rank_BA_sept_ov[,1])
  rank_BA_sept <- rank_BA_sept_ov %>% arrange(desc(-rank_BA_sept_ov$rank),decreasing=F)
  
  similarity_BA_juv[i] <- dyadic_similarity(rank_BA_jul$id, rank_BA_sept$id)
}


similarityBA_juv_random <- NA
for (i in 1:10000) {
  random_BA_jul<- sample(rank_BA_jul$id,size=nrow(rank_BA_jul),replace=F)
  random_BA_sept <- sample(rank_BA_sept$id,size=nrow(rank_BA_sept),replace=F)
  similarityBA_juv_random[i] <- dyadic_similarity(random_BA_jul, random_BA_sept)
}

mean(similarity_BA_juv)
sd(similarity_BA_juv)

mean(similarityBA_juv_random)
sd(similarityBA_juv_random)


## Males
similarity_BA_males <- NA
#dynamics_BA_ad <-list()

elo_BA_jul_males <- BA_scores_jul[which(rownames(BA_scores_jul) %in% males),1:10000]
elo_BA_sept_males <- BA_scores_sept[which(rownames(BA_scores_sept) %in% males),1:10000]



for (i in 1:ncol(elo_BA_jul_males)) {
  # DYNAMIC SIMILARITY
  rank_BA_jul_ov <- as.data.frame(elo_BA_jul_males[,i])
  rank_BA_jul_ov$id <- rownames(elo_BA_jul_males)
  rank_BA_jul_ov$rank <- rank(-rank_BA_jul_ov[,1])
  rank_BA_jul <- rank_BA_jul_ov %>% arrange(desc(-rank_BA_jul_ov$rank),decreasing=F)
  
  rank_BA_sept_ov <- as.data.frame(elo_BA_sept_males[,i])
  rank_BA_sept_ov$id <- rownames(elo_BA_sept_males)
  rank_BA_sept_ov$rank <- rank(-rank_BA_sept_ov[,1])
  rank_BA_sept <- rank_BA_sept_ov %>% arrange(desc(-rank_BA_sept_ov$rank),decreasing=F)
  
  similarity_BA_males[i] <- dyadic_similarity(rank_BA_jul$id, rank_BA_sept$id)
}


similarityBA_males_random <- NA
for (i in 1:10000) {
  random_BA_jul<- sample(rank_BA_jul$id,size=nrow(rank_BA_jul),replace=F)
  random_BA_sept <- sample(rank_BA_sept$id,size=nrow(rank_BA_sept),replace=F)
  similarityBA_males_random[i] <- dyadic_similarity(random_BA_jul, random_BA_sept)
}

mean(similarity_BA_males)
sd(similarity_BA_males)

mean(similarityBA_males_random)
sd(similarityBA_males_random)


## females
similarity_BA_females <- NA
#dynamics_BA_ad <-list()

elo_BA_jul_females <- BA_scores_jul[which(rownames(BA_scores_jul) %in% females),1:10000]
elo_BA_sept_females <- BA_scores_sept[which(rownames(BA_scores_sept) %in% females),1:10000]



for (i in 1:ncol(elo_BA_jul_females)) {
  # DYNAMIC SIMILARITY
  rank_BA_jul_ov <- as.data.frame(elo_BA_jul_females[,i])
  rank_BA_jul_ov$id <- rownames(elo_BA_jul_females)
  rank_BA_jul_ov$rank <- rank(-rank_BA_jul_ov[,1])
  rank_BA_jul <- rank_BA_jul_ov %>% arrange(desc(-rank_BA_jul_ov$rank),decreasing=F)
  
  rank_BA_sept_ov <- as.data.frame(elo_BA_sept_females[,i])
  rank_BA_sept_ov$id <- rownames(elo_BA_sept_females)
  rank_BA_sept_ov$rank <- rank(-rank_BA_sept_ov[,1])
  rank_BA_sept <- rank_BA_sept_ov %>% arrange(desc(-rank_BA_sept_ov$rank),decreasing=F)
  
  similarity_BA_females[i] <- dyadic_similarity(rank_BA_jul$id, rank_BA_sept$id)
}


similarityBA_females_random <- NA
for (i in 1:10000) {
  random_BA_jul<- sample(rank_BA_jul$id,size=nrow(rank_BA_jul),replace=F)
  random_BA_sept <- sample(rank_BA_sept$id,size=nrow(rank_BA_sept),replace=F)
  similarityBA_females_random[i] <- dyadic_similarity(random_BA_jul, random_BA_sept)
}

mean(similarity_BA_females)
sd(similarity_BA_females)

mean(similarityBA_females_random)
sd(similarityBA_females_random)




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
                            return.as.ranks = F
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
init_scores_NB_for_sept <- NB_scores_jul$Med.rank
names(init_scores_NB_for_sept) <- rownames(NB_scores_jul)

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
                             init.score = init_scores_NB_for_sept,
                             return.as.ranks = F
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

similarity_NB <- NA

for (i in 1:ncol(NB_scores_jul)) {
  rank_NB_jul_ov <- as.data.frame(NB_scores_jul[,i])
  rank_NB_jul_ov$id <- rownames(NB_scores_jul)
  rank_NB_jul_ov$rank <- rank(-rank_NB_jul_ov[,1])
  rank_NB_jul <- rank_NB_jul_ov %>% arrange(desc(-rank_NB_jul_ov$rank),decreasing=F)
  
  rank_NB_sept_ov <- as.data.frame(NB_scores_sept[,i])
  rank_NB_sept_ov$id <- rownames(NB_scores_sept)
  rank_NB_sept_ov$rank <- rank(-rank_NB_sept_ov[,1])
  rank_NB_sept <- rank_NB_sept_ov %>% arrange(desc(-rank_NB_sept_ov$rank),decreasing=F)
  
  similarity_NB[i] <- dyadic_similarity(rank_NB_jul$id, rank_NB_sept$id)
  
}
similarityNB_random <- NA
for (i in 1:10000) {
  random_NB_jul<- sample(rank_NB_jul$id,size=nrow(rank_NB_jul),replace=F)
  random_NB_sept <- sample(rank_NB_sept$id,size=nrow(rank_NB_sept),replace=F)
  similarityNB_random[i] <- dyadic_similarity(random_NB_jul, random_NB_sept)
}


## ADULTS
similarity_NB_ad <- NA
#dynamics_NB_ad <-list()

elo_NB_jul_adults <- NB_scores_jul[which(rownames(NB_scores_jul) %in% adults),1:10000]
elo_NB_2022_adults <- NB_scores_sept[which(rownames(NB_scores_sept) %in% adults),1:10000]



for (i in 1:ncol(elo_NB_jul_adults)) {
  # DYNAMIC SIMILARITY
  rank_NB_jul_ov <- as.data.frame(elo_NB_jul_adults[,i])
  rank_NB_jul_ov$id <- rownames(elo_NB_jul_adults)
  rank_NB_jul_ov$rank <- rank(-rank_NB_jul_ov[,1])
  rank_NB_jul <- rank_NB_jul_ov %>% arrange(desc(-rank_NB_jul_ov$rank),decreasing=F)
  
  rank_NB_sept_ov <- as.data.frame(elo_NB_2022_adults[,i])
  rank_NB_sept_ov$id <- rownames(elo_NB_2022_adults)
  rank_NB_sept_ov$rank <- rank(-rank_NB_sept_ov[,1])
  rank_NB_sept <- rank_NB_sept_ov %>% arrange(desc(-rank_NB_sept_ov$rank),decreasing=F)
  
  similarity_NB_ad[i] <- dyadic_similarity(rank_NB_jul$id, rank_NB_sept$id)
}


similarityNB_adults_random <- NA
for (i in 1:10000) {
  random_NB_jul<- sample(rank_NB_jul$id,size=nrow(rank_NB_jul),replace=F)
  random_NB_sept <- sample(rank_NB_sept$id,size=nrow(rank_NB_sept),replace=F)
  similarityNB_adults_random[i] <- dyadic_similarity(random_NB_jul, random_NB_sept)
}

mean(similarity_NB_ad)
sd(similarity_NB_ad)

mean(similarityNB_adults_random)
sd(similarityNB_adults_random)

## JUVENILES
similarity_NB_juv <- NA
#dynamics_NB_ad <-list()

elo_NB_jul_juv <- NB_scores_jul[which(rownames(NB_scores_jul) %in% juveniles),1:10000]
elo_NB_sept_juv <- NB_scores_sept[which(rownames(NB_scores_sept) %in% juveniles),1:10000]



for (i in 1:ncol(elo_NB_jul_juv)) {
  # DYNAMIC SIMILARITY
  rank_NB_jul_ov <- as.data.frame(elo_NB_jul_juv[,i])
  rank_NB_jul_ov$id <- rownames(elo_NB_jul_juv)
  rank_NB_jul_ov$rank <- rank(-rank_NB_jul_ov[,1])
  rank_NB_jul <- rank_NB_jul_ov %>% arrange(desc(-rank_NB_jul_ov$rank),decreasing=F)
  
  rank_NB_sept_ov <- as.data.frame(elo_NB_sept_juv[,i])
  rank_NB_sept_ov$id <- rownames(elo_NB_sept_juv)
  rank_NB_sept_ov$rank <- rank(-rank_NB_sept_ov[,1])
  rank_NB_sept <- rank_NB_sept_ov %>% arrange(desc(-rank_NB_sept_ov$rank),decreasing=F)
  
  similarity_NB_juv[i] <- dyadic_similarity(rank_NB_jul$id, rank_NB_sept$id)
}


similarityNB_juv_random <- NA
for (i in 1:10000) {
  random_NB_jul<- sample(rank_NB_jul$id,size=nrow(rank_NB_jul),replace=F)
  random_NB_sept <- sample(rank_NB_sept$id,size=nrow(rank_NB_sept),replace=F)
  similarityNB_juv_random[i] <- dyadic_similarity(random_NB_jul, random_NB_sept)
}

mean(similarity_NB_juv)
sd(similarity_NB_juv)

mean(similarityNB_juv_random)
sd(similarityNB_juv_random)


## Males
similarity_NB_males <- NA
#dynamics_NB_ad <-list()

elo_NB_jul_males <- NB_scores_jul[which(rownames(NB_scores_jul) %in% males),1:10000]
elo_NB_sept_males <- NB_scores_sept[which(rownames(NB_scores_sept) %in% males),1:10000]



for (i in 1:ncol(elo_NB_jul_males)) {
  # DYNAMIC SIMILARITY
  rank_NB_jul_ov <- as.data.frame(elo_NB_jul_males[,i])
  rank_NB_jul_ov$id <- rownames(elo_NB_jul_males)
  rank_NB_jul_ov$rank <- rank(-rank_NB_jul_ov[,1])
  rank_NB_jul <- rank_NB_jul_ov %>% arrange(desc(-rank_NB_jul_ov$rank),decreasing=F)
  
  rank_NB_sept_ov <- as.data.frame(elo_NB_sept_males[,i])
  rank_NB_sept_ov$id <- rownames(elo_NB_sept_males)
  rank_NB_sept_ov$rank <- rank(-rank_NB_sept_ov[,1])
  rank_NB_sept <- rank_NB_sept_ov %>% arrange(desc(-rank_NB_sept_ov$rank),decreasing=F)
  
  similarity_NB_males[i] <- dyadic_similarity(rank_NB_jul$id, rank_NB_sept$id)
}


similarityNB_males_random <- NA
for (i in 1:10000) {
  random_NB_jul<- sample(rank_NB_jul$id,size=nrow(rank_NB_jul),replace=F)
  random_NB_sept <- sample(rank_NB_sept$id,size=nrow(rank_NB_sept),replace=F)
  similarityNB_males_random[i] <- dyadic_similarity(random_NB_jul, random_NB_sept)
}

mean(similarity_NB_males)
sd(similarity_NB_males)

mean(similarityNB_males_random)
sd(similarityNB_males_random)


## females
similarity_NB_females <- NA
#dynamics_NB_ad <-list()

elo_NB_jul_females <- NB_scores_jul[which(rownames(NB_scores_jul) %in% females),1:10000]
elo_NB_sept_females <- NB_scores_sept[which(rownames(NB_scores_sept) %in% females),1:10000]



for (i in 1:ncol(elo_NB_jul_females)) {
  # DYNAMIC SIMILARITY
  rank_NB_jul_ov <- as.data.frame(elo_NB_jul_females[,i])
  rank_NB_jul_ov$id <- rownames(elo_NB_jul_females)
  rank_NB_jul_ov$rank <- rank(-rank_NB_jul_ov[,1])
  rank_NB_jul <- rank_NB_jul_ov %>% arrange(desc(-rank_NB_jul_ov$rank),decreasing=F)
  
  rank_NB_sept_ov <- as.data.frame(elo_NB_sept_females[,i])
  rank_NB_sept_ov$id <- rownames(elo_NB_sept_females)
  rank_NB_sept_ov$rank <- rank(-rank_NB_sept_ov[,1])
  rank_NB_sept <- rank_NB_sept_ov %>% arrange(desc(-rank_NB_sept_ov$rank),decreasing=F)
  
  similarity_NB_females[i] <- dyadic_similarity(rank_NB_jul$id, rank_NB_sept$id)
}


similarityNB_females_random <- NA
for (i in 1:10000) {
  random_NB_jul<- sample(rank_NB_jul$id,size=nrow(rank_NB_jul),replace=F)
  random_NB_sept <- sample(rank_NB_sept$id,size=nrow(rank_NB_sept),replace=F)
  similarityNB_females_random[i] <- dyadic_similarity(random_NB_jul, random_NB_sept)
}

mean(similarity_NB_females)
sd(similarity_NB_females)

mean(similarityNB_females_random)
sd(similarityNB_females_random)










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