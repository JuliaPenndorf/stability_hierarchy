##########################################################################################################################
# Calculating the dominance hierarchies of Sulphur-crested cockatoos at 3 study locations (CG, BA, NB)


##########################################################################################################################



#load packages
library (aniDom)
library(reshape2)
library(plyr)

#load data
agg <- read.csv('soc_data_2019.csv', stringsAsFactors = FALSE,header = T,row.names = 1)

sexing <- read.csv2('Marking_sheet_MASTER_corrected.csv')
vector<-read.csv("Vector_IDs_15July.csv")

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
CG_jul_sub <- CG_jul[which(CG_jul$WINNER %in% inds_CG_both,
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

CG_sept_sub <- CG_sept[which(CG_sept$WINNER %in% inds_CG_both,
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


