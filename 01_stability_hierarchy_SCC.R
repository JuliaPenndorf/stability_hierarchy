################################################################################
###               STABILITY HIERARCHY OF SCC (2019-2022)                     ###


# LOAD PACKAGES
library(stringr)
library(reshape2)
library(aniDom)
require(plyr)


# LOAD DATA
agg_2019 <- read.csv('soc_data_2019.csv',stringsAsFactors = F)
agg_2022 <- read.csv('agg_data_2022.csv',stringsAsFactors = F)

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

ids <- rbind(match_id,wing_both_m)

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
agg_CG_2019 <- agg_2019_sub#[which(agg_2019_sub$LOCATION=="CG"),]

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


# replace ids depending on years$
ranks_CG_2022$ID_2019 <- ids$ID_2019[match(rownames(ranks_CG_2022),
                                           ids$ID_2022)]
ranks_CG_2022$rank_2019 <- ranks_CG_2019$ranks_CG_2019[match(ranks_CG_2022$ID_2019,
                                                      rownames(ranks_CG_2019))]
cor.test(ranks_CG_2022$ranks_CG_2022,ranks_CG_2022$rank_2019)

lmm <- lm(ranks_CG_2022$ranks_CG_2022~ ranks_CG_2022$rank_2019)
summary(lmm)
plot(ranks_CG_2022$ranks_CG_2022~ ranks_CG_2022$rank_2019,
     xlab="Rank 2019",
     ylab="Rank 2022")
abline(x~y, col="red")


