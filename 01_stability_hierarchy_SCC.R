################################################################################
###               STABILITY HIERARCHY OF SCC (2019-2022)                     ###


# LOAD PACKAGES
library(stringr)
library(reshape2)

# LOAD DATA
agg_2019 <- read.csv('soc_data_2019.csv',stringsAsFactors = F)
agg_2022 <- read.csv('soc_data_2022.csv',stringsAsFactors = F)

relatedness <- read.csv('self_relatedness.csv',stringsAsFactors = F)
ids <- make.unique(relatedness$X)
rownames(relatedness) <- ids
relatedness <- relatedness[,2:121]

# re-identifying individuals across datasets
melted <- melt(as.matrix(relatedness))
match_id <- na.omit(melted)

# adding wingtagged individuals across years
wing_2019 <-c("X42","X78","X11","X35","X118","X93","X124","X5","X115","X15","X2","X77","X88",
              "X23","X136","X39","X107","X71","X40","X108","X27","X135","X31","X50","X53",
              "X103","X138","X139")
wing_2022 <- c("X35","X11","X143","X51","X5","X78","X42","X2","X52","X4","X39","X1","X135",
               "X12","X31","X40","X53","X142","X85","X48","X136","X105","X108")
wing_both <- Reduce(intersect, list(wing_2019,wing_2022))

#IDs by year
ids2019 <- ids[str_detect(ids, "_2019") ]
ids2022 <- ids[str_detect(ids, "_2022") ]

match_id$ids_across_years <- (match_id$Var1%in%ids2019&match_id$Var2%in%ids2022) |
                              (match_id$Var1%in%ids2022&match_id$Var2%in%ids2019)
ids_marked_both_years <- match_id[which(match_id$ids_across_years==T),]

ids_marked_both <- as.vector(unique(c(ids_marked_both_years$Var1,ids_marked_both_years$Var2)))
ids_marked_both <- str_replace_all(ids_marked_both, "_2019", "")
ids_marked_both <- str_replace_all(ids_marked_both, "_2022", "")
ids_marked_both <- str_replace_all(ids_marked_both, ".1", "")

ids_both <- c(ids_marked_both,wing_both)


