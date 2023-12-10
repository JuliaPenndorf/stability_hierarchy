#################################################################################
###         SIMULATION EFFECT OF MOUVEMENT ON DOMINANCE RANK AT EACH ROOST    ###
###                                                                           ###
#################################################################################

# FUNCTION
`%ni%` <- Negate(`%in%`)

# LOAD LIBRARIES
library(dplyr)
library(rptR)
library(data.table)

# CREATE DATA
Nind <- 262 # Nind appearing in 1 hierarchy
N3movers <- 2 #Nind appearing in hierarchy at 3 roost sites
N2movers <- 25  #Nind appearing in hierarchy at 2 roost sites

Nroost <- 3
Ncg <- 91 #91 individuals at CG
Nba <- 126 #126 individuals at BA
Nnb <- 77 #77 individuals at NB

Nba_cg <- 1 # individuals moving from BA to CG
Nba_nb <- 6# individuals moving from BA to NB
Ncg_ba <- 8 # individuals moving from CG to BA
Ncg_nb <- 1 # individuals moving from CG to NB
Ncg_nb <- 1 # individuals moving from CG to NB
Nnb_ba <- 11 # individuals moving from NB to BA
Nnb_cg <- 1 # individuals moving from NB to CG

Nmales <- rep("M",92)
Nfemales <- rep("F",73)
Nunknown <- rep("UK",112)
sexing <- c(Nmales,Nfemales,Nunknown)
Nmov_m <- 9
Nmov_f <- 12

# N repetitions
randomisations <- 100
individuals <- list()
ID <- matrix(NA,ncol=randomisations,nrow = 294 )
roosts <- matrix(NA,ncol=randomisations,nrow = 294 )
rank_pop<- matrix(NA,ncol=randomisations,nrow =294 )
status<- matrix(NA,ncol=randomisations,nrow =294 )
sex<- matrix(NA,ncol=randomisations,nrow =294 )

# randomly assign a individual to each roost
for (i in 1:randomisations){
  inds <- 1:(Nind)
  individuals_1 <- as.data.frame(c(inds))
  colnames(individuals_1) <- "ID"
  individuals_1$ID <- paste("N","",individuals_1$ID)
  individuals_1$rank_pop <- 1:nrow(individuals_1)
  individuals_1$sex <- sample(sexing,nrow(individuals_1),replace=F)
  
  BA <- rep("BA", times = Nba)
  CG <- rep("CG", times = Ncg)
  NB <- rep("NB", times = Nnb)
  roosts <- c(BA,CG,NB)
  individuals_1$roost <- sample(roosts,nrow(individuals_1),replace=F)
  individuals_1$status <- "resident"
  
  #create moving individuals 
  ### individuals with 2 roosts 
  movers2BA <- as.data.frame(sample(individuals_1$ID[individuals_1$roost=="BA"],(Nba_cg+Nba_nb),replace=F)) #randomnly select moving individuals between resident and 1 visited roost
  colnames(movers2BA) <- "ID"
  movers2BA$roost <- c(rep.int("CG", Nba_cg),rep.int("NB", Nba_nb))
  
  movers2CG <- as.data.frame(sample(individuals_1$ID[individuals_1$roost=="CG"],(Ncg_ba+Ncg_nb),replace=F)) #randomnly select moving individuals between resident and 1 visited roost
  colnames(movers2CG) <- "ID"
  movers2CG$roost <- c(rep.int("BA", Ncg_ba),rep.int("NB", Ncg_nb))
  
  movers2NB <- as.data.frame(sample(individuals_1$ID[individuals_1$roost=="NB"],(Nnb_ba+Nnb_cg),replace=F)) #randomnly select moving individuals between resident and 1 visited roost
  colnames(movers2NB) <- "ID"
  movers2NB$roost <- c(rep.int("BA", Nnb_ba),rep.int("CG", Nnb_cg))
  
  movers2 <- as.data.frame(rbind(movers2BA,movers2CG,movers2NB))
  movers2$status <- "visitor"
  movers2$rank_pop <- individuals_1$rank_pop[match(movers2$ID,individuals_1$ID)]
  movers2$sex <- individuals_1$sex[match(movers2$ID,individuals_1$ID)]
  
  movers2<- movers2[, c(1,4,5,2,3)]
  
  ### individuals with 3 roosts 
  movers3 <- sample(individuals_1$ID[individuals_1$ID %ni% movers2$ID],N3movers,replace=F)
  movers3 <- as.data.frame(movers3)
  colnames(movers3) <- "ID"
  
  movers3$roost_resident <-individuals_1$roost[match(movers3$ID,individuals_1$ID)]
  
  movers3_1 <- movers3
  movers3_1$roost <- NA
  for (j in 1:nrow(movers3_1)) {
    if(movers3_1$roost_resident[j]=="BA") {
      movers3_1$roost[j] <- "CG"}
    if(movers3_1$roost_resident[j]=="CG") {
       movers3_1$roost[j] <- "BA"}
    if(movers3_1$roost_resident[j]=="NB") {
      movers3_1$roost[j] <- "BA"}
    }
  
  movers3_1$status <- "vistor"
  movers3_1 <- movers3_1[,-2]
  
  movers3_2 <- movers3
  movers3_2$roost <- NA
  for (k in 1:nrow(movers3_2)) {
    if(movers3_2$roost_resident[k]=="BA") {
      movers3_2$roost[k] <- "NB"}
    if(movers3_2$roost_resident[k]=="CG") {
      movers3_2$roost[k] <- "NB"}
    if(movers3_2$roost_resident[k]=="NB") {
      movers3_2$roost[k] <- "CG"} 
  }
  
  movers3_2$status <- "visitor"
  movers3_2 <- movers3_2[,-2]
  
  movers3_com <- as.data.frame(rbind(movers3_1,movers3_2))
  movers3_com$rank_pop <- individuals_1$rank_pop[match(movers3_com$ID, 
                                                       individuals_1$ID)]
  
  movers3_com$sex <- individuals_1$sex[match(movers3_com$ID, 
                                             individuals_1$ID)]
  movers3_com<- movers3_com[, c(1,4,5,2,3)]
  
  
 
  ID <- c(individuals_1$ID,movers2$ID,movers3_com$ID)
  roosts <-c(individuals_1$roost,movers2$roost,movers3_com$roost)
  rank_pop <-c(individuals_1$rank_pop,movers2$rank_pop,movers3_com$rank_pop)
  sex <- c(individuals_1$sex,movers2$sex,movers3_com$sex)
  status <- c(individuals_1$status,movers2$status,movers3_com$status)
  round <- rep(i, times = length(status))
  individuals[[i]] <- cbind(ID,roosts,rank_pop,sex,status,round)
}

df <-  as.data.frame(do.call(rbind, individuals))
df$rank_pop <- as.numeric(df$rank_pop)

setDT(df)
df2 <- df[, rank_roost := frank(rank_pop),
          by = list(round,roosts,sex)]

df2$unique_ID <- paste(df2$round,"",df2$ID)

df2$st_rank[df2$roosts=="BA"] <- df2$rank_roost[df2$roosts=="BA"]/Nba
df2$st_rank[df2$roosts=="CG"] <- df2$rank_roost[df2$roosts=="CG"]/Nba
df2$st_rank[df2$roosts=="NB"] <- df2$rank_roost[df2$roosts=="NB"]/Nba

dupli <- df2$unique_ID[duplicated(df2$unique_ID)]

df3 <- df2[which(df2$unique_ID %in% dupli),]


Rpt_model <- rptGaussian(st_rank ~ 
                           (1|unique_ID), 
                         grname=c("unique_ID"), 
                         data = df3, 
                         nboot=1000, 
                         npermut=0,
                         adjusted = TRUE)
print(Rpt_model)
