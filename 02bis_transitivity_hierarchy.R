
##################################################################################################################
### following Mcdonald & Shizuka 2012

# LOAD PACKAGES
library(reshape2)
library(plyr)
library(aniDom)
library(asnipe)
library(igraph)
library(compete)


# LOAD DATA
sexing <- read.csv('Vector_IDs_15July.csv')
agg <- read.csv('aggressions_2019_cleaned.csv')
vector <-read.csv2("Marking_sheet_MASTER_corrected.csv", header=T)
agg_2022 <- read.csv('agg_data_cleaned.csv',stringsAsFactors = F)
sexing_2022 <- read.csv("sexing_2022.csv")


# subsetting to ids that have >=10 int - this is done at location level
agg.BAsub <- agg[agg$LOCATION=="BA",]
agg.CGsub <- agg[agg$LOCATION=="CG",]
agg.NBsub <- agg[agg$LOCATION=="NB",]

##
IDs.tot <- unique(c(agg$WINNER, agg$LOSER))
vector2 <- data.frame(ID=IDs.tot, stringsAsFactors=FALSE)
vector2$Sex <- vector$Assigned_Sex[match(vector2$ID, vector$ID_Site)]
vector2$Sex[is.na(vector2$Sex)] <- sexing$Sex[match(vector2$ID[is.na(vector2$Sex)], 
                                                    sexing$Social_ID)]

vector2$Age <- vector$Assigned_Age[match(vector2$ID, vector$ID_Site)]
vector2$Age[is.na(vector2$Age)] <- as.character(sexing$Age[match(vector2$ID[is.na(vector2$Age)], 
                                                                 sexing$Social_ID)])

vector2$Roost_Membership <- as.character(vector$Site[match(vector2$ID, vector$ID_Site)])

vector.males <- vector2[which(vector2$Sex=="M"),]
vector.females <- vector2[which(vector2$Sex=="F"),]


#Checking number interactions per indiv
### BA
agg.BA <- agg.BAsub
nbr_win_ba <- as.data.frame(table(agg.BA$WINNER))
nbr_loss_ba <- as.data.frame(table(agg.BA$LOSER))

names(nbr_win_ba) <- c("ID", "wins")
names(nbr_loss_ba) <- c("ID", "loss")

int <- rbind.fill(nbr_win_ba[c("wins","ID")], nbr_loss_ba[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
nbr.interactions <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(nbr.interactions) <- c("ID", "wins", "los")

for (i in 1:nrow(nbr.interactions)){ 
  nbr.interactions$total[i] <- sum(nbr.interactions[i,2]+nbr.interactions[i,3])
}


# creating new column indicating if the individual had more than 10 interactions 
nbr.interactions$over10 <- nbr.interactions$total>=10
ba.ov10 <- nbr.interactions[which(nbr.interactions$over10==TRUE),]
agg.BA.sub <- agg.BA[which((agg.BA$WINNER %in%ba.ov10$ID) &(agg.BA$LOSER%in%ba.ov10$ID)),]

network.BA <- matrix(0, nrow=nrow(ba.ov10), ncol=nrow(ba.ov10))
colnames(network.BA) <- ba.ov10$ID
rownames(network.BA) <- ba.ov10$ID

for (i in 1:nrow(network.BA)) {
  for (j in 1:nrow(network.BA)) {
    network.BA[i,j] <- sum(agg.BA.sub$WINNER == rownames(network.BA)[i] & agg.BA.sub$LOSER == colnames(network.BA)[j])
  }
}

network.prop <- matrix(0, nrow=nrow(ba.ov10), ncol=nrow(ba.ov10))
colnames(network.prop) <- ba.ov10$ID
rownames(network.prop) <- ba.ov10$ID
for (i in 1:nrow(network.prop)) {
  for (j in 1:nrow(network.prop)) {
    network.prop[i,j] <- network.BA[i,j] / (network.BA[i,j]+network.BA[j,i])
  }
}
network.prop[network.prop=="NaN"] <- 0

network.bin <- network.prop>0.5
network.bin[network.prop==0.5] <- TRUE
ttri(network.bin)

net.BA <- graph.adjacency(network.BA,
                          mode="plus", 
                          weighted=NULL, 
                          diag=FALSE)
edge_density(net.BA)


agg.BA.m <- agg.BA[which((agg.BA$WINNER %in%vector.males$ID) &( agg.BA$LOSER %in% vector.males$ID)),]

nbr_win_ba_m <- as.data.frame(table(agg.BA.m$WINNER))
nbr_los_ba_m <- as.data.frame(table(agg.BA.m$LOSER))

names(nbr_win_ba_m) <- c("ID", "wins")
names(nbr_los_ba_m) <- c("ID", "loss")

int <- rbind.fill(nbr_win_ba_m[c("wins","ID")], nbr_los_ba_m[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
nbr.interactions <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(nbr.interactions) <- c("ID", "wins", "los")

for (i in 1:nrow(nbr.interactions)){ 
  nbr.interactions$total[i] <- sum(nbr.interactions[i,2]+nbr.interactions[i,3])
}


# creating new column indicating if the individual had more than 10 interactions 
nbr.interactions$over10 <- nbr.interactions$total>=10
ba.m.ov10 <- nbr.interactions[which(nbr.interactions$over10==TRUE),]
agg.BA.m.sub <- agg.BA.m[which((agg.BA.m$WINNER %in%ba.m.ov10$ID) &(agg.BA.m$LOSER%in%ba.m.ov10$ID)),]

network.BA.m <- matrix(0, nrow=nrow(ba.m.ov10), ncol=nrow(ba.m.ov10))
colnames(network.BA.m) <- ba.m.ov10$ID
rownames(network.BA.m) <- ba.m.ov10$ID

for (i in 1:nrow(network.BA.m)) {
  for (j in 1:nrow(network.BA.m)) {
    network.BA.m[i,j] <- sum(agg.BA.m.sub$WINNER == rownames(network.BA.m)[i] & agg.BA.m.sub$LOSER == colnames(network.BA.m)[j])
  }
}



network.BA.com.m <- matrix(NA,nrow=nrow(network.BA.m),ncol=ncol(network.BA.m))
colnames(network.BA.com.m) <- colnames(network.BA.m)
rownames(network.BA.com.m) <- colnames(network.BA.m)

for (i in 1:nrow(network.BA.com.m)) {
  for (j in 1:nrow(network.BA.com.m)) {
    network.BA.com.m[i,j] <- sum((agg.BA.m.sub$WINNER == rownames(network.BA.com.m)[i] & agg.BA.m.sub$LOSER == colnames(network.BA.com.m)[j])|
                                   (agg.BA.m.sub$WINNER == rownames(network.BA.com.m)[j] & agg.BA.m.sub$LOSER == colnames(network.BA.com.m)[i]))
  }
}
network.BA.prop.m <- matrix(NA,nrow=nrow(network.BA.m),ncol=ncol(network.BA.m))
colnames(network.BA.prop.m) <- colnames(network.BA.m)
rownames(network.BA.prop.m) <- colnames(network.BA.m)
for (i in 1:nrow(network.BA.prop.m)) {
  for (j in 1:nrow(network.BA.prop.m)) {
    network.BA.prop.m[i,j] <- network.BA.m[i,j]/network.BA.com.m[i,j]
  }
}
network.BA.prop.m[network.BA.prop.m=="NaN"] <- 0
network.BA.prop.m2 <- network.BA.prop.m >0.5
ttri(network.BA.prop.m2)
net.BA.bin.m <- graph.adjacency(network.BA.m,
                                mode="directed", 
                                weighted=NULL, 
                                diag=FALSE)
edge_density(net.BA.bin.m)





#CG 2019

agg.CG.2019 <- agg.CGsub
nbr_win_CG.2019 <- as.data.frame(table(agg.CG.2019$WINNER))
nbr_loss_CG.2019 <- as.data.frame(table(agg.CG.2019$LOSER))

names(nbr_win_CG.2019) <- c("ID", "wins")
names(nbr_loss_CG.2019) <- c("ID", "loss")

int <- rbind.fill(nbr_win_CG.2019[c("wins","ID")], nbr_loss_CG.2019[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
nbr.interactions <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(nbr.interactions) <- c("ID", "wins", "los")

for (i in 1:nrow(nbr.interactions)){ 
  nbr.interactions$total[i] <- sum(nbr.interactions[i,2]+nbr.interactions[i,3])
}


# creating new column indicating if the individual had more than 10 interactions 
nbr.interactions$over10 <- nbr.interactions$total>=10
CG.2019.ov10 <- nbr.interactions[which(nbr.interactions$over10==TRUE),]
agg.CG.2019.sub <- agg.CG.2019[which((agg.CG.2019$WINNER %in%CG.2019.ov10$ID) &(agg.CG.2019$LOSER%in%CG.2019.ov10$ID)),]

network.CG.2019 <- matrix(0, nrow=nrow(CG.2019.ov10), ncol=nrow(CG.2019.ov10))
colnames(network.CG.2019) <- CG.2019.ov10$ID
rownames(network.CG.2019) <- CG.2019.ov10$ID

for (i in 1:nrow(network.CG.2019)) {
  for (j in 1:nrow(network.CG.2019)) {
    network.CG.2019[i,j] <- sum(agg.CG.2019.sub$WINNER == rownames(network.CG.2019)[i] & agg.CG.2019.sub$LOSER == colnames(network.CG.2019)[j])
  }
}

network.prop <- matrix(0, nrow=nrow(CG.2019.ov10), ncol=nrow(CG.2019.ov10))
colnames(network.prop) <- CG.2019.ov10$ID
rownames(network.prop) <- CG.2019.ov10$ID
for (i in 1:nrow(network.prop)) {
  for (j in 1:nrow(network.prop)) {
    network.prop[i,j] <- network.CG.2019[i,j] / (network.CG.2019[i,j]+network.CG.2019[j,i])
  }
}
network.prop[network.prop=="NaN"] <- 0

network.bin <- network.prop>0.5
network.bin[network.prop==0.5] <- TRUE
ttri(network.bin)

net.CG.2019 <- graph.adjacency(network.CG.2019,
                          mode="plus", 
                          weighted=NULL, 
                          diag=FALSE)
edge_density(net.CG.2019)


agg.CG.2019.m <- agg.CG.2019[which((agg.CG.2019$WINNER %in%vector.males$ID) &( agg.CG.2019$LOSER %in% vector.males$ID)),]

nbr_win_CG.2019_m <- as.data.frame(table(agg.CG.2019.m$WINNER))
nbr_los_CG.2019_m <- as.data.frame(table(agg.CG.2019.m$LOSER))

names(nbr_win_CG.2019_m) <- c("ID", "wins")
names(nbr_los_CG.2019_m) <- c("ID", "loss")

int <- rbind.fill(nbr_win_CG.2019_m[c("wins","ID")], nbr_los_CG.2019_m[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
nbr.interactions <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(nbr.interactions) <- c("ID", "wins", "los")

for (i in 1:nrow(nbr.interactions)){ 
  nbr.interactions$total[i] <- sum(nbr.interactions[i,2]+nbr.interactions[i,3])
}


# creating new column indicating if the individual had more than 10 interactions 
nbr.interactions$over10 <- nbr.interactions$total>=10
CG.2019.m.ov10 <- nbr.interactions[which(nbr.interactions$over10==TRUE),]
agg.CG.2019.m.sub <- agg.CG.2019.m[which((agg.CG.2019.m$WINNER %in%CG.2019.m.ov10$ID) &(agg.CG.2019.m$LOSER%in%CG.2019.m.ov10$ID)),]

network.CG.2019.m <- matrix(0, nrow=nrow(CG.2019.m.ov10), ncol=nrow(CG.2019.m.ov10))
colnames(network.CG.2019.m) <- CG.2019.m.ov10$ID
rownames(network.CG.2019.m) <- CG.2019.m.ov10$ID

for (i in 1:nrow(network.CG.2019.m)) {
  for (j in 1:nrow(network.CG.2019.m)) {
    network.CG.2019.m[i,j] <- sum(agg.CG.2019.m.sub$WINNER == rownames(network.CG.2019.m)[i] & agg.CG.2019.m.sub$LOSER == colnames(network.CG.2019.m)[j])
  }
}



network.CG.2019.com.m <- matrix(NA,nrow=nrow(network.CG.2019.m),ncol=ncol(network.CG.2019.m))
colnames(network.CG.2019.com.m) <- colnames(network.CG.2019.m)
rownames(network.CG.2019.com.m) <- colnames(network.CG.2019.m)

for (i in 1:nrow(network.CG.2019.com.m)) {
  for (j in 1:nrow(network.CG.2019.com.m)) {
    network.CG.2019.com.m[i,j] <- sum((agg.CG.2019.m.sub$WINNER == rownames(network.CG.2019.com.m)[i] & agg.CG.2019.m.sub$LOSER == colnames(network.CG.2019.com.m)[j])|
                                   (agg.CG.2019.m.sub$WINNER == rownames(network.CG.2019.com.m)[j] & agg.CG.2019.m.sub$LOSER == colnames(network.CG.2019.com.m)[i]))
  }
}
network.CG.2019.prop.m <- matrix(NA,nrow=nrow(network.CG.2019.m),ncol=ncol(network.CG.2019.m))
colnames(network.CG.2019.prop.m) <- colnames(network.CG.2019.m)
rownames(network.CG.2019.prop.m) <- colnames(network.CG.2019.m)
for (i in 1:nrow(network.CG.2019.prop.m)) {
  for (j in 1:nrow(network.CG.2019.prop.m)) {
    network.CG.2019.prop.m[i,j] <- network.CG.2019.m[i,j]/network.CG.2019.com.m[i,j]
  }
}
network.CG.2019.prop.m[network.CG.2019.prop.m=="NaN"] <- 0
network.CG.2019.prop.m2 <- network.CG.2019.prop.m >0.5
ttri(network.CG.2019.prop.m2)
net.CG.2019.bin.m <- graph.adjacency(network.CG.2019.m,
                                mode="directed", 
                                weighted=NULL, 
                                diag=FALSE)
edge_density(net.CG.2019.bin.m)


#CG 2022

agg.CG.2022 <- agg_2022[which(agg_2022$Location=="CG"),]
nbr_win_CG.2022 <- as.data.frame(table(agg.CG.2022$Winner))
nbr_loss_CG.2022 <- as.data.frame(table(agg.CG.2022$Loser))

names(nbr_win_CG.2022) <- c("ID", "wins")
names(nbr_loss_CG.2022) <- c("ID", "loss")

int <- rbind.fill(nbr_win_CG.2022[c("wins","ID")], nbr_loss_CG.2022[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
nbr.interactions <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(nbr.interactions) <- c("ID", "wins", "los")

for (i in 1:nrow(nbr.interactions)){ 
  nbr.interactions$total[i] <- sum(nbr.interactions[i,2]+nbr.interactions[i,3])
}


# creating new column indicating if the individual had more than 10 interactions 
nbr.interactions$over10 <- nbr.interactions$total>=10
CG.2022.ov10 <- nbr.interactions[which(nbr.interactions$over10==TRUE),]
agg.CG.2022.sub <- agg.CG.2022[which((agg.CG.2022$Winner %in%CG.2022.ov10$ID) &
                                      (agg.CG.2022$Loser%in%CG.2022.ov10$ID)),]

network.CG.2022 <- matrix(0, nrow=nrow(CG.2022.ov10), ncol=nrow(CG.2022.ov10))
colnames(network.CG.2022) <- CG.2022.ov10$ID
rownames(network.CG.2022) <- CG.2022.ov10$ID

for (i in 1:nrow(network.CG.2022)) {
  for (j in 1:nrow(network.CG.2022)) {
    network.CG.2022[i,j] <- sum(agg.CG.2022.sub$Winner == rownames(network.CG.2022)[i] & 
                                  agg.CG.2022.sub$Loser == colnames(network.CG.2022)[j])
  }
}

network.prop <- matrix(0, nrow=nrow(CG.2022.ov10), ncol=nrow(CG.2022.ov10))
colnames(network.prop) <- CG.2022.ov10$ID
rownames(network.prop) <- CG.2022.ov10$ID
for (i in 1:nrow(network.prop)) {
  for (j in 1:nrow(network.prop)) {
    network.prop[i,j] <- network.CG.2022[i,j] / (network.CG.2022[i,j]+network.CG.2022[j,i])
  }
}
network.prop[network.prop=="NaN"] <- 0

network.bin <- network.prop>0.5
network.bin[network.prop==0.5] <- TRUE
ttri(network.bin)

net.CG.2022 <- graph.adjacency(network.CG.2022,
                               mode="plus", 
                               weighted=NULL, 
                               diag=FALSE)
edge_density(net.CG.2022)

males.2022 <- sexing_2022$ID[sexing_2022$assigned_sex=="M"]
agg.CG.2022.m <- agg.CG.2022[which((agg.CG.2022$Winner %in%males.2022) &( agg.CG.2022$Loser %in% males.2022)),]

nbr_win_CG.2022_m <- as.data.frame(table(agg.CG.2022.m$Winner))
nbr_los_CG.2022_m <- as.data.frame(table(agg.CG.2022.m$Loser))

names(nbr_win_CG.2022_m) <- c("ID", "wins")
names(nbr_los_CG.2022_m) <- c("ID", "loss")

int <- rbind.fill(nbr_win_CG.2022_m[c("wins","ID")], nbr_los_CG.2022_m[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
nbr.interactions <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(nbr.interactions) <- c("ID", "wins", "los")

for (i in 1:nrow(nbr.interactions)){ 
  nbr.interactions$total[i] <- sum(nbr.interactions[i,2]+nbr.interactions[i,3])
}


# creating new column indicating if the individual had more than 10 interactions 
nbr.interactions$over10 <- nbr.interactions$total>=10
CG.2022.m.ov10 <- nbr.interactions[which(nbr.interactions$over10==TRUE),]
agg.CG.2022.m.sub <- agg.CG.2022.m[which((agg.CG.2022.m$Winner %in%CG.2022.m.ov10$ID) &
                                           (agg.CG.2022.m$Loser%in%CG.2022.m.ov10$ID)),]

network.CG.2022.m <- matrix(0, nrow=nrow(CG.2022.m.ov10), ncol=nrow(CG.2022.m.ov10))
colnames(network.CG.2022.m) <- CG.2022.m.ov10$ID
rownames(network.CG.2022.m) <- CG.2022.m.ov10$ID

for (i in 1:nrow(network.CG.2022.m)) {
  for (j in 1:nrow(network.CG.2022.m)) {
    network.CG.2022.m[i,j] <- sum(agg.CG.2022.m.sub$Winner == rownames(network.CG.2022.m)[i] & 
                                    agg.CG.2022.m.sub$Loser == colnames(network.CG.2022.m)[j])
  }
}



network.CG.2022.com.m <- matrix(NA,nrow=nrow(network.CG.2022.m),ncol=ncol(network.CG.2022.m))
colnames(network.CG.2022.com.m) <- colnames(network.CG.2022.m)
rownames(network.CG.2022.com.m) <- colnames(network.CG.2022.m)

for (i in 1:nrow(network.CG.2022.com.m)) {
  for (j in 1:nrow(network.CG.2022.com.m)) {
    network.CG.2022.com.m[i,j] <- sum((agg.CG.2022.m.sub$Winner == rownames(network.CG.2022.com.m)[i] & 
                                         agg.CG.2022.m.sub$Loser == colnames(network.CG.2022.com.m)[j])|
                                        (agg.CG.2022.m.sub$Winner == rownames(network.CG.2022.com.m)[j] & 
                                           agg.CG.2022.m.sub$Loser == colnames(network.CG.2022.com.m)[i]))
  }
}
network.CG.2022.prop.m <- matrix(NA,nrow=nrow(network.CG.2022.m),ncol=ncol(network.CG.2022.m))
colnames(network.CG.2022.prop.m) <- colnames(network.CG.2022.m)
rownames(network.CG.2022.prop.m) <- colnames(network.CG.2022.m)
for (i in 1:nrow(network.CG.2022.prop.m)) {
  for (j in 1:nrow(network.CG.2022.prop.m)) {
    network.CG.2022.prop.m[i,j] <- network.CG.2022.m[i,j]/network.CG.2022.com.m[i,j]
  }
}
network.CG.2022.prop.m[network.CG.2022.prop.m=="NaN"] <- 0
network.CG.2022.prop.m2 <- network.CG.2022.prop.m >0.5
ttri(network.CG.2022.prop.m2)
net.CG.2022.bin.m <- graph.adjacency(network.CG.2022.prop.m2,
                                     mode="directed", 
                                     weighted=NULL, 
                                     diag=FALSE)
edge_density(net.CG.2022.bin.m)


females.2022 <- sexing_2022$ID[sexing_2022$assigned_sex=="F"]

agg.CG.2022.f <- agg.CG.2022[which((agg.CG.2022$Winner %in%females.2022) &
                                    (agg.CG.2022$Loser %in% females.2022)),]

nbr_win_CG.2022_f <- as.data.frame(table(agg.CG.2022.f$Winner))
nbr_los_CG.2022_f <- as.data.frame(table(agg.CG.2022.f$Loser))

names(nbr_win_CG.2022_f) <- c("ID", "wins")
names(nbr_los_CG.2022_f) <- c("ID", "loss")

int <- rbind.fill(nbr_win_CG.2022_f[c("wins","ID")], nbr_los_CG.2022_f[c("ID","loss")])
int2 <- rbind(int[c("ID","wins", "loss")])
int2[is.na(int2)] <- 0
nbr.interactions <-aggregate(int2[,2:3], list(int2[,1]), sum)
names(nbr.interactions) <- c("ID", "wins", "los")

for (i in 1:nrow(nbr.interactions)){ 
  nbr.interactions$total[i] <- sum(nbr.interactions[i,2]+nbr.interactions[i,3])
}


# creating new column indicating if the individual had more than 10 interactions 
nbr.interactions$over10 <- nbr.interactions$total>=10
CG.2022.f.ov10 <- nbr.interactions[which(nbr.interactions$over10==TRUE),]
agg.CG.2022.f.sub <- agg.CG.2022.f[which((agg.CG.2022.f$Winner %in%CG.2022.f.ov10$ID) &
                                           (agg.CG.2022.f$Loser%in%CG.2022.f.ov10$ID)),]

network.CG.2022.f <- matrix(0, nrow=nrow(CG.2022.f.ov10), ncol=nrow(CG.2022.f.ov10))
colnames(network.CG.2022.f) <- CG.2022.f.ov10$ID
rownames(network.CG.2022.f) <- CG.2022.f.ov10$ID

for (i in 1:nrow(network.CG.2022.f)) {
  for (j in 1:nrow(network.CG.2022.f)) {
    network.CG.2022.f[i,j] <- sum(agg.CG.2022.f.sub$Winner == rownames(network.CG.2022.f)[i] & 
                                    agg.CG.2022.f.sub$Loser == colnames(network.CG.2022.f)[j])
  }
}



network.CG.2022.com.f <- matrix(NA,nrow=nrow(network.CG.2022.f),ncol=ncol(network.CG.2022.f))
colnames(network.CG.2022.com.f) <- colnames(network.CG.2022.f)
rownames(network.CG.2022.com.f) <- colnames(network.CG.2022.f)

for (i in 1:nrow(network.CG.2022.com.f)) {
  for (j in 1:nrow(network.CG.2022.com.f)) {
    network.CG.2022.com.f[i,j] <- sum((agg.CG.2022.f.sub$Winner == rownames(network.CG.2022.com.f)[i] & 
                                         agg.CG.2022.f.sub$Loser == colnames(network.CG.2022.com.f)[j])|
                                        (agg.CG.2022.f.sub$Winner == rownames(network.CG.2022.com.f)[j] & 
                                           agg.CG.2022.f.sub$Loser == colnames(network.CG.2022.com.f)[i]))
  }
}
network.CG.2022.prop.f <- matrix(NA,nrow=nrow(network.CG.2022.f),ncol=ncol(network.CG.2022.f))
colnames(network.CG.2022.prop.f) <- colnames(network.CG.2022.f)
rownames(network.CG.2022.prop.f) <- colnames(network.CG.2022.f)
for (i in 1:nrow(network.CG.2022.prop.f)) {
  for (j in 1:nrow(network.CG.2022.prop.f)) {
    network.CG.2022.prop.f[i,j] <- network.CG.2022.f[i,j]/network.CG.2022.com.f[i,j]
  }
}
network.CG.2022.prop.f[network.CG.2022.prop.f=="NaN"] <- 0
network.CG.2022.prop.f2 <- network.CG.2022.prop.f >0.5
ttri(network.CG.2022.prop.f2)
net.CG.2022.bin.f <- graph.adjacency(network.CG.2022.f,
                                     mode="directed", 
                                     weighted=NULL, 
                                     diag=FALSE)
edge_density(net.CG.2022.bin.f)
edge_density(net.CG.2022.bin.f, loops = FALSE)






network.NB.com <- matrix(NA,nrow=nrow(network.NB),ncol=ncol(network.NB))
colnames(network.NB.com) <- colnames(network.NB)
rownames(network.NB.com) <- colnames(network.NB)
for (i in 1:nrow(network.NB.com)) {
  for (j in 1:nrow(network.NB.com)) {
    network.NB.com[i,j] <- sum((agg.NB.sub$WINNER == rownames(network.NB.com)[i] & agg.NB.sub$LOSER == colnames(network.NB.com)[j])|
                                 (agg.NB.sub$WINNER == rownames(network.NB.com)[j] & agg.NB.sub$LOSER == colnames(network.NB.com)[i]) )
  }
}
network.NB.prop <- matrix(NA,nrow=nrow(network.NB),ncol=ncol(network.NB))
colnames(network.NB.prop) <- colnames(network.NB)
rownames(network.NB.prop) <- colnames(network.NB)
for (i in 1:nrow(network.NB.prop)) {
  for (j in 1:nrow(network.NB.prop)) {
    network.NB.prop[i,j] <- network.NB[i,j]/network.NB.com[i,j]
  }
}
network.NB.prop[network.NB.prop=="NaN"] <- 0
network.NB.prop2 <- network.NB.prop >0.5
net.NB.bin <- graph.adjacency(network.NB,
                              mode="directed", 
                              weighted=NULL, 
                              diag=FALSE)

ttri(net.NB.bin)
edge_density(net.NB.bin)




network.NB.com.m <- matrix(NA,nrow=nrow(network.NB.m),ncol=ncol(network.NB.m))
colnames(network.NB.com.m) <- colnames(network.NB.m)
rownames(network.NB.com.m) <- colnames(network.NB.m)

for (i in 1:nrow(network.NB.com.m)) {
  for (j in 1:nrow(network.NB.com.m)) {
    network.NB.com.m[i,j] <- sum((agg.NB.m.sub$WINNER == rownames(network.NB.com.m)[i] & agg.NB.m.sub$LOSER == colnames(network.NB.com.m)[j])|
                                   (agg.NB.m.sub$WINNER == rownames(network.NB.com.m)[j] & agg.NB.m.sub$LOSER == colnames(network.NB.com.m)[i]))
  }
}
network.NB.prop.m <- matrix(NA,nrow=nrow(network.NB.m),ncol=ncol(network.NB.m))
colnames(network.NB.prop.m) <- colnames(network.NB.m)
rownames(network.NB.prop.m) <- colnames(network.NB.m)
for (i in 1:nrow(network.NB.prop.m)) {
  for (j in 1:nrow(network.NB.prop.m)) {
    network.NB.prop.m[i,j] <- network.NB.m[i,j]/network.NB.com.m[i,j]
  }
}
network.NB.prop.m[network.NB.prop.m=="NaN"] <- 0
network.NB.prop.m2 <- network.NB.prop.m >0.5
net.NB.bin.m <- graph.adjacency(network.NB.m,
                                mode="directed", 
                                weighted=NULL, 
                                diag=FALSE)

ttri(network.NB.prop.m2)
edge_density(net.NB.bin.m)




#CG 2022

network.CG.com <- matrix(NA,nrow=nrow(network.CG),ncol=ncol(network.CG))
colnames(network.CG.com) <- colnames(network.CG)
rownames(network.CG.com) <- colnames(network.CG)
for (i in 1:nrow(network.CG.com)) {
  for (j in 1:nrow(network.CG.com)) {
    network.CG.com[i,j] <- sum((agg.CG.sub$WINNER == rownames(network.CG.com)[i] & agg.CG.sub$LOSER == colnames(network.CG.com)[j])|
                                 (agg.CG.sub$WINNER == rownames(network.CG.com)[j] & agg.CG.sub$LOSER == colnames(network.CG.com)[i]) )
  }
}
network.CG.prop <- matrix(NA,nrow=nrow(network.CG),ncol=ncol(network.CG))
colnames(network.CG.prop) <- colnames(network.CG)
rownames(network.CG.prop) <- colnames(network.CG)
for (i in 1:nrow(network.CG.prop)) {
  for (j in 1:nrow(network.CG.prop)) {
    network.CG.prop[i,j] <- network.CG[i,j]/network.CG.com[i,j]
  }
}
network.CG.prop[network.CG.prop=="NaN"] <- 0
network.CG.prop2 <- network.CG.prop >0.65
net.CG.bin <- graph.adjacency(network.CG.com,
                              mode="directed", 
                              weighted=NULL, 
                              diag=FALSE)
ttri(network.CG.prop2)
edge_density(net.CG.bin)


network.CG.com.m <- matrix(NA,nrow=nrow(network.CG.m),ncol=ncol(network.CG.m))
colnames(network.CG.com.m) <- colnames(network.CG.m)
rownames(network.CG.com.m) <- colnames(network.CG.m)

for (i in 1:nrow(network.CG.com.m)) {
  for (j in 1:nrow(network.CG.com.m)) {
    network.CG.com.m[i,j] <- sum((agg.CG.m.sub$WINNER == rownames(network.CG.com.m)[i] & agg.CG.m.sub$LOSER == colnames(network.CG.com.m)[j])|
                                   (agg.CG.m.sub$WINNER == rownames(network.CG.com.m)[j] & agg.CG.m.sub$LOSER == colnames(network.CG.com.m)[i]))
  }
}
network.CG.prop.m <- matrix(NA,nrow=nrow(network.CG.m),ncol=ncol(network.CG.m))
colnames(network.CG.prop.m) <- colnames(network.CG.m)
rownames(network.CG.prop.m) <- colnames(network.CG.m)
for (i in 1:nrow(network.CG.prop.m)) {
  for (j in 1:nrow(network.CG.prop.m)) {
    network.CG.prop.m[i,j] <- network.CG.m[i,j]/network.CG.com.m[i,j]
  }
}
network.CG.prop.m[network.CG.prop.m=="NaN"] <- 0
network.CG.prop.m2 <- network.CG.prop.m >0.5
net.CG.bin.m <- graph.adjacency(network.CG.m,
                                mode="directed", 
                                weighted=NULL, 
                                diag=FALSE)

ttri(network.CG.prop.m2)
edge_density(net.CG.bin.m)


network.CG.com.f <- matrix(NA,nrow=nrow(network.CG.f),ncol=ncol(network.CG.f))
colnames(network.CG.com.f) <- colnames(network.CG.f)
rownames(network.CG.com.f) <- colnames(network.CG.f)

for (i in 1:nrow(network.CG.com.f)) {
  for (j in 1:nrow(network.CG.com.f)) {
    network.CG.com.f[i,j] <- sum((agg.CG.f$WINNER == rownames(network.CG.com.f)[i] & agg.CG.f$LOSER == colnames(network.CG.com.f)[j])|
                                   (agg.CG.f$WINNER == rownames(network.CG.com.f)[j] & agg.CG.f$LOSER == colnames(network.CG.com.f)[i]))
  }
}
network.CG.prop.f <- matrix(NA,nrow=nrow(network.CG.f),ncol=ncol(network.CG.f))
colnames(network.CG.prop.f) <- colnames(network.CG.f)
rownames(network.CG.prop.f) <- colnames(network.CG.f)
for (i in 1:nrow(network.CG.prop.f)) {
  for (j in 1:nrow(network.CG.prop.f)) {
    network.CG.prop.f[i,j] <- network.CG.f[i,j]/network.CG.com.f[i,j]
  }
}
network.CG.prop.f[network.CG.prop.f=="NaN"] <- 0
network.CG.prop.f2 <- network.CG.prop.m >0.65
net.CG.bin.f <- graph.adjacency(network.CG.f,
                                mode="directed", 
                                weighted=NULL, 
                                diag=FALSE)

ttri(network.CG.prop.f2)
edge_density(net.CG.bin.f)