##########################################################################################################################
#                                           Code Figure 2
#
##########################################################################################################################

# VPT= JM
# LOAD PACKAGES
library(hrbrthemes)
library(GGally)
library(viridis)
library(MASS)
library(ggplot2)
library(ggExtra)
library(RColorBrewer)
library(cowplot)
library(ggpubr)
library(ggbump)
library(tidyverse)
library(wesanderson)

# LOAD DATA
#ranks_complete <- read.csv('ranks_2019_2022_bis.csv',stringsAsFactors = T,row.names = 1)
#ranks_complete$Rank <- as.numeric(ranks_complete$Rank)
ranks_comp_l <- read.csv('ranks_2019_2022_bis_long.csv',stringsAsFactors = T)

# ranks_2019 <- ranks_complete[which(ranks_complete$Year=="2019"),]
# ranks_2022 <- ranks_complete[which(ranks_complete$Year=="2022"),]
# 
# ranks_comb <- ranks_2019
# ranks_comb$Year.2022 <- ranks_2022$Rank[match(ranks_comb$ID,
#                                              ranks_2022$ID)]
# colnames(ranks_comb)[1] <-"Year.2019"
# Vector color: red if Setosa, grey otherwise.
# ranks_comb <- ranks_complete
# isMale <- ifelse(ranks_comb$Sex=="M","darkcyan","tomato")
# 
# ranks_complete
# 
# parcoord(ranks_comb[,c(6,7)] , 
#          lty=1,
#          lwd=1,
#          lend=5,
#          col=isMale,
#          var.label=T,
#          pch=16,
#          )
# 
# 
# 
# ggparcoord(ranks_comb,
#            columns = c(6,7), 
#            groupColumn = 4, 
#            order = "allClass",
#            scale="globalminmax",
#            showPoints = TRUE, 
#            title = "",
#            alphaLines = 0.8,
#            splineFactor =T
#            
# ) + 
#   theme_ipsum()+
#   theme(
#     legend.position="right",
#     plot.title = element_text(size=13)
#   ) +
#   xlab("")+
#   ylab("Dominance Rank")+
#   #geom_point(size = 5, aes(shape = Age)) +
#   scale_color_manual(values = c("tomato",
#                                 "darkcyan"
#                                 ))

  
ranks_comp_l$ID_short <- substr(ranks_comp_l$ID, start = 1, stop = 3)

ranks_comp_l$isMale <- ifelse(ranks_comp_l$Sex=="M","tomato","darkcyan")

ranks_comp_l$Sex2[ranks_comp_l$Sex=="M"] <- "Male"
ranks_comp_l$Sex2[ranks_comp_l$Sex=="F"] <- "Female"

ggplot(ranks_comp_l, aes(Year, Rank, color = Sex2)) +
  geom_point(size = 7, aes(shape = Age)
             ) +
 # geom_text(data = ranks_complete, aes(label = ID),size = 5, hjust = 1) +
  geom_text(data = ranks_comp_l, aes(label = ID_short),size = 3, hjust = 0.5,color="black") +
  #geom_bump(size = 0.8, smooth = 15,linetype = "dashed") +
  geom_bump(mapping=aes(x=Year,y=Rank, group=ID, color=Sex2),
            size = 0.6,alpha=0.5)+
  scale_x_continuous(breaks = c(2019,2022),labels=c("2019","2022")) +
  
  theme_void() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        axis.text=element_text(size=10),
        axis.text.y= element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title=element_text(size=16),
        axis.title.y=element_text(angle=90,margin = margin(t = 0, r = 20, b = 0, l = 0)),
          plot.margin = margin(1, 1, 1, 1, "cm")) +
  labs(y = "Dominance rank",
       x = NULL) +
  scale_y_reverse()+
  labs(col="Sex")


