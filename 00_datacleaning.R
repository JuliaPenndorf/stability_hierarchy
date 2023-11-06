# LOAD PACKAGES
library(stringr)
library(reshape2)

# LOAD DATA
agg_2019 <- read.csv('soc_data_2019.csv',stringsAsFactors = F)
agg_2022 <- read.csv('soc_data_2022.csv',stringsAsFactors = F)
agg_2022$Preening[agg_2022$Preening=="y"] <- "Y"

agg_2022<-agg_2022[which(agg_2022$Loser!=""),]

# adjusting names
agg_2022$Winner[agg_2022$Winner=="1"] <- "X1"
agg_2022$Loser[agg_2022$Loser=="1"] <- "X1"

agg_2022$Winner[agg_2022$Winner=="11"] <- "X11"
agg_2022$Loser[agg_2022$Loser=="11"] <- "X11"


agg_2022$Winner[agg_2022$Winner=="12"] <- "X12"
agg_2022$Loser[agg_2022$Loser=="12"] <- "X12"

agg_2022$Winner[agg_2022$Winner=="135"] <- "X135"
agg_2022$Loser[agg_2022$Loser=="135"] <- "X135"

agg_2022$Winner[agg_2022$Winner=="143"] <- "X143"
agg_2022$Loser[agg_2022$Loser=="143"] <- "X143"

agg_2022$Winner[agg_2022$Winner=="11"] <- "X11"
agg_2022$Loser[agg_2022$Loser=="11"] <- "X11"

agg_2022$Winner[agg_2022$Winner=="2"] <- "X2"
agg_2022$Loser[agg_2022$Loser=="2"] <- "X2"

agg_2022$Winner[agg_2022$Winner=="31"] <- "X31"
agg_2022$Loser[agg_2022$Loser=="131"] <- "X31"

agg_2022$Winner[agg_2022$Winner=="35"] <- "X35"
agg_2022$Loser[agg_2022$Loser=="35"] <- "X35"

agg_2022$Winner[agg_2022$Winner=="42"] <- "X42"
agg_2022$Loser[agg_2022$Loser=="42"] <- "X42"
agg_2022$Winner[agg_2022$Winner=="4"] <- "X42" #correcting typo
agg_2022$Loser[agg_2022$Loser=="4"] <- "X42"#correcting typo

agg_2022$Winner[agg_2022$Winner=="51"] <- "X51"
agg_2022$Loser[agg_2022$Loser=="51"] <- "X51"

agg_2022$Winner[agg_2022$Winner=="52"] <- "X52"
agg_2022$Loser[agg_2022$Loser=="52"] <- "X52"

agg_2022$Winner[agg_2022$Winner=="53"] <- "X53"
agg_2022$Loser[agg_2022$Loser=="53"] <- "X53"

agg_2022$Winner[agg_2022$Winner=="78"] <- "X78"
agg_2022$Loser[agg_2022$Loser=="78"] <- "X78"

agg_2022$Winner[agg_2022$Winner=="85"] <- "X85"
agg_2022$Loser[agg_2022$Loser=="85"] <- "X85"

agg_2022$Winner[agg_2022$Winner=="105"] <- "X105"
agg_2022$Loser[agg_2022$Loser=="105"] <- "X105"

agg_2022$Winner[agg_2022$Winner=="108"] <- "X108"
agg_2022$Loser[agg_2022$Loser=="108"] <- "X108"

agg_2022$Winner[agg_2022$Winner=="136"] <- "X136"
agg_2022$Loser[agg_2022$Loser=="136"] <- "X136"

agg_2022$Winner[agg_2022$Winner=="142"] <- "X142"
agg_2022$Loser[agg_2022$Loser=="142"] <- "X142"

agg_2022$Winner[agg_2022$Winner=="31"] <- "X31"
agg_2022$Loser[agg_2022$Loser=="31"] <- "X31"

agg_2022$Winner[agg_2022$Winner=="39"] <- "X39"
agg_2022$Loser[agg_2022$Loser=="39"] <- "X39"

agg_2022$Winner[agg_2022$Winner=="40"] <- "X40"
agg_2022$Loser[agg_2022$Loser=="40"] <- "X40"

agg_2022$Winner[agg_2022$Winner=="48"] <- "X48"
agg_2022$Loser[agg_2022$Loser=="48"] <- "X48"

agg_2022$Winner[agg_2022$Winner=="5"] <- "X5"
agg_2022$Loser[agg_2022$Loser=="5"] <- "X5"

#2022
agg_2022_sub <- agg_2022[!is.na(agg_2022$Loser),]
agg_2022_sub<- agg_2022_sub[(which(agg_2022_sub$Preening!="Y" &
                                    agg_2022_sub$Behaviour!="fo<1bdl")),]

inds_2022 <- table(c(agg_2022$Winner,agg_2022$Loser))

#correcting typos
agg_2022$Winner[agg_2022$Winner=="BGw_H_CG"] <- "GB_H_CG"
agg_2022$Loser[agg_2022$Loser=="BGw_H_CG"] <- "GB_H_CG"
agg_2022$Winner[agg_2022$Winner=="GBw_H_CG"] <- "GB_H_CG"
agg_2022$Loser[agg_2022$Loser=="GBw_H_CG"] <- "GB_H_CG"

agg_2022$Winner[agg_2022$Winner=="BN_B_CG"] <- "BNw_H_CG"
agg_2022$Loser[agg_2022$Loser=="BN_B_CG"] <- "BNw_H_CG"
agg_2022$Winner[agg_2022$Winner=="BN_H_CG"] <- "BNw_H_CG"
agg_2022$Loser[agg_2022$Loser=="BN_H_CG"] <- "BNw_H_CG"
agg_2022$Winner[agg_2022$Winner=="BNw_CG"] <- "BNw_H_CG"
agg_2022$Loser[agg_2022$Loser=="BNw_CG"] <- "BNw_H_CG"
agg_2022$Winner[agg_2022$Winner=="BNsm_CG"] <- "BNw_H_CG"
agg_2022$Loser[agg_2022$Loser=="BNsm_CG"] <- "BNw_H_CG"
agg_2022$Winner[agg_2022$Winner=="BNsm_H_CG"] <- "BNw_H_CG"
agg_2022$Loser[agg_2022$Loser=="BNsm_H_CG"] <- "BNw_H_CG"
agg_2022$Winner[agg_2022$Winner=="BNwing_h_cg"] <- "BNw_H_CG"
agg_2022$Loser[agg_2022$Loser=="BNwing_h_cg"] <- "BNw_H_CG"
agg_2022$Winner[agg_2022$Winner=="BNwing_H_CG"] <- "BNw_H_CG"
agg_2022$Loser[agg_2022$Loser=="BNwing_H_CG"] <- "BNw_H_CG"

agg_2022$Winner[agg_2022$Winner=="BN_V_CG"] <- "BN_V_BG"
agg_2022$Loser[agg_2022$Loser=="BN_V_CG"] <- "BN_V_BG"

agg_2022$Winner[agg_2022$Winner=="BP_V_BG"] <- "BP_H_CG"
agg_2022$Loser[agg_2022$Loser=="BP_V_BG"] <- "BP_H_CG"

agg_2022$Winner[agg_2022$Winner=="BNG_H_CG"] <- "BNG_V_BG"
agg_2022$Loser[agg_2022$Loser=="BNG_H_CG"] <- "BNG_V_BG"

agg_2022$Winner[agg_2022$Winner=="BPB_V_BG"] <- "BPB_H_CG"
agg_2022$Loser[agg_2022$Loser=="BPB_V_BG"] <- "BPB_H_CG"

agg_2022$Winner[agg_2022$Winner=="BPN_H_VG"] <- "BPN_H_CG"
agg_2022$Loser[agg_2022$Loser=="BPN_H_VG"] <- "BPN_H_CG"

agg_2022$Winner[agg_2022$Winner=="BVO_H_BG"] <- "BVO_V_BG"
agg_2022$Loser[agg_2022$Loser=="BVO_H_BG"] <- "BVO_V_BG"

agg_2022$Winner[agg_2022$Winner=="bWt"] <- "brokenWT"
agg_2022$Loser[agg_2022$Loser=="bWt"] <- "brokenWT"

agg_2022$Winner[agg_2022$Winner=="GBO_H_CG"] <- "GBO_V_BG"
agg_2022$Loser[agg_2022$Loser=="GBO_H_CG"] <- "GBO_V_BG"
agg_2022$Winner[agg_2022$Winner=="GBO_H_BG"] <- "GBO_V_BG"
agg_2022$Loser[agg_2022$Loser=="GBO_H_BG"] <- "GBO_V_BG"

agg_2022$Winner[agg_2022$Winner=="GBV_V_bG"] <- "GBV_V_BG"
agg_2022$Loser[agg_2022$Loser=="GBV_V_bG"] <- "GBV_V_BG"

agg_2022$Winner[agg_2022$Winner=="GNG_H_CG"] <- "GNG_V_BG"
agg_2022$Loser[agg_2022$Loser=="GNG_H_CG"] <- "GNG_V_BG"
agg_2022$Winner[agg_2022$Winner=="GNG_V_CG"] <- "GNG_V_BG"
agg_2022$Loser[agg_2022$Loser=="GNG_V_CG"] <- "GNG_V_BG"

agg_2022$Winner[agg_2022$Winner=="GNV_H_BG"] <- "GNV_V_BG"
agg_2022$Loser[agg_2022$Loser=="GNV_H_BG"] <- "GNV_V_BG"

agg_2022$Winner[agg_2022$Winner=="GOB_H_CG"] <- "GOB_V_BG"
agg_2022$Loser[agg_2022$Loser=="GOB_H_CG"] <- "GOB_V_BG"
agg_2022$Winner[agg_2022$Winner=="GOB_V_CG"] <- "GOB_V_BG"
agg_2022$Loser[agg_2022$Loser=="GOB_V_CG"] <- "GOB_V_BG"

agg_2022$Winner[agg_2022$Winner=="GOP_V_BG"] <- "GOP_H_CG"
agg_2022$Loser[agg_2022$Loser=="GOP_V_BG"] <- "GOP_H_CG"

agg_2022$Winner[agg_2022$Winner=="GPB_H_VG"] <- "GPB_H_CG"
agg_2022$Loser[agg_2022$Loser=="GPB_H_VG"] <- "GPB_H_CG"
agg_2022$Winner[agg_2022$Winner=="GPB_V_BG"] <- "GPB_H_CG"
agg_2022$Loser[agg_2022$Loser=="GPB_V_BG"] <- "GPB_H_CG"

agg_2022$Winner[agg_2022$Winner=="GPG_V_BG"] <- "GPG_H_BG"
agg_2022$Loser[agg_2022$Loser=="GPG_V_BG"] <- "GPG_H_BG"

agg_2022$Winner[agg_2022$Winner=="GPN_V_BG"] <- "GPN_H_CG"
agg_2022$Loser[agg_2022$Loser=="GPN_V_BG"] <- "GPN_H_CG"
agg_2022$Winner[agg_2022$Winner=="GPN_V_CG"] <- "GPN_H_CG"
agg_2022$Loser[agg_2022$Loser=="GPN_V_CG"] <- "GPN_H_CG"
agg_2022$Winner[agg_2022$Winner=="GPN_V_VG"] <- "GPN_H_CG"
agg_2022$Loser[agg_2022$Loser=="GPN_V_VG"] <- "GPN_H_CG"

agg_2022$Winner[agg_2022$Winner=="GPV_V_BG"] <- "GPG_H_CG"
agg_2022$Loser[agg_2022$Loser=="GPV_V_BG"] <- "GPG_H_CG"

agg_2022$Winner[agg_2022$Winner=="GPN_V_BG"] <- "GPN_H_CG"
agg_2022$Loser[agg_2022$Loser=="GPN_V_BG"] <- "GPN_H_CG"
agg_2022$Winner[agg_2022$Winner=="GPN_V_CG"] <- "GPN_H_CG"
agg_2022$Loser[agg_2022$Loser=="GPN_V_CG"] <- "GPN_H_CG"
agg_2022$Winner[agg_2022$Winner=="GPN_V_VG"] <- "GPN_H_CG"
agg_2022$Loser[agg_2022$Loser=="GPN_V_VG"] <- "GPN_H_CG"

agg_2022$Winner[agg_2022$Winner=="GPV_V_BG"] <- "GPV_H_CG"
agg_2022$Loser[agg_2022$Loser=="GPV_V_BG"] <- "GPV_H_CG"
agg_2022$Winner[agg_2022$Winner=="GPV_V_CG"] <- "GPV_H_CG"
agg_2022$Loser[agg_2022$Loser=="GPV_V_CG"] <- "GPV_H_CG"
agg_2022$Winner[agg_2022$Winner=="GPV_V_CG"] <- "GPV_H_CG"
agg_2022$Loser[agg_2022$Loser=="GPV_V_CG"] <- "GPV_H_CG"

agg_2022$Winner[agg_2022$Winner=="GVG_H_CG"] <- "GVG_V_BG"
agg_2022$Loser[agg_2022$Loser=="GVG_H_CG"] <- "GVG_V_BG"

agg_2022$Winner[agg_2022$Winner=="GVP_h_CG"] <- "GVP_H_CG"
agg_2022$Loser[agg_2022$Loser=="GVP_h_CG"] <- "GVP_H_CG"
agg_2022$Winner[agg_2022$Winner=="GVP_H_BG"] <- "GVP_H_CG"
agg_2022$Loser[agg_2022$Loser=="GVP_H_BG"] <- "GVP_H_CG"
agg_2022$Winner[agg_2022$Winner=="GVP_V_BG"] <- "GVP_H_CG"
agg_2022$Loser[agg_2022$Loser=="GVP_V_BG"] <- "GVP_H_CG"
agg_2022$Winner[agg_2022$Winner=="GVP_V_CG"] <- "GVP_H_CG"
agg_2022$Loser[agg_2022$Loser=="GVP_V_CG"] <- "GVP_H_CG"

agg_2022$Winner[agg_2022$Winner=="MB_H_CG"] <- "MB_V_BG"
agg_2022$Loser[agg_2022$Loser=="MB_H_CG"] <- "MB_V_BG"
agg_2022$Winner[agg_2022$Winner=="MB_V_G"] <- "MB_V_BG"
agg_2022$Loser[agg_2022$Loser=="MB_V_G"] <- "MB_V_BG"

agg_2022$Winner[agg_2022$Winner=="MBG_H_CG"] <- "MBG_V_BG"
agg_2022$Loser[agg_2022$Loser=="MBG_H_CG"] <- "MBG_V_BG"

agg_2022$Winner[agg_2022$Winner=="MBneck_BG"] <- "MB_V_BG"
agg_2022$Loser[agg_2022$Loser==" MBneck_BG"] <- "MB_V_BG"
agg_2022$Winner[agg_2022$Winner=="MBneck_H_CG"] <- "MB_V_BG"
agg_2022$Loser[agg_2022$Loser==" MBneck_H_CG"] <- "MB_V_BG"
agg_2022$Winner[agg_2022$Winner=="MBneck_V_BG"] <- "MB_V_BG"
agg_2022$Loser[agg_2022$Loser=="MBneck_V_BG"] <- "MB_V_BG"

agg_2022$Winner[agg_2022$Winner=="MBP_V_BG"] <- "MBP_H_CG"
agg_2022$Loser[agg_2022$Loser=="MBP_V_BG"] <- "MBP_H_CG"

agg_2022$Winner[agg_2022$Winner=="MGBsm_V_BG"] <- "MGB_V_BG"
agg_2022$Loser[agg_2022$Loser=="MGBsm_V_BG"] <- "MGB_V_BG"

agg_2022$Winner[agg_2022$Winner=="MGV_H_CG"] <- "MGV_V_BG"
agg_2022$Loser[agg_2022$Loser=="MGV_H_CG"] <- "MGV_V_BG"
agg_2022$Winner[agg_2022$Winner=="MGV_H_BG"] <- "MGV_V_BG"
agg_2022$Loser[agg_2022$Loser=="MGV_H_BG"] <- "MGV_V_BG"
agg_2022$Winner[agg_2022$Winner=="MGV_V_CG"] <- "MGV_V_BG"
agg_2022$Loser[agg_2022$Loser=="MGV_V_CG"] <- "MGV_V_BG"

agg_2022$Winner[agg_2022$Winner=="MNG_V_CG"] <- "MNG_V_BG"
agg_2022$Loser[agg_2022$Loser=="MNG_V_CG"] <- "MNG_V_BG"

agg_2022$Winner[agg_2022$Winner=="MPN_V_BG"] <- "MPN_H_CG"
agg_2022$Loser[agg_2022$Loser=="MPN_V_BG"] <- "MPN_H_CG"

agg_2022$Winner[agg_2022$Winner=="MPneck_CG"] <- "PMneck_CG"
agg_2022$Loser[agg_2022$Loser=="MPneck_CG"] <- "PMneck_CG"

agg_2022$Winner[agg_2022$Winner=="NBO_H_CG"] <- "NBO_V_BG"
agg_2022$Loser[agg_2022$Loser=="NBO_H_CG"] <- "NBO_V_BG"
agg_2022$Winner[agg_2022$Winner=="NBO_H_CG"] <- "NBO_V_CG"
agg_2022$Loser[agg_2022$Loser=="NBO_H_CG"] <- "NBO_V_CG"

agg_2022$Winner[agg_2022$Winner=="NBP_V_BG"] <- "NBP_H_CG"
agg_2022$Loser[agg_2022$Loser=="NBP_V_BG"] <- "NBP_H_CG"

agg_2022$Winner[agg_2022$Winner=="NBsm_H_CG"] <- "BNw_H_CG"
agg_2022$Loser[agg_2022$Loser=="NBsm_H_CG"] <- "BNw_H_CG"
agg_2022$Winner[agg_2022$Winner=="NBwing_H_CG"] <- "BNw_H_CG"
agg_2022$Loser[agg_2022$Loser=="NBwing_H_CG"] <- "BNw_H_CG"

agg_2022$Winner[agg_2022$Winner=="NGV_V_bG"] <- "NGV_V_BG"
agg_2022$Loser[agg_2022$Loser=="NGV_V_bG"] <- "NGV_V_BG"

agg_2022$Winner[agg_2022$Winner=="NOV_B_BG"] <- "NOV_V_BG"
agg_2022$Loser[agg_2022$Loser=="NOV_B_BG"] <- "NOV_V_BG"

agg_2022$Winner[agg_2022$Winner=="NPG_h_CG"] <- "NPG_H_CG"
agg_2022$Loser[agg_2022$Loser=="NPG_h_CG"] <- "NPG_H_CG"
agg_2022$Winner[agg_2022$Winner=="NPG_V_C"] <- "NPG_H_CG"
agg_2022$Loser[agg_2022$Loser=="NPG_V_C"] <- "NPG_H_CG"

agg_2022$Winner[agg_2022$Winner=="NPV_V_BG"] <- "NPV_H_CG"
agg_2022$Loser[agg_2022$Loser=="NPV_V_BG"] <- "NPV_H_CG"

agg_2022$Winner[agg_2022$Winner=="NVB_H_BG"] <- "NVB_V_BG"
agg_2022$Loser[agg_2022$Loser=="NVB_H_BG"] <- "NVB_V_BG"

agg_2022$Winner[agg_2022$Winner=="NVP_V_CG"] <- "NVP_H_CG"
agg_2022$Loser[agg_2022$Loser=="NVP_V_CG"] <- "NVP_H_CG"

agg_2022$Winner[agg_2022$Winner=="OGMV_V_CG"] <- "OGMV_V_BG"
agg_2022$Loser[agg_2022$Loser=="OGMV_V_CG"] <- "OGMV_V_BG"

agg_2022$Winner[agg_2022$Winner=="OGV_H_CG"] <- "OGV_V_BG"
agg_2022$Loser[agg_2022$Loser=="OGV_H_CG"] <- "OGV_V_BG"

agg_2022$Winner[agg_2022$Winner=="OVG_H_CG"] <- "OVG_V_BG"
agg_2022$Loser[agg_2022$Loser=="OVG_H_CG"] <- "OGV_V_BG"
agg_2022$Winner[agg_2022$Winner=="OVG_V_CG"] <- "OVG_V_BG"
agg_2022$Loser[agg_2022$Loser=="OVG_V_CG"] <- "OGV_V_BG"

agg_2022$Winner[agg_2022$Winner=="PGV_H_VG"] <- "PGV_H_CG"
agg_2022$Loser[agg_2022$Loser=="PGV_H_VG"] <- "PGV_H_CG"

agg_2022$Winner[agg_2022$Winner=="PMB_H_BG"] <- "PMB_H_CG"
agg_2022$Loser[agg_2022$Loser=="PMB_H_BG"] <- "PMB_H_CG"

agg_2022$Winner[agg_2022$Winner=="PMG_V_BG"] <- "PMG_H_CG"
agg_2022$Loser[agg_2022$Loser=="PMG_V_BG"] <- "PMG_H_CG"

agg_2022$Winner[agg_2022$Winner=="PMneck_H_CG"] <- "PMneck_CG"
agg_2022$Loser[agg_2022$Loser=="PMneck_H_CG"] <- "PMneck_CG"

agg_2022$Winner[agg_2022$Winner=="PNG_H_VG"] <- "PNG_H_CG"
agg_2022$Loser[agg_2022$Loser=="PNG_H_VG"] <- "PNG_H_CG"
agg_2022$Winner[agg_2022$Winner=="PNG_V_BG"] <- "PNG_H_CG"
agg_2022$Loser[agg_2022$Loser=="PNG_V_BG"] <- "PNG_H_CG"

agg_2022$Winner[agg_2022$Winner=="PVB_V_BG"] <- "PVB_H_CG"
agg_2022$Loser[agg_2022$Loser=="PVB_V_BG"] <- "PVB_H_CG"

agg_2022$Winner[agg_2022$Winner=="PVBN_H_VG"] <- "PVBN_H_CG"
agg_2022$Loser[agg_2022$Loser=="PVBN_H_VG"] <- "PVBN_H_CG"

agg_2022$Winner[agg_2022$Winner=="V_BG"] <- "V_V_BG"
agg_2022$Loser[agg_2022$Loser=="V_BG"] <- "V_V_BG"

agg_2022$Winner[agg_2022$Winner=="VMV_V-BG"] <- "VMV_V_BG"
agg_2022$Loser[agg_2022$Loser=="VMV_V-BG"] <- "VMV_V_BG"

agg_2022$Winner[agg_2022$Winner=="VNG_H_CG"] <- "VNG_V_BG"
agg_2022$Loser[agg_2022$Loser=="VNG_H_CG"] <- "VNG_V_BG"

agg_2022$Winner[agg_2022$Winner=="VNM_H_BG"] <- "VNM_V_BG"
agg_2022$Loser[agg_2022$Loser=="VNM_H_BG"] <- "VNM_V_BG"

agg_2022$Winner[agg_2022$Winner=="VON_H_BG"] <- "VON_V_BG"
agg_2022$Loser[agg_2022$Loser=="VON_H_BG"] <- "VON_V_BG"

agg_2022$Winner[agg_2022$Winner=="VPG_H_BG"] <- "VPG_H_CG"
agg_2022$Loser[agg_2022$Loser=="VPG_H_BG"] <- "VPG_H_CG"

agg_2022$Winner[agg_2022$Winner=="VPM_H_VG"] <- "VPM_H_CG"
agg_2022$Loser[agg_2022$Loser=="VPM_H_VG"] <- "VPM_H_CG"
agg_2022$Winner[agg_2022$Winner=="VPM_V_BG"] <- "VPM_H_CG"
agg_2022$Loser[agg_2022$Loser=="VPM_V_BG"] <- "VPM_H_CG"

agg_2022$Winner[agg_2022$Winner=="0_V_BG"] <- "O_V_BG"
agg_2022$Loser[agg_2022$Loser=="0_V_BG"] <- "O_V_BG"

agg_2022$Winner[agg_2022$Winner=="PVBsm_V_CG"] <- "PVBsm_H_CG"
agg_2022$Loser[agg_2022$Loser=="PVBsm_V_CG"] <- "PVBsm_H_CG"

agg_2022$Winner[agg_2022$Winner=="PVBNsm_H_CG"] <- "PVBN_H_CG"
agg_2022$Loser[agg_2022$Loser=="PVBNsm_H_CG"] <- "PVBN_H_CG"

inds_2022 <- table(c(agg_2022$Winner,agg_2022$Loser))


write.csv(agg_2022,"agg_data_2022.csv")
