################################################################################################################
####                                    CLEANING SOCIAL DATA 2022


# LOAD DATA
soc_data <- read.csv('soc_data_2022 (1).csv')
scans <- read.csv('scans2022.csv', stringsAsFactors = F)
scans[is.na(scans)] <-0


colnames(scans)

scans$PGM_H_CG <- scans$PGM_H_CG+scans$PGM_H_CG.1
scans$PM_H_CG <- scans$PM_H_CG+scans$PM_H_CG.1
scans$O_V_BG <- scans$O_V_BG+scans$O_V_BG.1
scans$NMO_V_BG <- scans$NMO_V_BG+scans$NMO_V_BG.1
scans$MBG_V_BG <- scans$MBG_V_BG+scans$MBG_V_BG.1

colnames(scans)[which(names(scans) == "OVG1_V_bG")] <- "OVG1_V_BG"
colnames(scans)[which(names(scans) == "PMV._H_CG")] <- "PMV_H_CG"



scans_sub <- subset(scans,select =-c(PGM_H_CG.1,
                                     PM_H_CG.1,
                                     OVG_V_BG,
                                     O_V_BG.1,
                                     NMO_V_BG.1,
                                     MBG_V_BG.1,
                                     OMO_V_BG #double
                                     ))
                    
soc_data$W_in_scans <- soc_data$Winner %in% colnames(scans_sub) 
soc_data$L_in_scans <- soc_data$Loser %in% colnames(scans_sub)


unique(soc_data$Winner[soc_data$W_in_scans==F])


soc_data$Winner[soc_data$Winner=="35"] <- "X35"
soc_data$Loser[soc_data$Loser=="35"] <- "X35"

soc_data$Winner[soc_data$Winner=="11"] <- "X11"
soc_data$Loser[soc_data$Loser=="11"] <- "X11"

soc_data$Winner[soc_data$Winner=="143"] <- "X143"
soc_data$Loser[soc_data$Loser=="143"] <- "X143"

soc_data$Winner[soc_data$Winner=="5"] <- "X5"
soc_data$Loser[soc_data$Loser=="5"] <- "X5"

soc_data$Winner[soc_data$Winner=="51"] <- "X51"
soc_data$Loser[soc_data$Loser=="51"] <- "X51"

soc_data$Winner[soc_data$Winner=="78"] <- "X78"
soc_data$Loser[soc_data$Loser=="78"] <- "X78"

soc_data$Winner[soc_data$Winner=="42"] <- "X42"
soc_data$Loser[soc_data$Loser=="42"] <- "X42"

soc_data$Winner[soc_data$Winner=="2"] <- "X2"
soc_data$Loser[soc_data$Loser=="2"] <- "X2"

soc_data$Winner[soc_data$Winner=="52"] <- "X52"
soc_data$Loser[soc_data$Loser=="52"] <- "X52"

soc_data$Winner[soc_data$Winner=="4"] <- "X42"
soc_data$Loser[soc_data$Loser=="4"] <- "X42"

soc_data$Winner[soc_data$Winner=="39"] <- "X39"
soc_data$Loser[soc_data$Loser=="39"] <- "X39"

soc_data$Winner[soc_data$Winner=="1"] <- "X1"
soc_data$Loser[soc_data$Loser=="1"] <- "X1"

soc_data$Winner[soc_data$Winner=="135"] <- "X135"
soc_data$Loser[soc_data$Loser=="135"] <- "X135"

soc_data$Winner[soc_data$Winner=="12"] <- "X12"
soc_data$Loser[soc_data$Loser=="12"] <- "X12"

soc_data$Winner[soc_data$Winner=="31"] <- "X31"
soc_data$Loser[soc_data$Loser=="31"] <- "X31"

soc_data$Winner[soc_data$Winner=="40"] <- "X40"
soc_data$Loser[soc_data$Loser=="40"] <- "X40"

soc_data$Winner[soc_data$Winner=="53"] <- "X53"
soc_data$Loser[soc_data$Loser=="53"] <- "X53"

soc_data$Winner[soc_data$Winner=="142"] <- "X142"
soc_data$Loser[soc_data$Loser=="142"] <- "X142"

soc_data$Winner[soc_data$Winner=="85"] <- "X85"
soc_data$Loser[soc_data$Loser=="85"] <- "X85"

soc_data$Winner[soc_data$Winner=="48"] <- "X48"
soc_data$Loser[soc_data$Loser=="48"] <- "X48"

soc_data$Winner[soc_data$Winner=="0_V_BG"] <- "O_V_BG"
soc_data$Loser[soc_data$Loser=="0_V_BG"] <- "O_V_BG"

soc_data$Winner[soc_data$Winner=="0_V_BG"] <- "O_V_BG"
soc_data$Loser[soc_data$Loser=="0_V_BG"] <- "O_V_BG"

#correcting typos
soc_data$Winner[soc_data$Winner=="BGw_H_CG"] <- "GB_H_CG"
soc_data$Loser[soc_data$Loser=="BGw_H_CG"] <- "GB_H_CG"
soc_data$Winner[soc_data$Winner=="GBw_H_CG"] <- "GB_H_CG"
soc_data$Loser[soc_data$Loser=="GBw_H_CG"] <- "GB_H_CG"

soc_data$Winner[soc_data$Winner=="BN_B_CG"] <- "BNw_H_CG"
soc_data$Loser[soc_data$Loser=="BN_B_CG"] <- "BNw_H_CG"
soc_data$Winner[soc_data$Winner=="BN_H_CG"] <- "BNw_H_CG"
soc_data$Loser[soc_data$Loser=="BN_H_CG"] <- "BNw_H_CG"
soc_data$Winner[soc_data$Winner=="BNw_CG"] <- "BNw_H_CG"
soc_data$Loser[soc_data$Loser=="BNw_CG"] <- "BNw_H_CG"
soc_data$Winner[soc_data$Winner=="BNsm_CG"] <- "BNw_H_CG"
soc_data$Loser[soc_data$Loser=="BNsm_CG"] <- "BNw_H_CG"
soc_data$Winner[soc_data$Winner=="BNsm_H_CG"] <- "BNw_H_CG"
soc_data$Loser[soc_data$Loser=="BNsm_H_CG"] <- "BNw_H_CG"
soc_data$Winner[soc_data$Winner=="BNwing_h_cg"] <- "BNw_H_CG"
soc_data$Loser[soc_data$Loser=="BNwing_h_cg"] <- "BNw_H_CG"
soc_data$Winner[soc_data$Winner=="BNwing_H_CG"] <- "BNw_H_CG"
soc_data$Loser[soc_data$Loser=="BNwing_H_CG"] <- "BNw_H_CG"

soc_data$Winner[soc_data$Winner=="BN_V_CG"] <- "BN_V_BG"
soc_data$Loser[soc_data$Loser=="BN_V_CG"] <- "BN_V_BG"

soc_data$Winner[soc_data$Winner=="BP_V_BG"] <- "BP_H_CG"
soc_data$Loser[soc_data$Loser=="BP_V_BG"] <- "BP_H_CG"

soc_data$Winner[soc_data$Winner=="BNG_H_CG"] <- "BNG_V_BG"
soc_data$Loser[soc_data$Loser=="BNG_H_CG"] <- "BNG_V_BG"

soc_data$Winner[soc_data$Winner=="BPB_V_BG"] <- "BPB_H_CG"
soc_data$Loser[soc_data$Loser=="BPB_V_BG"] <- "BPB_H_CG"

soc_data$Winner[soc_data$Winner=="BPN_H_VG"] <- "BPN_H_CG"
soc_data$Loser[soc_data$Loser=="BPN_H_VG"] <- "BPN_H_CG"

soc_data$Winner[soc_data$Winner=="BVO_H_BG"] <- "BVO_V_BG"
soc_data$Loser[soc_data$Loser=="BVO_H_BG"] <- "BVO_V_BG"

soc_data$Winner[soc_data$Winner=="bWt"] <- "brokenWT"
soc_data$Loser[soc_data$Loser=="bWt"] <- "brokenWT"

soc_data$Winner[soc_data$Winner=="GBO_H_CG"] <- "GBO_V_BG"
soc_data$Loser[soc_data$Loser=="GBO_H_CG"] <- "GBO_V_BG"
soc_data$Winner[soc_data$Winner=="GBO_H_BG"] <- "GBO_V_BG"
soc_data$Loser[soc_data$Loser=="GBO_H_BG"] <- "GBO_V_BG"

soc_data$Winner[soc_data$Winner=="GBV_V_bG"] <- "GBV_V_BG"
soc_data$Loser[soc_data$Loser=="GBV_V_bG"] <- "GBV_V_BG"

soc_data$Winner[soc_data$Winner=="GNG_H_CG"] <- "GNG_V_BG"
soc_data$Loser[soc_data$Loser=="GNG_H_CG"] <- "GNG_V_BG"
soc_data$Winner[soc_data$Winner=="GNG_V_CG"] <- "GNG_V_BG"
soc_data$Loser[soc_data$Loser=="GNG_V_CG"] <- "GNG_V_BG"

soc_data$Winner[soc_data$Winner=="GNV_H_BG"] <- "GNV_V_BG"
soc_data$Loser[soc_data$Loser=="GNV_H_BG"] <- "GNV_V_BG"

soc_data$Winner[soc_data$Winner=="GOB_H_CG"] <- "GOB_V_BG"
soc_data$Loser[soc_data$Loser=="GOB_H_CG"] <- "GOB_V_BG"
soc_data$Winner[soc_data$Winner=="GOB_V_CG"] <- "GOB_V_BG"
soc_data$Loser[soc_data$Loser=="GOB_V_CG"] <- "GOB_V_BG"

soc_data$Winner[soc_data$Winner=="GOP_V_BG"] <- "GOP_H_CG"
soc_data$Loser[soc_data$Loser=="GOP_V_BG"] <- "GOP_H_CG"

soc_data$Winner[soc_data$Winner=="GPB_H_VG"] <- "GPB_H_CG"
soc_data$Loser[soc_data$Loser=="GPB_H_VG"] <- "GPB_H_CG"
soc_data$Winner[soc_data$Winner=="GPB_V_BG"] <- "GPB_H_CG"
soc_data$Loser[soc_data$Loser=="GPB_V_BG"] <- "GPB_H_CG"

soc_data$Winner[soc_data$Winner=="GPG_V_BG"] <- "GPG_H_BG"
soc_data$Loser[soc_data$Loser=="GPG_V_BG"] <- "GPG_H_BG"

soc_data$Winner[soc_data$Winner=="GPN_V_BG"] <- "GPN_H_CG"
soc_data$Loser[soc_data$Loser=="GPN_V_BG"] <- "GPN_H_CG"
soc_data$Winner[soc_data$Winner=="GPN_V_CG"] <- "GPN_H_CG"
soc_data$Loser[soc_data$Loser=="GPN_V_CG"] <- "GPN_H_CG"
soc_data$Winner[soc_data$Winner=="GPN_V_VG"] <- "GPN_H_CG"
soc_data$Loser[soc_data$Loser=="GPN_V_VG"] <- "GPN_H_CG"

soc_data$Winner[soc_data$Winner=="GPV_V_BG"] <- "GPG_H_CG"
soc_data$Loser[soc_data$Loser=="GPV_V_BG"] <- "GPG_H_CG"

soc_data$Winner[soc_data$Winner=="GPN_V_BG"] <- "GPN_H_CG"
soc_data$Loser[soc_data$Loser=="GPN_V_BG"] <- "GPN_H_CG"
soc_data$Winner[soc_data$Winner=="GPN_V_CG"] <- "GPN_H_CG"
soc_data$Loser[soc_data$Loser=="GPN_V_CG"] <- "GPN_H_CG"
soc_data$Winner[soc_data$Winner=="GPN_V_VG"] <- "GPN_H_CG"
soc_data$Loser[soc_data$Loser=="GPN_V_VG"] <- "GPN_H_CG"

soc_data$Winner[soc_data$Winner=="GPV_V_BG"] <- "GPV_H_CG"
soc_data$Loser[soc_data$Loser=="GPV_V_BG"] <- "GPV_H_CG"
soc_data$Winner[soc_data$Winner=="GPV_V_CG"] <- "GPV_H_CG"
soc_data$Loser[soc_data$Loser=="GPV_V_CG"] <- "GPV_H_CG"
soc_data$Winner[soc_data$Winner=="GPV_V_CG"] <- "GPV_H_CG"
soc_data$Loser[soc_data$Loser=="GPV_V_CG"] <- "GPV_H_CG"

soc_data$Winner[soc_data$Winner=="GVG_H_CG"] <- "GVG_V_BG"
soc_data$Loser[soc_data$Loser=="GVG_H_CG"] <- "GVG_V_BG"

soc_data$Winner[soc_data$Winner=="GVP_h_CG"] <- "GVP_H_CG"
soc_data$Loser[soc_data$Loser=="GVP_h_CG"] <- "GVP_H_CG"
soc_data$Winner[soc_data$Winner=="GVP_H_BG"] <- "GVP_H_CG"
soc_data$Loser[soc_data$Loser=="GVP_H_BG"] <- "GVP_H_CG"
soc_data$Winner[soc_data$Winner=="GVP_V_BG"] <- "GVP_H_CG"
soc_data$Loser[soc_data$Loser=="GVP_V_BG"] <- "GVP_H_CG"
soc_data$Winner[soc_data$Winner=="GVP_V_CG"] <- "GVP_H_CG"
soc_data$Loser[soc_data$Loser=="GVP_V_CG"] <- "GVP_H_CG"

soc_data$Winner[soc_data$Winner=="MB_H_CG"] <- "MB_V_BG"
soc_data$Loser[soc_data$Loser=="MB_H_CG"] <- "MB_V_BG"
soc_data$Winner[soc_data$Winner=="MB_V_G"] <- "MB_V_BG"
soc_data$Loser[soc_data$Loser=="MB_V_G"] <- "MB_V_BG"

soc_data$Winner[soc_data$Winner=="MBG_H_CG"] <- "MBG_V_BG"
soc_data$Loser[soc_data$Loser=="MBG_H_CG"] <- "MBG_V_BG"

soc_data$Winner[soc_data$Winner=="MBneck_BG"] <- "MB_V_BG"
soc_data$Loser[soc_data$Loser==" MBneck_BG"] <- "MB_V_BG"
soc_data$Winner[soc_data$Winner=="MBneck_H_CG"] <- "MB_V_BG"
soc_data$Loser[soc_data$Loser==" MBneck_H_CG"] <- "MB_V_BG"
soc_data$Winner[soc_data$Winner=="MBneck_V_BG"] <- "MB_V_BG"
soc_data$Loser[soc_data$Loser=="MBneck_V_BG"] <- "MB_V_BG"

soc_data$Winner[soc_data$Winner=="MBP_V_BG"] <- "MBP_H_CG"
soc_data$Loser[soc_data$Loser=="MBP_V_BG"] <- "MBP_H_CG"

soc_data$Winner[soc_data$Winner=="MGBsm_V_BG"] <- "MGB_V_BG"
soc_data$Loser[soc_data$Loser=="MGBsm_V_BG"] <- "MGB_V_BG"

soc_data$Winner[soc_data$Winner=="MGV_H_CG"] <- "MGV_V_BG"
soc_data$Loser[soc_data$Loser=="MGV_H_CG"] <- "MGV_V_BG"
soc_data$Winner[soc_data$Winner=="MGV_H_BG"] <- "MGV_V_BG"
soc_data$Loser[soc_data$Loser=="MGV_H_BG"] <- "MGV_V_BG"
soc_data$Winner[soc_data$Winner=="MGV_V_CG"] <- "MGV_V_BG"
soc_data$Loser[soc_data$Loser=="MGV_V_CG"] <- "MGV_V_BG"

soc_data$Winner[soc_data$Winner=="MNG_V_CG"] <- "MNG_V_BG"
soc_data$Loser[soc_data$Loser=="MNG_V_CG"] <- "MNG_V_BG"

soc_data$Winner[soc_data$Winner=="MPN_V_BG"] <- "MPN_H_CG"
soc_data$Loser[soc_data$Loser=="MPN_V_BG"] <- "MPN_H_CG"

soc_data$Winner[soc_data$Winner=="MPneck_CG"] <- "PMneck_CG"
soc_data$Loser[soc_data$Loser=="MPneck_CG"] <- "PMneck_CG"

soc_data$Winner[soc_data$Winner=="NBO_H_CG"] <- "NBO_V_BG"
soc_data$Loser[soc_data$Loser=="NBO_H_CG"] <- "NBO_V_BG"
soc_data$Winner[soc_data$Winner=="NBO_H_CG"] <- "NBO_V_CG"
soc_data$Loser[soc_data$Loser=="NBO_H_CG"] <- "NBO_V_CG"

soc_data$Winner[soc_data$Winner=="NBP_V_BG"] <- "NBP_H_CG"
soc_data$Loser[soc_data$Loser=="NBP_V_BG"] <- "NBP_H_CG"

soc_data$Winner[soc_data$Winner=="NBsm_H_CG"] <- "BNw_H_CG"
soc_data$Loser[soc_data$Loser=="NBsm_H_CG"] <- "BNw_H_CG"
soc_data$Winner[soc_data$Winner=="NBwing_H_CG"] <- "BNw_H_CG"
soc_data$Loser[soc_data$Loser=="NBwing_H_CG"] <- "BNw_H_CG"

soc_data$Winner[soc_data$Winner=="NGV_V_bG"] <- "NGV_V_BG"
soc_data$Loser[soc_data$Loser=="NGV_V_bG"] <- "NGV_V_BG"

soc_data$Winner[soc_data$Winner=="NOV_B_BG"] <- "NOV_V_BG"
soc_data$Loser[soc_data$Loser=="NOV_B_BG"] <- "NOV_V_BG"

soc_data$Winner[soc_data$Winner=="NPG_h_CG"] <- "NPG_H_CG"
soc_data$Loser[soc_data$Loser=="NPG_h_CG"] <- "NPG_H_CG"
soc_data$Winner[soc_data$Winner=="NPG_V_C"] <- "NPG_H_CG"
soc_data$Loser[soc_data$Loser=="NPG_V_C"] <- "NPG_H_CG"

soc_data$Winner[soc_data$Winner=="NPV_V_BG"] <- "NPV_H_CG"
soc_data$Loser[soc_data$Loser=="NPV_V_BG"] <- "NPV_H_CG"

soc_data$Winner[soc_data$Winner=="NVB_H_BG"] <- "NVB_V_BG"
soc_data$Loser[soc_data$Loser=="NVB_H_BG"] <- "NVB_V_BG"

soc_data$Winner[soc_data$Winner=="NVP_V_CG"] <- "NVP_H_CG"
soc_data$Loser[soc_data$Loser=="NVP_V_CG"] <- "NVP_H_CG"

soc_data$Winner[soc_data$Winner=="OGMV_V_CG"] <- "OGMV_V_BG"
soc_data$Loser[soc_data$Loser=="OGMV_V_CG"] <- "OGMV_V_BG"

soc_data$Winner[soc_data$Winner=="OGV_H_CG"] <- "OGV_V_BG"
soc_data$Loser[soc_data$Loser=="OGV_H_CG"] <- "OGV_V_BG"

soc_data$Winner[soc_data$Winner=="OVG_H_CG"] <- "OVG_V_BG"
soc_data$Loser[soc_data$Loser=="OVG_H_CG"] <- "OGV_V_BG"
soc_data$Winner[soc_data$Winner=="OVG_V_CG"] <- "OVG_V_BG"
soc_data$Loser[soc_data$Loser=="OVG_V_CG"] <- "OGV_V_BG"

soc_data$Winner[soc_data$Winner=="PGV_H_VG"] <- "PGV_H_CG"
soc_data$Loser[soc_data$Loser=="PGV_H_VG"] <- "PGV_H_CG"

soc_data$Winner[soc_data$Winner=="PMB_H_BG"] <- "PMB_H_CG"
soc_data$Loser[soc_data$Loser=="PMB_H_BG"] <- "PMB_H_CG"

soc_data$Winner[soc_data$Winner=="PMG_V_BG"] <- "PMG_H_CG"
soc_data$Loser[soc_data$Loser=="PMG_V_BG"] <- "PMG_H_CG"

soc_data$Winner[soc_data$Winner=="PMneck_H_CG"] <- "PMneck_CG"
soc_data$Loser[soc_data$Loser=="PMneck_H_CG"] <- "PMneck_CG"

soc_data$Winner[soc_data$Winner=="PNG_H_VG"] <- "PNG_H_CG"
soc_data$Loser[soc_data$Loser=="PNG_H_VG"] <- "PNG_H_CG"
soc_data$Winner[soc_data$Winner=="PNG_V_BG"] <- "PNG_H_CG"
soc_data$Loser[soc_data$Loser=="PNG_V_BG"] <- "PNG_H_CG"

soc_data$Winner[soc_data$Winner=="PVB_V_BG"] <- "PVB_H_CG"
soc_data$Loser[soc_data$Loser=="PVB_V_BG"] <- "PVB_H_CG"

soc_data$Winner[soc_data$Winner=="PVBN_H_VG"] <- "PVBN_H_CG"
soc_data$Loser[soc_data$Loser=="PVBN_H_VG"] <- "PVBN_H_CG"

soc_data$Winner[soc_data$Winner=="V_BG"] <- "V_V_BG"
soc_data$Loser[soc_data$Loser=="V_BG"] <- "V_V_BG"

soc_data$Winner[soc_data$Winner=="VMV_V-BG"] <- "VMV_V_BG"
soc_data$Loser[soc_data$Loser=="VMV_V-BG"] <- "VMV_V_BG"

soc_data$Winner[soc_data$Winner=="VNG_H_CG"] <- "VNG_V_BG"
soc_data$Loser[soc_data$Loser=="VNG_H_CG"] <- "VNG_V_BG"

soc_data$Winner[soc_data$Winner=="VNM_H_BG"] <- "VNM_V_BG"
soc_data$Loser[soc_data$Loser=="VNM_H_BG"] <- "VNM_V_BG"

soc_data$Winner[soc_data$Winner=="VON_H_BG"] <- "VON_V_BG"
soc_data$Loser[soc_data$Loser=="VON_H_BG"] <- "VON_V_BG"

soc_data$Winner[soc_data$Winner=="VPG_H_BG"] <- "VPG_H_CG"
soc_data$Loser[soc_data$Loser=="VPG_H_BG"] <- "VPG_H_CG"

soc_data$Winner[soc_data$Winner=="VPM_H_VG"] <- "VPM_H_CG"
soc_data$Loser[soc_data$Loser=="VPM_H_VG"] <- "VPM_H_CG"
soc_data$Winner[soc_data$Winner=="VPM_V_BG"] <- "VPM_H_CG"
soc_data$Loser[soc_data$Loser=="VPM_V_BG"] <- "VPM_H_CG"

soc_data$Winner[soc_data$Winner=="PVBsm_V_CG"] <- "PVBsm_H_CG"
soc_data$Loser[soc_data$Loser=="PVBsm_V_CG"] <- "PVBsm_H_CG"

soc_data$Winner[soc_data$Winner=="PVBNsm_H_CG"] <- "PVBN_H_CG"
soc_data$Loser[soc_data$Loser=="PVBNsm_H_CG"] <- "PVBN_H_CG"

soc_data$Winner[soc_data$Winner=="PVBNsm_H_CG"] <- "PVBN_H_CG"
soc_data$Loser[soc_data$Loser=="PVBNsm_H_CG"] <- "PVBN_H_CG"

soc_data$Winner[soc_data$Winner=="BPN_H_CG"] <- "BPN_V_CG"
soc_data$Loser[soc_data$Loser=="BPN_H_CG"] <- "BPN_V_CG" 

soc_data$Winner[soc_data$Winner=="PVBsm_H_CG"] <- "PVB_H_CG"
soc_data$Loser[soc_data$Loser=="PVBsm_H_CG"] <- "PVB_H_CG" 

soc_data$Winner[soc_data$Winner=="PMneck_CG"] <- "PMneck_H_CG"
soc_data$Loser[soc_data$Loser=="PMneck_CG"] <- "PMneck_H_CG" 

soc_data$Winner[soc_data$Winner=="BPG_H_CG"] <- "BPG_V_CG"
soc_data$Loser[soc_data$Loser=="BPG_H_CG"] <- "BPG_V_CG" 

soc_data$Winner[soc_data$Winner=="Nsh_V_BG"] <- "Nsh_BG"
soc_data$Loser[soc_data$Loser=="Nsh_V_BG"] <- "Nsh_BG" 

soc_data$Winner[soc_data$Winner=="Mwing_H_CG"] <- "Mwing_BG"
soc_data$Loser[soc_data$Loser=="Mwing_H_CG"] <- "Mwing_BG" 

soc_data$Winner[soc_data$Winner=="PGM_V_BG"] <- "PGM_H_CG"
soc_data$Loser[soc_data$Loser=="PGM_V_BG"] <- "PGM_H_CG" 

soc_data$Winner[soc_data$Winner=="PGB_V_BG"] <- "PGB_H_CG"
soc_data$Loser[soc_data$Loser=="PGB_V_BG"] <- "PGB_H_CG" 

soc_data$Winner[soc_data$Winner=="OGV1_V_BG"] <- "OVG1_V_BG"
soc_data$Loser[soc_data$Loser=="OGV1_V_BG"] <- "OVG1_V_BG" 


soc_data$Winner[soc_data$Winner=="NBw_CG"] <- "NBw_H_CG"
soc_data$Loser[soc_data$Loser=="NBw_CG"] <- "NBw_H_CG" 

soc_data$Winner[soc_data$Winner=="OV2_V_BG"] <- "OV_V_BG"
soc_data$Loser[soc_data$Loser=="OV2_V_BG"] <- "OV_V_BG" 

soc_data$Winner[soc_data$Winner=="OBGO_V_BG"] <- "OBG_V_BG"
soc_data$Loser[soc_data$Loser=="OBGO_V_BG"] <- "OBG_V_BG" 

soc_data$Winner[soc_data$Winner=="PGN_V_CG"] <- "PGN_H_CG"
soc_data$Loser[soc_data$Loser=="PGN_V_CG"] <- "PGN_H_CG" 

soc_data$Winner[soc_data$Winner=="OVBN_H_CG"] <- "OVB_H_CG"
soc_data$Loser[soc_data$Loser=="OVBN_H_CG"] <- "OVB_H_CG" 

soc_data$Winner[soc_data$Winner=="NVneck_BG"] <- "NV_V_BG"
soc_data$Loser[soc_data$Loser=="NVneck_BG"] <- "NV_V_BG" 

soc_data$Winner[soc_data$Winner=="OMBO_V_BG"] <- "OBMO_V_BG"
soc_data$Loser[soc_data$Loser=="OMBO_V_BG"] <- "OBMO_V_BG" 

soc_data$Winner[soc_data$Winner=="OGV2_V_BG"] <- "OVG2_V_BG"
soc_data$Loser[soc_data$Loser=="OGV2_V_BG"] <- "OVG2_V_BG"

soc_data$Winner[soc_data$Winner=="OVB_H_CG"] <- "OVB_V_BG"
soc_data$Loser[soc_data$Loser=="OVB_H_CG"] <- "OVB_V_BG"

soc_data$Winner[soc_data$Winner=="MBneck_H_CG"] <- "MB_V_BG"
soc_data$Loser[soc_data$Loser=="MBneck_H_CG"] <- "MB_V_BG"

soc_data$Winner[soc_data$Winner=="NBO_V_CG"] <- "NBO_V_BG"
soc_data$Loser[soc_data$Loser=="NBO_V_CG"] <- "NBO_V_BG"

soc_data$Winner[soc_data$Winner=="PVM_V_BG"] <- "PVM_H_CG"
soc_data$Loser[soc_data$Loser=="PVM_V_BG"] <- "PVM_H_CG"

soc_data$Winner[soc_data$Winner=="Nneck_V_BG"] <- "Nneck_BG"
soc_data$Loser[soc_data$Loser=="Nneck_V_BG"] <- "Nneck_BG"

soc_data$Winner[soc_data$Winner=="NB_V_BG"] <- "NB_V_CG"
soc_data$Loser[soc_data$Loser=="NB_V_BG"] <- "NB_V_CG"

soc_data$Winner[soc_data$Winner=="OMOsm_V_BG"] <- "OMO_V_BG"
soc_data$Loser[soc_data$Loser=="OMOsm_V_BG"] <- "OMO_V_BG"

soc_data$Winner[soc_data$Winner=="VPBsm_H_CG"] <- "VPB_H_CG"
soc_data$Loser[soc_data$Loser=="VPBsm_H_CG"] <- "VPB_H_CG"

soc_data$Winner[soc_data$Winner=="MONO_V_BG"] <- "MON_V_BG"
soc_data$Loser[soc_data$Loser=="MONO_V_BG"] <- "MON_V_BG"

soc_data$Winner[soc_data$Winner=="136"] <- "X136"
soc_data$Loser[soc_data$Loser=="136"] <- "X136"

soc_data$Winner[soc_data$Winner=="105"] <- "X105"
soc_data$Loser[soc_data$Loser=="105"] <- "X105"

soc_data$Winner[soc_data$Winner=="MPB_V_BG"] <- "MPB_H_CG"
soc_data$Loser[soc_data$Loser=="MPB_V_BG"] <- "MPB_H_CG"

soc_data$Winner[soc_data$Winner=="PVNB_H_CG"] <- "PVNB_H_CG"
soc_data$Loser[soc_data$Loser=="PVNB_H_CG"] <- "PVNB_H_CG"

soc_data$Winner[soc_data$Winner=="NPG_V_CG"] <- "NPG_H_CG"
soc_data$Loser[soc_data$Loser=="NPG_V_CG"] <- "NPG_H_CG"

soc_data$Winner[soc_data$Winner=="MBneck_BG"] <- "MB_V_BG"
soc_data$Loser[soc_data$Loser=="MBneck_BG"] <- "MB_V_BG"

soc_data$Winner[soc_data$Winner=="GPG_H_BG"] <- "GPG_H_CG"
soc_data$Loser[soc_data$Loser=="GPG_H_BG"] <- "GPG_H_CG"

soc_data$Winner[soc_data$Winner=="108"] <- "X108"
soc_data$Loser[soc_data$Loser=="108"] <- "X108"

soc_data$Winner[soc_data$Winner=="GPM_H_CG"] <- "GPM_V_BG"
soc_data$Loser[soc_data$Loser=="GPM_H_CG"] <- "GPM_V_BG"

soc_data$W_in_scans <- soc_data$Winner %in% colnames(scans_sub) 
soc_data$L_in_scans <- soc_data$Loser %in% colnames(scans_sub)

unique(soc_data$Winner[soc_data$W_in_scans==F])
unique(soc_data$Loser[soc_data$L_in_scans==F])

soc_data_sub <- soc_data[which(soc_data$W_in_scans==T &
                               soc_data$L_in_scans==T),]
write.csv(soc_data_sub,'agg_data_cleaned.csv')
write.csv(scans_sub,'scans_cleaned.csv')
