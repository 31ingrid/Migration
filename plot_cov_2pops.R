library(ggplot2)

setwd("/Users/ingridspies/Documents/GOA_cod/SimulationResults/cod_m.2_Niter100_Nrep3/")
Niter=100
m=seq(0,.2,.01)
#Means_survey is mean significant comparisons with obs. error, fishery selectivity and maturity applied
Means_survey=matrix(0,8,11)
#Means_raw is just the actual numbers with maturity applied
Means_raw=matrix(0,8,11)

for(popstat in 1:8){
 for (exp in 1:11){
 x=read.csv(paste("outMAT_popstat=",popstat,"_exp=",exp,".csv",sep=""))  
 Means_survey[popstat,exp]=m[max(which(x$rowMeans.Mean_mat.<.95*Niter))]
 Means_raw[popstat,exp]=m[max(which(x$rowMeans.Mean_raw_mat.<.95*Niter))]
  }
}
#dimensions are 
#number of rows is the length of m (seq(0,0.04,0.001)).

#plot survey data results

DFmean <- data.frame(Popsizes = c("A. 6,834, 6,834", "B. 6,834, 683*", "C. 6,834, 68*", "D. 683, 6,834*","A. 6,834, 6,834", "B. 6,834, 683*", "C. 6,834, 68*", "D. 683, 6,834*"),Means_survey)
DFmean$Popsizes = c("A. 6,834, 6,834", "B. 6,834, 683*", "C. 6,834, 68*", "D. 683, 6,834*","A. 6,834, 6,834", "B. 6,834, 683*", "C. 6,834, 68*", "D. 683, 6,834*")
DFmean$Group=c("A","B","C","D","E","F","G","H")
DFlong=reshape2::melt(DFmean)
colnames(DFlong)[3]=c("Mean")
DFlong$Type=rep(c(rep("One way",4),rep("Two way",4)),11)
DFlong$variable=c(rep("Base case",8),rep("M=0.02, M=0.02",8),rep("M=0.04, M=0.02",8),rep("M=0.02, M=0.04",8),rep("Earlier maturity",8),rep("Earlier/Later Maturity",8),rep("Later/Earlier Maturity",8),rep("Lower sigmaR .25,.25",8),rep("Higher sigmaR 0.66,0.66",8),rep("SigmaR 0.66,0.25",8),rep("SigmaR 0.25,0.66",8))
DFlong$variable = factor(DFlong$variable, levels=c("Base case","M=0.02, M=0.02","M=0.04, M=0.02","M=0.02, M=0.04","Earlier maturity","Earlier/Later Maturity","Later/Earlier Maturity","Lower sigmaR .25,.25","Higher sigmaR 0.66,0.66","SigmaR 0.66,0.25","SigmaR 0.25,0.66")) 
DFlong$Corr=.2-DFlong$value
DFlong2=reshape2::melt(DFlong,value.name="value")
colnames(DFlong2)[5]="Exp"
DFlong2$variable=str_replace(DFlong2$variable,"Corr","Correlated")
DFlong2$variable=str_replace(DFlong2$variable,"value","Uncorrelated")
DFlong2$Popsizes=as.factor(DFlong2$Popsizes)
DFlong2$Type=as.factor(DFlong2$Type)
DFlong2$variable=as.factor(DFlong2$variable)

ggplot(DFlong, aes(x = Type, y = value, fill =as.factor(Popsizes)), width=0.75) + #ylim(c(0, 0.10))+
 labs(y = "Correlation calculated on populations with observation error, survey selectivity, mature fish only", x = NULL, fill = NULL) +
 geom_bar(stat = "identity",position="dodge") +
 facet_wrap(~variable) +
 coord_flip() +ggtitle("Pacific cod, lowest migration rate at which population sizes are correlated")+
 theme_bw() + theme( strip.background  = element_blank(), panel.grid.major = element_line(colour = "grey80"),panel.border = element_blank(),axis.ticks = element_line(size = 0),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank() ) +
 theme(legend.position="bottom")+scale_fill_brewer(palette="Set2")

ggplot(DFlong2,aes(x=Group,y=value,fill=variable))+geom_bar(stat="identity")+facet_wrap(~Exp)+theme_bw()+
xlab("Correlation calculated on populations with observation error, \nsurvey selectivity, mature fish only")+
 ylab("Migration rate")+
 coord_flip()+
 ggtitle("Pacific cod, lowest migration rate at which population sizes are correlated")+
 theme( strip.background  = element_blank(), panel.grid.major = element_line(colour = "grey80"),panel.border = element_blank(),axis.ticks = element_line(size = 0),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank() )+
 theme(legend.position="right")

ggplot(data = DFlong2[which(DFlong2$Exp=="Base case"),], aes(x = Group, y=value,fill = Popsizes, pattern = variable)) +
 geom_bar_pattern(stat="identity",
                  color = "black", 
                  pattern_fill = "black",
                  pattern_angle = 45,
                  pattern_density = 0.1,
                  pattern_spacing = 0.025,
                  pattern_key_scale_factor = 0.6) + 
 scale_pattern_manual(values = c(Uncorrelated = "stripe", Correlated = "none"))+
 guides(pattern = guide_legend(override.aes = list(fill = "white")),
        fill = guide_legend(override.aes = list(pattern = "none")))+
 labs(x = "Correlation calculated on populations with observation error, \nsurvey selectivity, mature fish only", y = "Migration Rate", pattern = "Result",fill="Relative population size")+
 scale_x_discrete(labels=c('','One way', '','','','Two way','',''))+theme_bw()

ggplot(data = DFlong2, aes(x = Group, y=value,fill = Popsizes, pattern =variable)) +
 geom_bar_pattern(stat="identity",
                  color = "black", 
                  pattern_fill = "black",
                  pattern_angle = 45,
                  pattern_density = 0.1,
                  pattern_spacing = 0.025,
                  pattern_key_scale_factor = 0.6) + 
 scale_pattern_manual(values = c(Uncorrelated = "stripe", Correlated = "none"))+
 guides(pattern = guide_legend(override.aes = list(fill = "white")),
        fill = guide_legend(override.aes = list(pattern = "none")))+
 labs(x = "Correlation calculated on populations with observation error, \nsurvey selectivity, mature fish only", y = "Migration Rate", pattern = "Result",fill="Relative population size")+
 scale_x_discrete(labels=c('','One way', '','','','Two way','',''))+facet_wrap(~Exp)+theme_bw()+
 theme( strip.background  = element_blank(), panel.grid.major = element_line(colour = "grey80"),panel.border = element_blank(),axis.ticks = element_line(size = 0),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank())+
ggtitle("Pacific cod correlation in population size relative to migration")

#plot raw data results
DFmean <- data.frame(Popsizes = c("A. 6,834, 6,834", "B. 6,834, 683*", "C. 6,834, 68*", "D. 683, 6,834*","A. 6,834, 6,834", "B. 6,834, 683*", "C. 6,834, 68*", "D. 683, 6,834*"),Means_raw)
DFmean$Popsizes = c("A. 6,834, 6,834", "B. 6,834, 683*", "C. 6,834, 68*", "D. 683, 6,834*","A. 6,834, 6,834", "B. 6,834, 683*", "C. 6,834, 68*", "D. 683, 6,834*")
DFmean$Group=c("A","B","C","D","E","F","G","H")
DFlong=reshape2::melt(DFmean)
colnames(DFlong)[3]=c("Mean")
DFlong$Type=rep(c(rep("One way",4),rep("Two way",4)),11)
DFlong$variable=c(rep("Base case",8),rep("M=0.02, M=0.02",8),rep("M=0.04, M=0.02",8),rep("M=0.02, M=0.04",8),rep("Earlier maturity",8),rep("Earlier/Later Maturity",8),rep("Later/Earlier Maturity",8),rep("Lower sigmaR .25,.25",8),rep("Higher sigmaR 0.66,0.66",8),rep("SigmaR 0.66,0.25",8),rep("SigmaR 0.25,0.66",8))
DFlong$variable = factor(DFlong$variable, levels=c("Base case","M=0.02, M=0.02","M=0.04, M=0.02","M=0.02, M=0.04","Earlier maturity","Earlier/Later Maturity","Later/Earlier Maturity","Lower sigmaR .25,.25","Higher sigmaR 0.66,0.66","SigmaR 0.66,0.25","SigmaR 0.25,0.66")) 
DFlong$Corr=.2-DFlong$value
DFlong2=reshape2::melt(DFlong,value.name="value")
colnames(DFlong2)[5]="Exp"
DFlong2$variable=str_replace(DFlong2$variable,"Corr","Correlated")
DFlong2$variable=str_replace(DFlong2$variable,"value","Uncorrelated")
DFlong2$Popsizes=as.factor(DFlong2$Popsizes)
DFlong2$Type=as.factor(DFlong2$Type)
DFlong2$variable=as.factor(DFlong2$variable)
colnames(DFlong2)[6]="Result"

ggplot(data = DFlong2, aes(x = Group, y=value,fill = Popsizes, pattern = Result)) +
 geom_bar_pattern(stat="identity",
                  color = "black", 
                  pattern_fill = "black",
                  pattern_angle = 45,
                  pattern_density = 0.1,
                  pattern_spacing = 0.025,
                  pattern_key_scale_factor = 0.6) + 
 # scale_fill_manual(values = colorRampPalette(c("#0066CC","#FFFFFF","#FF8C00"))(4)) +
 scale_pattern_manual(values = c(Uncorrelated = "stripe", Correlated = "none"))+
 guides(pattern = guide_legend(override.aes = list(fill = "white")),
        fill = guide_legend(override.aes = list(pattern = "none")))+
 labs(x = "Correlation calculated on populations with mature fish only", y = "Migration Rate", pattern = "Result",fill="Relative population size")+
 scale_x_discrete(labels=c('','One way', '','','','Two way','',''))+facet_wrap(~Exp)+theme_bw()+
 theme( strip.background  = element_blank(), panel.grid.major = element_line(colour = "grey80"),panel.border = element_blank(),axis.ticks = element_line(size = 0),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank())+
 ggtitle("Pacific cod correlation in population size relative to migration")


setwd("/Users/ingridspies/Documents/GOA_cod/SimulationResults/blackspotted_m.1_Niter100_Nrep3/")
Niter=100
m=seq(0,.1,.005)
#Means_survey is mean significant comparisons with obs. error, fishery selectivity and maturity applied
Means_survey=matrix(0,8,11)
#Means_raw is just the actual numbers with maturity applied
Means_raw=matrix(0,8,11)

for(popstat in 1:8){
 for (exp in 1:11){
  x=read.csv(paste("outMAT_popstat=",popstat,"_exp=",exp,".csv",sep=""))  
  Means_survey[popstat,exp]=m[max(which(x$rowMeans.Mean_mat.<.95*Niter))]
  Means_raw[popstat,exp]=m[max(which(x$rowMeans.Mean_raw_mat.<.95*Niter))]
 }
}
#dimensions are 
#number of rows is the length of m (seq(0,0.04,0.001)).

#plot survey data results
DFmean <- data.frame(Popsizes = c("A. 434, 750*", "B. 750, 434*", "C. 217, 750*", "D. 750, 217*","E. 434, 750", "F. 217, 750", "G. 868, 750", "H. 434, 1500"),Means_survey)
DFlong=reshape2::melt(DFmean)
colnames(DFlong)[3]=c("Mean")
DFlong$Type=rep(c(rep("One way",4),rep("Two way",4)),11)
DFlong$variable=c(rep("Base case",8),rep("M=0.02, M=0.02",8),rep("M=0.04, M=0.02",8),rep("M=0.02, M=0.04",8),rep("Earlier maturity",8),rep("Earlier/Later Maturity",8),rep("Later/Earlier Maturity",8),rep("Lower sigmaR .25,.25",8),rep("Higher sigmaR 0.66,0.66",8),rep("SigmaR 0.66,0.25",8),rep("SigmaR 0.25,0.66",8))
DFlong$variable = factor(DFlong$variable, levels=c("Base case","M=0.02, M=0.02","M=0.04, M=0.02","M=0.02, M=0.04","Earlier maturity","Earlier/Later Maturity","Later/Earlier Maturity","Lower sigmaR .25,.25","Higher sigmaR 0.66,0.66","SigmaR 0.66,0.25","SigmaR 0.25,0.66")) 
DFlong$variable = factor(DFlong$variable, levels=c("Base case","M=0.02, M=0.02","M=0.04, M=0.02","M=0.02, M=0.04","Earlier maturity","Earlier/Later Maturity","Later/Earlier Maturity","Lower sigmaR .25,.25","Higher sigmaR 0.66,0.66","SigmaR 0.66,0.25","SigmaR 0.25,0.66")) 
DFlong$Corr=.1-DFlong$Mean
DFlong2=reshape2::melt(DFlong,value.name="value")
colnames(DFlong2)[2]="Exp"
DFlong2$variable=str_replace(DFlong2$variable,"Corr","Correlated")
DFlong2$variable=str_replace(DFlong2$variable,"Mean","Uncorrelated")
DFlong2$Popsizes=as.factor(DFlong2$Popsizes)
DFlong2$Type=as.factor(DFlong2$Type)
DFlong2$variable=as.factor(DFlong2$variable)

ggplot(DFlong, aes(x = Type, y = Mean, fill =as.factor(Popsizes)), width=0.75) + 
 labs(y = "Correlation calculated on populations with observation error, survey selectivity, mature fish only", x = NULL, fill = NULL) +
 geom_bar(stat = "identity",position="dodge") +
 facet_wrap(~variable) +
 coord_flip() +ggtitle("Blackspotted rockfish, lowest migration rate at which population sizes are correlated")+
 theme_bw() + theme( strip.background  = element_blank(), panel.grid.major = element_line(colour = "grey80"),panel.border = element_blank(),axis.ticks = element_line(size = 0),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank() ) +
 theme(legend.position="bottom")+scale_fill_brewer(palette="Set2")

ggplot(data = DFlong2, aes(x = Popsizes, y=value,fill = Popsizes, pattern = variable)) +
 geom_bar_pattern(stat="identity",
                  color = "black", 
                  pattern_fill = "black",
                  pattern_angle = 45,
                  pattern_density = 0.1,
                  pattern_spacing = 0.025,
                  pattern_key_scale_factor = 0.6) + 
 # scale_fill_manual(values = colorRampPalette(c("#0066CC","#FFFFFF","#FF8C00"))(4)) +
 scale_pattern_manual(values = c(Uncorrelated = "stripe", Correlated = "none"))+
 guides(pattern = guide_legend(override.aes = list(fill = "white")),
        fill = guide_legend(override.aes = list(pattern = "none")))+
 labs(x = "Correlation calculated on populations with observation error, \nsurvey selectivity, mature fish only", y = "Migration Rate", pattern = "Result",fill="Relative population size")+
 scale_x_discrete(labels=c('','One way', '','','','Two way','',''))+facet_wrap(~Exp)+theme_bw()+
 theme( strip.background  = element_blank(), panel.grid.major = element_line(colour = "grey80"),panel.border = element_blank(),axis.ticks = element_line(size = 0),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank())+
 ggtitle("Blackspotted rockfish correlation in population size relative to migration")

#plot raw data results
DFmean <- data.frame(Popsizes = c("A. 434, 750*", "B. 750, 434*", "C. 217, 750*", "D. 750, 217*","E. 434, 750", "F. 217, 750", "G. 868, 750", "H. 434, 1500"),Means_raw)
DFlong=reshape2::melt(DFmean)
colnames(DFlong)[3]=c("Mean")
DFlong$Type=rep(c(rep("One way",4),rep("Two way",4)),11)
DFlong$variable=c(rep("Base case",8),rep("M=0.02, M=0.02",8),rep("M=0.04, M=0.02",8),rep("M=0.02, M=0.04",8),rep("Earlier maturity",8),rep("Earlier/Later Maturity",8),rep("Later/Earlier Maturity",8),rep("Lower sigmaR .25,.25",8),rep("Higher sigmaR 0.66,0.66",8),rep("SigmaR 0.66,0.25",8),rep("SigmaR 0.25,0.66",8))
DFlong$variable = factor(DFlong$variable, levels=c("Base case","M=0.02, M=0.02","M=0.04, M=0.02","M=0.02, M=0.04","Earlier maturity","Earlier/Later Maturity","Later/Earlier Maturity","Lower sigmaR .25,.25","Higher sigmaR 0.66,0.66","SigmaR 0.66,0.25","SigmaR 0.25,0.66")) 
DFlong$variable = factor(DFlong$variable, levels=c("Base case","M=0.02, M=0.02","M=0.04, M=0.02","M=0.02, M=0.04","Earlier maturity","Earlier/Later Maturity","Later/Earlier Maturity","Lower sigmaR .25,.25","Higher sigmaR 0.66,0.66","SigmaR 0.66,0.25","SigmaR 0.25,0.66")) 
DFlong$Corr=.1-DFlong$Mean
DFlong2=reshape2::melt(DFlong,value.name="value")
colnames(DFlong2)[2]="Exp"
DFlong2$variable=str_replace(DFlong2$variable,"Corr","Correlated")
DFlong2$variable=str_replace(DFlong2$variable,"Mean","Uncorrelated")
DFlong2$Popsizes=as.factor(DFlong2$Popsizes)
DFlong2$Type=as.factor(DFlong2$Type)
DFlong2$variable=as.factor(DFlong2$variable)

ggplot(DFlong, aes(x = Type, y = Mean, fill =as.factor(Popsizes)), width=0.75) + 
 labs(y = "Correlation calculated on model estimates of population size, mature fish only", x = NULL, fill = NULL) +
 geom_bar(stat = "identity",position="dodge") +
 facet_wrap(~variable) +
 coord_flip() +ggtitle("Blackspotted rockfish, lowest migration rate at which population sizes are correlated")+
 theme_bw() + theme( strip.background  = element_blank(), panel.grid.major = element_line(colour = "grey80"),panel.border = element_blank(),axis.ticks = element_line(size = 0),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank() ) +
 theme(legend.position="bottom")+scale_fill_brewer(palette="Set2")

ggplot(data = DFlong2, aes(x = Popsizes, y=value,fill = Popsizes, pattern = variable)) +
 geom_bar_pattern(stat="identity",
                  color = "black", 
                  pattern_fill = "black",
                  pattern_angle = 45,
                  pattern_density = 0.1,
                  pattern_spacing = 0.025,
                  pattern_key_scale_factor = 0.6) + 
 # scale_fill_manual(values = colorRampPalette(c("#0066CC","#FFFFFF","#FF8C00"))(4)) +
 scale_pattern_manual(values = c(Uncorrelated = "stripe", Correlated = "none"))+
 guides(pattern = guide_legend(override.aes = list(fill = "white")),
        fill = guide_legend(override.aes = list(pattern = "none")))+
 labs(x = "Correlation calculated on populations with mature fish only", y = "Migration Rate", pattern = "Result",fill="Relative population size")+
 scale_x_discrete(labels=c('','One way', '','','','Two way','',''))+facet_wrap(~Exp)+theme_bw()+
 theme( strip.background  = element_blank(), panel.grid.major = element_line(colour = "grey80"),panel.border = element_blank(),axis.ticks = element_line(size = 0),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank())+
 ggtitle("Blackspotted rockfish correlation in population size relative to migration")

#plot comparisons for blackspooted that only vary by order of magnitude not ratio
#note relpop3 is with a different protocol to add observation error.
setwd("/Users/ingridspies/Documents/GOA_cod/SimulationResults/blackspotted_relpop4/")
Niter=100
m=seq(0,.1,.005)
#Means_survey is mean significant comparisons with obs. error, fishery selectivity and maturity applied
Means_survey=vector()
#Means_raw is just the actual numbers with maturity applied
Means_raw=vector()

for(popstat in 1:8){
 for (exp in 1:1){
  x=read.csv(paste("outMAT_popstat=",popstat,"_exp=",exp,".csv",sep=""))  
  Means_survey[popstat]=m[max(which(x$rowMeans.Mean_mat.<.95*Niter))]
  Means_raw[popstat]=m[max(which(x$rowMeans.Mean_raw_mat.<.95*Niter))]
 }
}
#dimensions are 
#number of rows is the length of m (seq(0,0.04,0.001)).

#plot survey data results

DFmean <- data.frame(Popsizes = c("A. 43, 75*", "B. 434, 750*", "C. 4340, 7500*",  "D. 43400, 75000*","A. 43, 75*","B. 434, 750*", "C. 4340, 7500*",  "D. 43400, 75000*"),Means_survey[c(3,1,2,4,7,5,6,8)])
DFlong=reshape2::melt(DFmean)
#DFvar <- data.frame(Popsizes = c("A. 14,000, 14,000", "B. 14,000, 1400*", "C. 14,000, 140*", "D. 1400, 14,000*"),var[1:8,2:12])
#DFlongV=reshape2::melt(DFvar)
colnames(DFlong)[3]=c("Mean")
DFlong$Type=rep(c(rep("One way",4),rep("Two way",4)),1)
#DFlong$variable=c(rep("Base case",8),rep("M=0.02, M=0.02",8),rep("M=0.04, M=0.02",8),rep("M=0.02, M=0.04",8),rep("Earlier maturity",8),rep("Earlier/Later Maturity",8),rep("Later/Earlier Maturity",8),rep("Lower sigmaR .25,.25",8),rep("Higher sigmaR 0.66,0.66",8),rep("SigmaR 0.66,0.25",8),rep("SigmaR 0.25,0.66",8))
#DFlong$variable = factor(DFlong$variable, levels=c("Base case","M=0.02, M=0.02","M=0.04, M=0.02","M=0.02, M=0.04","Earlier maturity","Earlier/Later Maturity","Later/Earlier Maturity","Lower sigmaR .25,.25","Higher sigmaR 0.66,0.66","SigmaR 0.66,0.25","SigmaR 0.25,0.66")) 
ggplot(DFlong, aes(x = Type, y = Mean, fill =as.factor(Popsizes)), width=0.75) + 
 labs(y = "Lowest migration rate at which \npopulation sizes are correlated \nobservation error, survey selectivity, \nmature fish only", x = NULL, fill = NULL) +
 geom_bar(stat = "identity",position="dodge") +
 ggtitle("Blackspotted rockfish, mature fish with \nobservation error, survey selectivity applied")+
 theme_bw() + theme( strip.background  = element_blank(), panel.grid.major = element_line(colour = "grey80"),panel.border = element_blank(),axis.ticks = element_line(size = 0),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank() ) +
 theme(legend.position="bottom")+scale_fill_brewer(palette="Set2")

#plot survey data results

DFmean <- data.frame(Popsizes = c("A. 43, 75*", "B. 434, 750*", "C. 4340, 7500*",  "D. 43400, 75000*","A. 43, 75*","B. 434, 750*", "C. 4340, 7500*",  "D. 43400, 75000*"),Means_raw[c(3,1,2,4,7,5,6,8)])
DFlong=reshape2::melt(DFmean)
#DFvar <- data.frame(Popsizes = c("A. 14,000, 14,000", "B. 14,000, 1400*", "C. 14,000, 140*", "D. 1400, 14,000*"),var[1:8,2:12])
#DFlongV=reshape2::melt(DFvar)
colnames(DFlong)[3]=c("Mean")
DFlong$Type=rep(c(rep("One way",4),rep("Two way",4)),1)
#DFlong$variable=c(rep("Base case",8),rep("M=0.02, M=0.02",8),rep("M=0.04, M=0.02",8),rep("M=0.02, M=0.04",8),rep("Earlier maturity",8),rep("Earlier/Later Maturity",8),rep("Later/Earlier Maturity",8),rep("Lower sigmaR .25,.25",8),rep("Higher sigmaR 0.66,0.66",8),rep("SigmaR 0.66,0.25",8),rep("SigmaR 0.25,0.66",8))
#DFlong$variable = factor(DFlong$variable, levels=c("Base case","M=0.02, M=0.02","M=0.04, M=0.02","M=0.02, M=0.04","Earlier maturity","Earlier/Later Maturity","Later/Earlier Maturity","Lower sigmaR .25,.25","Higher sigmaR 0.66,0.66","SigmaR 0.66,0.25","SigmaR 0.25,0.66")) 
ggplot(DFlong, aes(x = Type, y = Mean, fill =as.factor(Popsizes)), width=0.75) + 
 labs(y = "Lowest migration rate at which \npopulation sizes are correlated", x = NULL, fill = NULL) +
 geom_bar(stat = "identity",position="dodge") +
 ggtitle("Blackspotted rockfish, mature fish (raw data)")+
 theme_bw() + theme( strip.background  = element_blank(), panel.grid.major = element_line(colour = "grey80"),panel.border = element_blank(),axis.ticks = element_line(size = 0),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank() ) +
 theme(legend.position="bottom")+scale_fill_brewer(palette="Set2")


