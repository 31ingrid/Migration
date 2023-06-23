library(ggplot2)
library(tidyverse)
library(reshape2)
library(ggpattern)



setwd("/Users/ingrid.spies/Documents/GOA_cod/SimulationResults/cod_m.5_Niter100_Nrep3/")
Niter=100
m=seq(0,.5,.02)#FILL THESE IN
max=.5
lenM=length(m)
#Means_survey is mean significant comparisons with obs. error, fishery selectivity and maturity applied
Means_survey=matrix(0,8,11)
#Means_raw is just the actual numbers with maturity applied
Means_raw=matrix(0,8,11)

detailed_survey=matrix(0,length(m),8);colnames(detailed_survey)=rep(seq(1,4,1),2)
detailed_raw=matrix(0,length(m),8)

#This one gives you the minimum at which 80% correlation is reached.
TEST=vector()
for(popstat in 1:8){
 for (exp in 1:11){
  x=read.csv(paste("outMAT_popstat=",popstat,"_exp=",exp,".csv",sep=""))  
  if(exp==1){
    detailed_survey[,popstat]=x$rowMeans.Mean_mat.   
 #   detailed_survey[,popstat+2]=x$rowVars.Mean_mat.
    detailed_raw[,popstat]=x$rowMeans.Mean_raw_mat.
#    detailed_raw[,popstat+2]=x$rowVars.Mean_raw_mat.
    }
    Means_survey[popstat,exp]=x$m[max(which(round(x$rowMeans.Mean_mat.)<.9*Niter))]
  for(i in 1:length(m)){TEST[i]=x$rowMeans.Mean_mat.[i]>=.79*100}
  if(sum(TEST)>0){Means_survey[popstat,exp]=x$m[min(which(TEST==TRUE))]}else(Means_survey[popstat,exp]=max(m))
  for(i in 1:length(m)){TEST[i]=x$rowMeans.Mean_raw_mat.[i]>=.79*100}
  if(sum(TEST)>0){Means_raw[popstat,exp]=x$m[min(which(TEST==TRUE))]}else(Means_raw[popstat,exp]=max(m))
  Means_raw[popstat,exp]=x$m[max(which(round(x$rowMeans.Mean_raw_mat.)<.79*Niter))]
 }
}

#only for base case (simulated survey data)
detailed_survey2=reshape2::melt(detailed_survey)
detailed_survey2$Direction=c(rep("One-way",104),rep("Two-way",104))
detailed_survey2$Migration=rep(m,8)
detailed_survey2$Popsizes = rep(c(rep("A. 6,834, 6,834",length(m)),rep("B. 6,834, 683*",length(m)),rep("C. 6,834, 68*",length(m)),rep("D. 683, 6,834*",length(m))),2)
ggplot(detailed_survey2)+geom_line(aes(x=Migration,y=value,col=Popsizes))+facet_wrap(~as.factor(Direction))+theme_bw()+
 ylab("Percentage with significant correlation")+
 ggtitle("Pacific cod correlation in population size relative to one and two-way migration\n (simulated survey data)")

#only for base case (raw data)
detailed_raw2=reshape2::melt(detailed_raw)
detailed_raw2$Direction=c(rep("One-way",104),rep("Two-way",104))
detailed_raw2$Migration=rep(m,8)
detailed_raw2$Popsizes = rep(c(rep("A. 6,834, 6,834",length(m)),rep("B. 6,834, 683*",length(m)),rep("C. 6,834, 68*",length(m)),rep("D. 683, 6,834*",length(m))),2)
ggplot(detailed_raw2)+geom_line(aes(x=Migration,y=value,col=Popsizes))+facet_wrap(~as.factor(Direction))+theme_bw()+
 ylab("Percentage with significant correlation")+
 ggtitle("Pacific cod correlation in population size relative to one and two-way migration\n (raw data)")

#Make the ine plot for survey and raw
detailed_both=rbind(detailed_raw2,detailed_survey2)
detailed_both$Type=c(rep("Raw",208),rep("Survey",208))
colnames(detailed_both)[6]="Sizes"
ggplot(detailed_both)+geom_line(aes(x=Migration,y=value,col=Sizes,linetype=Type))+facet_wrap(~as.factor(Direction))+theme_bw()+
  ylab("Percentage with significant correlation")+
  ggtitle("Pacific cod correlation in population size relative to one and two-way migration")

#dimensions of detailed_raw2 are 8 sensitivities * length of m (seq(0,0.04,0.001))

DFmean <- data.frame(Popsizes = c("A. 6,834, 6,834", "B. 6,834, 683*", "C. 6,834, 68*", "D. 683, 6,834*","A. 6,834, 6,834", "B. 6,834, 683*", "C. 6,834, 68*", "D. 683, 6,834*"),Means_survey)
DFmean$Popsizes = c("A. 6,834, 6,834", "B. 6,834, 683*", "C. 6,834, 68*", "D. 683, 6,834*","A. 6,834, 6,834", "B. 6,834, 683*", "C. 6,834, 68*", "D. 683, 6,834*")
DFmean$Group=c("A","B","C","D","E","F","G","H")
DFlong=reshape2::melt(DFmean)
#colnames(DFlong)[4]=c("Mean")
DFlong$Type=rep(c(rep("One way",4),rep("Two way",4)),11)
DFlong$variable=c(rep("Base case",8),rep("M=0.25, M=0.25",8),rep("M=0.55, M=0.25",8),rep("M=0.25, M=0.55",8),rep("Earlier maturity",8),rep("Earlier/Later Maturity",8),rep("Later/Earlier Maturity",8),rep("Lower sigmaR .25, .25",8),rep("Higher sigmaR 0.85, 0.85",8),rep("SigmaR 0.85, 0.25",8),rep("SigmaR 0.25, 0.85",8))
DFlong$variable = factor(DFlong$variable, levels=c("Base case","M=0.25, M=0.25","M=0.55, M=0.25","M=0.25, M=0.55","Earlier maturity","Earlier/Later Maturity","Later/Earlier Maturity","Lower sigmaR .25, .25","Higher sigmaR 0.85, 0.85","SigmaR 0.85, 0.25","SigmaR 0.25, 0.85")) 
DFlong$Corr=max-DFlong$value
DFlong2=reshape2::melt(DFlong,value.name="value")
colnames(DFlong2)[3]="Exp"
DFlong2$variable=str_replace(DFlong2$variable,"Corr","Correlated")
DFlong2$variable=str_replace(DFlong2$variable,"value","Uncorrelated")
DFlong2$Popsizes=as.factor(DFlong2$Popsizes)
DFlong2$Type=as.factor(DFlong2$Type)
DFlong2$variable=as.factor(DFlong2$variable)

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

Tab_cod=matrix(DFlong2$value[1:88],8,11)
colnames(Tab_cod)=unique(DFlong$variable)
rownames(Tab_cod)=DFmean$Popsizes

#This is the threshold at which 80% of 100 populations went from uncorrelated to correlated.
write.csv((Tab_cod),"/Users/ingrid.spies/Documents/GOA_cod/SimulationResults/Tab_cod.csv")

#Blackspotted 55 out of 88 tests had correlation eventually.
mean(Tab_BS[which(Tab_BS<.5)])#0.093 was the mean rate that correlation occurred
hist(Tab_BS[which(Tab_BS<.5)],main="Blackspotted rockfish",xlab="Migration rate",xlim=c(0,.5))

#Cod 68 out of 88 tests had correlation eventually.
#correlation happened more commonly at small numbers
mean(Tab_cod[which(Tab_cod<.5)]) #Mean = 0.047
hist(Tab_cod[which(Tab_cod<.5)],main="Pacific cod",xlab="Migration rate",xlim=c(0,.5))

Freq_both=data.frame(Rate=c(Tab_BS[which(Tab_BS<.5)],Tab_cod[which(Tab_cod<.5)]),Species=c(rep("Pacific cod",74),rep("Blackspotted rockfish",56)))
#Also Underlying distributino of whether it happens or not.

ggplot(data=Freq_both,aes(x=Rate))+geom_histogram(aes(fill=Species),position="dodge")+theme_bw()+ylab("Frequency")+xlab("Migration Rate")

#plot raw data results
DFmean <- data.frame(Popsizes = c("A. 6,834, 6,834", "B. 6,834, 683*", "C. 6,834, 68*", "D. 683, 6,834*","A. 6,834, 6,834", "B. 6,834, 683*", "C. 6,834, 68*", "D. 683, 6,834*"),Means_raw)
DFmean$Popsizes = c("A. 6,834, 6,834", "B. 6,834, 683*", "C. 6,834, 68*", "D. 683, 6,834*","A. 6,834, 6,834", "B. 6,834, 683*", "C. 6,834, 68*", "D. 683, 6,834*")
DFmean$Group=c("A","B","C","D","E","F","G","H")
DFlong=reshape2::melt(DFmean)
#colnames(DFlong)[3]=c("Mean")
DFlong$Type=rep(c(rep("One way",4),rep("Two way",4)),11)
DFlong$variable=c(rep("Base case",8),rep("M=0.25, M=0.25",8),rep("M=0.55, M=0.25",8),rep("M=0.25, M=0.55",8),rep("Earlier maturity",8),rep("Earlier/Later Maturity",8),rep("Later/Earlier Maturity",8),rep("Lower sigmaR .25, .25",8),rep("Higher sigmaR 0.85, 0.85",8),rep("SigmaR 0.85, 0.25",8),rep("SigmaR 0.25, 0.85",8))
DFlong$variable = factor(DFlong$variable, levels=c("Base case","M=0.25, M=0.25","M=0.55, M=0.25","M=0.25, M=0.55","Earlier maturity","Earlier/Later Maturity","Later/Earlier Maturity","Lower sigmaR .25, .25","Higher sigmaR 0.85, 0.85","SigmaR 0.85, 0.25","SigmaR 0.25, 0.85")) 
DFlong$Corr=max-DFlong$value
DFlong2=reshape2::melt(DFlong,value.name="value")
colnames(DFlong2)[3]="Exp"
DFlong2$variable=str_replace(DFlong2$variable,"Corr","Correlated")
DFlong2$variable=str_replace(DFlong2$variable,"value","Uncorrelated")
DFlong2$Popsizes=as.factor(DFlong2$Popsizes)
DFlong2$Type=as.factor(DFlong2$Type)
DFlong2$variable=as.factor(DFlong2$variable)
colnames(DFlong2)[5]="Result"

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

setwd("/Users/ingrid.spies/Documents/GOA_cod/SimulationResults/blackspotted_m.5_Niter100_Nrep3/")
Niter=100
m=seq(0,.5,.02)#FILL THESE IN
max=.5
lenM=length(m)
#Means_survey is mean significant comparisons with obs. error, fishery selectivity and maturity applied
Means_survey=matrix(0,8,11)
#Means_raw is just the actual numbers with maturity applied
Means_raw=matrix(0,8,11)

detailed_survey=matrix(0,length(m),8);colnames(detailed_survey)=rep(seq(1,4,1),2)
detailed_raw=matrix(0,length(m),8)

#This one gives you the minimum at which 80% correlation is reached.
for(popstat in 1:8){
  for (exp in 1:11){
    x=read.csv(paste("outMAT_popstat=",popstat,"_exp=",exp,".csv",sep=""))  
    if(exp==1){
      detailed_survey[,popstat]=x$rowMeans.Mean_mat.   
      #   detailed_survey[,popstat+2]=x$rowVars.Mean_mat.
      detailed_raw[,popstat]=x$rowMeans.Mean_raw_mat.
      #    detailed_raw[,popstat+2]=x$rowVars.Mean_raw_mat.
    }
    Means_survey[popstat,exp]=x$m[max(which(round(x$rowMeans.Mean_mat.)<.9*Niter))]
    for(i in 1:length(m)){TEST[i]=x$rowMeans.Mean_mat.[i]>=.79*100}
    if(sum(TEST)>0){Means_survey[popstat,exp]=x$m[min(which(TEST==TRUE))]}else(Means_survey[popstat,exp]=max(m))
    for(i in 1:length(m)){TEST[i]=x$rowMeans.Mean_raw_mat.[i]>=.79*100}
    if(sum(TEST)>0){Means_raw[popstat,exp]=x$m[min(which(TEST==TRUE))]}else(Means_raw[popstat,exp]=max(m))
    Means_raw[popstat,exp]=x$m[max(which(round(x$rowMeans.Mean_raw_mat.)<.79*Niter))]
  }
}

#only for base case (simulated survey data)
detailed_survey2=reshape2::melt(detailed_survey)
detailed_survey2$Direction=c(rep("One-way",104),rep("Two-way",104))
detailed_survey2$Migration=rep(m,8)
detailed_survey2$Popsizes = rep(c(rep("A. 750, 750",length(m)),rep("B. 430, 750*",length(m)),rep("C. 75, 750*",length(m)),rep("D. 750, 75*",length(m))),2)
ggplot(detailed_survey2)+geom_line(aes(x=Migration,y=value,col=Popsizes))+facet_wrap(~as.factor(Direction))+theme_bw()+
  ylab("Percentage with significant correlation")+
  ggtitle("Blackspotted rockfish correlation in population size relative to one and\ntwo-way migration (simulated survey data)")

#only for base case (raw data)
detailed_raw2=reshape2::melt(detailed_raw)
detailed_raw2$Direction=c(rep("One-way",104),rep("Two-way",104))
detailed_raw2$Migration=rep(m,8)
detailed_raw2$Popsizes = rep(c(rep("A. 750, 750",length(m)),rep("B. 430, 750*",length(m)),rep("C. 75, 750*",length(m)),rep("D. 750, 75*",length(m))),2)
ggplot(detailed_raw2)+geom_line(aes(x=Migration,y=value,col=Popsizes))+facet_wrap(~as.factor(Direction))+theme_bw()+
  ylab("Percentage with significant correlation")+
  ggtitle("Blackspotted rockfish correlation in population size relative to one and two-way migration\n (raw data)")

#Make the ine plot for survey and raw
detailed_both=rbind(detailed_raw2,detailed_survey2)
detailed_both$Type=c(rep("Raw",208),rep("Survey",208))
colnames(detailed_both)[6]="Sizes"
ggplot(detailed_both)+geom_line(aes(x=Migration,y=value,col=Sizes,linetype=Type))+facet_wrap(~as.factor(Direction))+theme_bw()+
  ylab("Percentage with significant correlation")+
  ggtitle("Blackspotted rockfish correlation in population size relative to one and two-way\nmigration")


#plot survey data results
DFmean <- data.frame(Popsizes = c("A. 750, 750*", "B. 430, 750*", "C. 75, 750*", "D. 750, 75*","A. 750, 750*", "B. 430, 750*", "C. 75, 750*", "D. 750, 75*"),Means_survey)
DFmean$Group=c("A","B","C","D","E","F","G","H")
DFlong=reshape2::melt(DFmean)
colnames(DFlong)[4]=c("Mean")
DFlong$Type=rep(c(rep("One way",4),rep("Two way",4)),11)
DFlong$variable=c(rep("Base case",8),rep("M=0.02, M=0.02",8),rep("M=0.04, M=0.02",8),rep("M=0.02, M=0.04",8),rep("Earlier maturity",8),rep("Earlier/Later Maturity",8),rep("Later/Earlier Maturity",8),rep("Lower sigmaR .25,.25",8),rep("Higher sigmaR 0.85,0.85",8),rep("SigmaR 0.85,0.25",8),rep("SigmaR 0.25,0.85",8))
DFlong$variable = factor(DFlong$variable, levels=c("Base case","M=0.02, M=0.02","M=0.04, M=0.02","M=0.02, M=0.04","Earlier maturity","Earlier/Later Maturity","Later/Earlier Maturity","Lower sigmaR .25,.25","Higher sigmaR 0.85,0.85","SigmaR 0.85,0.25","SigmaR 0.25,0.85")) 
DFlong$Corr=max-DFlong$Mean
DFlong2=reshape2::melt(DFlong,value.name="value")
colnames(DFlong2)[3]="Exp"
DFlong2$variable=str_replace(DFlong2$variable,"Corr","Correlated")
DFlong2$variable=str_replace(DFlong2$variable,"Mean","Uncorrelated")
DFlong$Popsizes=as.factor(DFlong$Popsizes)
DFlong$Type=as.factor(DFlong$Type)
#DFlong2$variable=as.factor(DFlong2$variable)

ggplot(DFlong2, aes(x = Type, y = value, fill =as.factor(Popsizes)), width=0.75) + 
 labs(y = "Correlation calculated on populations with observation error, survey selectivity, mature fish only", x = NULL, fill = NULL) +
 geom_bar(stat = "identity",position="dodge") +
 facet_wrap(~Exp) +
 coord_flip() +ggtitle("Blackspotted rockfish, lowest migration rate at which population sizes are correlated")+
 theme_bw() + theme( strip.background  = element_blank(), panel.grid.major = element_line(colour = "grey80"),panel.border = element_blank(),axis.ticks = element_line(size = 0),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank() ) +
 theme(legend.position="bottom")+scale_fill_brewer(palette="Set2")

ggplot(data = DFlong2, aes(x = Group, y=value,fill = Popsizes, pattern = variable))+
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

Tab_BS=matrix(DFlong2$value[1:88],8,11)
colnames(Tab_BS)=unique(DFlong$variable)
rownames(Tab_BS)=DFmean$Popsizes

#This is the threshold at which 80% of 100 populations went from uncorrelated to correlated.
write.csv((Tab_BS),"/Users/ingrid.spies/Documents/GOA_cod/SimulationResults/Tab_BS.csv")

#plot raw data results
DFmean <- data.frame(Popsizes = c("A. 750, 750*", "B. 430, 750*", "C. 75, 750*", "D. 750, 75*","A. 750, 750*", "B. 430, 750*", "C. 75, 750*", "D. 750, 75*"),Means_raw)

DFmean$Group=c("A","B","C","D","E","F","G","H")
DFlong=reshape2::melt(DFmean)
colnames(DFlong)[4]=c("Mean")
DFlong$Type=rep(c(rep("One way",4),rep("Two way",4)),11)
DFlong$variable=c(rep("Base case",8),rep("M=0.02, M=0.02",8),rep("M=0.04, M=0.02",8),rep("M=0.02, M=0.04",8),rep("Earlier maturity",8),rep("Earlier/Later Maturity",8),rep("Later/Earlier Maturity",8),rep("Lower sigmaR .25,.25",8),rep("Higher sigmaR 0.85,0.85",8),rep("SigmaR 0.85,0.25",8),rep("SigmaR 0.25,0.85",8))
DFlong$variable = factor(DFlong$variable, levels=c("Base case","M=0.02, M=0.02","M=0.04, M=0.02","M=0.02, M=0.04","Earlier maturity","Earlier/Later Maturity","Later/Earlier Maturity","Lower sigmaR .25,.25","Higher sigmaR 0.85,0.85","SigmaR 0.85,0.25","SigmaR 0.25,0.85")) 
DFlong$variable = factor(DFlong$variable, levels=c("Base case","M=0.02, M=0.02","M=0.04, M=0.02","M=0.02, M=0.04","Earlier maturity","Earlier/Later Maturity","Later/Earlier Maturity","Lower sigmaR .25,.25","Higher sigmaR 0.85,0.85","SigmaR 0.85,0.25","SigmaR 0.25,0.85")) 
DFlong$Corr=max-DFlong$Mean
DFlong2=reshape2::melt(DFlong,value.name="value")
colnames(DFlong2)[3]="Exp"
DFlong2$variable=str_replace(DFlong2$variable,"Corr","Correlated")
DFlong2$variable=str_replace(DFlong2$variable,"Mean","Uncorrelated")
DFlong2$Popsizes=as.factor(DFlong2$Popsizes)
DFlong2$Type=as.factor(DFlong2$Type)
DFlong2$variable=as.factor(DFlong2$variable)

ggplot(data = DFlong2, aes(x = Group, y=value,fill = Popsizes, pattern = variable)) +
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


setwd("/Users/ingrid.spies/Documents/GOA_cod/SimulationResults/blackspotted_forcesizes/")
Niter=100
m=seq(0,.5,.02)
max=.5
lenM=length(m)
#Means_survey is mean significant comparisons with obs. error, fishery selectivity and maturity applied
Means_survey=matrix(0,8,11)
#Means_raw is just the actual numbers with maturity applied
Means_raw=matrix(0,8,11)

detailed_survey=matrix(0,length(m),8);colnames(detailed_survey)=rep(seq(1,4,1),2)
detailed_raw=matrix(0,length(m),8)

TEST=vector()
for(popstat in 1:2){
  for (exp in 1:1){
    x=read.csv(paste("outMAT_popstat=",popstat,"_exp=",exp,".csv",sep=""))  
    if(exp==1){
      detailed_survey[,popstat]=x$rowMeans.Mean_mat.   
      detailed_survey[,popstat+2]=x$rowVars.Mean_mat.
      detailed_raw[,popstat]=x$rowMeans.Mean_raw_mat.
      detailed_raw[,popstat+2]=x$rowVars.Mean_raw_mat.}
    #Means_survey[popstat,exp]=x$m[max(which(round(x$rowMeans.Mean_mat.)<.9*Niter))]
    for(i in 1:length(m)){TEST[i]=x$rowMeans.Mean_mat.[i]>=.89*100}
    if(sum(TEST)>0){Means_survey[popstat,exp]=x$m[min(which(TEST==TRUE))]}else(Means_survey[popstat,exp]=max(m))
    for(i in 1:length(m)){TEST[i]=x$rowMeans.Mean_raw_mat.[i]>=.89*100}
    if(sum(TEST)>0){Means_raw[popstat,exp]=x$m[min(which(TEST==TRUE))]}else(Means_raw[popstat,exp]=max(m))
    #Means_raw[popstat,exp]=x$m[max(which(round(x$rowMeans.Mean_raw_mat.)<.9*Niter))]
  }
}
#dimensions are 
#number of rows is the length of m (seq(0,0.04,0.001)).
detailed_survey2=reshape2::melt(detailed_survey[,1:2])
detailed_survey2$Direction=c(rep("One-way",length(m)),rep("Two-way",length(m)))
detailed_survey2$Migration=rep(m,2)
detailed_survey2$HCI=c(detailed_survey[,1]+1.96*sqrt(detailed_survey[,3]),detailed_survey[,2]+sqrt(detailed_survey[,4]))
detailed_survey2$LCI=c(detailed_survey[,1]-1.96*sqrt(detailed_survey[,3]),detailed_survey[,2]-sqrt(detailed_survey[,4]))

DFmean <- data.frame(Means_survey)

detailed_survey2$Type = rep("Survey",2*length(m))
detailed_raw2=reshape2::melt(detailed_raw[,1:2])
detailed_raw2$Direction=c(rep("One-way",length(m)),rep("Two-way",length(m)))
detailed_raw2$Migration=rep(m,2)
detailed_raw2$Type=rep("Raw",2*length(m))
detailed_raw2$HCI=c(detailed_raw[,1]+1.96*sqrt(detailed_raw[,3]),detailed_raw[,2]+sqrt(detailed_raw[,4]))
detailed_raw2$LCI=c(detailed_raw[,1]-1.96*sqrt(detailed_raw[,3]),detailed_raw[,2]-sqrt(detailed_raw[,4]))

detailed_RS=rbind(detailed_survey2,detailed_raw2)


ggplot(detailed_RS)+geom_line(aes(x=Migration,y=value,col=Type))+facet_wrap(~as.factor(Direction))+theme_bw()+
  ylab("Percentage with significant correlation")+geom_ribbon(aes(x=Migration,ymin = LCI, ymax = HCI,fill=Type),alpha=0.4)+
  ggtitle("Blackspotted rockfish correlation in population size relative to one and two-way migration\nwith 95% confidence intervals on simulated survey and raw data")

#plot comparisons for blackspooted that only vary by order of magnitude not ratio
#note relpop3 is with a different protocol to add observation error.
setwd("/Users/ingrid.spies/Documents/GOA_cod/SimulationResults/blackspotted_relpop_s09/")
Niter=100
m=seq(0,.5,.02)
max=.5
lenM=length(m)


#Make the results into a line plot
detailed_survey=matrix(0,length(m),12);colnames(detailed_survey)=rep(seq(1,4,1),3)
detailed_raw=matrix(0,length(m),12)

#This one gives you the minimum at which 90% correlation is reached.
Means_survey=matrix(0,12,1)
#Means_raw is just the actual numbers with maturity applied
TEST=vector()
Means_raw=matrix(0,12,1)
for(popstat in 1:12){
    x=read.csv(paste("outMAT_popstat=",popstat,"_exp=",exp,".csv",sep=""))  
      detailed_survey[,popstat]=x$rowMeans.Mean_mat.   
      #   detailed_survey[,popstat+2]=x$rowVars.Mean_mat.
      detailed_raw[,popstat]=x$rowMeans.Mean_raw_mat.
      #    detailed_raw[,popstat+2]=x$rowVars.Mean_raw_mat.
    Means_survey[popstat,]=x$m[max(which(round(x$rowMeans.Mean_mat.)<.9*Niter))]
    for(i in 1:length(m)){TEST[i]=x$rowMeans.Mean_mat.[i]>=.89*100}
    if(sum(TEST)>0){Means_survey[popstat,]=x$m[min(which(TEST==TRUE))]}else(Means_survey[popstat,exp]=max(m))
    for(i in 1:length(m)){TEST[i]=x$rowMeans.Mean_raw_mat.[i]>=.89*100}
    if(sum(TEST)>0){Means_raw[popstat,]=x$m[min(which(TEST==TRUE))]}else(Means_raw[popstat,exp]=max(m))
    Means_raw[popstat,]=x$m[max(which(round(x$rowMeans.Mean_raw_mat.)<.9*Niter))]
  }


#only for base case (simulated survey data)
detailed_survey2=reshape2::melt(detailed_survey)
detailed_survey2$Direction=c(rep("One-way",104),rep("One-way (R)",104),rep("Two-way",104))
detailed_survey2$Migration=rep(m,2)
detailed_survey2$Popsizes = rep(c(rep("A. 43, 75",length(m)),rep("B. 434, 750",length(m)),rep("C. 4340, 7500",length(m)),rep("D. 43400, 75000",length(m))),3)

#only for base case (raw data)
detailed_raw2=reshape2::melt(detailed_raw)
detailed_raw2$Direction=c(rep("One-way",104),rep("One-way (R)",104),rep("Two-way",104))
detailed_raw2$Migration=rep(m,2)
detailed_raw2$Popsizes = rep(c(rep("A. 43, 75",length(m)),rep("B. 434, 750",length(m)),rep("C. 4340, 7500",length(m)),rep("D. 43400, 75000",length(m))),3)

detailed_both=rbind(detailed_raw2,detailed_survey2)
detailed_both$Type=c(rep("Raw",312),rep("Survey",312))
colnames(detailed_both)[6]="Sizes"
ggplot(detailed_both)+geom_line(aes(x=Migration,y=value,col=Sizes,linetype=Type))+facet_wrap(~as.factor(Direction))+theme_bw()+
  ylab("Percentage with significant correlation")+
  ggtitle("Blackspotted rockfish correlation in population size relative to migration")


#Make the ine plot for survey and raw
detailed_both=rbind(detailed_raw2,detailed_survey2)
detailed_both$Type=c(rep("Raw",208),rep("Survey",208))
colnames(detailed_both)[6]="Sizes"
ggplot(detailed_both)+geom_line(aes(x=Migration,y=value,col=Sizes,linetype=Type))+facet_wrap(~as.factor(Direction))+theme_bw()+
  ylab("Percentage with significant correlation")+
  ggtitle("Pacific cod correlation in population size relative to one and two-way migration")

#Means_survey is mean significant comparisons with obs. error, fishery selectivity and maturity applied
Means_survey=vector()
#Means_raw is just the actual numbers with maturity applied
Means_raw=vector()

TEST=vector()
for(popstat in 1:12){
 for (exp in 1:1){
  x=read.csv(paste("outMAT_popstat=",popstat,"_exp=",exp,".csv",sep=""))  
  #Means_survey[popstat,exp]=x$m[max(which(round(x$rowMeans.Mean_mat.)<.9*Niter))]
  for(i in 1:length(m)){TEST[i]=x$rowMeans.Mean_mat.[i]>=.89*100}
  if(sum(TEST)>0){Means_survey[popstat]=x$m[min(which(TEST==TRUE))]}else(Means_survey[popstat]=max(m))
  for(i in 1:length(m)){TEST[i]=x$rowMeans.Mean_raw_mat.[i]>=.89*100}
  if(sum(TEST)>0){Means_raw[popstat]=x$m[min(which(TEST==TRUE))]}else(Means_raw[popstat]=max(m))
  #Means_raw[popstat,exp]=x$m[max(which(round(x$rowMeans.Mean_raw_mat.)<.9*Niter))]
 }
}
#dimensions are 
#number of rows is the length of m (seq(0,0.04,0.001)).

#plot raw data results

DFmean <- data.frame(Popsizes = rep(c("A. 43, 75", "B. 434, 750", "C. 4340, 7500",  "D. 43400, 75000"),3),Means_raw[c(3,1,2,4,7,5,6,8,11,9,10,12)])#"A. 43, 75*","B. 434, 750*", "C. 4340, 7500*",  "D. 43400, 75000*"),Means_survey[c(3,1,2,4,7,5,6,8)])
DFmean$Type=c(rep("One way small->large",4),rep("One way large->small",4),rep("Two way",4))
colnames(DFmean)[2]="Mean"
DFmean$Corr=max-DFmean$Mean
DFlong=reshape2::melt(DFmean,value.name="value")
DFlong$variable=str_replace(DFlong$variable,"Corr","Correlated")
DFlong$variable=str_replace(DFlong$variable,"Mean","Uncorrelated")
DFlong$Popsizes=as.factor(DFlong$Popsizes)
DFlong$Type=as.factor(DFlong$Type)
DFlong$variable=as.factor(DFlong$variable)

ggplot(data = DFlong, aes(x = Popsizes, y=value,fill = Popsizes, pattern = variable)) +
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
 scale_x_discrete(labels=c("A","B","C","D"))+facet_wrap(~Type)+theme_bw()+
 theme( strip.background  = element_blank(), panel.grid.major = element_line(colour = "grey80"),panel.border = element_blank(),axis.ticks = element_line(size = 0),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank())+
 ggtitle("Blackspotted rockfish correlation in population size relative to migration (raw data)")

#plot survey data results


DFmean <- data.frame(Popsizes = rep(c("A. 43, 75", "B. 434, 750", "C. 4340, 7500",  "D. 43400, 75000"),3),Means_survey[c(3,1,2,4,7,5,6,8,11,9,10,12)])#"A. 43, 75*","B. 434, 750*", "C. 4340, 7500*",  "D. 43400, 75000*"),Means_survey[c(3,1,2,4,7,5,6,8)])
DFmean$Type=c(rep("One way small->large",4),rep("One way large->small",4),rep("Two way",4))
colnames(DFmean)[2]="Mean"
DFmean$Corr=max-DFmean$Mean
DFlong=reshape2::melt(DFmean,value.name="value")
DFlong$variable=str_replace(DFlong$variable,"Corr","Correlated")
DFlong$variable=str_replace(DFlong$variable,"Mean","Uncorrelated")
DFlong$Popsizes=as.factor(DFlong$Popsizes)
DFlong$Type=as.factor(DFlong$Type)
DFlong$variable=as.factor(DFlong$variable)

ggplot(data = DFlong, aes(x = Popsizes, y=value,fill = Popsizes, pattern = variable)) +
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
 labs(x = "Correlation calculated on populations with mature fish, survey selectivity, and observation error", y = "Migration Rate", pattern = "Result",fill="Relative population size")+
 scale_x_discrete(labels=c("A","B","C","D"))+facet_wrap(~Type)+theme_bw()+
 theme( strip.background  = element_blank(), panel.grid.major = element_line(colour = "grey80"),panel.border = element_blank(),axis.ticks = element_line(size = 0),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank())+
 ggtitle("Blackspotted rockfish correlation in population size relative to migration (simulated survey)")

#plot comparisons for cod that only vary by order of magnitude not ratio
setwd("/Users/ingrid.spies/Documents/GOA_cod/SimulationResults/cod_relpop1/")
Niter=100
m=seq(0,.5,.02)
max=.5
lenM=length(m)
#Means_survey is mean significant comparisons with obs. error, fishery selectivity and maturity applied

#Make the results into a line plot
detailed_survey=matrix(0,length(m),12);colnames(detailed_survey)=rep(seq(1,4,1),3)
detailed_raw=matrix(0,length(m),12)

#This one gives you the minimum at which 90% correlation is reached.
Means_survey=matrix(0,12,1)
#Means_raw is just the actual numbers with maturity applied
TEST=vector()
Means_raw=matrix(0,12,1)
for(popstat in 1:12){
  x=read.csv(paste("outMAT_popstat=",popstat,"_exp=",exp,".csv",sep=""))  
  detailed_survey[,popstat]=x$rowMeans.Mean_mat.   
  #   detailed_survey[,popstat+2]=x$rowVars.Mean_mat.
  detailed_raw[,popstat]=x$rowMeans.Mean_raw_mat.
  #    detailed_raw[,popstat+2]=x$rowVars.Mean_raw_mat.
  Means_survey[popstat,]=x$m[max(which(round(x$rowMeans.Mean_mat.)<.9*Niter))]
  for(i in 1:length(m)){TEST[i]=x$rowMeans.Mean_mat.[i]>=.89*100}
  if(sum(TEST)>0){Means_survey[popstat,]=x$m[min(which(TEST==TRUE))]}else(Means_survey[popstat,]=max(m))
  for(i in 1:length(m)){TEST[i]=x$rowMeans.Mean_raw_mat.[i]>=.89*100}
  if(sum(TEST)>0){Means_raw[popstat,]=x$m[min(which(TEST==TRUE))]}else(Means_raw[popstat,]=max(m))
  Means_raw[popstat,]=x$m[max(which(round(x$rowMeans.Mean_raw_mat.)<.9*Niter))]
}


#only for base case (simulated survey data)
detailed_survey2=reshape2::melt(detailed_survey)
detailed_survey2$Direction=c(rep("One-way",104),rep("One-way (R)",104),rep("Two-way",104))
detailed_survey2$Migration=rep(m,2)
detailed_survey2$Popsizes = rep(c(rep("A. 683, 683",length(m)),rep("B. 6,834, 6,834",length(m)),rep("C. 68,340, 68,340",length(m)),rep("D. 683,400, 683,400",length(m))),3)
#only for base case (raw data)
detailed_raw2=reshape2::melt(detailed_raw)
detailed_raw2$Direction=c(rep("One-way",104),rep("One-way (R)",104),rep("Two-way",104))
detailed_raw2$Migration=rep(m,2)
detailed_raw2$Popsizes = rep(c(rep("A. 683, 683",length(m)),rep("B. 6,834, 6,834",length(m)),rep("C. 68,340, 68,340",length(m)),rep("D. 683,400, 683,400",length(m))),3)

detailed_both=rbind(detailed_raw2,detailed_survey2)
detailed_both$Type=c(rep("Raw",312),rep("Survey",312))
colnames(detailed_both)[6]="Sizes"
ggplot(detailed_both)+geom_line(aes(x=Migration,y=value,col=Sizes,linetype=Type))+facet_wrap(~as.factor(Direction))+theme_bw()+
  ylab("Percentage with significant correlation")+
  ggtitle("Pacific cod correlation in population size relative to migration")


Means_survey=vector()
#Means_raw is just the actual numbers with maturity applied
Means_raw=vector()

TEST=vector()
for(popstat in 1:12){
 for (exp in 1:1){
  x=read.csv(paste("outMAT_popstat=",popstat,"_exp=",exp,".csv",sep=""))  
  #Means_survey[popstat,exp]=x$m[max(which(round(x$rowMeans.Mean_mat.)<.9*Niter))]
  for(i in 1:length(m)){TEST[i]=x$rowMeans.Mean_mat.[i]>=.89*100}
  if(sum(TEST)>0){Means_survey[popstat]=x$m[min(which(TEST==TRUE))]}else(Means_survey[popstat]=max(m))
  for(i in 1:length(m)){TEST[i]=x$rowMeans.Mean_raw_mat.[i]>=.89*100}
  if(sum(TEST)>0){Means_raw[popstat]=x$m[min(which(TEST==TRUE))]}else(Means_raw[popstat]=max(m))
  #Means_raw[popstat,exp]=x$m[max(which(round(x$rowMeans.Mean_raw_mat.)<.9*Niter))]
 }
}
#dimensions are 
#number of rows is the length of m (seq(0,0.04,0.001)).

#plot raw data results

DFmean <- data.frame(Popsizes = rep(c("A. 683, 683", "B. 6,834, 6,834", "C. 68,340, 68,340",  "D. 683,400, 683,400"),3),Means_raw[c(3,1,2,4,7,5,6,8,11,9,10,12)])#"A. 43, 75*","B. 434, 750*", "C. 4340, 7500*",  "D. 43400, 75000*"),Means_survey[c(3,1,2,4,7,5,6,8)])
DFmean$Type=c(rep("One way small->large",4),rep("One way large->small",4),rep("Two way",4))
colnames(DFmean)[2]="Mean"
DFmean$Corr=max-DFmean$Mean
DFlong=reshape2::melt(DFmean,value.name="value")
DFlong$variable=str_replace(DFlong$variable,"Corr","Correlated")
DFlong$variable=str_replace(DFlong$variable,"Mean","Uncorrelated")
DFlong$Popsizes=as.factor(DFlong$Popsizes)
DFlong$Type=as.factor(DFlong$Type)
DFlong$variable=as.factor(DFlong$variable)

ggplot(data = DFlong, aes(x = Popsizes, y=value,fill = Popsizes, pattern = variable)) +
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
 scale_x_discrete(labels=c("A","B","C","D"))+facet_wrap(~Type)+theme_bw()+
 theme( strip.background  = element_blank(), panel.grid.major = element_line(colour = "grey80"),panel.border = element_blank(),axis.ticks = element_line(size = 0),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank())+
 ggtitle("Pacific cod correlation in population size relative to migration (raw data)")

#plot survey data results
DFmean <- data.frame(Popsizes = rep(c("A. 683, 683", "B. 6,834, 6,834", "C. 68,340, 68,340",  "D. 683,400, 683,400"),3),Means_survey[c(3,1,2,4,7,5,6,8,11,9,10,12)])#"A. 43, 75*","B. 434, 750*", "C. 4340, 7500*",  "D. 43400, 75000*"),Means_survey[c(3,1,2,4,7,5,6,8)])
DFmean$Type=c(rep("One way small->large",4),rep("One way large->small",4),rep("Two way",4))
colnames(DFmean)[2]="Mean"
DFmean$Corr=max-DFmean$Mean
DFlong=reshape2::melt(DFmean,value.name="value")
DFlong$variable=str_replace(DFlong$variable,"Corr","Correlated")
DFlong$variable=str_replace(DFlong$variable,"Mean","Uncorrelated")
DFlong$Popsizes=as.factor(DFlong$Popsizes)
DFlong$Type=as.factor(DFlong$Type)
DFlong$variable=as.factor(DFlong$variable)

ggplot(data = DFlong, aes(x = Popsizes, y=value,fill = Popsizes, pattern = variable)) +
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
 labs(x = "Correlation calculated on populations with mature fish, survey selectivity, and observation error", y = "Migration Rate", pattern = "Result",fill="Relative population size")+
 scale_x_discrete(labels=c("A","B","C","D"))+facet_wrap(~Type)+theme_bw()+
 theme( strip.background  = element_blank(), panel.grid.major = element_line(colour = "grey80"),panel.border = element_blank(),axis.ticks = element_line(size = 0),panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank())+
 ggtitle("Pacific cod correlation in population size relative to migration (simulated survey)")



#Plot raw population size
pop=read.csv("/Users/ingrid.spies/Documents/GOA_cod/Biomass by NMFS Reporting Area - AI_blackspotted_allyrs.csv",header=TRUE)
area=table(pop$NMFS.Reporting.Area)
yrs=as.numeric(names(table(pop$Year)))
spp=table(pop$Species.Code)
table(pop$Common.Name)

BiomCentral=vector();BiomWestern=vector()
BiomCentral_Var=vector();BiomWestern_Var=vector()
#We can combine variances as long as it is reasonable to assume they are independent.
for(i in 1:length(yrs)){
  BiomCentral[i]=sum(pop$Area.Biomass[which(pop$Year==yrs[i]&pop$NMFS.Reporting.Area=="Central Aleutians"&pop$Species.Code!=30051)])
  BiomWestern[i]=sum(pop$Area.Biomass[which(pop$Year==yrs[i]&pop$NMFS.Reporting.Area=="Western Aleutians"&pop$Species.Code!=30051)])
  BiomCentral_Var[i]=sum(pop$Area.Biomass.Var[which(pop$Year==yrs[i]&pop$NMFS.Reporting.Area=="Central Aleutians"&pop$Species.Code!=30051)])
  BiomWestern_Var[i]=sum(pop$Area.Biomass.Var[which(pop$Year==yrs[i]&pop$NMFS.Reporting.Area=="Western Aleutians"&pop$Species.Code!=30051)])
}

BS=data.frame(Year=(rep(yrs,2)),Biomass=c(BiomWestern,BiomCentral),Variance=c(BiomWestern_Var,BiomCentral_Var),Region=as.factor(c(rep("Western",16),rep("Central",16))))

#you want the variance of the log of the biomass estimate which is here below
BS$varLogBiom=log(1+(BS$Variance/(BS$Biomass^2)))
BS$LCI=BS$Biomass/exp(1.96*sqrt(BS$varLogBiom))
BS$HCI=BS$Biomass*exp(1.96*sqrt(BS$varLogBiom))
#BS$LCI=BS$Biomass-1.96*sqrt(BS$Variance)
#BS$HCI=BS$Biomass+1.96*sqrt(BS$Variance)

ggplot(BS,aes(x=Year,y=Biomass,group=Region))+geom_line(aes(x=Year,y=Biomass,col=Region))+
  geom_ribbon(aes(x=Year,ymin = LCI, ymax = HCI,fill=Region),alpha=0.4)+theme_bw()+ylim(c(0,40000))

ggplot(BS,aes(x=Year,y=Biomass,group=Region))+geom_line(aes(x=Year,y=Biomass,col=Region))+
  theme_bw()+ylim(c(0,10000))
#zoom in
ggplot(BS[which(BS$Region=="Western"),],aes(x=Year,y=Biomass,group=Region))+geom_line(aes(x=Year,y=Biomass,col=Region))+
  geom_ribbon(aes(x=Year,ymin = LCI, ymax = HCI,fill=Region),alpha=0.4)+theme_bw()+ylim(c(0,1e4))

ggplot(BS[which(BS$Region=="Central"),],aes(x=Year,y=Biomass,group=Region))+geom_line(aes(x=Year,y=Biomass,col=Region))+
  geom_ribbon(aes(x=Year,ymin = LCI, ymax = HCI,fill=Region),alpha=0.4)+theme_bw()+ylim(c(0,2e4))

#There appears to be a step change in the amount of W.blackspotted.
mean(BS$Biomass[which(BS$Region=="Western"&BS$Year>1999)])
#910.7
mean(BS$Biomass[which(BS$Region=="Western"&BS$Year%in%c(1980,1983,1991,1994,1997))]) #Yeras prior to 2000 excluding the high year 1986
#2772.2


