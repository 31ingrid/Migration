rm(list = ls(all.names = TRUE))

library(matrixStats)
#Parameters based on Pacific cod
L_all=vector()
Wt_all=vector()
srvsel=vector()
mat=vector()
nages=10
age=seq(1,nages,1)
Linf=140.94 
K=0.126 
A0=-0.2974 
psi=5.025e-6
theta=3.199
h = 0.765
F40=c(0.528,0.528)
SS0=c(6834,6834)

#selectiivty comes from 2022 SS3 model double normal
#See file called SS3_cod_selectivity.R this comes from lenage2

srvsel=c(0.1801, 0.5245, 0.8802, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0,1)
fishsel1=c(0.0008, 0.0106, 0.0624, 0.2070, 0.4506, 0.7197, 0.9182, 0.9981, 1.0000,1)
fishsel2=fishsel1
natmort=0.4
sigmaR=sqrt(0.411)

#maturity Aleutian Islands

#maturity
mat_slope=1.03584  #slope is 1/B2
mat_a50=4.883261  #A50=-A/B
B1=-4.7143
B2=0.9654

for(i in 1:nages){
 mat[i]=1/(1+exp(-(B1+age[i]*B2)))}

#earlier maturity multiply B*.75
mat_a5012=vector()#This is earlier maturity - keep slope the same and reduce A40
for(i in 1:nages){
 mat_a5012[i]=1/(1+exp(-(.75*B1+age[i]*B2)))}

Q1=mat
Q2=mat
natmort=c(0.4,0.4)

#at m=0.1 the raw is not significant(uncorrelated) and the error is correlated.
#plot(rowSums(P2_raw[51:Nyrs, ]),type="l",ylim=c(0,max(rowSums(P2_raw[51:Nyrs, ]))));lines(rowSums(P1_raw[51:Nyrs, ]),col="red")
#plot(rowSums(P2_srvsel[51:Nyrs, ]),type="l",ylim=c(0,max(rowSums(P2_srvsel[51:Nyrs, ]))));lines(rowSums(P1_srvsel[51:Nyrs, ]),col="red")
#plot(rowSums(P2_raw[51:Nyrs, ]),type="l",ylim=c(0,max(rowSums(P2_srvsel[51:Nyrs, ]))));lines(rowSums(P2_srvsel[51:Nyrs, ]),col="red")

#note all parameters are either a vector of 2 or there are 2 vectors for each population.

pops=function(Linf,K,A0,psi,theta,fishsel1,fishsel2,natmort,Q1,Q2,SS0,m,MIG,sigmaR2_1,sigmaR2_2,Nyrs)
{
 outpops=list()
 # establish length, weight 
 L_all=vector();Wt_all=vector()
 for (i in 1:nages){ 
   L_all[i]=Linf*(1-exp(-K*(age[i]-A0)));#cm
   Wt_all[i]=0.001*psi*(Linf*(1-exp(-K*(age[i]-A0))))^theta};  #divide by 1000 for kg to mt conversion
 
 
 N_til_init_P1=vector();N_til_init_P2=vector();
 N_til_init_P1[1]=1;N_til_init_P2[1]=1;
 for (i in 2:nages){  
   N_til_init_P1[i]=N_til_init_P1[i-1]*exp(-natmort[1]);
   N_til_init_P2[i]=N_til_init_P2[i-1]*exp(-natmort[2]);}
 N_til_init_P1[nages]=N_til_init_P1[nages-1]*exp(-natmort[1])/(1-exp(-natmort[1]));
 N_til_init_P2[nages]=N_til_init_P2[nages-1]*exp(-natmort[2])/(1-exp(-natmort[2]));
 
 Rec0_P1=SS0[1]/(0.5*sum(Wt_all*Q1*N_til_init_P1))
 Rec0_P2=SS0[2]/(0.5*sum(Wt_all*Q2*N_til_init_P2))
 N_init_P1=Rec0_P1*N_til_init_P1
 N_init_P2=Rec0_P2*N_til_init_P2
 
 
 #establish the SBPR with 1 recruit and F40%
 N_til_init_P1_F40=vector();N_til_init_P2_F40=vector();
 N_til_init_P1_F40[1]=1;N_til_init_P2_F40[1]=1;
 for (i in 2:(nages-1)){  #fill in N_til_init;
   N_til_init_P1_F40[i]=N_til_init_P1_F40[i-1]*exp(-(natmort[1]+fishsel1[i-1]*F40[1]));
   N_til_init_P2_F40[i]=N_til_init_P2_F40[i-1]*exp(-(natmort[2]+fishsel2[i-1]*F40[2]));}
 N_til_init_P1_F40[nages]=N_til_init_P1_F40[nages-1]*exp(-(natmort[1]+fishsel1[nages]*F40[1]))/(1-exp(-(natmort[1]+fishsel1[nages]*F40[1])));
 N_til_init_P2_F40[nages]=N_til_init_P2_F40[nages-1]*exp(-(natmort[2]+fishsel2[nages]*F40[2]))/(1-exp(-(natmort[2]+fishsel2[nages]*F40[2])));
 
 Rec0_P1_F40=SS0[1]/(0.5*sum(Wt_all*Q1*N_til_init_P1_F40))
 Rec0_P2_F40=SS0[2]/(0.5*sum(Wt_all*Q2*N_til_init_P2_F40))
 N_init_P1_F40=Rec0_P1_F40*N_til_init_P1_F40
 N_init_P2_F40=Rec0_P2_F40*N_til_init_P2_F40
 
 
 #For this study, therefore, steepness was selected so that FMSY = F35%: 

 FSB_P1=vector();FSB_P2=vector();
 migrants_to2=vector();migrants_to1=vector();
 rate=vector()
 
 Natage_P1_Nyrs=matrix(0,Nyrs,nages)
 Natage_P2_Nyrs=matrix(0,Nyrs,nages)
 Natage_P1_Nyrs[1,]=N_init_P1
 Natage_P2_Nyrs[1,]=N_init_P2
 
 for (y in 2:Nyrs){
  
   FSB_P1[y-1]=0.5*sum(Wt_all*Natage_P1_Nyrs[y-1,]*Q1)
   VBRec_P1=(4*h*Rec0_P1*FSB_P1[y-1])/(SS0[1]*(1-h)+(FSB_P1[y-1]*((5*h)-1)))*exp(rnorm(1,0,sigmaR2_1))
   
   FSB_P2[y-1]=0.5*sum(Wt_all*Natage_P2_Nyrs[y-1,]*Q2)
   VBRec_P2=(4*h*Rec0_P2*FSB_P2[y-1])/(SS0[2]*(1-h)+(FSB_P2[y-1]*((5*h)-1)))*exp(rnorm(1,0,sigmaR2_2))
   
   #Try 10% migration from P1 to P2 and the other way at the age 0 phase (larval)
   
   Natage_P1_Nyrs[y,1]=VBRec_P1;
   for(i in 2:(nages-1)){Natage_P1_Nyrs[y,i]=Natage_P1_Nyrs[y,i-1]*exp(-(natmort[1]+fishsel1[i-1]*F40[1]))}
   Natage_P1_Nyrs[y,nages]=Natage_P1_Nyrs[y,nages-1]*exp(-(natmort[1]+fishsel1[nages-1]*F40[1]))/(1-exp(-(natmort[1]+fishsel1[nages-1]*F40[1])))
   
   Natage_P2_Nyrs[y,1]=VBRec_P2;
   for(i in 2:(nages-1)){Natage_P2_Nyrs[y,i]=Natage_P2_Nyrs[y,i-1]*exp(-(natmort[2]+fishsel2[i-1]*F40[2]))}
   Natage_P2_Nyrs[y,nages]=Natage_P2_Nyrs[y,nages-1]*exp(-(natmort[2]+fishsel2[nages-1]*F40[2]))/(1-exp(-(natmort[2]+fishsel2[nages-1]*F40[2])))
   
  if (MIG==1){  #Just one way from pop1 to pop2
   Natage_P1_Nyrs[y,]=(1-m)*Natage_P1_Nyrs[y,]
   Natage_P2_Nyrs[y,]=Natage_P2_Nyrs[y,]+(m*Natage_P1_Nyrs[y,])
   migrants_to2[y]=sum(m*Natage_P1_Nyrs[y,])/sum(Natage_P2_Nyrs[y,])
   migrants_to1[y]=sum(m*Natage_P1_Nyrs[y,])/sum(Natage_P1_Nyrs[y,])
  }
  
  if (MIG==2){ #both ways migration
   Natage_P1_Nyrs[y,]=Natage_P1_Nyrs[y,]*(1-m)+Natage_P2_Nyrs[y,]*m
   Natage_P2_Nyrs[y,]=Natage_P2_Nyrs[y,]*(1-m)+Natage_P1_Nyrs[y,]*m
   migrants_to2[y]=sum(m*Natage_P1_Nyrs[y,])/sum((1-m)*Natage_P2_Nyrs[y,])
   migrants_to1[y]=sum(m*Natage_P2_Nyrs[y,])/sum((1-m)*Natage_P1_Nyrs[y,])
  }
  
  
 }#repeat number of runs
 
 #Maybe what we actually care about is number of mature females. 

 #Are they correlated
 #All individuals after the first 10
 #multiply the matrix of numbers at age by fishsel.
 P1_srvsel=matrix(0,Nyrs,nages);P2_srvsel=P1_srvsel

  sigmaS=sqrt(0.013) #This is from the mean CVs and then appluying log(CVs^2+1) formula
  etaS1=rnorm(Nyrs,0,sigmaS); etaS2=rnorm(Nyrs,0,sigmaS); 
  for(i in 1:Nyrs){P1_srvsel[i,]=exp(etaS1[i]-sigmaS^2/2)*(Q1*srvsel*Natage_P1_Nyrs[i,]);
  P2_srvsel[i,]=exp(etaS2[i]-sigmaS^2/2)*(Q2*srvsel*Natage_P2_Nyrs[i,]) }
  
 pcor=cor.test(rowSums(P1_srvsel[51:Nyrs,]),rowSums(P2_srvsel[51:Nyrs,]),alternative="greater")
 
 #Also do correlation test among the true 
 P1_raw=matrix(0,Nyrs,nages);P2_raw=P1_raw

 for(i in 1:Nyrs)
 {
 P1_raw[i,]=(Q1*Natage_P1_Nyrs[i,]);
 P2_raw[i,]=(Q2*Natage_P2_Nyrs[i,]) }
 
 pcor_raw=cor.test(rowSums(P1_raw[51:Nyrs,]),rowSums(P2_raw[51:Nyrs,]),alternative="greater")

 #if MIG=1 quantify migration rate which is number of migtants (age1) from 1 to 2 divided by total number in pop2
 rate[1]=mean(migrants_to2,na.rm=TRUE)
 rate[2]=mean(migrants_to1,na.rm=TRUE)
 
 if(pcor$estimate>0){outpops[[1]]=pcor$p.value}else{outpops[[1]]=1};
 if(pcor_raw$estimate>0){outpops[[2]]=pcor_raw$p.value}else{outpops[[2]]=1};
  outpops[[3]]=rate[1];
 outpops[[4]]=rate[2];
 outpops[[5]]=rowSums(Natage_P1_Nyrs);
 outpops[[6]]=rowSums(Natage_P2_Nyrs);
 return(outpops)
}

#This function should do the first 4 rows of unequal migration
FINMATmeanGM=matrix(0,8,11);FINMATvarGM=matrix(0,8,11);FINMATvarGM_raw=matrix(0,8,11);FINMATmeanGM_raw=matrix(0,8,11);
for(exp in 1:11){#was 1:11
 print("exp=");print(exp);
 age=seq(1,nages,1)
 Linf=140.94 
 K=0.126 
 A0=-0.2974 
 psi=5.025e-6
 theta=3.199
 sigmaR2_1=sqrt(0.408)
 sigmaR2_2=sqrt(0.408)
 
 
 fishsel1=c(0.0008, 0.0106, 0.0624, 0.2070, 0.4506, 0.7197, 0.9182, 0.9981, 1.0000,1)
 fishsel2=c(0.0008, 0.0106, 0.0624, 0.2070, 0.4506, 0.7197, 0.9182, 0.9981, 1.0000,1)
 natmort=c(0.4,0.4)
 Q1=mat
 Q2=mat
 Nyrs=300 #Number of years to run the simulation
 Niter=100#number of iterations of the simulation
 Nrep=3
 
 if (exp==1){}
 if (exp==2){natmort=c(.25,.25)}
 if (exp==3){natmort=c(.55,.25)}
 if (exp==4){natmort=c(.25,.55)}
 if (exp==5){Q2=mat;Q1=mat}
 if (exp==6){Q2=mat}
 if (exp==7){Q1=mat}
 if (exp==8){sigmaR2_1=0.25;sigmaR2_2=0.25}
 if (exp==9){sigmaR2_1=0.85;sigmaR2_2=0.85}
 if (exp==10){sigmaR2_1=0.85;sigmaR2_2=0.25}
 if (exp==11){sigmaR2_1=0.25;sigmaR2_2=0.85}
 for(popstat in 1:8){  #was 1:4
  if(popstat<5){ MIG=1;}
  if(popstat==1){SS0=c(6834,6834)}
  if(popstat==2){SS0=c(6834,683)}
  if(popstat==3){SS0=c(6834,68)}
  if(popstat==4){SS0=c(683,6834)}
  if(popstat>4){ MIG=2;}
  if(popstat==5){SS0=c(6834,6834)}
  if(popstat==6){SS0=c(6834,683)}
  if(popstat==7){SS0=c(6834,68)}
  if(popstat==8){SS0=c(683,6834)}
  
  test=list();meanzGM=vector();meanzGM_raw=vector();varGM=vector();stats=vector();migrants1_to2=vector();migrants2=vector();cor=vector();cor_raw=vector();migsGM1_to2=vector();migsGM2_to1=vector();stats2_to1=vector();#mig==2 should have 2 vlues
  setwd("/Users/ingrid.spies/Documents/GOA_cod/SimulationResults/COV_2pops_GM/")
  #Make a matrix of all the migration rates by the number of reps so for the k loop you can get means of Nreps over Niter runs and variance
  m=seq(0,.5,.02)#m=seq(0,0.03,.001); Second is in all cases except where Popsize =50000,500
  Mean_mat=matrix(0,length(m),Nrep)
  Mean_raw_mat=matrix(0,length(m),Nrep)
  Mig1to2_mat=matrix(0,length(m),Nrep)
  Mig2to1_mat=matrix(0,length(m),Nrep)
  popA_vec=vector();popA_vec2=vector()
  popB_vec=vector();popB_vec2=vector()
  popA_mat=matrix(0,length(m),Nrep)
  popB_mat=matrix(0,length(m),Nrep)
  
  #migrants1_to2 is the proportion of migrants from 1 to 2 divided by the size of pop2
  #migrants2 is the opposite, but For MIG==1 (1 way migration) it is the proportion from 1 to 2 divided by the size of 1
  for (k in 1:Nrep){  #k is number of Nrep
   for(j in 1:length(m)){   #m is the different migration rates you are testing
    for(i in 1:Niter){  #Call this Niter
     test=pops(Linf,K,A0,psi,theta,fishsel1,fishsel2,natmort,Q1,Q2,SS0,m[j],MIG,sigmaR2_1,sigmaR2_2,Nyrs);
     cor[i]=test[[1]];    #pcor$p.value  #this is the p-value for each run of 2 populations for 300 years (minus 10 years first) plus maturity and survey selectiivty and observation error
     cor_raw[i]=test[[2]];    #pcor_raw$p.value  #this is the p-value for each run of 2 populations for 300 years (minus 10 years first) raw data that is mature fish
     migrants1_to2[i]=test[[3]];    #mean(migrants_to2)
     migrants2[i]=test[[4]];  #mean(migrants_to1,na.rm=TRUE)
     popA_vec[i]=mean(test[[5]][250:300])
     popB_vec[i]=mean(test[[6]][250:300])
    }
    #code to apply false discovery rate p-value adjustment
    cor2=sort(cor);
    if(identical(which(cor2<0.05*seq(1:length(cor2))/length(cor2)),integer(0))){BH=0}else(BH=max(which(cor2<0.05*seq(1:length(cor2))/length(cor2))))
    if(BH[1]>0){BHp=BH}else(BHp=0)#if none are significant then BHp is zero.
    meanzGM[j]=BHp#out of 100 times how many were significantly corrrelated populations? each level represents an increasing value of migratoin
    
    cor2_raw=sort(cor_raw);
    if(identical(which(cor2_raw<0.05*seq(1:length(cor2_raw))/length(cor2_raw)),integer(0))){BH_raw=0}else(BH_raw=max(which(cor2_raw<0.05*seq(1:length(cor2_raw))/length(cor2_raw))))
    if(BH_raw[1]>0){BHp_raw=BH_raw}else(BHp_raw=0)
    meanzGM_raw[j]=BHp_raw#out of 100 times how many were significantly corrrelated populations? each level represents an increasing value of migratoin
    
        #
    migsGM1_to2[j]=mean(migrants1_to2)#this is at each level of migration the actual migration rate (migrants into popn 2)
    migsGM2_to1[j]=mean(migrants2)
    popA_vec2[j]=mean(popA_vec)
    popB_vec2[j]=mean(popB_vec)
    
    #Year=c(rep(seq(1,Nyrs,1),2))
    #Number=c(test[[4]],test[[5]])
    #Name=c(rep("PopA",100),rep("PopB",100))
    #Res=data.frame(Number,Name,Year)
    #print(ggplot(Res)+geom_line(aes(x=Year,y=Number,col=Name))+theme_bw())
   }
   Mean_mat[,k]=meanzGM
   Mean_raw_mat[,k]=meanzGM_raw
   Mig1to2_mat[,k]=migsGM1_to2
   Mig2to1_mat[,k]=migsGM2_to1
   popA_mat[,k]=popA_vec2
   popB_mat[,k]=popB_vec2
   
   #plot migration rate and mean number correlated
   #varGM[k]=var(test[which(test<0.05)])
   print("k=");print(k);
   #out=data.frame(m,meanzGM,meanzGM_raw,migsGM1_to2,migsGM2_to1)#add in varGM later
   #colnames(out)=c("Migration","Mean","Mean_raw","True_Mig1to2","True_Mig2to1")
     }
  outFIN=data.frame(m,rowMeans(Mean_mat),rowVars(Mean_mat),rowMeans(Mean_raw_mat),rowVars(Mean_raw_mat),rowMeans(Mig1to2_mat),rowVars(Mig1to2_mat),rowMeans(Mig2to1_mat),rowVars(Mig2to1_mat))#add in varGM later
#  a=sum(rowMeans(Mean_mat)>=0.95*Niter)
#  if(a>0){FINMATmeanGM[popstat,exp]=min(which(rowMeans(Mean_mat)>=0.95*Niter))}else(FINMATmeanGM[popstat,exp]=-1)
#  a=sum(rowMeans(Mean_raw_mat)>=0.95*Niter)
#  if(a>0){FINMATmeanGM_raw[popstat,exp]=min(which(rowMeans(Mean_raw_mat)>=0.95*Niter))}else(FINMATmeanGM_raw[popstat,exp]=-1)
  
    #  FINMATmeanGM[popstat,exp]=mean(stats)
#  FINMATmeanGM2[popstat,exp]=mean(stats2_to1)
#  FINMATvarGM[popstat,exp]=var(stats)
#  FINMATvarGM2[popstat,exp]=var(stats2_to1)
  write.csv(outFIN,paste("outMAT_popstat=",popstat,"_exp=",exp,".csv",sep=""))
   }
}





Year=c(rep(seq(1,Nyrs,1),2))
Number=c(test[[4]],test[[5]])
Name=c(rep("PopA",Nyrs),rep("PopB",Nyrs))
Res=data.frame(Number,Name,Year)
ggplot(Res)+geom_line(aes(x=Year,y=Number,col=Name))+theme_bw()

