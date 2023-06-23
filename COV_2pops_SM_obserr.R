library(matrixStats)
#Parameters based on Pacific cod
L_all=vector()
Wt_all=vector()
fishsel=vector()
srvsel=vector()
mat=vector()
nages=45
age=seq(1,nages,1)
Linf=51.53
K=0.06 
A0=-3.40 
psi=6.54e-6
theta=3.24
Nyrs=300

srv_slope=0.33
srv_A50=16.18
srvsel=(1/(1+exp(-(srv_slope*(age-srv_A50)))))

fsh_slope=0.69
fsh_A50=13.5
fishsel=(1/(1+exp(-(fsh_slope*(age-fsh_A50)))))

fishsel1=fishsel
fishsel2=fishsel

sigmaR=0.64

#maturity
mat_slope=0.1546  #slope is 1/B
mat_a50=24.5  #A50=-A/B

for(i in 1:nages){
 mat[i]=(1/(1+exp(-(mat_slope*(age[i]-mat_a50)))))
}

#earlier maturity multiply B*.75
mat_a5012=vector()#This is earlier maturity - keep slope the same and reduce A40
for(i in 1:nages){
  mat_a5012[i]=(1/(1+exp(-((mat_slope)*(age[i]-.75*mat_a50)))))
}

Q1=mat
Q2=mat
natmort=c(0.05,0.05)

# establish length, weight
survey=matrix(0,length(seq(0,1,.02)),1000)

for(k in 1:1000){
 L_all=vector();Wt_all=vector()
 for (i in 1:nages){ 
   L_all[i]=Linf*(1-exp(-K*(age[i]-A0)));#cm
   Wt_all[i]=0.001*psi*(Linf*(1-exp(-K*(age[i]-A0))))^theta};  #divide by 1000 for kg to mt conversion
 
 N_til_init_P1=vector();
 N_til_init_P1[1]=1;
 for (i in 2:nages){  
   N_til_init_P1[i]=N_til_init_P1[i-1]*exp(-natmort[1]);}
 N_til_init_P1[nages]=N_til_init_P1[nages-1]*exp(-natmort[1])/(1-exp(-natmort[1]));

 Rec0_P1=SS0[1]/(0.5*sum(Wt_all*Q1*N_til_init_P1))
 N_init_P1=Rec0_P1*N_til_init_P1

 #establish the SBPR with 1 recruit and F40%
 F40=c(0.038,.038) #I dont actually know what is F40. F40% The fishing mortality at which spawning biomass-per-recruit is 40% of its unfished level..2389 from Spies Punt
 N_til_init_P1_F40=vector();
 N_til_init_P1_F40[1]=1;
 for (i in 2:(nages-1)){  #fill in N_til_init;
   N_til_init_P1_F40[i]=N_til_init_P1_F40[i-1]*exp(-(natmort[1]+fishsel1[i-1]*F40[1]));}
N_til_init_P1_F40[nages]=N_til_init_P1_F40[nages-1]*exp(-(natmort[1]+fishsel1[nages-1]*F40[1]))/(1-exp(-(natmort[1]+fishsel1[nages-1]*F40[1])));

 Rec0_P1_F40=SS0[1]/(0.5*sum(Wt_all*Q1*N_til_init_P1_F40))
 N_init_P1_F40=Rec0_P1_F40*N_til_init_P1_F40

 #For this study, therefore, steepness was selected so that FMSY = F35%: 

 FSB_P1=vector();#FSB_P2=vector();

 Natage_P1_Nyrs=matrix(0,Nyrs,nages)
 Natage_P1_Nyrs[1,]=N_init_P1
 
 for (y in 2:Nyrs){
  
   FSB_P1[y-1]=0.5*sum(Wt_all*Natage_P1_Nyrs[y-1,]*Q1)
   VBRec_P1=(4*h*Rec0_P1*FSB_P1[y-1])/(SS0[1]*(1-h)+(FSB_P1[y-1]*((5*h)-1)))*exp(rnorm(1,0,sigmaR2_1))
   Natage_P1_Nyrs[y,1]=VBRec_P1;
   for(i in 2:(nages-1)){Natage_P1_Nyrs[y,i]=Natage_P1_Nyrs[y,i-1]*exp(-(natmort[1]+fishsel1[i-1]*F40[1]))}
   Natage_P1_Nyrs[y,nages]=Natage_P1_Nyrs[y,nages-1]*exp(-(natmort[1]+fishsel1[nages-1]*F40[1]))/(1-exp(-(natmort[1]+fishsel1[nages-1]*F40[1])))
   
 }#repeat number of runs

 P1_srvsel=matrix(0,Nyrs,nages);P2_srvsel=P1_srvsel

 #Test a range of sigmaS.
 sigmaS=sqrt(seq(0,1,.02))

 for(j in 1:length(sigmaS)){
   #This is from the mean CVs and then appluying log(CVs^2+1) formula
  etaS1=rnorm(Nyrs,0,sigmaS[j]); etaS2=rnorm(Nyrs,0,sigmaS[j]); 
  for(i in 1:Nyrs){P1_srvsel[i,]=exp(etaS1[i]-(sigmaS[j]^2)/2)*(Q1*srvsel*Natage_P1_Nyrs[i,]);
  P2_srvsel[i,]=exp(etaS2[i]-(sigmaS[j]^2)/2)*(Q2*srvsel*Natage_P1_Nyrs[i,]) 
  }
 
#  plot(rowSums(P1_srvsel[51:Nyrs,]),type="l");lines(rowSums(P2_srvsel[51:Nyrs,]))
 pcor=cor.test(rowSums(P1_srvsel[51:Nyrs,]),rowSums(P2_srvsel[51:Nyrs,]))

 if(pcor$estimate>0){survey[j,k]=pcor$p.value}else{survey[j,k]=1};
 }
 print(k);
 }

#For the result with the uniformaly distributed sigmaS
write.csv(survey,"/Users/ingrid.spies/Documents/GOA_cod/survey_obserror_runifSM.csv") 
survey=read.csv("/Users/ingrid.spies/Documents/GOA_cod/survey_obserror_runifSM.csv",header=TRUE) 

sigmaS=(seq(0,1,.02))
survey_means=vector()
for(i in 2:length(sigmaS)){survey_means[i]=mean(as.numeric(survey[i,2:1001])[which(as.numeric(survey[i,2:1001])<1)])}
survey_means[1]=0

#Mean WAI and CAI sigma2 for past 6 surveys 
s2=c(0.122850826,0.186017769,0.175392739,0.074798277,0.071507789,0.054890043,0.052691662,0.207234386,0.127276267,0.102786127,0.139076912,0.115591937)
plot(sigmaS,survey_means,xlab="Sigma squared - Observation error",ylab="P-value significance for correlation")
abline(h=.05)
#abline(v=s2)

sigs=data.frame(Sigma=sigmaS,Significance=survey_means)
ggplot(data=sigs)+geom_point(aes(x=Sigma,y=Significance))+theme_bw()+ geom_hline(yintercept=0.05)+geom_vline(xintercept=0.345,linetype="dashed")+
  xlab("Sigma squared - Observation error")+ylab("P-value significance for correlation")+ggtitle("Blackspotted rockfish parameterization")
