#clear everything
rm(list = ls())

#*************************************************************************
#Define parameters for Pacific cod (1 sex)
#Number of ages for this fish
nages=21

fishsel=vector();
fishsel[1]=0;fishsel[2]=0.0002386166;fishsel[3]=0.0146155849;fishsel[4]=0.1851750145;fishsel[5]=0.5952833774;fishsel[6]=0.9030184396;fishsel[7]=1;fishsel[8]=0.9919588363;
fishsel[9]=0.9551342775;fishsel[10]=0.9184023284;fishsel[11]=0.8889373231;fishsel[12]=0.8668498915;fishsel[13]=0.8506209140;fishsel[14]=0.8387155050;fishsel[15]=0.8299334774;fishsel[16]=0.8234043416;
fishsel[17]=0.8185108308;fishsel[18]=0.8148161694;fishsel[19]=0.8120088399;fishsel[20]=0.8098642013;fishsel[21]=0.8072448763;#fisheries selectivity  

natmort=0.34

#Maturity at age
A=-4.7143
B=0.9654
age=seq(1,nages,1)
Q=1/(1+exp(-(A+age*B)))

#Length at age for males and females
#FEMALE(2) MALE(1) Weight at age von bert
psi=6.537e-6
theta=3.164
Linf=c(110.06,120.39)
A0=c(-0.1814,-.2233)
K=c(0.1449,0.1277)
age=seq(0,20,1)

Wt_all=psi*(Linf[2]*(1-exp(-K[2]*(age-A0[2]))))^theta

#*****************************************************************************
#Set up for Blackspotted Rockfish
nages=75
mat=vector()
mat_a75=vector()
age=seq(1,nages,1) #21 modeled ages
Linf=mean(51.53)#Linf=c(110.06,120.39)#I am taking the mean of growth params to create a single sex model females 2nd
K=0.06
A0=-3.40
psi=6.54e-6
theta=3.24

L_all=vector();Wt_all=vector();
for(i in 1:nages){
 L_all[i]=Linf*(1-exp(-K*(age[i]-A0)));
 Wt_all[i]=psi*(Linf*(1-exp(-K*(age[i]-A0))))^theta;}  #0.001 is for kg to mt conversion

fishsel=vector()
sel_slope=0.34
sel_a50=13.5
for(i in 1:nages){
 fishsel[i]=1/(1+exp(-(sel_slope*(age[i]-sel_a50))))}

#maturity
Q=vector()
mat_slope=3.792
mat_a50=24.5
B1=-6.4691
B2=0.2637

for(i in 1:nages){
 Q[i]=1/(1+exp(-(B1+age[i]*B2)))}

#earlier maturity just multiply B1*.75

for(i in 1:nages){
 mat_a75[i]=1/(1+exp(-(.75*B1+age[i]*B2)))}

natmort=0.05

#*******************************************************************************
#Calculate fishing mortality rate that maximizes TAC
fmort=seq(0,0.06,.001)

N_til_init_a=vector()  #initial numbers when there is one recruit
N_til_init_a[1]=1
for(i in 2:(nages-1)){
 N_til_init_a[i]=N_til_init_a[i-1]*exp(-natmort)
}
N_til_init_a[nages]=N_til_init_a[nages-1]*exp(-natmort)/(1-exp(-natmort))

N_til_init_Frange=vector()
N_til_init_Frange[1]=1;
FSB_F=vector()
for(j in 1:length(fmort)){
 for (i in 2:nages){  #fill in N_til_init;
  N_til_init_Frange[i]=N_til_init_Frange[i-1]*exp(-(natmort+fishsel[i-1]*fmort[j]));
 }
 FSB_F[j]=sum(Wt_all*N_til_init_Frange*.5*Q)
}


FSB_init=sum(Wt_all*N_til_init_a*Q*.5);FSB_init
FSB_F/FSB_init
plot(fmort,FSB_F/FSB_init);abline(h=c(.35,.4))

fmort[min(which(FSB_F/FSB_init<0.4))]#This is F40%  
fmort[min(which(FSB_F/FSB_init<0.35))]#This is F35% 

#F40%  #SM:0.0328  GM: 0.279
#F35%  #SM: 0.039  GM: 0.34

#*****************************************************************************
#This is the function that varies steepness and fishing mortality rate
getFx_Fish_1sex=function(Fish,h){  #this version finds where Fmax occurs for different h's
 SSBinit=100000   #Initial spawning stock biomass (chosen)
 S_l0=SSBinit    #S_l0 initial spawning stock biomass in population #same as SSBinit
 
 N_til_init_a=vector()  #initial numbers when there is one recruit
 N_til_init_a[1]=1
 for(i in 2:(nages-1)){
  N_til_init_a[i]=N_til_init_a[i-1]*exp(-natmort)
 }
 N_til_init_a[nages]=N_til_init_a[nages-1]*exp(-natmort)/(1-exp(-natmort))
 
 R_l0=2*S_l0/sum(Wt_all*N_til_init_a*Q) #initial number of recruits #denominator is spawning biomass per recruit.
 
 N_init_a=R_l0*N_til_init_a;N_init_a    #now this is initial numbers in the population  
 #total initial numbers
 N_init=sum(N_init_a);N_init
 
 #N_init is the number of fish for which I should simulate genotypes (2*1473)
 
 #########################################################################
 
 S_lyINIT=0.5*sum(Wt_all*N_init_a*Q);S_lyINIT  #initial SSB
 
 #Start loop here
 for (k in 1:100){
  #what is age structure next year?
  S_ly=0.5*sum(Wt_all*N_init_a*Q);S_ly  #SSB
  SPR=S_ly/N_init_a[1];SPR
  catch=sum((Wt_all)*((fishsel*Fish)/(fishsel*Fish+natmort)*(1-exp(-(fishsel*Fish+natmort)))*N_init_a));catch
  N_a=vector()  #numbers next year
  N_a[1]=(4*h*R_l0*S_ly)/(S_l0*(1-h)+S_ly*(5*h-1))
  for(i in 2:(nages-1)){
   N_a[i]=N_init_a[i-1]*exp(-(fishsel[i-1]*Fish+natmort))
  }
  N_a[nages]=N_init_a[nages-1]*((exp(-(fishsel[nages-1]*Fish+natmort)))/(1-exp(-(fishsel[nages-1]*Fish+natmort))));#fix this part
  N_init_a=N_a
 }
 
 out=list()
 out[[1]]=SPR
 out[[2]]=S_ly
 out[[3]]=S_ly/S_lyINIT  #Spawning biomass relative to unfished.
 out[[4]]=h
 out[[5]]=catch#calculated same as TAC
 return(out)
}

#Find F40% and F35%
#Because F40% is Fmort that declines pop to 40% of its initial FSB.
N_til_init_a=vector()  #initial numbers when there is one recruit
N_til_init_a[1]=1
for(i in 2:(nages-1)){
 N_til_init_a[i]=N_til_init_a[i-1]*exp(-natmort)
}
N_til_init_a[nages]=N_til_init_a[nages-1]*exp(-natmort)/(1-exp(-natmort))


#Now run function for a range of hs and Fs

#see what affect different H's have
hs=seq(.76,0.79,.005);Fish=seq(.2,.5,.01) #cod
hs=seq(.64,0.7,.01);Fish=seq(.03,.05,.001) #blackspotted
res=matrix(0,length(Fish),length(hs))
SSB=matrix(0,length(Fish),length(hs))
TAC=matrix(0,length(Fish),length(hs))
SPR=matrix(0,length(Fish),length(hs))
h=matrix(0,length(Fish),length(hs))
a=list();
for(j in 1:length(hs)){
 for (i in 1:length(Fish)){
 a=getFx_Fish_1sex(Fish[i],hs[j])
 res[i,j]=a[[3]]
 SSB[i,j]=a[[2]]
 SPR[i,j]=a[[1]]
 TAC[i,j]=a[[5]]
 h[i,j]=a[[4]]
 }}
rownames(TAC)=Fish
colnames(TAC)=hs

plot(Fish,TAC[,1],ylim=c(2500,max(TAC)),type="l",col=1)
for(i in 2:ncol(TAC)){lines(Fish,TAC[,i])}
abline(v=F35)


#For Blackspotted, F35%=FMSY when steepness = 0.67.hs=seq(.64,0.7,.01);Fish=seq(.03,.05,.001)
#For Pcod, F35%=FMSY when steepness = 0.76. hs=seq(.76,0.79,.005);Fish=seq(.2,.5,.01)

