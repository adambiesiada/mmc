###################################################################
#####################   Metody Monte Carlo   ##################### 
######################     Projekt 9.1      ##################### 
################################################################


### calka 1 ###

f1 = function(n){
  x=runif(n,-1,6)
  y=sin(x)/(1+x^2)
  mean(y)
}


### calka 2 ###

f2 = function(n){
  x=1/runif(n,0,0.5)
  y=exp(-(x^2-4*x+6)/2)/(x^2)
  mean(y)
}

### symulacje ###

tst=seq(1000,1000000,by=1000)
int1=seq(0,0,len=length(tst))
int2=seq(0,0,len=length(tst))


for (i in 1:length(tst)) int1[i]=f1(tst[i])

for (i in 1:length(tst)) int2[i]=f2(tst[i])

plot(tst,int1)

plot(tst,int2)


################ czas wykonania w zaleznosci od liczby symulacji ################ 

tst2=c(100,1000,10000,100000,1000000,10000000)
time1=seq(0,0,len=length(tst2))
time2=seq(0,0,len=length(tst2))

for (i in 1:length(tst2)) time1[i]=system.time(f1(tst2[i]))[1] 

for (i in 1:length(tst2)) time2[i]=system.time(f2(tst2[i]))[1] 

plot(time1)

plot(time2)


################ wariancja wyniku w zaleznosci od liczby symulacji ################ 

### variancja dla calki 1 ###

tst3=seq(50,10000,by=50)
nrow=length(tst3)
ncol=200

int1.2=matrix(nrow=nrow,ncol=ncol)

for (i in 1:ncol){
  for (j in 1:nrow) int1.2[i,j]=f1(tst3[j])
}

var1=seq(0,0,len=length(tst3))

for (i in 1:length(tst3)) var1[i]=var(int1.2[,i])

plot(tst3,var1)

### variancja dla calki 2  ###

int2.2=matrix(nrow=nrow,ncol=ncol)

for (i in 1:ncol){
  for (j in 1:nrow) int2.2[i,j]=f2(tst3[j])
}

var2=seq(0,0,len=length(tst3))

for (i in 1:length(tst3)) var2[i]=var(int2.2[,i])

plot(tst3,var2)
