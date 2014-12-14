###################################################################
#####################   Metody Monte Carlo   ##################### 
######################     Projekt 9.1      ##################### 
################################################################


### calka 1 ###

# Dla rozkladu jednostajnego

f1 = function(n){
  x=runif(n,-1,6)
  y=7*sin(x)/(1+x^2)
  mean(y)
}

# Dla rozkladu Cauchy'ego

f1_2 = function(n){
  y=seq(0,0,len=n)
  for (i in 1:n){
    x=rcauchy(1)
    if (x>=-1 & x<=6) y[i]=pi*sin(x)
    }
  mean(y)
}


### calka 2 ###

f2 = function(n){
  y=seq(0,0,len=n)
  for (i in 1:n){
    x=rnorm(1,2,1)
    if (x>=2) y[i]=sqrt(2*pi)/(exp(1)*x^2)
    }  
  mean(y)
}



### symulacje ###

tst=seq(1000,100000,by=1000)
int1=seq(0,0,len=length(tst))
int1_2=seq(0,0,len=length(tst))
int2=seq(0,0,len=length(tst))


for (i in 1:length(tst)) int1[i]=f1(tst[i])
for (i in 1:length(tst)) int1_2[i]=f1_2(tst[i])

for (i in 1:length(tst)) int2[i]=f2(tst[i])


par(mfrow=c(1,2))

plot(tst,int1)
plot(tst,int1_2)

integrate(function(x) sin(x)/(1+x^2),-1,6)

par(mfrow=c(1,1))
plot(tst,int2)

integrate(function(x) exp(-(x^2-4*x+6)/2)/(x^2),2,Inf)


################ czas wykonania w zaleznosci od liczby symulacji ################ 

tst2=c(100,1000,10000,100000,1000000,10000000)
time1=seq(0,0,len=length(tst2))
time1_2=seq(0,0,len=length(tst2))
time2=seq(0,0,len=length(tst2))

for (i in 1:length(tst2)) time1[i]=system.time(f1(tst2[i]))[1] 
for (i in 1:length(tst2)) time1_2[i]=system.time(f1_2(tst2[i]))[1] 

for (i in 1:length(tst2)) time2[i]=system.time(f2(tst2[i]))[1] 

par(mfrow=c(1,2))
plot(time1)
plot(time1_2)

par(mfrow=c(1,1))
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

par(mfrow=c(1,1))
plot(tst3,var1)



### variancja dla calki 2  ###

int2.2=matrix(nrow=nrow,ncol=ncol)

for (i in 1:ncol){
  for (j in 1:nrow) int2.2[i,j]=f2(tst3[j])
}

var2=seq(0,0,len=length(tst3))

for (i in 1:length(tst3)) var2[i]=var(int2.2[,i])

plot(tst3,var2)
