library(quantmod)
library(PortfolioAnalytics)

Quote = function(index,universe){
  temp = lapply(universe,get)
  return(temp[[index]])
}
SS180 <- c("600009.SS", "600010.SS", "600015.SS","600018.SS", "600019.SS", "600021.SS","600023.SS", "600031.SS", "600037.SS","600053.SS", "600060.SS", "600061.SS","600064.SS", "600066.SS", "600068.SS","600074.SS", "600079.SS", "600085.SS","600089.SS", "600094.SS", "600115.SS","600118.SS", "600120.SS", "600150.SS","600153.SS", "600157.SS", "600158.SS","600160.SS", "600177.SS", "600196.SS","600208.SS", "600219.SS", "600221.SS","600240.SS", "600252.SS", "600266.SS","600271.SS", "600276.SS", "600297.SS","600315.SS", "600325.SS", "600332.SS","600340.SS", "600352.SS", "600362.SS","600369.SS", "600372.SS", "600376.SS","600383.SS", "600406.SS", "600415.SS","600435.SS", "600446.SS", "600466.SS","600482.SS", "600489.SS", "600497.SS","600503.SS", "600516.SS", "600522.SS","600528.SS", "600535.SS", "600549.SS","600565.SS", "600570.SS", "600585.SS","600588.SS", "600604.SS", "600606.SS","600639.SS", "600643.SS", "600645.SS","600649.SS", "600654.SS", "600663.SS","600674.SS", "600688.SS", "600690.SS","600699.SS", "600703.SS", "600704.SS","600705.SS", "600737.SS", "600739.SS","600741.SS", "600754.SS", "600770.SS","600773.SS", "600783.SS", "600795.SS","600804.SS", "600807.SS", "600816.SS","600820.SS", "600827.SS", "600839.SS","600873.SS", "600886.SS", "600895.SS","600900.SS", "600959.SS", "600978.SS","601009.SS", "601018.SS", "601099.SS","601111.SS", "601118.SS", "601127.SS","601155.SS", "601216.SS", "601555.SS","601600.SS", "601607.SS", "601608.SS",
"601618.SS", "601633.SS","601669.SS", "601699.SS", "601718.SS","601872.SS", "601899.SS","601919.SS", "601928.SS", "601933.SS","601939.SS", "603589.SS", "603993.SS")from <- c("2014-04-01","2014-05-01","2014-06-01","2014-07-01","2014-08-01","2014-09-01","2014-10-01","2014-11-01","2014-12-01","2015-01-01","2015-02-01","2015-03-01","2015-04-01","2015-05-01","2015-06-01","2015-07-01","2015-08-01","2015-09-01","2015-10-01","2015-11-01","2015-12-01","2016-01-01","2016-02-01","2016-03-01")to <- c("2014-04-30","2014-05-31","2014-06-30","2014-07-31","2014-08-31","2014-09-30","2014-10-31","2014-11-30","2014-12-31","2015-01-31","2015-02-28","2015-03-31","2015-04-30","2015-05-31","2015-06-30","2015-07-31","2015-08-31","2015-09-30","2015-10-31","2015-11-30","2015-12-31","2016-01-31","2016-02-28","2016-03-31")
src = "yahoo"

T <- length(from)
T1 <- 3
T2 <-3
p <- floor((T-T2)/T1)
depth <- length(SS180)
aaret <- matrix(nrow=1+T,ncol=depth)
mr <- matrix(nrow=T)
for( t in 1:(T-T2)){
quantmod::getSymbols(SS180,from=from[t],to=to[t],src=src)
period <- length(Quote(1,SS180))/6
ret <- matrix(nrow=period, ncol=depth)
aret <- matrix(nrow=period, ncol=depth)
for( k in 1:depth ){
	ret[,k] <- dailyReturn(Quote(k,SS180))
}
aret[1,] <- c(rep(0,depth))
#累积收益率
for( k in 1:depth ){
	for( m in 2:period ){
		if( ret[m,k] == 0 ) aret[m,k] <- aret[m-1,k]  else 
			{ aret[m,k] <- (1+aret[m-1,k])*(1+ret[m,k])-1 }
	}
}

#市场收益率
M <- c("000001.SS")
quantmod::getSymbols(M,from=from[t],to=to[t],src=src)
depthm <- length(M)
periodm <- length(Quote(1,M))/6
r <- dailyReturn(Quote(1,M))
ar <- matrix(nrow=periodm)
ar[1] <- c(0)
for( m in 2:periodm ){
	if( r[m] == 0 ) ar[m] <- ar[m-1]  else 
		{ ar[m] <- (1+ar[m-1])*(1+r[m])-1 }
}
mr[t] <- ar[periodm]
#月超额收益率

for( k in 1:depth ){
	aaret[1,k] <- k
	aaret[1+t,k] <- aret[period,k]-ar[periodm]
}
}
price <- matrix(nrow=p,ncol=depth+1)
for( i in 1:p){
	for( k in 1:depth ){
		price[i,k]<-Quote(k,SS180)[(i-1)*T1+1,4]
	}
	price[i,depth+1]<-Quote(1,M)[(i-1)*T1+1,4]
}
#计算形成期内平均超额收益率

maaret <- matrix(nrow=1+p,ncol=depth)
for( k in 1: depth){
	maaret[1,k] <- k
	for( i in 1:p ){
	     sumt <- 0
	     for( t in (T1*(i-1)+1) : (T1*i)){
	     	sumt <- sumt+aaret[t,k]
	}
	maaret[i+1,k] <- sumt/T1
}
}

rmaaret <- matrix(nrow=p,ncol=depth)
maarett <- maaret 

#按照形成期内平均超额收益率进行排序
for( i in 1:p ){
	maarett <- maaret[,order(maaret[i+1,],decreasing=T)]
	rmaaret[i,] <- maarett[1,]
}
#计算各股票beta
beta <- matrix(nrow=2,ncol=depth)
x <- c(rep(0,T-T2))
y <- c(rep(0,T-T2))
for( k in 1:depth ){
	beta[1,k] <- k
	for( t in 1:(T-T2)){
		x[t] <- (aaret[1+t,k]+mr[t])
		y[t] <- mr[t]
	}
	beta[2,k] <- cov(x,y)/var(y)
}

#构造股票组合
N <- 10
WP <- matrix(nrow=p,ncol=N)
LP <- matrix(nrow=p,ncol=N)
betaP <- matrix(nrow=1+p,ncol=2)
betaP[1,] <- c("WP's beta","LP's beta")
for( i in 1:p ){
	sumt1 <- 0
	sumt2 <- 0
	for( k in 1:N){
	WP[i,k]<-SS180[rmaaret[i,k]]
	LP[i,k]<-SS180[rmaaret[i,depth+1-k]]
	sumt1 <- (sumt1+beta[2,rmaaret[i,k]])
	sumt2 <- (sumt2+beta[2,rmaaret[i,depth+1-k]])
}
    betaP[1+i,1] <- as.numeric(sumt1/N)
    betaP[1+i,2] <- as.numeric(sumt2/N)
}

#计算持有期累积收益率
#以WP为例
aaret <- matrix(nrow=1+T,ncol=N)
WP.maaret <- matrix(nrow=1+p,ncol=N)
M.ret <- matrix(nrow=p,ncol=T)
WP.minret <- c(rep(0,N))
for( i in 1:p){
	for( t in (i*T1+1):(i*T1+T2)){
quantmod::getSymbols(WP,from=from[t],to=to[t],src=src)
period <- length(Quote(1,WP))/6
ret <- matrix(nrow=period, ncol=N)
aret <- matrix(nrow=period, ncol=N)
for( k in 1:N ){
	ret[,k] <- dailyReturn(Quote(k,WP))
}
aret[1,] <- c(rep(0,N))
#累积收益率
for( k in 1:N ){
	for( m in 2:period ){
		if( ret[m,k] == 0 ) aret[m,k] <- aret[m-1,k]  else 
			{ aret[m,k] <- (1+aret[m-1,k])*(1+ret[m,k])-1 }
	}
}

#市场收益率
M <- c("000001.SS")
quantmod::getSymbols(M,from=from[t],to=to[t],src=src)
depthm <- length(M)
periodm <- length(Quote(1,M))/6
r <- dailyReturn(Quote(1,M))
ar <- matrix(nrow=periodm)
ar[1] <- c(0)
for( m in 2:periodm ){
	if( r[m] == 0 ) ar[m] <- ar[m-1]  else 
		{ ar[m] <- (1+ar[m-1])*(1+r[m])-1 }
}

#计算最大回撤
for( k in 1:N ){
	for( m in 2:period ){
		WP.minret[k] <- min(aret[,k])
	}
}
#记录市场月收益率M.ret以计算股票组合beta
M.ret[i,t] <- ar[periodm]
#月超额收益率

for( k in 1:N ){
	aaret[1,k] <- k
	aaret[1+t,k] <- aret[period,k]-ar[periodm]
}
}
#持有期内平均超额收益率
for( k in 1:N ){
	sumt <- 0 
   for( t in (i*T1+1):(i*T1+T2)){
   	sumt <- sumt+aaret[t+1,k]}
    WP.maaret[i+1,k] <- sumt/T2
    WP.maaret[1,k] <- WP[k]
}

}
#等量持有赢家组合的平均收益率
WP.ret <- matrix(nrow=p)
for( i in 1:p ){
	sumt <- 0
	for( k in 1:N){
		sumt <- sumt+as.numeric(WP.maaret[1+i,k])
	}
	WP.ret[i] <- sumt/N
}

#计算持有期累积收益率
#LP
aaret <- matrix(nrow=1+T,ncol=N)
LP.maaret <- matrix(nrow=1+p,ncol=N)
LP.minret <- c(rep(0,N))
for( i in 1:p){
	for( t in (i*T1+1):(i*T1+T2)){
quantmod::getSymbols(LP,from=from[t],to=to[t],src=src)
period <- length(Quote(1,LP))/6
ret <- matrix(nrow=period, ncol=N)
aret <- matrix(nrow=period, ncol=N)
for( k in 1:N ){
	ret[,k] <- dailyReturn(Quote(k,LP))
}
aret[1,] <- c(rep(0,N))
#累积收益率
for( k in 1:N ){
	for( m in 2:period ){
		if( ret[m,k] == 0 ) aret[m,k] <- aret[m-1,k]  else 
			{ aret[m,k] <- (1+aret[m-1,k])*(1+ret[m,k])-1 }
	}
}
#计算最大回撤
for( k in 1:N ){
	for( m in 2:period ){
		LP.minret[k] <- min(aret[,k])
	}
}
#市场收益率
M <- c("000001.SS")
quantmod::getSymbols(M,from=from[t],to=to[t],src=src)
depthm <- length(M)
periodm <- length(Quote(1,M))/6
r <- dailyReturn(Quote(1,M))
ar <- matrix(nrow=periodm)
ar[1] <- c(0)
for( m in 2:periodm ){
	if( r[m] == 0 ) ar[m] <- ar[m-1]  else 
		{ ar[m] <- (1+ar[m-1])*(1+r[m])-1 }
}

#月超额收益率

for( k in 1:N ){
	aaret[1,k] <- k
	aaret[1+t,k] <- aret[period,k]-ar[periodm]
}
}
#持有期内平均超额收益率
for( k in 1:N ){
	sumt <- 0 
   for( t in (i*T1+1):(i*T1+T2)){
   	sumt <- sumt+aaret[t+1,k]}
   	LP.maaret[i+1,k] <- sumt/T2
   	LP.maaret[1,k] <- LP[k]


}

}
#等量持有输家组合的平均收益率
LP.ret <- matrix(nrow=p)
for( i in 1:p ){
	sumt <- 0
	for( k in 1:N){
		sumt <- sumt+as.numeric(LP.maaret[1+i,k])
	}
	LP.ret[i] <- sumt/N
}

#输出策略结果
sret <- matrix(nrow=1+p,ncol=4)
sret[1,]=c("Formative","Holding","Winner Portfolio","Loser Portfolio")
for( i in 1:p){
	sret[1+i,1]=paste(paste(from[(i-1)*T1+1],","),to[i*T1]);
	sret[1+i,2]=paste(paste(from[i*T1+1],","),to[i*T1+T2]);
	sret[1+i,3]=as.numeric(WP.ret[i]);
	sret[1+i,4]=as.numeric(LP.ret[i]);
}
betaP
print("(T1, T2, N)=")
pa <- c(T1,T2,N)
pa
sret

print("WP,LP最大回撤")
mean(WP.minret)
mean(LP.minret)
