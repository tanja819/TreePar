pnshiftlog<-function(n,time,t,lambda,mu,rho=1){
	i <- inter(time,t)
	rho1<-rho
	rho<-lambda*0+1
	rho[1]<-rho1
	probext<-q2(i,time,t,lambda,mu,rho)
	if (n==0){res<- log(probext)} else {
	res<-log(1-probext)
	Finv<- 1/Ffuncshift(time,t,lambda,mu,rho1)
	res<-res + log(Finv) + (n-1)*log(1-Finv)}
	res
}