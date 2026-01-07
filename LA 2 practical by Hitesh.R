#Topic : maxima and minima of the  quadratic form 
#q1
A=matrix(c(1,2,-3,2,9,-2,-3,-2,15),nrow = 3);r=100;
A
m=nrow(A);n=ncol(A);
l=eigen(A)$vales;P=eigen(A)$vectors;
cat("eigen values of A are","\n",l)
Q=rep(0,100);
x=matrix(runif(n*r,0,1),nrow=n,ncol = r);
x=cbind(x,P);
x
for ( i in 1: (r+n))
  {
  x[,i]=x[,i]/sqrt(sum(x[,i]^2));
  Q[i]=as.numeric(t(x[,i])%*%A%*%(x[,i]));
}
plot(1:103,Q,xlab = "x",ylab = "quadratic form ",col=1:103);
abline(h=max(Q));abline(v=which.max(Q));
abline(h=min(Q));abline(v=which.min(Q));
cat(" observed min Q: ", min (Q),"\n")
cat("theoretical min eigenval:",min(l),"theoretical max eigenval:", max(l), "\n")



#algebric multiplicity =


























































































































































