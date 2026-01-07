library("MASS")
library("Matrix")
library("ggplot2")
library("reshape2")

p1=1/5; p2=2/5;p3=1-p1-p2;
n=10;m=100;

#sampling from BVBD
set.seed(123)
y1=rbinom(m,n,p1)
y2 <- array(0,dim=(m))
for (i in 1:m) 
{
  y2[i]=rbinom(1,n-y1[i],p2/(1-p1))
}
y_=data.frame(y1,y2)
y1_vals=0:n
y2_vals=0:n
pmf_joint=matrix(0,nrow = n+1,ncol = n+1)

#compute joint pmf using ,multinomial fromula
for(i in 0:n){
  for (j in 0:(n-i)) in {# y1+y2<=n
    k=n-i-j
    pmf_joint[i+1,j+1]=factorial(n)*(p1^i)*(p2^j)*(p3^k)/
      (factorial(i)*factorial(j)*factorial(k))
    }
}
df_joint=melt(pmf_joint)
colnames(df_joint)=c("Y1","Y2","Probability")
df_joint$Y1=df_joint$Y1-1
df_joint$Y2=df_joint$Y2-1

x=rep(0:n,each=n+1)
y=rep(0:n,times=n+1)
z=as.vector(pmf_joint)
df=data.frame(Y1=x,Y2=y,Probability=z)

#Filter only feasible points where Y1+Y2<=n
df=df[df$Y1+df$Y2<=n,]
#3D bar-like plot using cloud
cloud(Probability~y1*Y2,data=df,
      xlab="Y1",ylab="Y2",zlab="Probability",
      screen=list(z=30,x=-60),      
      col.facet=y+2,
      scales=list(arrows=FALSE),
      main="Joint PMF of (Y1,Y2)",
      panel.3d.cloud=panel.3dbars)

pmf_y1=rowSums(pmf_joint)
pmf_y2=colSums(pmf_joint)

#plot marginal pmf y1               
df_y1=data.frame(Y1=0:n, Probability = pmf_y1)
ggplot(df_y1,aes(x=Y1,y= Probability))+
  geom_bar(stat="identity",fill="steelblue")+
  labs(title="Marginal PMF of Y1",x="Y1",y="Probability")+
  theme_minimal()

#plot marginal yq2
df_y2=data.frame(Y2=0:n, Probability = pmf_y2)
ggplot(df_y2,aes(x=Y2,y= Probability))+
  geom_bar(stat="identity",fill="tomato")+
  labs(title="Marginal PMF of Y2",x="Y2",y="Probability")+
  theme_minimal()

#estimation
sm=rbind(mean(y1),mean(y2))
pm=c(n*p1,n*p2)         
cbind(sm,pm)
svcm=cov(y_)
pvcm=matrix(c(n*p1*(1-p1),-n*p1*p2,-n*p1*p2,n*p2*(1-p2)),nrow = 2)
cbind(svcm,pvcm)
