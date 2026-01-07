A=matrix(c(2,3,4,4,5,6,2,3,4),nrow= 3, byrow = TRUE)
A
dim(A)
 m=nrow(A)
n=ncol(A) 
n
m
if(m==n){
  cat(" the given matrix is sqaure matrix ","\n")
}else{
  cat(" the gievn matrix is not sqaure matrix","\n")
}
if (all(A==t(A))==1){
  cat(" the given matrix is symmetric matrix ","\n")
}else{
  cat("the given matrix is not symmetric matrix ","\n")
}
if(min(A%*%t(A)==diag(c(1,1,1)))==1){
  cat("the given matrix is orthagonal matrix ","\n")
}else{
  cat("the given matrix is orthagonal matrix ","\n")
}
if(det(A)==0){
  cat("the given matrix is singular matrix ","\n")
}else {
  cat("the given matrix is not Singular matrix ","\n")
}
if(all(A==A^2)==1){
  cat("the given matrix is idempotent matrix ","\n")
}else{
  cat("the given matrix is not idempotent matrix ","\n")
}
e<- eigen(A)$value
if(all(e>0)){
  cat(" positive definite","\n")
}else if(all(e>=0)){
  cat(" positive semi definite","\n")
}else if(all(e<0)){
  cat(" negative definite")
}else if(all(e<=0)){
  cat(" negative semi definite")
}else {
  ("indefinite")
}

