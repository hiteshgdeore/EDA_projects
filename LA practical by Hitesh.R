A=matrix(-100,nrow = 6,ncol = 6,byrow=TRUE)
A
diag(A) <- 4
diag(A)
A


b=upper.tri(A,diag = TRUE)
b
A[row(A)+2==col(A)] <- c(9,100,-20,80)
A
A[row(A)+1==col(A)] <- c(-10,3,89,11)
A
A[row(A)+3==col(A)] <- c(5,6,-9)
A
A[row(A)-4==col(A)] <- c(20,-9)
A
diag(A)=c(4,4,4,4,0,0)
A
