Ts <- 0.002
f <- 192000
i <- seq(0, (Ts-(1/f)),by = 1/f)
y5 <- sin(2 * pi * 500 * i)
y6 <- (-1) * y5

n <- length(y5)
y_000 <- y5
y_000[n:(2*n-1)] <- y5
y_000[(2*n):(3*n-1)] <- y6

y_rez <- 0
y_rez[1:length(i)] <- 0
y_rez[(length(i)+1):(1+length(y_000)+length(i)-1)] <- y_000
y_rez[(1+length(y_000)+length(i)):(1+length(y_000)+11*length(i))] <- 0
y_rez[(2+length(y_000)+11*length(i)):(length(y_000)+14*length(i))] <- (0.1)*y_000
y_rez[(1+length(y_000)+14*length(i)):(1+length(y_000)+17*length(i))] <- 0
plot(y_rez, type = "l")

y_rez2<-y_rez
y_rez2[(2+length(y_000)+11*length(i)):(length(y_000)+14*length(i))] <- 0

y_rez[(length(i)+1):(1+length(y_000)+length(i)-1)] <- 0
plot(y_rez,type="l")
plot(y_rez2,type="l")

##========================

N<-length(y_rez)
Rxy<-rep(0, N-1)
for(j in 1:N){
  sum1 <- 0
  
  for(i1 in 1:(N-j+1)){ 
    
    sum1 <- sum1 + y_rez2[i1]*y_rez[i1+j-1]
    
    ##print(sum1)
  }
  
  Rxy[j]<-sum1
}   
plot(Rxy,type="l")