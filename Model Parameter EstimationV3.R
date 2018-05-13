#Non linear Parameter Estimation for the Model

library(gtrendsR)
library(ggplot2)
library(nls2)
library(BAS)
library(zoo)
library(tseries)
library(forecast)

#Set system time
Sys.setenv(TZ = "UTC")

#Retrieve Data
google.trends = gtrends(c("blu-ray"), gprop = "web", time = "all")[[1]]
google.trends = google.trends[,c("date","hits")]

z <- read.zoo(google.trends, format = "%Y-%m-%d")
z <- as.zooreg(z,deltat = 1/12)
z.m <- as.zooreg(aggregate(z, as.yearmon, mean), freq = 12)
as.ts(z.m)
z.mclean <- tsclean(z.m)
z.mcleanma3 <- tsclean(ma(z.mclean, order = 3))

series <- data.frame(z.mclean,z.mcleanma3)
series["Date"] <- rownames(series)

#Plot data using smooth to see underlying pattern.
ggplot(data=google.trends, aes(x= date, y = hits))+
  geom_line()+
  scale_x_date(date_labels = "%b-%Y")+
  geom_smooth()+
  xlab("")+
  ylab("Monthly Views")+
  theme_classic()

#Decompose the model
count_ma = ts(na.omit(daily_data$cnt_ma), frequency=30)
decomp = stl(z.mclean, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

#Access decomposed parts
movingavg <- as.vector(z.mcleanma3)
trend <- as.vector(decomp$time.series[,2])
random <- as.vector(decomp$time.series[,3])
mixed <- as.vector(trend+random)

###
#Define all functions
###

#Gaussian hypergeometric function:


H1 <- function(w,q2,q1,p1,p2,t){
  H1 <- rep(0,times =length(t))
  for (i in 1:length(t)){
    a <- 1
    b <- (w*q2)/q1
    c <- 1+(w*q2)/q1-(p2+q2)/(p1+q1)
    z <- p1/(p1+q1*(exp(-(p1+q1)*i)))
    
    if(z>=0 & z<1){
      H1[i] <- hypergeometric2F1(a=1, b=b,
                               c= c, z=z)
    }else{
      H1[i] <- hypergeometric2F1(a=c-a, b=b,
                                 c= c, z=1-1/(1-z))/(1-z)^b
    }
    if(is.infinite(H1[i])){
      H1[i] <- max(H1[is.finite(H1)])
    }
  }
  return(H1)
}


H2 <- function(w,q2,q1,p2,p1){
  a <- 1
  b <- (w*q2)/q1
  c <- 1+(w*q2)/q1-(p2+q2)/(p1+q1)
  z <- p1/(p1+q1)
  
  if(z>=0 & z<1){
    H2 <- hypergeometric2F1(a=1, b=b,
                               c= c, z=z)
  }else{
    H2 <- hypergeometric2F1(a=c-a, b=b,
                               c= c, z=1-1/(1-z))/(1-z)^b
  }
  return(H2)
}

#cumulated function for innovators:
F1 <- function(p1,q1,t){
  F1 <- (1-exp(-(p1+q1)*t)) / (1+(q1/p1)*exp(-(p1+q1)*t))
}

#cumulated function for immitaters:
F2 <- function(p2,q1,q2,p1,w,t){
  F2 <- 1 + (p2*q1 + q2*(p1*w-q1*(1-w)))*((q1*q2*(1-w)*H1(w,q2,q1,p1,p2,t) +
      (exp((p2+q2)*t)) * (((p1+q1*(exp(-(p1+q1)*t))) /
                             (p1+q1))**(w*q2/q1))* (p2*q1 + q2*(q1*(1-w)*(1-H2(w,q2,q1,p2,p1))-p1*w)))**(-1))
}

#cumulated function of whole pop
Fm <- function(theta,p1,p2,q1,q2,w,t){
  Fm <- theta*F1(p1,q1,t) + (1-theta)*F2(p2,q1,q2,p1,w,t)
}

#adoption rate innovators:
f1 <- function(p1,q1,t){
  f1 <- (p1*((1+q1/p1)**2) * exp(-(p1+q1)*t)) / ((1+(q1/p1)*exp(-(p1+q1)*t))**2)
}

#adoption rate immitators:
f2 <- function(p1,p2,q1,q2,w,t){
  f2 <- q2*(w*F1(p1,q1,t)+(1-w)*F2(p2,q1,q2,p1,w,t))*(1-F2(p2,q1,q2,p1,w,t))
}

#adoption rate cummulated:
fm <- function(p1,p2,q1,q2,w,t,theta){
  fm <- theta*f1(p1,q1,t) + (1-theta)*f2(p1,p2,q1,q2,w,t)
}

#Adoption dX

dXdt <- function(p1,q1,q2,w,t,theta,M,hits){
  dXdt <- M*(theta*f1(p1,q1,t)+(1-theta)*q2*
               (w*F1(p1,q1,t)+(1-w)*((hits/M-theta*F1(p1,q1,t))/(1-theta))*
               (1-((hits/M-theta*F1(p1,q1,t))/(1-theta)))))
}

#define starting values
p_1_start <- 0
q_1_start <- 0.01
p_2_start <- 0
q_2_start <- 0
theta_start <- 0.01
w_start <- 0.01

p_1_end <- 20
q_1_end <- 20
p_2_end <- 20
q_2_end <- 20
theta_end <- 1
w_end <- 1

hits <- cumsum(google.trends$hits)
M <- sum(hits)
dX <- diff(hits)
dX <- c(0,dX)
t <- seq_along(hits)

st2 <- data.frame(p1 = c(p_1_start, p_1_end),
                  q1 = c(q_1_start, q_1_end),
                  q2 = c(q_2_start, q_2_end),
                  theta = c(theta_start, theta_end),
                  w = c(w_start, w_end))

#We use grid search to find good starting values
m <- nls2(dX ~ dXdt(p1,q1,q2,w,t,theta,M=M,hits=hits), start = st2, algorithm = "random-search")
#We use the input from the search as starting values
final <- nlsLM(dX ~ dXdt(p1,q1,q2,w,t,theta,M=M,hits=hits), start = as.list(coef(m)),lower = c(0,0.0001,0,0.01,0.01),
               upper = c(100,100,100,1,1))
summary(final)

parameters <- coef(final)
p_1 <- parameters["p_1"]
q_1 <- parameters["q_1"]
p_2 <- parameters["p_2"]
q_2 <- parameters["q_2"]
theta <- parameters["theta"]
w <- parameters["w"]


