#Non linear Parameter Estimation for the Model

library(gtrendsR)
library(ggplot2)
library(deSolve)
library(data.table)

#Set system time
Sys.setenv(TZ = "UTC")

#Retrieve Data
google.trends = gtrends(c("blu-ray"), gprop = "web", time = "all")[[1]]
google.trends = google.trends[,c("date","hits")]
google.trends$times = seq_along(google.trends$date)

#Create deltavariable
google.trends <- setDT(google.trends)
setkey(google.trends, "hits","date")
google.trends$deltahits <- google.trends$hits - shift(google.trends$hits, fill = 0)

#F_1t <- formula(F_1 ~ 1-(p_1*exp(-p_1*times)))
F_1t <- function(times=google.trends$times){
                  1-(p_1*exp(-p_1*times))
                  }

#f_1t <- formula(f_1 ~ p_1*exp(-p_1*times))
f_1t <- function(times=google.trends$times){
                  p_1*exp(-p_1*times)
                  }

#define starting values
M <- max(google.trends$hits)
p_1_start <- 0
q_1_start <- 0
p_2_start <- 0
q_2_start <- 0
theta_start <- 0
w_start <- 0.0001

#define the function
dXdt <- formula(deltahits ~
  M*(theta*f_1t()+(1-theta)*q_2*(w*F_1t()+(1-w)*((hits/M-theta*F_1t())/(1-theta)))
     *(1-((google.trends$hits/M-theta*F_1t())/(1-theta))))
)

#fit the model
m <- nls(dXdt,
         data = google.trends,
         start=list(p_1 = p_1_start,
                         q_1 = q_1_start,
                         p_2 = p_2_start,
                         q_2 = q_2_start,
                         theta = theta_start,
                         w = w_start),
         algorithm = "port",
         lower = c(0,0,0,0,0,0.0001),
         upper = c(100,100,100,100,1,1))
#estimated parameters
summary(m)

library(nlmrt)
library(nls2)

st <- c(p_1 = p_1_start,
        q_1 = q_1_start,
        p_2 = p_2_start,
        q_2 = q_2_start,
        theta = theta_start,
        w = w_start)

fit.nls2 <- nls2(dXdt,data = google.trends, start = data.frame(rbind(st/10, 10*st)), alg = "brute")

#let's check how well this worked

cor(google.trends$hits,predict(fit.nls2))

google.trends$predictions <- predict(fit.nls2)

plot(google.trends$times,google.trends$hits)
lines(google.trends$times,predict(fit.nls2),col="red",lty=2,lwd=3)


parameters <- coef(fit.nls2)
p_1 <- parameters["p_1"]
q_1 <- parameters["q_1"]
p_2 <- parameters["p_2"]
q_2 <- parameters["q_2"]
theta <- parameters["theta"]
w <- parameters["w"]
