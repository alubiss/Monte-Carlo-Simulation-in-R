library(knitr)
library(kableExtra)
knitr::opts_chunk$set(fig.align = 'center', out.width = '120%', echo = TRUE)
library("readxl")
library(dplyr)

price_of_meal = c(10,12,15,17,20,22)
n_meals = c(1,2,3)
n_seats = seq(0,25,1)
FC = 40000


one_hour <- function(){
  full_seats=sample(n_seats,1)
  orders = data.frame()
  costs_profits_one_hour= data.frame()
  for(i in 1:full_seats){
    new_one=data.frame()
    meals_ordered=sample(n_meals,1)
    if(meals_ordered==3) {p1=sample(price_of_meal,1) 
    p2=sample(price_of_meal,1) 
    p3= sample(price_of_meal,1) }
    if(meals_ordered==2) {p1=sample(price_of_meal,1) 
    p2=sample(price_of_meal,1) 
    p3= 0 }
    if(meals_ordered==1) {p1=sample(price_of_meal,1) 
    p2=0 
    p3=0 }
    P=p1+p2+p3
    MC= 0.4 * P
    new_one=cbind(p1,p2,p3, P, MC, full_seats)
    orders=rbind(orders,new_one)
  }
  sum_P= sum(orders$P)
  sum_MC= sum(orders$MC)
  if(full_seats>=0 & full_seats<10) {n_waitress=1}
  if(full_seats>=10 & full_seats<20) {n_waitress=2}
  if(full_seats>=20 & full_seats<=25) {n_waitress=3}
  sum_cost_weitress= n_waitress*20
  a=cbind(sum_P,sum_MC, sum_cost_weitress)
  costs_profits_one_hour=rbind(costs_profits_one_hour,a)
  costs_profits_one_hour=data.frame(costs_profits_one_hour)
  colnames(costs_profits_one_hour) <- c("sum_P","sum_MC", "sum_cost_weitress")
  return(costs_profits_one_hour)
}
one_hour()


# W danej godzinie jest zajęte `full_seats` miejsc w restauracji.
# Każda z tych `full_seats` osób może zjeść od jednego do trzech posiłków w cenach podanych z menu.


B <- 340
events <- replicate(B,one_hour(), simplify = 'rank')
event=as.data.frame(events)
event=as.data.frame(lapply(event, unlist))


# W takiej sytuacji zysk miesięczny wynosi:

library(dplyr)
dane= event[,1:340]
dane <- dane %>% mutate_if(is.character, as.numeric)
dane <- dane %>% mutate(row_sum= rowSums(dane[,sapply(dane, is.numeric)]))

PROFIT <-  dane$row_sum[1] - FC - dane$row_sum[2] - dane$row_sum[3]
PROFIT

one_month_profits <- function(){
  full_seats=sample(n_seats,1)
  orders = data.frame()
  costs_profits_one_hour= data.frame()
  for(i in 1:full_seats){
    new_one=data.frame()
    meals_ordered=sample(n_meals,1)
    if(meals_ordered==3) {p1=sample(price_of_meal,1) 
    p2=sample(price_of_meal,1) 
    p3= sample(price_of_meal,1) }
    if(meals_ordered==2) {p1=sample(price_of_meal,1) 
    p2=sample(price_of_meal,1) 
    p3= 0 }
    if(meals_ordered==1) {p1=sample(price_of_meal,1) 
    p2=0 
    p3=0 }
    P=p1+p2+p3
    MC= 0.2 * P
    new_one=cbind(p1,p2,p3, P, MC)
    orders=rbind(orders,new_one)
  }
  sum_P= sum(orders$P)
  sum_MC= sum(orders$MC)
  if(full_seats>=0 & full_seats<10) {n_waitress=1}
  if(full_seats>=10 & full_seats<20) {n_waitress=2}
  if(full_seats>=20 & full_seats<=25) {n_waitress=3}
  sum_cost_weitress= n_waitress*20
  a=cbind(sum_P,sum_MC, sum_cost_weitress)
  costs_profits_one_hour=rbind(costs_profits_one_hour,a)
  costs_profits_one_hour=data.frame(costs_profits_one_hour)
  colnames(costs_profits_one_hour) <- c("sum_P","sum_MC", "sum_cost_weitress")
  
  B <- 340
  events <- replicate(B,one_hour(), simplify = 'rank')
  event=as.data.frame(events)
  event=as.data.frame(lapply(event, unlist))
  
  dane= event[,1:340]
  dane <- dane %>% mutate_if(is.character, as.numeric)
  dane <- dane %>% mutate(row_sum= rowSums(dane[,sapply(dane, is.numeric)]))
  
  PROFIT <-  dane$row_sum[1] - FC - dane$row_sum[2] - dane$row_sum[3]
  return(PROFIT)
}

A <- 10
months_profits <- replicate(A,one_month_profits())
months_profits

summary(months_profits)

hist <- hist(months_profits, xlab="x", ylab="", main="Distribution of profits" )