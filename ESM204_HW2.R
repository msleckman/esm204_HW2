## Celine Mol and Margaux Sleckman
## ESM 204 HW 2

## ---------- Facts ---------- ##

## 'High' income: linear, downward sloping demand curve with choke price of $100/gal
## 'Low' income: linear, downward sloping demand curve with choke price of $70/gal
## The current gas price is $3.50/gal
## At current price, 'High' consumers purchase 10 units, 'Low' consumers purchase 4 units
## Marginal cost of producing a gallong of gas is linear with y intercept of 0
## Consuming one gallong of gas creates an environmental and health externality of $2.00

#install.packages("devtools")
library(devtools)

#install_github("andrewheiss/reconPlots")
library(reconPlots)
library(tidyverse)
library(scales)

low_demand <- function(q) ifelse(q >= 0, 70 - 14*q, 0)
MC <- function(q) 3.5*q
high_demand <- function(q) ifelse(q >= 0, 100 - 6.5*q, 0)
low_demand_price<-function(p) {
  ifelse(p>70, 0, ((-70+p)/-14))
}

high_demand_price<-function(p){
  ifelse(p>100, 0,((-100+p)/-6.5))
}


#aggregated <- function(q) b - m*q
MSC <- function(q) 3.5*q + 2
aggregated1 <-function(p) low_demand_price(p)+high_demand_price(p)
aggregated <-function(q) low_demand(q)+high_demand(q)


###

# low_d_vector<-low_demand(0:20)
# high_d_vector<-high_demand(0:15)
# low_high_df<-merge(low_d_vector,low_d_p_vector, high_d_vector,high_d_p_vector)
# colnames(low_high_df)<-c("low_D_P", "high_D_P")
# low_d_p_vector<-low_demand_price(100:0)
# high_d_p_vector<-high_demand_price(100:0)
# high_d_p_vector

x_range1 <- 0:5
x_range2 <- 0:20
x_range3<-0:50

curve_low <- curve_intersect(low_demand, MC, empirical = FALSE, 
                                      domain = c(min(x_range1), max(x_range1)))
curve_high <- curve_intersect(high_demand, MC, empirical = FALSE,
                                       domain = c(min(x_range2), max(x_range2)))
curve_ag <- curve_intersect(aggregated, MC, empirical = FALSE,
                                       domain = c(min(x_range2), max(x_range2)))
MSC_curve <- curve_intersect(aggregated, MSC, empirical = FALSE,
                             domain = c(min(x_range2), max(x_range2)))
ggplot()+
  stat_function(aes(x_range1), color = "red", size = 1, fun = low_demand) +
  stat_function(aes(x_range2), color = "blue", size = 1, fun = MC) +
  stat_function(aes(x_range2), color = "red", size = 1, fun = high_demand) +
  stat_function(aes(x_range2), color = "green", size = 1, fun = aggregated) +
  stat_function(aes(x_range2), color = "blue", size = 1, fun = MSC)+
  geom_vline(xintercept = curve_low$x, linetype = "dotted") +
  geom_hline(yintercept = curve_low$y, linetype = "dotted") +
  geom_vline(xintercept = curve_high$x, linetype = "dotted") +
  geom_hline(yintercept = curve_high$y, linetype = "dotted") +
  geom_vline(xintercept = curve_ag$x, linetype = "dotted") +
  geom_hline(yintercept = curve_ag$y, linetype = "dotted") +
  geom_vline(xintercept = MSC_curve$x, linetype = "dotted") +
  geom_hline(yintercept = MSC_curve$y, linetype = "dotted") +
  theme_classic() +
  xlab("Gas (billion gallons)") +
  ylab("Price per unit of Gas") +
  ylim(0,100)+
  #scale_x_continuous(label=comma, expand=c(0,0), breaks=c(0,curve_intersection$x,1000))+
  #scale_y_continuous(label=comma, breaks=c(curve_intersection$y,500), expand=c(0,0)) +
  theme(legend.title=element_blank()) +
  ggtitle("Aggregate demand for Gas for High and Low income consumers")

## Calculate consumer surplus + producer surplus
areabox = (curve_ag$x * curve_ag$y)

CS = integrate(aggregated, lower = 0, upper = curve_ag$x)$value - areabox

PS = areabox - integrate(MC, lower = 0, upper = curve_ag$x)$value

## Calculate the environmental cost 
deadweight = 0.5 * 2 * (curve_ag$x - MSC_curve$x)

## Calculate the consumer benefit divided between 'High' and 'Low' income
area_low = curve_low$x * curve_low$y
area_high = curve_high$x * curve_high$y

CS_low = integrate(low_demand, lower = 0, upper = curve_low$x)$value - area_low
CS_high = integrate(high_demand, lower = 0, upper = curve_high$x)$value - area_high

