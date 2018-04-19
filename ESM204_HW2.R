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

curve1 <- function(q) 70 - 14*q
MC <- function(q) 3.5*q
curve3 <- function(q) 100 - 6.5*q
curve4 <- function(q) b - m*q

x_range1 <- 0:5
x_range2 <- 0:20

curve_intersection <- curve_intersect(curve1, MC, empirical = FALSE, 
                                      domain = c(min(x_range1), max(x_range1)))
curve_intersection2 <- curve_intersect(curve3, MC, empirical = FALSE,
                                       domain = c(min(x_range2), max(x_range2)))
curve_intersection3 <- curve_intersect(curve4, MC, empirical = FALSE,
                                       domain = c(min(x_range2), max(x_range2)))
ggplot() +
  stat_function(aes(x_range1), color = "red", size = 1, fun = curve1) +
  stat_function(aes(x_range2), color = "blue", size = 1, fun = MC) +
  stat_function(aes(x_range2), color = "red", size = 1, fun = curve3) +
  stat_function(aes(x_range2), color = "green", size = 1, fun = curve4) +
  geom_vline(xintercept = curve_intersection$x, linetype = "dotted") +
  geom_hline(yintercept = curve_intersection$y, linetype = "dotted") +
  geom_vline(xintercept = curve_intersection2$x, linetype = "dotted") +
  geom_hline(yintercept = curve_intersection2$y, linetype = "dotted") +
  geom_vline(xintercept = curve_intersection3$x, linetype = "dotted") +
  geom_hline(yintercept = curve_intersection3$y, linetype = "dotted") +
  theme_classic() +
  xlab("Gas (billion gallons)") +
  ylab("Price per unit of Gas") +
  ylim(0,100) +
  #scale_x_continuous(label=comma, expand=c(0,0), breaks=c(0,curve_intersection$x,1000)) +
  #scale_y_continuous(label=comma, breaks=c(curve_intersection$y,500), expand=c(0,0)) +
  theme(legend.title=element_blank()) +
  ggtitle("Aggregate demand for Gas for High and Low income consumers")


## Calculate consumer surplus + producer surplus
areabox = (curve_intersection3$x * curve_intersection3$y)

CS = integrate(curve4, lower = 0, upper = curve_intersection3$x)$value - areabox

PS = areabox - integrate(curve2, lower = 0, upper = curve_intersection3$x)$value
