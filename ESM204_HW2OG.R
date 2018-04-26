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
#install.packages("GoFKernel")
# library(GoFKernel)
library(reshape2)

low_demand <- function(q) ifelse(q >= 0, 70 - 16.625*q, 0)
MC <- function(q) 0.25*q
high_demand <- function(q) ifelse(q >= 0, 100 - 9.65*q, 0)
aggregate <- function(q) ifelse(q < 3.108808, 100 - 9.65*q, 88.98194 - 6.105853*q)
MSC <- function(q) 0.25*q + 2
OG_Price <- function(q) 3.5

# aggregated1 <- function(p) low_demand_price(p) + high_demand_price(p)
# low_inv <- inverse(low_demand, 0, 100)
# high_inv <- inverse(high_demand, 0, 100)
# aggregated <- function(p) low_inv(p) + high_inv(p)
# aggregate_inv <- inverse(aggregated, 0, 100)
# aggregate_fn <- function(q) aggregate_inv(q)
# low_demand_price<-function(p) {
#   ifelse(p>70, 0, ((-70+p)/-14))
# }
# 
# high_demand_price<-function(p){
#   ifelse(p>100, 0,((-100+p)/-6.5))
# }


###

# low_d_vector<-low_demand(0:20)
# high_d_vector<-high_demand(0:15)
# low_high_df<-merge(low_d_vector,low_d_p_vector, high_d_vector,high_d_p_vector)
# colnames(low_high_df)<-c("low_D_P", "high_D_P")
# low_d_p_vector<-low_demand_price(100:0)
# high_d_p_vector<-high_demand_price(100:0)
# high_d_p_vector

x_range1 <- 0:5
x_range2 <- 0:15
x_range3 <-0:50

curve_low <- curve_intersect(low_demand, OG_Price, empirical = FALSE, 
                             domain = c(min(x_range1), max(x_range1)))
curve_high <- curve_intersect(high_demand, OG_Price, empirical = FALSE,
                              domain = c(min(x_range2), max(x_range2)))
curve_ag <- curve_intersect(aggregate, OG_Price, empirical = FALSE,
                            domain = c(min(x_range2), max(x_range2)))
MSC_curve <- curve_intersect(aggregate, MSC, empirical = FALSE,
                             domain = c(min(x_range2), max(x_range2)))
ggplot()+
  stat_function(aes(x_range1), color = "red", size = 1, fun = low_demand) +
  stat_function(aes(x_range2), color = "blue", size = 1, fun = MC) +
  stat_function(aes(x_range2), color = "red", size = 1, fun = high_demand) +
  stat_function(aes(x_range2), color = "green", size = 1, fun = aggregate) +
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
  ggtitle("Aggregate Demand for Gas for High and Low Income Consumers")

## Part 1. Calculate consumer surplus + producer surplus
areabox = (curve_ag$x * curve_ag$y)

CS = integrate(aggregate, lower = 0, upper = curve_ag$x)$value - areabox
CS

PS = areabox - integrate(MC, lower = 0, upper = curve_ag$x)$value
PS

## Calculate the environmental cost 
deadweight = 0.5 * 2 * (curve_ag$x - MSC_curve$x)
trapezoid = 2 * 14

## Part 2. Calculate the consumer benefit divided between 'High' and 'Low' income
area_low = curve_low$x * curve_low$y
area_high = curve_high$x * curve_high$y

CS_low = integrate(low_demand, lower = 0, upper = curve_low$x)$value - area_low
CS_high = integrate(high_demand, lower = 0, upper = curve_high$x)$value - area_high
CS_low
CS_high

# 3. A gas tax of $0.50/gal. is proposed. What would be the effects of this tax on: 
tax <- function(q) 0.25*q + 0.5
curve_tax <- curve_intersect(aggregate, tax, empirical = FALSE,
                             domain = c(min(x_range2), max(x_range2)))

## Check the decrease in consumer surplus
CS_withtax <- integrate(aggregate, lower = 0, upper = curve_tax$x)$value -
  (curve_tax$x * curve_tax$y)
CS_withtax

#   a. The amount of gasoline produced and sold. 
curve_tax$x

#   b. The price of gasoline. 
curve_tax$y # should be $4.

#   c. “High” income consumers. 
four <- function(q) 4 
tax_high <- curve_intersect(high_demand, four, empirical = FALSE,
                            domain = c(0, 15))
newCS_high <- integrate(high_demand, lower = 0, upper = tax_high$x)$value - 
  (tax_high$x * tax_high$y)
newCS_high

#   d. “Low” income consumers

tax_low <- curve_intersect(low_demand, four, empirical = FALSE,
                           domain = c(0, 15))
newCS_low <- integrate(low_demand, lower = 0, upper = tax_low$x)$value -
  (tax_low$x * tax_low$y)
newCS_low

#   e. Gas producers. 
new_PS <- (curve_tax$x * curve_tax$y) - 
  integrate(tax, lower = 0, upper = curve_tax$x)$value
new_PS

#   f. Total environmental damage. 
## The 50 cent tax DOES NOT internalize the environmental cost... - Austin
new_trapezoid = 2 * curve_tax$x
new_trapezoid

#   g. Total revenue generated by the tax
## area between producer and consumer surplus
totalrev = 0.5 * curve_tax$x
totalrev

##  4. Now, assume that all revenue from a tax will be used for infrastructure repairs, and that the benefit of this is proportional to the amount you drive.  Also assume that “Low” income consumers bear all environmental costs. For a range of gas taxes (ranging from $0 - $5.00/gal), calculate the effects of the tax on: 
# a. "High" income consumers
# b. “Low” income consumers 
# c. Gas producers 

tax_seq <- seq(0, 5, 0.5)

## Assign all arrays
lowCS_values = numeric(length(tax_seq))
highCS_values = numeric(length(tax_seq))
PS_values = numeric(length(tax_seq))
TEC_values = numeric(length(tax_seq))
NBhigh = numeric(length(tax_seq))
NBlow = numeric(length(tax_seq))
TaxRevHigh = numeric(length(tax_seq))
TaxRevLow = numeric(length(tax_seq))
TaxRevtotal= numeric(length(tax_seq)) 
Qconsumedlow = numeric(length(tax_seq))
Qconsumedhigh = numeric(length(tax_seq))


for (i in 1:length(tax_seq)) {
  ## Step 1. Create new supply curve
  tax_fn <- function(q) 0.25*q + tax_seq[i]
  ## Step 2. Prep calculation for CS and PS
  get_y <- curve_intersect(aggregate, tax_fn, empirical = FALSE,
                           domain = c(min(x_range2), max(x_range2)))
  y_value <- function(q) get_y$y
  
  ## Step 3. Calculate high income CS and append to list
  new_tax_high <- curve_intersect(high_demand, y_value, empirical = FALSE,
                                  domain = c(min(x_range2), max(x_range2)))
  highCS <- integrate(high_demand, lower = 0, upper = new_tax_high$x)$value - 
    (new_tax_high$x * new_tax_high$y)
  highCS_values[i] <- highCS
  Qconsumedhigh[i] <- new_tax_high$x
  
  ## Step 4. Calculate low income CS and append to list
  new_tax_low <- curve_intersect(low_demand, y_value, empirical = FALSE,
                                 domain = c(min(x_range2), max(x_range2)))
  lowCS <- integrate(low_demand, lower = 0, upper = new_tax_low$x)$value -
    (new_tax_low$x * new_tax_low$y)
  lowCS_values[i] <- lowCS
  Qconsumedlow[i] <- new_tax_low$x
  
  ## Step 5. Calculate PS and append to list
  PSvalue <- (get_y$x * get_y$y) - 
    integrate(tax_fn, lower = 0, upper = get_y$x)$value
  PS_values[i] <- PSvalue
  
  ## Step 6. Calculate tax revenue for high and low income consumers (Qlow/totalQ)
  TRlow = new_tax_low$x * tax_seq[i]
  TRhigh = new_tax_high$x * tax_seq[i]
  TaxRevHigh[i] = TRhigh
  TaxRevLow[i] = TRlow
  TaxRevtotal[i]= TRlow+TRhigh
  
  ## Step 7. Calculate total environmental cost
  TEC = 2 * get_y$x
  TEC_values[i] = TEC
  
  ## Step 8. Calculate NB high = CShigh + TaxRevhigh
  NBhigh[i] = highCS + TRhigh
  
  ## Step 9. Calculate NB low = CSlow + TaxRevlow - TEC
  NBlow[i] = lowCS + TRlow - TEC
  
  ## Two dollar values
  if (tax_seq[i] == 2.0) {
    Twodollar = cbind('Scenario' = '$2 Gas Tax',
                      'High income gas consumption' = new_tax_high$x,
                      'Low income gas consumption' = new_tax_low$x,
                      'High income Consumer surplus' = highCS,
                      'Low income Consumer surplus' = lowCS,
                      'Aggregate gas price' = get_y$y,
                      'Environmental Damage from Gas' = TEC)
  }
}

tax_values <- cbind(tax_seq, TaxRevHigh, TaxRevLow,TaxRevtotal, NBhigh, NBlow, 
                    PS_values, lowCS_values, highCS_values, Qconsumedhigh, 
                    Qconsumedlow, TEC_values)
##Rounded
tax_values<-as.data.frame((tax_values))
tax_values<-round(tax_values2, 2)
class(tax_values)
View(tax_values)

## 5. Finally, assume that electric cars will gain popularity and that in the future this will lower the demand curves of all income groups by half (vertically).  Under these new demand curves, what are the effects on: 


low_demandEV <- function(q) ifelse(q >= 0, 0.5*(70 - 16.625*q), 0)
high_demandEV <- function(q) ifelse(q >= 0, 0.5*(100 - 9.65*q), 0)

curve_lowEV <- curve_intersect(low_demandEV, MC, empirical = FALSE, 
                               domain = c(min(x_range1), max(x_range1)))
curve_highEV <- curve_intersect(high_demandEV, MC, empirical = FALSE,
                                domain = c(min(x_range2), max(x_range2)))
## Find aggregate
aggregateEV <- function(q) ifelse(q < 3.10880829, 0.5*(100 - 9.65*q), -3.052926*q + 44.49097)

curve_agEV <- curve_intersect(aggregateEV, MC, empirical = FALSE,
                              domain = c(min(x_range2), max(x_range2)))
y_EV <- function(q) curve_agEV$y

new_tax_highEV <- curve_intersect(high_demandEV, y_EV, empirical = FALSE,
                                  domain = c(min(x_range2), max(x_range2)))
highCS_EV <- integrate(high_demandEV, lower = 0, upper = new_tax_highEV$x)$value - 
  (new_tax_highEV$x * new_tax_highEV$y)


new_tax_lowEV <- curve_intersect(low_demandEV, y_EV, empirical = FALSE,
                                 domain = c(min(x_range2), max(x_range2)))
lowCS_EV <- integrate(low_demandEV, lower = 0, upper = new_tax_lowEV$x)$value -
  (new_tax_lowEV$x * new_tax_lowEV$y)


ggplot()+
  stat_function(aes(x_range1), color = "red", size = 1, fun = low_demandEV) +
  stat_function(aes(x_range2), color = "blue", size = 1, fun = MC) +
  stat_function(aes(x_range2), color = "red", size = 1, fun = high_demandEV) +
  stat_function(aes(x_range2), color = "green", size = 1, fun = aggregateEV) +
  geom_vline(xintercept = curve_lowEV$x, linetype = "dotted") +
  geom_hline(yintercept = curve_lowEV$y, linetype = "dotted") +
  geom_vline(xintercept = curve_highEV$x, linetype = "dotted") +
  geom_hline(yintercept = curve_highEV$y, linetype = "dotted") +
  geom_vline(xintercept = curve_agEV$x, linetype = "dotted") +
  geom_hline(yintercept = curve_agEV$y, linetype = "dotted") +
  theme_classic() +
  xlab("Gas (billion gallons)") +
  ylab("Price per unit of Gas") +
  ylim(0,100)+
  #scale_x_continuous(label=comma, expand=c(0,0), breaks=c(0,curve_intersection$x,1000))+
  #scale_y_continuous(label=comma, breaks=c(curve_intersection$y,500), expand=c(0,0)) +
  theme(legend.title=element_blank()) +
  ggtitle("Aggregate Demand for Gas for High and Low Income Consumers\n Under the Electric Vehicle Scenario")

area_lowEV = curve_lowEV$x * curve_lowEV$y
area_highEV = curve_highEV$x * curve_highEV$y

CS_lowEV = integrate(low_demandEV, lower = 0, upper = curve_lowEV$x)$value - area_lowEV
CS_highEV = integrate(high_demandEV, lower = 0, upper = curve_highEV$x)$value - area_highEV
CS_lowEV
CS_highEV

# a. Gas consumption by “High” income consumers 
curve_highEV$x
## vs...
curve_high$x
# b. Gas consumption by “Low” income consumers 
curve_lowEV$x
## vs...
curve_low$x
# c. Gas price 
# Cumulative gas consumption
curve_agEV$x
curve_ag$x

# Gas price comparison
curve_agEV$y
curve_ag$y

# d. Environmental damage from gasoline 
envdamEV = 2 * curve_agEV$x
envdamEV

## two dollar comparison vs. electric vehicle
EVscenario = cbind('Scenario' = 'Electric Vehicle',
                   'High income gas consumption' = new_tax_highEV$x,
                   'Low income gas consumption' = new_tax_lowEV$x,
                   'High income Consumer surplus' = highCS_EV,
                   'Low income Consumer surplus' = lowCS_EV,
                   'Aggregate gas price' = curve_agEV$y,
                   'Environmental Damage from Gas' = envdamEV)

scenarios = rbind(Twodollar, EVscenario)



## Making Graphs
## Net Benefits graph
tax_values <- as.data.frame(tax_values)
incomes <- c(rep('High', 11), rep('Low', 11))
nBenefits <- c(tax_values$NBhigh, tax_values$NBlow)
nBenefits <- round(nBenefits, digits = 2)
taxseqs <- c(rep(tax_values$tax_seq, 2))
ggnb <- cbind(incomes, nBenefits, taxseqs)
ggnb <- as.data.frame(ggnb, round = 2)

ggplot(ggnb, aes(taxseqs, nBenefits, group = factor(incomes))) +
  geom_point(aes(colour = factor(incomes))) +
  geom_line(aes(colour = factor(incomes))) +
  labs(
    x = "Tax Values",
    y = "Net Benefits ($/gal)",
    title = "Net Benefits to High and Low Income Gas Consumers",
    colour = "Income") +
  scale_y_discrete() +
  #scale_colour_discrete(breaks=c("$15 Million", "$10 Million", "$7.5 Million", "$5 Million")) +
  theme_bw()

## Consumer Surplus graph

CSs <- round(c(tax_values$highCS_values, tax_values$lowCS_values), digits = 2)
ggcs <- as.data.frame(cbind(incomes, CSs, taxseqs))

ggplot(ggcs, aes(taxseqs, CSs, group = factor(incomes))) +
  geom_point(aes(colour = factor(incomes))) +
  geom_line(aes(colour = factor(incomes))) +
  labs(
    x = "Tax Values",
    y = "Consumer Surplus ($/gal)",
    title = "Consumer Surplus to High and Low Income Gas Consumers",
    colour = "Income") +
  scale_y_discrete() +
  #scale_colour_discrete(breaks=c("$15 Million", "$10 Million", "$7.5 Million", "$5 Million")) +
  theme_bw()


