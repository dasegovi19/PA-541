#LOAD YOUR LIBRARIES
library(haven)
library(tidyverse)
library(lmtest)
library(car)
library(nortest)

setwd("~/Downloads/PA 541 Adv Data Analysis I/HW 1")
Fatalities <- read_csv("Fatalities.csv")
Fatalities



### Question 1: 
#a) 
fatality2 <- Fatalities%>% 
  select(fatal, state, year, spirits, unemp, income, dry, pop, miles)
fatality2 
view(fatality2)

#b)
#Total fatalities in each year: 1982-1988

#1982: 
fatality2_1982 <- fatality2 %>% filter(year == 1982) 
sum(fatality2_1982$fatal) # 43,642 total fatalities in 1982


#1983:
fatality2_1983 <- fatality2 %>% filter(year == 1983) 
sum(fatality2_1983$fatal) #42,232 total fatalities in 1983

#1984:
fatality2_1984 <- fatality2 %>% filter(year == 1984) 
sum(fatality2_1984$fatal) #43,921 total fatalities in 1984

#1985:
fatality2_1985 <- fatality2 %>% filter(year == 1985) 
sum(fatality2_1985$fatal) # 43,512 total fatalities in 1985

#1986:
fatality2_1986 <- fatality2 %>% filter(year == 1986)
sum(fatality2_1986$fatal) #45,818 total fatalities in 1986

#1987:
fatality2_1987 <- fatality2 %>% filter(year == 1987)
sum(fatality2_1987$fatal) #46,118 total fatalities in 1987

#1988:
fatality2_1988 <- fatality2 %>% filter(year == 1988)
sum(fatality2_1988$fatal) #46,788 total fatalities in 1988

#c) 
fatality2_1982
max(fatality2_1982$fatal)
#The maximum is 4615 total fatalities in 1982. This belongs to California 



#d) 
fatality2

fatality2_partd <- fatality2 %>%
  filter(fatal >1000 & dry >20)
fatality2_partd

###Answer:
# Alabama in 1986-1988. North Carolina from 1982-1988


#e)
fatality2

average_fatalities <- fatality2 %>%
  group_by (state) %>%
  summarize (averagefatal = mean(fatal)) %>%
   print(n=48)

average_fatalities





#2) 
fatality2 = fatality2 %>% 
  mutate(fatal.cat= case_when( fatal > 1000 ~ 'high',
                               fatal  > 300 & 
                                fatal  <= 1000 ~ 'mid',
                               fatal  <= 300 ~ 'low'))
fatality2 %>%
  group_by (fatal.cat) %>%
  summarize (average_miles = mean(miles)) %>%
  print(n=3)





### Part two: limit to 1987
fatality3 = fatality2 %>% filter(year ==1987)

### Question 3:

cor.test(fatality3$miles, fatality3$fatal)

# The correlation between average miles per driver and number of vehicle fatalities is -0.20
# The size of the correlation coefficient is small and direction is negative.
# There is a small, negative correlation between miles and fatalities. 
#This value is not significant (p-value= 0.1691), p>.05


### Question 4:

fatality3 = 
  fatality3 %>%
  mutate(pop_100k = pop/100000)


lmfatality = lm(fatal ~ pop_100k, data=fatality3)
summary(lmfatality)





(a)
#Intercept: the number of vehicle fatalities for states with no population is 66.84. 
#This intercept is not significant because P>.05 nor does it make sense

# pop_100k coefficient: For a one unit increase in pop_100k, that is, for every one hundred thousand people,
# the number of vehicle fatalities goes up by 17.79  **

(b) #92.39% of the variation in vehicle fatalities can be explained by population size (per 100K)

(c) #66.84+ 17.7922(population) 

# 66.84+ 17.7922(8 million) 
# 66.84+ 17.7922(80) 
66.84+ (17.7922*80)  # 1490.216

#The predicted number of fatalities in a state with 8 million people is 1490.216


### Question 5: 

fatality3$lmfatality.fits = predict(lmfatality)

fatality3$lmfatality.resids = resid(lmfatality)




fatality3$lmfatality.resids
# residual is difference between an observed value and value predicted by the model. 

max(fatality3$lmfatality.resids) #  632.995   Florida
min(fatality3$lmfatality.resids) # -905.3028  New York
view(fatality3)

# is this normally distributed?
qqnorm(fatality3$lmfatality.resids)
qqline(fatality3$lmfatality.resids)
## it is not normally distributed

shapiro.test(fatality3$lmfatality.resids)
 #p value is less than .05. Reject null, accept alternative 
# which is that dataset is not normally distributed

#make the plot
ggplot(fatality3, aes(lmfatality.fits, lmfatality.resids)) + geom_point()

### Question 6:
lmfatality2 = lm(fatal ~ pop_100k + miles + dry, data=fatality3)
summary(lmfatality2)

# a) 94.64% of the variation in vehicle fatalities can be explained by population (per 100k),
# number of miles drove, and percent residing in 'dry' countries

# b) 
#pop_100K coefficient: When controlling for average miles per driver and percent residing in 'dry' countries, 
# for every 100k, the number of fatalities goes up by 18.78 

#miles coefficient: When controlling for population and percent residing in 'dry' countries, 
# for every average mile per driver, the number of fatalities goes down by .1464 

#dry coefficient: When controlling for population and average miles per driver, 
# for every one percent residing in 'dry countries', the number of fatalities goes up by 6.990 

#c) The p value for dry is 0.04. This means that the dry coefficient is a significant predictor and
# the probability that the relationship between the two is 0 is really low.

#d) The adjusted r-squared here is 0.9464. 
# The initial r-squared with pop_100k as the only predictor was 92.39%. The R-squared increased by 2.25%

### Question 7:

lmfatality3 = lm(fatal ~ miles , data=fatality3)
summary(lmfatality3)

# the coefficient of miles is -0.1879 and not significant. That is, for every additional average mile driven,
# the number of fatalities goes down by -0.1879

lmfatality4 = lm(fatal ~ + miles + pop_100k, data=fatality3)
summary(lmfatality4)

# the coefficient of miles is 0.1382 and statistically significant. For every additional average mile driven,
# the number of fatalities goes up by 0.1382 when controlling for population.


# I think that miles and population are correlated. First of all, the relationship between the predictor (population)
# and the DV, fatal, is positive as we saw from question 4. That is, for every one hundred thousand people,
# the number of vehicle fatalities goes up by 17.79. Now let's see correlation between miles and pop

cor.test(fatality3$miles, fatality3$pop_100k) 
# correlation between the two is -0.345.

# There is ommitted bias here. Population is a significant predictor variable. By ommitting this variable,
# the miles coefficient picks up. By looking at the table, since the coefficient of population is positive and
# the correlation is negative between population and miles, there is negative bias. This is why the miles 
# coefficient is negative for the simple regression model. By controlling for population, the coefficient 
# of miles turns positive. 










