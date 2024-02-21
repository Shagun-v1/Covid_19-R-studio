rm(list=ls()) # removes all variables stored previously
library(Hmisc) # import

data <- read.csv("C:/Users/shagu/Downloads/COVID19_line_list_data.csv")


#cleaning up death column
data$death_dummy <- as.integer(data$death != 0)
# death rate in covid_19
# total number of death
sum(data$death_dummy) / nrow(data)
 # 58% is the death rate


# AGE
# Old people died

dead= subset(data, death_dummy== 1)
alive = subset(data, death_dummy== 0)
 # 1022 are alive and 63 are dead who are older
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

# Statistically significance
# using t test

t.test(alive$age, dead$age, alternative= "two.sided", conf.level = 0.95)
 # chances of a person to be alive at 95% will be (24,17)
#means that young age will be alive.

# confidence interval = 0.99

t.test(alive$age, dead$age, alternative= "two.sided", conf.level = 0.99)
 # with 99% confidence young people will be alive in the range (24, 15)

# when p-value < 0.05 , we reject the null hypothesis
# p- value <0.05, then we reject null hypothesis
# data is statistically significant


## GENDER ###
#Gender having no effect

male = subset(data, gender =='male')
female = subset(data, gender =='female')
## 382 are female who died
## 520 are male who died.

mean(male$death_dummy, na.rm= TRUE) #8.4%
mean(female$death_dummy, na.rm= TRUE) #3.6%

# T Test to check significance with conf.level = 0.95
t.test(male$death_dummy, female$death_dummy, alternative= "two.sided", conf.level= 0.95)
# 95% confidence level: male have from 0.2% to 0.8% high chance of dying.
#p- value = 0.002 < 0.05, so significant


#conf.level = 0.99
t.test(male$death_dummy, female$death_dummy, alternative= "two.sided", conf.level= 0.99)

#99% confidence level : male have from 0.8% to 8.8% chances of dying.
# p- value : 0.002< 0.05 then significant.


## VIF IN MULTICOLLINEARITY ##

x <- read.csv("C:/Users/shagu/Downloads/COVID19_line_list_data.csv")
attach(x)
colnames(x)  
x_reg=lm(visiting.Wuhan ~ from.Wuhan+ death+recovered)
## this represents the status in wuhan
summary(x_reg)
## correlation matrix
library(class)
vif(x_reg)
