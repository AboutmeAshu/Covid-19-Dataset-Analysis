rm(list=ls())  # removes all previously stored variables
library(Hmisc)

Covid_data <- read.csv("C:/Users/Ashutosh/Desktop/Covid-19 Analysis/COVID19_line_list_data.csv")

describe(Covid_data)

#For death in dataset 14 distict values i.e 0(if person didn't die), 1(if person dies) and respective dates .
# This is inconsistant and difficult to work with so, we have to clean this data , so we are creating new variable for that

Covid_data$death_clean <- as.integer(Covid_data$death != 0)  # will create new column with 0(didn't die) and 1(died) .

#death rate = sum(died_persons) / total no. of rows

death_rate <- sum(Covid_data$death_clean)/nrow(Covid_data)   # 0.05806452

# Our Media says that older person died more than the younger person due to this covid-19
# Let us prove this claim using our dataset .

# first Variable of died person
died <- subset(Covid_data, death_clean==1)   #63

# second variable is for alive person
alive <- subset(Covid_data, death_clean==0)  #1022

# Let us see the mean age of dead and alive person

mean(died$age, na.rm=TRUE)   # by removing NA entries we get mean age of died person is 68 .
mean(alive$age, na.rm=TRUE)  # And for alive person mean age is 48 .

# difference between died and alive person is 20 year . Is this significant ?
# Let us check using t-test

t.test(alive$age, died$age, alternative = "two.sided" , conf.level = 0.96)  # alternative = two.sided/less/greater

# with 96% confidence the t-test gives difference betweeen age of died and alive person is 24-16 .
# And p-value = 2.2e-16 ~ 0 
# if p < 0.05 we reject the null hypothesis .

# So here we conclude that " The people who died due to corona virus is much more older that the the people who do not died "



# Now let us see the people who died are MEN or WOMEN .

men <- subset(Covid_data, gender == "male")       # 520
female <- subset(Covid_data, gender == "female")   #382
mean(men$death_clean)
mean(female$death_clean)

# Will give 8.5% for men and 3.6% for women . here difference is about 5% .

# Let us check the significance with t-test .
t.test(men$death_clean, female$death_clean, alternative = "two.sided" , conf.level = 0.96)

# so with 96% of the confidence difference between men and the women death rate will be in between 1.5% to 8% .
# here , p-value = 0.0021 i.e < 0.05 , So we reject the null hypothesis and the value is statistically significant .

# So, we conclude that " Men are having higher death rate than women ."

