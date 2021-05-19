################################################################################
################################################################################
############################### FINAL PROJECT ##################################
############################# SANTIAGO CORNEJO #################################
################################################################################

#package installing and loading
install.packages("tidyverse")
library(dplyr)
library(ggplot2)
library(readxl)
#check working directory
getwd()
#######################visualization of the dataframe###########################
#reading & viewing the dataframe
disney_gross <- read_excel("disney_gross.xlsx")
disney_gross
####################data manipulation####################
#selecting only the numerical variables from the table
disney_numbers <- disney_gross %>%
  select(movie_title, budget, total_gross)
#Total income arranged from biggest to smallest
disney_numbers %>%
  arrange(desc(total_gross))
#Total budget arranged from biggest to smallest
disney_numbers %>%
  arrange(desc(budget))
##categorical manipulation##
#Counting the amount of movies by genre
disney_gross %>%
  count(genre,sort = TRUE)
#Counting the amount of movies by mpaa_rating
disney_gross %>%
  count(mpaa_rating, sort = TRUE)
#Counting amount of movies by year
disney_gross %>%
  count(release_year, sort = TRUE)
#################################graphs#########################################
#Histogram income
ggplot(disney_gross, aes(x = total_gross)) +
  geom_histogram(col = "red")
#Histogram budget
ggplot(disney_gross, aes(x = budget)) +
  geom_histogram(col= "red")
#bar genre
ggplot(disney_gross, aes(x = genre)) +
  geom_bar(col= "green")
#bar mpaa
ggplot(disney_gross, aes(x = mpaa_rating)) +
  geom_bar(col= "yellow")
#bar year
ggplot(disney_gross, aes(x = release_year)) +
  geom_bar(col= "purple")
############################linear regression##################################
##SCATTER PLOT
ggplot(disney_gross, aes(x = budget, y = total_gross)) +
  geom_point() +
  stat_smooth(
    method = "lm",
    color = "red",
    se = FALSE
  )
  #LINEAR MODEL
modelo <- lm(total_gross ~ budget, data = disney_gross)
confint(modelo, level = 0.95)
summary(modelo)

#prediction
new.dat <- data.frame(budget= 800000000)
predict(modelo,newdata = new.dat, interval = 'confidence')

##############################Hypothesis Testing################################
# Ho: mean1 = mean2 -> (mean1 - mean2) = 0
# Ha: mean1 ??? mean2
disneyextra <- read_excel("genre-income.xlsx")
x<- disneyextra$adventure_income
y<- disneyextra$comedy_income
mean(x)
mean(y)
t.test(x,y, alternative = "two.sided", mu=0)
# Ho: mean1 = true mean
# Ha: mean1 ??? true mean
true_mean <- mean(disney_gross$total_gross)
t.test(x = disney_gross$high_income, mu = true_mean, alternative = "two.sided")

# Ho: mean2 = true mean
# Ha: mean2 ??? true mean
t.test(x = disney_gross$low_income, mu = true_mean, alternative = "two.sided")
# Ho: mean3 = true mean
# Ha: mean3 ??? true mean
t.test(x = disney_gross$random_income, mu = true_mean, alternative = "two.sided")


#top earners
tope <- disney_gross %>%
  count(movie_title, wt = total_gross,sort = TRUE)
head(tope)
#low earners
lowe <- disney_gross %>%
  count(movie_title, wt = total_gross,sort = TRUE)
tail(lowe)

#most exp
mexp <- disney_gross %>%
  count(movie_title, wt = budget,sort = TRUE)
head(mexp)

#cheap
cheap <- disney_gross %>%
  count(movie_title, wt = budget,sort = TRUE)
tail(cheap)
