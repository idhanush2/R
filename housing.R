housing <- read.csv(file.choose())
str(housing)
housing$ocean_proximity <- as.factor(housing$ocean_proximity)
str(housing)
summary(housing)
hist(housing$median_house_value)
hist(housing$median_income)
hist(housing$housing_median_age, breaks = 20)
hist(housing$total_bedrooms)
cor(housing[,3:9])
cor(housing[,3:9], use = 'complete.obs') #to leave out datapoints with NAs and use complete obs

plot(housing$housing_median_age, housing$median_house_value)
plot(housing$total_rooms, housing$median_house_value)
plot(housing$total_bedrooms, housing$median_house_value)
plot(housing$population, housing$median_house_value)
plot(housing$households, housing$median_house_value)
plot(housing$median_income, housing$median_house_value)


#simple model building

m1 <- lm(median_house_value ~ housing_median_age, data = housing)
summary(m1)

m2 <- lm(median_house_value ~ total_rooms, data = housing)
summary(m2)

m3 <- lm(median_house_value ~ total_bedrooms, data = housing)
summary(m3)

m4 <- lm(median_house_value ~ population, data = housing)
summary(m4)

m5 <- lm(median_house_value ~ households, data = housing)
summary(m5)

m6 <- lm(median_house_value ~ total_bedrooms, data = housing)
summary(m6)

m7 <- lm(median_house_value ~ housing_median_age +
           population + households + median_income, data = housing)
summary(m7)

m8 <- lm(median_house_value ~ housing_median_age +
           population + households + median_income +
           population * median_income, data = housing)
summary(m8)

housing$mi2 <- housing$median_income^2
str(housing)

m9 <- lm(median_house_value ~ housing_median_age +
           population + households + median_income +
           mi2, data = housing)
summary(m9)

m10 <- lm(median_house_value ~ housing_median_age +
            population + households + median_income +
            ocean_proximity, data = housing)
summary(m10)
