head(mtcars)
summary(mtcars)
names(mtcars)
mtcars$cyl
mycars <- mtcars[mtcars$cyl == 4 & mtcars$mpg > 18, c("cyl","mpg","hp","wt")] 
head(mycars)
mtcars[1:6,1:6]
mycars$mpgPerWeight = mycars$mpg / mycars$wt
head(mycars)
head(mtcars)
boxplot(mtcars$hp, mtcars$disp) #plotting
mean(mtcars$cyl)
median(mtcars$cyl)
sd(mtcars$cyl)
IQR(mtcars$cyl)
summary(mtcars)
library(psych) #install.packages("psych")
describe(mtcars) #shows sd, skew, etc.
boxplot(mtcars)
hist(mtcars$cyl, nclass = 4) #histogram for 4 bins; nclass = bins
library(tidyverse)
mycars <- filter(mtcars, cyl == 4 & mpg > 18)
mycars <- select(mycars, cyl, mpg, hp, wt)
mycars <- mutate(mycars, mpgPerWeight = mpg / wt)
head(mycars)
# x %>% f(a, b) is same as > f(x, a, b) 
# x %>% f(a, b) %>% g(c, d) is same as g(f(a, b), c, d)
mycars <- mtcars %>%
  filter(cyl == 4 & mpg > 18) %>%
  select(cyl, mpg, hp, wt) %>%
  mutate(mpgPerWeight = mpg / wt)
head(mycars)
mycars %>% rename_with(tolower)
mycars %>% relocate(mpgPerWeight, .after = mpg) #relocates the column mentioned
library(hflights)
data(hflights)
flights = hflights
flights %>%
  group_by(Dest) %>% #group by destination
  summarise("count" = n()) #n() gives the counts and "count is used to rename the column"
#summarise() ungrouping the output
flights %>% 
  group_by(Dest) %>%
  summarise(avg_delay = mean(ArrDelay, na.rm=TRUE)) #na.rm here handles the missing values
flights %>% # for each dest show no of cancelled flights
  group_by(Dest) %>%
  select(Dest, Cancelled) %>%
  table() %>%
  head()
flights %>% filter(Dest == "AEX") %>%
  rowwise() %>%
  mutate(TotalDelay = sum(ArrDelay, DepDelay)) %>%
  summarise(TotalDelay)
#pivoting
arrest <- USArrests[,1:3] #takes first three columns for the demo
#notice
arrest <- arrest %>%
  rownames_to_column(var = "State") %>%
  pivot_longer(cols = c("Murder", "Assault"), names_to = "Crime", values_to = "value")
head(arrest)
