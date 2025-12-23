library(astsa)
library(ggplot2)
head(gdp)
# convert list to dataframe
df <- as.data.frame(gdp)
# adding a col representing year and quarter
df$quarter <- seq(1947.25, 2018.75, by = 0.25)
# creating ggplot object
plt <- ggplot(df, aes(x=quarter, y=x))
# adding a line layer
plt + geom_line()

plt + geom_line() +
  xlab("Time") +
  ylab("GDP") +
  ggtitle("GDP Time Series")

# BAR CHART 
ggplot(diamonds, aes(x = clarity)) + geom_bar()
# stack 
p <- ggplot(diamonds, aes(x = clarity, fill = cut))
p + geom_bar(position = "stack")
# geom_col with y 
p <- ggplot(diamonds, aes(x = clarity, y = price, fill = cut))
p + geom_col()

#HISTOGRAMS 

ggplot(mtcars, aes(hp)) + geom_histogram(binwidth = 60)

# comparing different distributions using facet_wrap layer
p <- ggplot(data = mtcars, aes(x = hp)) +
  geom_histogram(binwidth = 30) +
  facet_wrap(~cyl)
p

#BOXPLOT

ggplot(mtcars, aes(x = as.factor(cyl), y = mpg)) +
  geom_boxplot() +
  xlab("cyl")

#SCATTERPLOTS 

ggplot(mtcars, aes(mpg, hp)) + geom_point()

ggplot(mtcars, aes(mpg, hp)) +
  geom_point() +
  geom_smooth(method = lm) #adds a linear reg line 

ggplot(mtcars, aes(mpg, hp)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = lm)

#CONTINGENCY PLOTS

ggplot(mtcars, aes(x = cyl, y = gear, fill = wt)) + geom_tile()
#color is wt of the car

#QQ PLOT

ggplot(mtcars, aes(sample = hp)) +
  stat_qq() + #gets points on the plot
  stat_qq_line(col = 'red') #draws the line on the points

