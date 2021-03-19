#Exam 1
#Bridget Keele

# Clear console
rm(list=ls(all=TRUE))
cat("\014")

library(gapminder)
library(tidyverse)
library(socviz)

# set working directory 
# Temp <- getwd()

AAPL <- read_csv("AAPL.csv")


# Set n to length of data set
n <- length(AAPL$'Adj Close')

# Initialize new variable in data frame for incremental cases
# 
AAPL$incr_cases <- 1

View(AAPL)

# Calculating values for other than first row using for loop
# Starting incremental cases in second row-Adj.Close in first to last row 
for (i in 2:n) {AAPL$incr_cases[i] <- (AAPL$'Adj Close'[i]-AAPL$'Adj Close'[i-1])}
View(AAPL)



p <- ggplot(data = AAPL,
            mapping = aes(x = Date,
                          y = incr_cases))

p + geom_point() +
  labs(x = "03/19/2020-03/18/2021", y = "Change in Adjusted Closing Price",
       title = "Changes in AAPL Daily Prices over Last Year",
       subtitle = "Bridget Keele",
       caption = "exam 1")

## Beginning color assignments

# Finding mean'

mean(AAPL$incr_cases)

meanadj <- mean(AAPL$incr_cases)

# Defining above and below mean cases

for (i in 1:n) {if(AAPL$incr_cases[i]>=meanadj) {AAPL$above_cases[i] <- AAPL$incr_cases[i] 
}else{
    AAPL$below_cases[i] <- AAPL$incr_cases[i]
}
  }

View(AAPL)




# Plotting
p <- ggplot(data = AAPL,
            mapping = aes(x = Date,
                          y = incr_cases))
p = ggplot() + 
  geom_point(data = AAPL, aes(x = Date, y = above_cases), color = "blue") +
  geom_point(data = AAPL, aes(x = Date, y = below_cases), color = "red") +
  labs(x = "03/19/2020-03/18/2021", y = "Change in Adjusted Closing Price",
       title = "Changes in AAPL Daily Prices over Last Year", 
       caption = "Exam 1")

p
