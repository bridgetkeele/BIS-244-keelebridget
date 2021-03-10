president <- read_csv("PRESIDENT_precinct_primary.csv")
library(tidyverse)
filter_president <- president %>% select(votes, state, candidate) %>% 
  group_by(state, candidate) %>% summarize(votes=sum(votes))
p1 <- ggplot(data = filter_president,
             mapping = aes(x = state,
                           y = votes))
p1 + geom_point(mapping=aes(color = candidate)) +theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text=element_text(size=5)) + labs(x = "State", y = "Total Votes", title = "Winning Presidential Candidate by State")
