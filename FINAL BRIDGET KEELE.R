# BIS244 Final Exam - Bridget Keele

# Clear out the environment
rm(list=ls(all=TRUE)) 
cat("\014") 

#Install and run packages
library(gapminder) 
library(tidyverse) 
library(ggrepel) 
library(socviz) 
library(grid) 
library(maps)
library(readr)
counties <- map_data("county")

head(counties)
dim(counties)

party_colors <- c("#2E74C0", "#CB454A", "#FFFFFF") 

#Structrue data set
df <- read_csv("finaldata.csv")
df <- as.data.frame(df)

view(counties)

# Color and make map for the plot
df$region <- tolower(df$state)

df$party <- 0
datalength <- length(df$fips)

for (i in 1:datalength){
  if(df$trumpd[i] > df$bidenj[i]){
    df$party[i] <- "Republican"
  } else if (df$trumpd[i] < df$bidenj[i]){
    df$party[i] <- "Democratic"
  } 
}
View(df)
df$subregion <- tolower(df$name)
elections <- left_join(counties, df)

View(elections)


p <- ggplot(data = elections,
            aes(x = long, y = lat,
                group = group, fill = party))

p + geom_polygon(color = "gray90", size = 0.1) + guides(fill = FALSE)


theme_map <- function(base_size=9, base_family="") {
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
    )
}

#Complete the plot
p0 <- ggplot(data = elections,
             mapping = aes(x = long, y = lat,
                           group = group, fill = party))
p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
p2 <- p1 + scale_fill_manual(values = party_colors) +
  labs(title = "Presidential Election Results 2020", subtitle = "created by Bridget Keele 5/15/2021", fill = NULL)
p2 + theme_map()

