#Assignment 05 

library(tidyverse)
library(readxl)
library("ggplot2")
library("maps")

us_states <- map_data("state")
Scorecard <- read_excel("CollegeScorecardDataDictionary.xlsx", sheet=4) 
Data <- read_csv("most-recent-cohorts-all-data-elements-1.csv")

#'x' is the column of a data.frame that holds 2 digit state codes
stateFromLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
  
}
Data$STABBRn<-stateFromLower(Data$STABBR)

Data2 <- Data %>% select(C150_4, STABBRn) %>% 
  mutate(C150_4 = as.numeric(C150_4)) %>% group_by(STABBRn) %>% 
  summarize(Percent_Completion = mean(C150_4, na.rm = TRUE)*100)


us_states <- left_join(us_states, Data2, 
                       by = c("region" = "STABBRn"))

# Coloring the states
p <- ggplot(data = us_states,
            aes(x = long, y = lat,
                group = group, fill = Percent_Completion))

p + geom_polygon(color = "gray90", size = 0.1) + ggtitle(" Average Completion rate for first-time, full-time students 
at four-year institutions (150% of expected time to completion)")
