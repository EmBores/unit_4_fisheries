##EmBores
##2023-03-16

library(tidyverse)
load('data/RAMLDB v4.495/R Data/DBdata[asmt][v4.495].RData')
head(area)

#metadata- information about the data

glimpse(stock)
glimpse(timeseries_values_views)
glimpse(taxonomy)


fish= timeseries_values_views %>%
  left_join(stock)%>%
  left_join(taxonomy)%>%
  select(stockid, stocklong, year, TCbest, tsn, scientificname, commonname, region,
         FisheryType, taxGroup)

#start with long data (time series) and left joing with shorter tables

glimpse(fish)
dim(timeseries_values_views)
dim(fish)


glimpse(tsmetrics)
tsmetrics%>%filter(tsshort=="TCbest")

ggplot()+
  geom_line(aes(x=year, y=TCbest, color=stockid), data=fish)+
  theme(legend.position="none")

#which fish has the largest stock

fish %>% filter(TCbest>20000000)


#the red line on ggplot above is the acadian redfish, which isn't correct (authors made a mistake) so 
# we are removing it

fish=fish %>%
  filter(stockid != "ACADREDGOMGB")

ggplot()+
  geom_line(aes(x=year, y=TCbest, color=stockid), data=fish)+
  theme(legend.position="none")

fish %>% filter(TCbest>6000000)

#canadian cod stock collapses

fish%>%
  filter(scientificname=="Gadus morhua") %>%
  distinct(region) #distinct dplyr= unique versions of a single variable

cod_can= fish %>%
  filter(scientificname=="Gadus morhua",
         region=="Canada East Coast",
         !is.na(TCbest))
head(cod_can)

ggplot(data=cod_can)+
  geom_line(aes(x=year, y=TCbest, color=stockid))

#add all cod stock together

cod_can_total=cod_can %>%
  group_by(year) %>%
  summarize(total_catch=sum(TCbest))
head(cod_can_total)

ggplot(data=cod_can_total)+
  geom_line(aes(x=year, y=total_catch))

##did the cod fishery collapse? What is the maximum historical catch, and is the 
##catch this year less than 10% of that?

#showing cumulative function

dat=c(1,3,6,2,3,9,-1)
dat_max= cummax(dat)
dat_sum=cumsum(dat)
cbind(dat, dat_max, dat_sum)

cod_collapse= cod_can_total %>%
  mutate(historical_max_catch=cummax(total_catch),
         collapse=total_catch <= 0.1*historical_max_catch)

head(cod_collapse)
##in 1920, did our stock collapse? true or false?

#rises with total catch but never falls

tail(cod_collapse)

#figure with vertical line where cod collapse- earliest year where it goes from false to true

cod_collapse_year= cod_collapse %>%
  filter(collapse==TRUE)%>%
  summarize(year=min(year)) %>%
  pull(year) #does not make it a tibble

ggplot()+
  geom_line(aes(x=year, y=total_catch, color=collapse), data=cod_collapse)+
  geom_vline(xintercept=cod_collapse_year)

#what other stocks have collapsed?? global stock collapse assessment??

collapse= fish %>%
  filter(!is.na(TCbest)) %>%
  group_by(stockid) %>%
  mutate(historical_max_catch= cummax(TCbest), 
         current_collapse=TCbest <0.1*historical_max_catch,
         collapsed_yet=cumsum(current_collapse)>0) %>%
  ungroup()#resets us to a regular data frame not grouped
glimpse(collapse)

collapse_yr= collapse %>%
  group_by(stockid)%>%
  filter(collapsed_yet==TRUE)%>%
  summarize(first_collapse_year=min(year))
glimpse(collapse_yr)
head(collapse_yr)
