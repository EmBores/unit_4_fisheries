##EmB
##class notes
## 2023-03-21

source("build_collapse_table.R")
head(fish)
head(collapse)

#logistic regression
model_data=collapse %>%
  group_by(stockid)%>%
  summarize(ever_collapsed=any(current_collapse))%>%
  ungroup()%>%
  left_join(stock)%>%
  left_join(metadata)%>%
  mutate(FisheryType=as.factor(FisheryType))%>%
  filter(!is.na(FisheryType))
  
glimpse(model_data)
summary(model_data)

# build logistic regression
model_l=glm(ever_collapsed ~ FisheryType, data=model_data, family="binomial")
summary(model_l)

model_data %>% distinct(FisheryType)%>% arrange(FisheryType)

FisheryType=model_data%>%
  distinct(FisheryType)

model_l_predict= predict(model_l, newdata=FisheryType, se.fit=TRUE, type="response")
head(model_l_predict)
collapse_fishery_type_prediction=cbind(FisheryType, model_l_predict)

ggplot(data=collapse_fishery_type_prediction, 
       aes(x=FisheryType, y=fit, fill=FisheryType)) +
  geom_bar(stat="identity", show.legend=FALSE)+
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), width=0.2)+
  coord_flip()

#poisson model

u_summary= timeseries_values_views %>%
  filter(!is.na(UdivUmsypref),
         !is.na(BdivBmsypref))%>%
  group_by(stockid)%>%
  summarize(yrs_data=n(),
            ratio_yrs_overfished=sum(UdivUmsypref>1)/yrs_data,
            ratio_yrs_low_stock=sum(BdivBmsypref<1)/yrs_data) %>%
  select(-yrs_data) #hyphen says drop it
head(u_summary)

collapse_summary= collapse %>%
  group_by(stockid)%>%
  summarize(yrs_data=n(),
            yrs_collapsed=sum(current_collapse))%>%
  inner_join(u_summary, by="stockid")

head(collapse_summary)
hist(collapse_summary$yrs_collapsed)
table(collapse_summary$yrs_collapsed)

collapse_summary_zero_trunc= collapse_summary %>% 
  filter(yrs_collapsed >0)
table(collapse_summary_zero_trunc$yrs_collapsed)

#build model

model_p=glm(yrs_collapsed ~ ratio_yrs_overfished+ratio_yrs_low_stock,
            offset(log(yrs_data)),
            data=collapse_summary_zero_trunc,
            family="poisson")
summary(model_p)
library(AER)
AER::dispersiontest(model_p) # p value below alpha, model is dispersed

#solution quasi poison to deal with over-dispersion

model_qp=glm(yrs_collapsed ~ ratio_yrs_overfished+ratio_yrs_low_stock,
            offset(log(yrs_data)),
            data=collapse_summary_zero_trunc,
            family="quasipoisson")
summary(model_qp)


