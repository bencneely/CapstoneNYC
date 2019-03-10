## Ben Neely
## PH125_9x Capstone project

## Clear R
cat("\014")  
rm(list=ls())

## Install packages
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(lubridate)) install.packages("lubridate")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(scales)) install.packages("scales")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(smatr)) install.packages("smatr")
library(smatr)

## Location of downloaded data
## NYC Property Sales through kaggle:
## https://www.kaggle.com/new-york-city/nyc-property-sales
## https://www.kaggle.com/new-york-city/nyc-property-sales/downloads/nyc-property-sales.zip/1

## Import data from github
dat=read.csv("https://raw.githubusercontent.com/bencneely/CapstoneNYC/master/nyc-rolling-sales.csv")

## Import data that is locally stored if github doesn't work
#dat=read.csv("C:/Users/Ben.Neely/Desktop/Harvardx data science certificate/Capstone project/nyc-rolling-sales.csv")

## Rename columns to improve workability
names(dat)=c("id","borough","hood","bldg.class.cat","tax.class","block","lot","easement","bldg.class.prsnt",
             "address","apt.num","zip","res.units","com.units","tot.units","land.area","gross.area","yr.built",
             "tax.class.sale","bldg.class.sale","price","datetime")

## Identify some descriptive aspects of the raw data set
nrow(dat)
min(as.Date(dat$datetime))
max(as.Date(dat$datetime))

#################################################################################
## Data cleanup
#################################################################################

## Go through variables individually to reformat (typically to factor)
## and remove variables that aren't needed for analyses
## The goal here is to get a bunch of predictive factors that can be 
## related to sale price in an effort to predict sale price
## Some things of note...
## I grouped residential units into 0, 1, 2, 3, 4-10, 11-50, and 50+
## I grouped commercial units into 0, 1, 2, 3, and 4+
## I classified a property as residential or commercial depending on which had more units
## Properties were identified as residential if there were equal res and com units
## I grouped land area and gross area into 0, 1-2000, 2001-4000, and 4001+
## I deleted properties built prior to 1800 (most were missing data)
## I removed properties with sale price < $10000 and > $1000000000 to get rid of deed transfers and outliers

nyc=dat%>%
  select(-id)%>%
  mutate(borough=as.factor(borough))%>%
  select(-hood,-bldg.class.cat,-tax.class,-block,-lot,-easement,-bldg.class.prsnt,-address,-apt.num)%>%
  mutate(zip=as.factor(zip),
         resunits=ifelse(res.units==0,"0",
                  ifelse(res.units==1,"1",
                  ifelse(res.units==2,"2",
                  ifelse(res.units==3,"3",
                  ifelse(res.units>=4&res.units<=10,"4:10",
                  ifelse(res.units>=11&res.units<=50,"11:50",
                  ifelse(res.units>=51,"51+",NA))))))),
         resunits=as.factor(resunits),
         comunits=ifelse(com.units==0,"0",
                  ifelse(com.units==1,"1",
                  ifelse(com.units==2,"2",
                  ifelse(com.units==3,"3",
                  ifelse(com.units>=4,"4+",NA))))),
         comunits=as.factor(comunits),
         rescom=ifelse(res.units>=com.units,"res","com"),
         rescom=as.factor(rescom))%>%
  select(-res.units,-com.units,-tot.units)%>%
  mutate(land.area=as.numeric(land.area),
         landareacat=ifelse(land.area==0,"0",
                     ifelse(land.area>0&land.area<=2000,"1:2000",
                     ifelse(land.area>2000&land.area<=4000,"2001:4000",
                     ifelse(land.area>4000,"4000+",NA)))),
         landareacat=as.factor(landareacat),
         gross.area=as.numeric(gross.area),
         grossareacat=ifelse(gross.area==0,"0",
                      ifelse(gross.area>0&gross.area<=2000,"1:2000",
                      ifelse(gross.area>2000&gross.area<=4000,"2001:4000",
                      ifelse(gross.area>4000,"4000+",NA)))),
         grossareacat=as.factor(grossareacat),
         decade=as.factor(yr.built-(yr.built%%10)),
         tax.class=as.factor(tax.class.sale),
         bldg.class=as.character(bldg.class.sale),
         bldg.class=substr(bldg.class,1,1),
         bldg.class=as.factor(bldg.class),
         datetime=as.Date(datetime),
         day=as.factor(weekdays(datetime)),
         month=as.factor(month(datetime)))%>%
  select(-land.area,-gross.area,-yr.built,-tax.class.sale,-bldg.class.sale,-datetime)%>%
  mutate(price=gsub(" -  ",NA,price),
         price=as.numeric(price))%>%
  filter(price>=10000&price<1000000000)%>%
  drop_na(price)

## Check out data structure and a few rows
str(nyc)
head(nyc,20)

## Note that zip has 186 levels so it won't work as a predictor
## Note that there are 24 decades so something might be off
xtabs(~decade,nyc)
## There aren't enough built before 1890 to get good predictions
## We'll delete all rows built before 1890
## Note that there are 25 building classes that should be reduced
xtabs(~bldg.class,nyc)
## Only retain classes that have more than 100 rows
keeps=c("A","B","C","D","E","F","G","H","K","O","R","S","V")

nycdat=nyc%>%
  select(-zip)%>%
  mutate(decade=as.numeric(as.character(decade)))%>%
  filter(decade>=1890)%>%
  mutate(decade=as.factor(decade))%>%
  filter(bldg.class %in% keeps)%>%
  droplevels()

## Recheck data
str(nycdat)
head(nycdat,20)

## Write data to a csv file for manual checking to make sure eveything looks good
write.csv(nycdat,"C:/Users/Ben.Neely/Desktop/Harvardx data science certificate/Capstone project/cleandat.csv")

## Look at distribution of price
nycdat%>%
  ggplot(aes(x=price))+
  geom_histogram(bins=100)+
  scale_x_continuous(labels=dollar)
  labs(x="Price (USD)",y="Count")+
  theme_classic()

## Log10 transform price and look at distribution
nycdat=nycdat%>%
  mutate(logprice=log10(price))
nycdat%>%
  ggplot(aes(x=logprice))+
  geom_histogram(bins=100)+
  scale_x_continuous(labels=dollar)
  labs(x="Log10 Price $USD",y="Count")+
  theme_classic()
## Develop predictive models on log10 price to account for non-normality of price

## Examine boxplots of price by factor
(borough_plot=nycdat%>%
  ggplot(aes(x=borough,y=price))+
  geom_boxplot()+
  scale_y_log10(labels=dollar)+
  labs(x="Borough",y="Price"))+
  theme_classic()

(resunits_plot=nycdat%>%
  ggplot(aes(x=resunits,y=price))+
  geom_boxplot()+
  scale_y_log10(labels=dollar)+
  scale_x_discrete(limits=c("0","1","2","3","4:10","11:50","51+"))+
  labs(x="Residential Units",y="Price"))+
  theme_classic()
  
(comunits_plot=nycdat%>%
  ggplot(aes(x=comunits,y=price))+
  geom_boxplot()+
  scale_y_log10(labels=dollar)+
  labs(x="Commercial Units",y="Price"))+
  theme_classic()
  
(rescom_plot=nycdat%>%
  ggplot(aes(x=rescom,y=price))+
  geom_boxplot()+
  scale_y_log10(labels=dollar)+
  labs(x="Residential vs Commercial",y="Price"))+
  theme_classic()
  
(landarea_plot=nycdat%>%
  ggplot(aes(x=landareacat,y=price))+
  geom_boxplot()+
  scale_y_log10(labels=dollar)+
  labs(x="Land area (sq ft)",y="Price"))+
  theme_classic()
  
(grossarea_plot=nycdat%>%
  ggplot(aes(x=grossareacat,y=price))+
  geom_boxplot()+
  scale_y_log10(labels=dollar)+
  labs(x="Gross area (sq ft)",y="Price"))+
  theme_classic()

(decadebuilt_plot=nycdat%>%
  ggplot(aes(x=decade,y=price))+
  geom_boxplot()+
  scale_y_log10(labels=dollar)+
  labs(x="Decade built",y="Price"))+
  theme_classic()
  
(taxclass_plot=nycdat%>%
  ggplot(aes(x=tax.class,y=price))+
  geom_boxplot()+
  scale_y_log10(labels=dollar)+
  labs(x="Tax class",y="Price"))+
  theme_classic()
  
(buildingclass_plot=nycdat%>%
  ggplot(aes(x=bldg.class,y=price))+
  geom_boxplot()+
  scale_y_log10(labels=dollar)+
  labs(x="Building class",y="Price"))+
  theme_classic()
  
(day_plot=nycdat%>%
  ggplot(aes(x=day,y=price))+
  geom_boxplot()+
  scale_y_log10(labels=dollar)+
  scale_x_discrete(limits=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  labs(x="Day of week",y="Price"))+
  theme_classic()

(month_plot=nycdat%>%
  ggplot(aes(x=month,y=price))+
  geom_boxplot()+
  scale_y_log10(labels=dollar)+
  labs(x="Month",y="Price"))+
  theme_classic()
  
grid.arrange(borough_plot,resunits_plot,comunits_plot,rescom_plot,landarea_plot,grossarea_plot,
             decadebuilt_plot,taxclass_plot,buildingclass_plot,day_plot,month_plot,nrow=3)
## Because this is a data exercise, I will go through all predictors even though 
## some look like they won't affect purchase price

## Create test data set that consists of 25% of log10 prices
set.seed(1)
test_index=createDataPartition(y=nycdat$logprice,times=1,p=0.25,list=FALSE)

validation=nycdat[-test_index,]
train=nycdat[test_index,]

#################################################################################
## Create RMSE function to evaluate models
RMSE=function(true_price,predicted_price){
  sqrt(mean((true_price-predicted_price)^2))
}
#################################################################################
## Look at average of all the log10 prices from training set
(mu_hat=mean(train$logprice))

## Look at RMSE by simply predicting each movie to have the mean log10 price (mu_hat)
(naive_rmse=RMSE(train$logprice,mu_hat))

## Create table to store results from different modeling approaches
rmse_results=tibble(Model="Average model",RMSE=naive_rmse)
kable(rmse_results)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
#################################################################################

## Estimate average price by borough as b_b (Borough Effects Model)
mu=mean(train$logprice)
borough_avgs=train%>%
  group_by(borough)%>%
  summarize(b_b=mean(logprice-mu))

## Run predictions using borough effect model (b_b) calculated above
predicted_price=mu+train%>%
  left_join(borough_avgs,by='borough')%>%
  .$b_b

model_1_rmse=RMSE(predicted_price,train$logprice)
rmse_results=bind_rows(rmse_results,
                       tibble(Model="Borough Effect Model",
                              RMSE=model_1_rmse))

## Add model and RMSE to table
kable(rmse_results)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
#################################################################################

## Estimate average price by number of residential units as b_r (Residential units Effect Model)
resunits_avgs=train%>%
  left_join(borough_avgs,by='borough')%>%
  group_by(resunits)%>%
  summarize(b_r=mean(logprice-mu-b_b))

## Run predictions using borough effect model (b_b) and res units effect model (b_r) calculated above
predicted_price=train%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  mutate(pred=mu+b_b+b_r)%>%
  .$pred

model_2_rmse=RMSE(predicted_price,train$logprice)
rmse_results=bind_rows(rmse_results,
                       data_frame(Model="B + Residential Units Effects Model",
                                  RMSE=model_2_rmse))

## Add model and RMSE to table
kable(rmse_results)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
#################################################################################

## Estimate average price by commercial units as b_c (Commercial Units Effect Model)
comunits_avgs=train%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  group_by(comunits)%>%
  summarize(b_c=mean(logprice-mu-b_b-b_r))

## Run predictions using borough effect model (b_b), res units effect model (b_r),
## and comm units effect model (b_c) calculated above
predicted_price=train%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  left_join(comunits_avgs,by='comunits')%>%
  mutate(pred=mu+b_b+b_r+b_c)%>%
  .$pred

model_3_rmse=RMSE(predicted_price,train$logprice)
rmse_results=bind_rows(rmse_results,
                       data_frame(Model="B + RU + Commercial Units Effects Model",
                                  RMSE=model_3_rmse))

## Add model and RMSE to table
kable(rmse_results)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
#################################################################################

## Estimate average price by property type (res or com) as b_pt (Property Type Effect Model)
proptype_avgs=train%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  left_join(comunits_avgs,by='comunits')%>%
  group_by(rescom)%>%
  summarize(b_pt=mean(logprice-mu-b_b-b_r-b_c))

## Run predictions using borough effect model (b_b), res unit effect model (b_r), 
## com unit effect model (b_c), and property type (b_pt) calculated above
predicted_price=train%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  left_join(comunits_avgs,by='comunits')%>%
  left_join(proptype_avgs,by='rescom')%>%
  mutate(pred=mu+b_b+b_r+b_c+b_pt)%>%
  .$pred

model_4_rmse=RMSE(predicted_price,train$logprice)
rmse_results=bind_rows(rmse_results,
                       data_frame(Model="B + RU + CU + Property Type Effects Model",
                                  RMSE=model_4_rmse))

## Add model and RMSE to table
kable(rmse_results)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
#################################################################################

## Estimate average price by land area as b_la (Land Area Effect Model)
landarea_avgs=train%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  left_join(comunits_avgs,by='comunits')%>%
  left_join(proptype_avgs,by='rescom')%>%
  group_by(landareacat)%>%
  summarize(b_la=mean(logprice-mu-b_b-b_r-b_c-b_pt))

## Run predictions using borough effect model (b_b), res unit effect model (b_r), 
## com unit effect model (b_c), property type (b_pt), and land area (b_la) calculated above
predicted_price=train%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  left_join(comunits_avgs,by='comunits')%>%
  left_join(proptype_avgs,by='rescom')%>%
  left_join(landarea_avgs,by='landareacat')%>%
  mutate(pred=mu+b_b+b_r+b_c+b_pt+b_la)%>%
  .$pred

model_5_rmse=RMSE(predicted_price,train$logprice)
rmse_results=bind_rows(rmse_results,
                       data_frame(Model="B + RU + CU + PT + Land Area Effects Model",
                                  RMSE=model_5_rmse))

## Add model and RMSE to table
kable(rmse_results)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
#################################################################################

## Estimate average price by gross area as b_ga (Gross Area Effect Model)
grossarea_avgs=train%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  left_join(comunits_avgs,by='comunits')%>%
  left_join(proptype_avgs,by='rescom')%>%
  left_join(landarea_avgs,by='landareacat')%>%
  group_by(grossareacat)%>%
  summarize(b_ga=mean(logprice-mu-b_b-b_r-b_c-b_pt-b_la))

## Run predictions using borough effect model (b_b), res unit effect model (b_r), 
## com unit effect model (b_c), property type (b_pt), land area (b_la), and 
## gross area (b_ga) calculated above
predicted_price=train%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  left_join(comunits_avgs,by='comunits')%>%
  left_join(proptype_avgs,by='rescom')%>%
  left_join(landarea_avgs,by='landareacat')%>%
  left_join(grossarea_avgs,by='grossareacat')%>%
  mutate(pred=mu+b_b+b_r+b_c+b_pt+b_la+b_ga)%>%
  .$pred

model_6_rmse=RMSE(predicted_price,train$logprice)
rmse_results=bind_rows(rmse_results,
                       data_frame(Model="B + RU + CU + PT + LA + Gross Area Effects Model",
                                  RMSE=model_6_rmse))

## Add model and RMSE to table
kable(rmse_results)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
#################################################################################

## Estimate average price by decade built as b_d (Decade Built Effect Model)
decade_avgs=train%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  left_join(comunits_avgs,by='comunits')%>%
  left_join(proptype_avgs,by='rescom')%>%
  left_join(landarea_avgs,by='landareacat')%>%
  left_join(grossarea_avgs,by='grossareacat')%>%
  group_by(decade)%>%
  summarize(b_d=mean(logprice-mu-b_b-b_r-b_c-b_pt-b_la-b_ga))

## Run predictions using borough effect model (b_b), res unit effect model (b_r), 
## com unit effect model (b_c), property type (b_pt), land area (b_la),  
## gross area (b_ga), and decade built (b_d) calculated above
predicted_price=train%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  left_join(comunits_avgs,by='comunits')%>%
  left_join(proptype_avgs,by='rescom')%>%
  left_join(landarea_avgs,by='landareacat')%>%
  left_join(grossarea_avgs,by='grossareacat')%>%
  left_join(decade_avgs,by='decade')%>%
  mutate(pred=mu+b_b+b_r+b_c+b_pt+b_la+b_ga+b_d)%>%
  .$pred

model_7_rmse=RMSE(predicted_price,train$logprice)
rmse_results=bind_rows(rmse_results,
                       data_frame(Model="B + RU + CU + PT + LA + GA + Decade Effects Model",
                                  RMSE=model_7_rmse))

## Add model and RMSE to table
kable(rmse_results)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
#################################################################################

## Estimate average price by tax class as b_t (Tax Class Effect Model)
tax_avgs=train%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  left_join(comunits_avgs,by='comunits')%>%
  left_join(proptype_avgs,by='rescom')%>%
  left_join(landarea_avgs,by='landareacat')%>%
  left_join(grossarea_avgs,by='grossareacat')%>%
  left_join(decade_avgs,by='decade')%>%
  group_by(tax.class)%>%
  summarize(b_t=mean(logprice-mu-b_b-b_r-b_c-b_pt-b_la-b_ga-b_d))

## Run predictions using borough effect model (b_b), res unit effect model (b_r), 
## com unit effect model (b_c), property type (b_pt), land area (b_la),  
## gross area (b_ga), decade built (b_d), and tax class (b_t) calculated above
predicted_price=train%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  left_join(comunits_avgs,by='comunits')%>%
  left_join(proptype_avgs,by='rescom')%>%
  left_join(landarea_avgs,by='landareacat')%>%
  left_join(grossarea_avgs,by='grossareacat')%>%
  left_join(decade_avgs,by='decade')%>%
  left_join(tax_avgs,by='tax.class')%>%
  mutate(pred=mu+b_b+b_r+b_c+b_pt+b_la+b_ga+b_d+b_t)%>%
  .$pred

model_8_rmse=RMSE(predicted_price,train$logprice)
rmse_results=bind_rows(rmse_results,
                       data_frame(Model="B + RU + CU + PT + LA + GA + D + Tax Class Effects Model",
                                  RMSE=model_8_rmse))

## Add model and RMSE to table
kable(rmse_results)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
#################################################################################

## Estimate average price by building class as b_bc (Building Class Effect Model)
bldgclass_avgs=train%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  left_join(comunits_avgs,by='comunits')%>%
  left_join(proptype_avgs,by='rescom')%>%
  left_join(landarea_avgs,by='landareacat')%>%
  left_join(grossarea_avgs,by='grossareacat')%>%
  left_join(decade_avgs,by='decade')%>%
  left_join(tax_avgs,by='tax.class')%>%
  group_by(bldg.class)%>%
  summarize(b_bc=mean(logprice-mu-b_b-b_r-b_c-b_pt-b_la-b_ga-b_d-b_t))

## Run predictions using borough effect model (b_b), res unit effect model (b_r), 
## com unit effect model (b_c), property type (b_pt), land area (b_la),  
## gross area (b_ga), decade built (b_d), tax class (b_t), and building class (b_bc) calculated above
predicted_price=train%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  left_join(comunits_avgs,by='comunits')%>%
  left_join(proptype_avgs,by='rescom')%>%
  left_join(landarea_avgs,by='landareacat')%>%
  left_join(grossarea_avgs,by='grossareacat')%>%
  left_join(decade_avgs,by='decade')%>%
  left_join(tax_avgs,by='tax.class')%>%
  left_join(bldgclass_avgs,by='bldg.class')%>%
  mutate(pred=mu+b_b+b_r+b_c+b_pt+b_la+b_ga+b_d+b_t+b_bc)%>%
  .$pred

model_9_rmse=RMSE(predicted_price,train$logprice)
rmse_results=bind_rows(rmse_results,
                       data_frame(Model="B + RU + CU + PT + LA + GA + D + TC + Building Class Effects Model",
                                  RMSE=model_9_rmse))

## Add model and RMSE to table
kable(rmse_results)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

#################################################################################

## Estimate average price by day sold as b_ds (Day Sold Effect Model)
daysold_avgs=train%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  left_join(comunits_avgs,by='comunits')%>%
  left_join(proptype_avgs,by='rescom')%>%
  left_join(landarea_avgs,by='landareacat')%>%
  left_join(grossarea_avgs,by='grossareacat')%>%
  left_join(decade_avgs,by='decade')%>%
  left_join(tax_avgs,by='tax.class')%>%
  left_join(bldgclass_avgs,by='bldg.class')%>%
  group_by(day)%>%
  summarize(b_ds=mean(logprice-mu-b_b-b_r-b_c-b_pt-b_la-b_ga-b_d-b_t-b_bc))

## Run predictions using borough effect model (b_b), res unit effect model (b_r), 
## com unit effect model (b_c), property type (b_pt), land area (b_la),  
## gross area (b_ga), decade built (b_d), tax class (b_t), building class (b_bc),
## and day sold (b_ds) calculated above
predicted_price=train%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  left_join(comunits_avgs,by='comunits')%>%
  left_join(proptype_avgs,by='rescom')%>%
  left_join(landarea_avgs,by='landareacat')%>%
  left_join(grossarea_avgs,by='grossareacat')%>%
  left_join(decade_avgs,by='decade')%>%
  left_join(tax_avgs,by='tax.class')%>%
  left_join(bldgclass_avgs,by='bldg.class')%>%
  left_join(daysold_avgs,by='day')%>%
  mutate(pred=mu+b_b+b_r+b_c+b_pt+b_la+b_ga+b_d+b_t+b_bc+b_ds)%>%
  .$pred

model_10_rmse=RMSE(predicted_price,train$logprice)
rmse_results=bind_rows(rmse_results,
                       data_frame(Model="B + RU + CU + PT + LA + GA + D + TC + BC + Day Sold Effects Model",
                                  RMSE=model_10_rmse))

## Add model and RMSE to table
kable(rmse_results)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
#################################################################################

## Estimate average price by day sold as b_ms (Month Sold Effect Model)
monthsold_avgs=train%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  left_join(comunits_avgs,by='comunits')%>%
  left_join(proptype_avgs,by='rescom')%>%
  left_join(landarea_avgs,by='landareacat')%>%
  left_join(grossarea_avgs,by='grossareacat')%>%
  left_join(decade_avgs,by='decade')%>%
  left_join(tax_avgs,by='tax.class')%>%
  left_join(bldgclass_avgs,by='bldg.class')%>%
  left_join(daysold_avgs,by='day')%>%
  group_by(month)%>%
  summarize(b_ms=mean(logprice-mu-b_b-b_r-b_c-b_pt-b_la-b_ga-b_d-b_t-b_bc-b_ds))

## Run predictions using borough effect model (b_b), res unit effect model (b_r), 
## com unit effect model (b_c), property type (b_pt), land area (b_la),  
## gross area (b_ga), decade built (b_d), tax class (b_t), building class (b_bc),
## day sold (b_ds), and month sold (b_ms) calculated above
predicted_price=train%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  left_join(comunits_avgs,by='comunits')%>%
  left_join(proptype_avgs,by='rescom')%>%
  left_join(landarea_avgs,by='landareacat')%>%
  left_join(grossarea_avgs,by='grossareacat')%>%
  left_join(decade_avgs,by='decade')%>%
  left_join(tax_avgs,by='tax.class')%>%
  left_join(bldgclass_avgs,by='bldg.class')%>%
  left_join(daysold_avgs,by='day')%>%
  left_join(monthsold_avgs,by='month')%>%
  mutate(pred=mu+b_b+b_r+b_c+b_pt+b_la+b_ga+b_d+b_t+b_bc+b_ds+b_ms)%>%
  .$pred

model_11_rmse=RMSE(predicted_price,train$logprice)
rmse_results=bind_rows(rmse_results,
                       data_frame(Model="B + RU + CU + PT + LA + GA + D + TC + BC + DS + Month Sold Effects Model",
                                  RMSE=model_11_rmse))

## Add model and RMSE to table
kable(rmse_results)%>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

#################################################################################
## Borough + Res Units + Com Units + Property Type + Land Area + Gross Area +
## Decade Built + Tax Class + Buildling Class + Day Sold + Month Sold model has
## lowest RMSE (0.331)
## Apply this model to the validation data set to assess accuracy

## Predict price on validation data set
predicted_price=validation%>%
  left_join(borough_avgs,by='borough')%>%
  left_join(resunits_avgs,by='resunits')%>%
  left_join(comunits_avgs,by='comunits')%>%
  left_join(proptype_avgs,by='rescom')%>%
  left_join(landarea_avgs,by='landareacat')%>%
  left_join(grossarea_avgs,by='grossareacat')%>%
  left_join(decade_avgs,by='decade')%>%
  left_join(tax_avgs,by='tax.class')%>%
  left_join(bldgclass_avgs,by='bldg.class')%>%
  left_join(daysold_avgs,by='day')%>%
  left_join(monthsold_avgs,by='month')%>%
  mutate(pred=mu+b_b+b_r+b_c+b_pt+b_la+b_ga+b_d+b_t+b_bc+b_ds+b_ms)%>%
  .$pred

## Accuracy isn't the best indicator here because price is continuous
## Isolate predicted price and logprice as data pairs
valcomp=validation%>%
  mutate(pred_price=predicted_price)%>%
  select(logprice,pred_price)%>%
  mutate(diff=abs(10^pred_price-10^logprice),
         price=10^logprice,
         pred=10^pred_price)%>%
  as.data.frame()

## Look at summary statistics of price for later comparison
(mean_price=mean(valcomp$price))
(sd_price=sd(valcomp$price))
(n_price=length(valcomp$price))
(se1_price=sd_price/sqrt(n_price))
(se2_price=2*se1_price)

## Create linear model of log10 price vs log10 predicted price
valcomp.lm=lm(valcomp$logprice~valcomp$pred_price)
summary(valcomp.lm)
confint(valcomp.lm)

## Use slope.test to compare slope of regression line to 1
slope.test(y=valcomp$logprice,x=valcomp$pred_price,test.value=1,method='OLS')
## F-test: P<0.0001 -- Slope of line does not equal 1

## Plot predicted price vs logprice with regression line and 1:1 line 
valcomp%>%
  ggplot(aes(x=10^pred_price,y=10^logprice))+
  geom_segment(x=log10(80000),y=log10(80000),xend=log10(110000000),yend=log10(110000000),col="red",linetype="dashed",size=2)+
  geom_point(col=rgb(0,0,0,0.1))+
  scale_y_log10(limits=c(10000,2200000000),labels=dollar)+
  scale_x_log10(limits=c(10000,2200000000),labels=dollar)+
  geom_smooth(method='lm',se=F,col="blue",size=2)+
  labs(x="Predicted price (USD)",y="Actual price (USD)")+
  annotate(geom="text",x=50000000,y=90000,label="y = 1.047x - 0.276",col="blue",hjust=0,size=6)+
  annotate(geom="text",x=50000000,y=54000,label="y = x",col="red",hjust=0,size=6)+
  annotate(geom="text",x=50000000,y=30000,label="F-Test: p < 0.0001",hjust=0,size=6)+
  theme_classic()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"))

## See how frequently predictions were within 1 and 2 SE of actual price
valcomp=valcomp%>%
  mutate(SE1=ifelse(diff<=se1_price,1,0),
         SE2=ifelse(diff<=se2_price,1,0))

(SE1=sum(valcomp$SE1/n_price))
(SE2=sum(valcomp$SE2/n_price))

## Plot difference in actual price and predicted price as histogram
valcomp%>%
  ggplot(aes(x=diff))+
  geom_histogram(bins=100)+
  scale_x_log10(expand=c(0,0),limits=c(1,1000000000),labels=dollar)+
  scale_y_continuous(expand=c(0,0),limits=c(0,3000))+
  labs(x="Difference Actual and Predicted (USD)",y="Frequency")+
  theme_classic()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"))