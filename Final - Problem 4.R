# FINAL, Problem 4: Air Traffic Time Series Predictive Model
# Due Sunday December 21st, 2014
# By Daniel Bejarano


library(caret)
library(pROC)
library(kernlab)

load("C:/Users/dbejarano/Dropbox/aFALL 2014/DATA ANALYTICS/FINAL/Monthly+Temperatures.RData")

traffic = AirCarrierTrafficStatistics_20141209
oil = CrudeOilPrices
gdp = GDP
temp = Avg_Temp

#Converting GDP per quarter to GDP per month
gdp_monthly = NULL
for (i in 1:nrow(gdp)){
  gdp_monthly$gdp[(i*2+(i-2)):((i*2+(i-2))+2)] = gdp$GDP[i] 
}
gdp_monthly = data.frame(gdp_monthly)

#Create dataframe with all variables
air_data = NULL
air_data$row = as.factor(1:224)
air_data$year = c(substring(traffic$Date[1:60], 5), substring(traffic$Date[61:168], 1,1),
                  substring(traffic$Date[169:224], 1,2))
air_data$month = c(substring(traffic$Date[1:60], 1,3), substring(traffic$Date[61:168], 3),
                   substring(traffic$Date[169:224], 4))
air_data$oil = CrudeOilPrices[1:224,2]
air_data$temp = temp
air_data$gdp = gdp_monthly[1:224,]
air_data$boardings = traffic$Passenger.Boardings..thousands.
air_data$revenue = traffic$Revenue.Passenger.Miles..thousands.
air_data$freight = traffic$Freight.Ton.Miles..thousands.

air_data = data.frame(air_data)

#replace(air_data$month, which(is.na(air_data$month)), "Jnu")

#Specify order for months
air_data$month = factor(air_data$month, c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", 
                                "Nov", "Dec"))

#Predictions on explanatory variables:
air_pred = NULL
air_pred$row[1:4] = c("225", "226", "227", "228")
air_pred$year[1:4] = c("14", "14", "14", "14")
air_pred$month[1:4] = c("Sep", "Oct", "Nov", "Dec")
air_pred$oil[1:4] = c(93.21, 84.40, 75.62, 67.00)
air_pred$temp[1:4] = c(66, 57, 39, 32)
air_pred$gdp[1:4] = c(17555.2, 17500, 17500, 17500)
air_pred$boardings = NA
air_pred$revenue = NA
air_pred$freight = NA
air_pred = data.frame(air_pred)

#Merge previous data with that to be predicted
air_merged = rbind(air_data, air_pred)

#Histograms
hist(air_data$oil)
hist(air_data$temp)
hist(air_data$gdp)
hist(air_data$boardings)
hist(air_data$revenue)
hist(air_data$freight)

#PLOTS
pairs(air_data)
plot(gdp~month, data=air_data)
plot(gdp~row, data=air_data)
plot(temp~row, data=air_data)
plot(oil~row, data=air_data)
plot(boardings~row, data=air_data)
plot(revenue~row, data=air_data)
plot(freight~row, data=air_data)
plot(boardings~month, data=air_data)
plot(revenue~month, data=air_data)
plot(freight~month, data=air_data)

### 1. ADDING DIRECT FEATURES ###

# 1.1 New Dataset: Remove last 4 years
air_direct = air_merged[61:228,]

# 1.2 Same month last year boardings, revenue and freight
air_direct$last_year_boardings = air_merged$boardings[49:(228-12)] # if not use air_data
air_direct$last_year_revenue = air_merged$revenue[49:(228-12)]
air_direct$last_year_freight = air_merged$freight[49:(228-12)]

#1.3 Previous quarter boardings, revenue and freight
air_direct$prev_month_boardings = air_merged$boardings[57:(228-4)]
air_direct$prev_month_revenue = air_merged$revenue[57:(228-4)]
air_direct$prev_month_freight = air_merged$freight[57:(228-4)]

### 2. ADDING ENGINEERED FEATURES ###

# 2.1 Average of last 4 years for boardings, revenue and freight
air_all = air_direct
air_all$avg_boardings = NULL
air_all$avg_revenue = NULL
air_all$avg_freight = NULL
for (i in 49:(228-12)){
  air_all$avg_boardings[(i-48)] = mean(air_data$boardings[(i-13)], air_data$boardings[(i-25)],
                                    air_data$boardings[(i-37)], air_data$boardings[(i-49)])
  air_all$avg_revenue[(i-48)] = mean(air_data$revenue[(i-13)], air_data$revenue[(i-25)],
                                       air_data$revenue[(i-37)], air_data$revenue[(i-49)])
  air_all$avg_freight[(i-48)] = mean(air_data$freight[(i-13)], air_data$freight[(i-25)],
                                       air_data$freight[(i-37)], air_data$freight[(i-49)])
}

#2.2 Average 4 and 5 two months temperature
prev_month_temp = air_merged$temp[57:224]
prev_2month_temp = air_merged$temp[56:223]
air_all$avg_prev_temp = (prev_month_temp + prev_2month_temp)/2

#2.3 Previous quarter GDP/OIL ratio
prev_month_gdp = air_merged$gdp[57:(228-4)]
prev_month_oil = air_merged$oil[57:(228-4)]
air_direct$prev_month_gdp.oil = prev_month_gdp/prev_month_oil

#2,4 Miles traveled last year in same month
air_all$last_year_miles = air_direct$last_year_revenue/air_direct$last_year_boardings

#2.5 Last year times the ratio of (last_year)/(2_years_ago), per month per variable of interest
two_year_boardings = air_merged$boardings[37:(228-24)] # if not use air_data
two_year_revenue = air_merged$revenue[37:(228-24)]
two_year_freight = air_merged$freight[37:(228-24)]
air_all$last2_ratio_boardings = (air_direct$last_year_boardings^2)/(two_year_boardings)
air_all$last2_ratio_revenue = (air_direct$last_year_revenue^2)/(two_year_revenue)
air_all$last2_ratio_freight = (air_direct$last_year_freight^2)/(two_year_freight)

#2.6 (boardings last year)/(GDP last year) * GDP last quarter
last_year_gdp = air_merged$gdp[49:(228-12)]
last_quarter_gdp = air_merged$gdp[57:224]
air_all$boardings_gdp = ((air_direct$last_year_boardings)/(last_year_gdp))*last_quarter_gdp
air_all$revenue_gdp = ((air_direct$last_year_revenue)/(last_year_gdp))*last_quarter_gdp
air_all$freight_gdp = ((air_direct$last_year_freight)/(last_year_gdp))*last_quarter_gdp

### 3) CREATE MODEL ###

lr_air = lm(boardings ~ year_n + month_n + gdp + oil + temp, data = air_all)

# 3.1 Divide in train and test
air_all$year_n = air_years$year_n[61:228]
air_all$month_n = air_years$month_n[61:228]
air_final = air_all[1:164,]
to_pred_boa = air_all[165:168,]
to_pred_rev = air_all[165:168,]
to_pred_fre = air_all[165:168,]

index_train_b = unlist(createDataPartition(air_final$boardings, p = 0.8))
index_train_r = unlist(createDataPartition(air_final$revenue, p=0.8))
index_train_f = unlist(createDataPartition(air_final$freight, p=0.8))
train_boa = air_final[index_train_b,]
test_boa = air_final[-index_train_b,]
train_rev = air_final[index_train_r,]
test_rev = air_final[-index_train_r,]
train_fre = air_final[index_train_f,]
test_fre = air_final[-index_train_f,]

# 3.2 BOARDINGS TRAINING & TESTING
# 3.2.1 Variable Selection
variables_boa = regsubsets(boardings ~  year_n + oil + temp + gdp + last_year_boardings + prev_month_boardings +
                             avg_boardings + avg_prev_temp + last_year_miles + last2_ratio_boardings + boardings_gdp,
                   data = train_boa, nvmax = 12)
summary(variables_boa)

marsGrid = expand.grid(.degree=1:2, .nprune=10:13) # grid of tuning parameters for a MARS model

mars_boa = train(boardings ~ boardings_gdp + gdp + month,
                 #year + month + oil + temp + gdp + last_year_boardings + prev_month_boardings +
                 #avg_boardings + avg_prev_temp + last_year_miles + last2_ratio_boardings + boardings_gdp
                  data = train_boa,
                  method = "earth", 
                  tuneGrid = marsGrid, 
                  trControl = trainControl(method='repeatedcv', number = 10, repeats = 4)) 

summary(mars_boa) # you can see which variables, in which combinations, were selected by the best model from CV
plot(mars_boa) # plot the results of cross-validation

mars_boa$results # all of the cross-validated RMSEs for all tuning parameter combinations
mars_boa$bestTune # the best parameters picked by the cross validation

pred_boa = predict(mars_boa, test_boa) # use the optimally-tuned model to predict on the test set
#plot(pred_boa, test_boa$boardings) # best if these are all on a straight line
rmse_boa = RMSE(pred_boa, test_boa$boardings) 
rmse_boa/mean(air_all$boardings[1:164])

# 3.3 REVENUE TRAINING & TESTING

# 3.2.1 Variable Selection
variables_rev = regsubsets(revenue ~ year + month + oil + temp + gdp + last_year_revenue + prev_month_revenue +
                             avg_revenue + avg_prev_temp + last_year_miles + last2_ratio_revenue + revenue_gdp,
                           data = train_rev, nvmax = 12)
summary(variables_rev)

mars_rev = train(revenue ~ month + temp + gdp + last_year_revenue,# + revenue_gdp,
                 data = train_rev,
                 method = "earth", 
                 tuneGrid = marsGrid, 
                 trControl = trainControl(method='repeatedcv', number = 10, repeats = 4)) 

summary(mars_rev) 
plot(mars_rev) 

mars_rev$results 
mars_rev$bestTune 

pred_rev = predict(mars_rev, test_rev) 
plot(pred_rev, test_rev$boardings) 
rmse_rev = RMSE(pred_rev, test_rev$revenue) 
rmse_rev/mean(air_all$revenue[1:164])

# 3.3 FREIGHT TRAINING & TESTING

# 3.2.1 Variable Selection
variables_fre = regsubsets(freight ~ year + month + oil + temp + gdp + last_year_freight + prev_month_freight +
                             avg_freight + avg_prev_temp + last_year_miles + last2_ratio_freight + freight_gdp,
                           data = train_fre, nvmax = 12)
summary(variables_fre)

mars_fre = train(freight ~  month + oil + temp + gdp + last_year_freight + 
                   avg_prev_temp +  freight_gdp,
                 data = train_fre,
                 method = "earth", 
                 tuneGrid = marsGrid, 
                 trControl = trainControl(method='repeatedcv', number = 10, repeats = 4)) 

summary(mars_fre) 
plot(mars_fre) 

mars_fre$results 
mars_fre$bestTune 

pred_fre = predict(mars_fre, test_fre) 
plot(pred_fre, test_fre$freight) 
rmse_fre = RMSE(pred_fre, test_fre$freight)
rmse_fre/mean(air_all$freight[1:164])

# 4. FINAL MODEL PREDICTIONS

final_boa = train(boardings ~ year + month + oil + temp + gdp + last_year_boardings + prev_month_boardings +
                    avg_boardings + avg_prev_temp + last_year_miles + last2_ratio_boardings + boardings_gdp,
                  data=air_all,
                  method="earth",
                  tuneGrid = mars_boa$bestTune,
                  trControl=trainControl(method="none"))
pred_final_boa = predict(final_boa, to_pred_boa)

final_rev = train(revenue ~ year + month + oil + temp + gdp + last_year_revenue + prev_month_revenue +
                    avg_revenue + avg_prev_temp + last_year_miles + last2_ratio_revenue + revenue_gdp,
                  data=air_all,
                  method="earth",
                  tuneGrid = mars_rev$bestTune,
                  trControl=trainControl(method="none"))
pred_final_rev = predict(final_rev, to_pred_rev)

final_fre = train(freight ~ year + month + oil + temp + gdp + last_year_freight + prev_month_freight +
                    avg_freight + avg_prev_temp + last_year_miles + last2_ratio_freight + freight_gdp,
                  data=air_all,
                  method="earth",
                  tuneGrid = mars_fre$bestTune,
                  trControl=trainControl(method="none"))
pred_final_fre = predict(final_fre, to_pred_fre)