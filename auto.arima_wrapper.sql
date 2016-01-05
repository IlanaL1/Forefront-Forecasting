--- Name: missingDates
--- Pre-conditions: Assumes already called procedure checkXreg
--- Post-conditions: returns forecast new horizon dates, forecast values, upper 95% CI, lower 95%CI
--- Function: Fill in missing dates 
-- To do: should be able to specify CI as parameter, should ideally check xreg within function.. 
DROP PROCEDURE autoarimaRWrapper;
CREATE PROCEDURE autoarimaRWrapper(IN timeseries "model", IN param "paramTable", IN xreg "XREG", IN newxreg "NEWXREG", OUT fit_result "forecastFitted", OUT horizon_result "forecastHorizon", OUT actuals_table "actuals")
LANGUAGE RLANG AS
BEGIN
### diagnostic results

library("forecast")
library("utility.forecast")

# timeseries
DATE_ID=c(timeseries$DATE_ID)
actuals=c(timeseries$TOTAL)


#param
parameters_key<-c(param$KEY)
parameters_value<-c(param$VALUE)
parameters<-data.frame(parameters_key, parameters_value)
colnames(parameters)<-c("key", "value")
hor=parameters$value[parameters$key %in% "HORIZON"]
freq=parameters$value[parameters$key %in% "FREQUENCY"]
num_var=parameters$value[parameters$key %in% "NUM_VAR"]
freq_type=parameters$value[parameters$key %in% "FREQUENCY_TYPE"]

xreg_date=c(xreg$DATEI)
VAR1=c(xreg$VAR1)
VAR2=c(xreg$VAR2)
VAR3=c(xreg$VAR3)
xreg_df<-data.frame("value1"=VAR1,"value2"=VAR2,"value3"=VAR3)
xreg_df<-data.frame(xreg_df[,1:num_var])

newxreg_date=c(newxreg$DATEI)
VAR1f=c(newxreg$VAR1)
VAR2f=c(newxreg$VAR2)
VAR3f=c(newxreg$VAR3)
newxreg_df<-data.frame("value1"=VAR1f,"value2"=VAR2f,"value3"=VAR3f)
newxreg_df<-data.frame(newxreg_df[,1:num_var])

## Need to insert proper check on xreg, newxreg
#xreg_df<-NULL 	
#newxreg_df<-NULL
#checkXreg

## need utility functions to sequence horizon dates.. 

#### Set date variables for training and horizon periods
freq_descrip=type_to_descrip(freq_type) #utility.forecast function

train_date_list<-get_train_start_end(DATE_ID)
train_start_date<-train_date_list$train_start_date
train_end_date<-train_date_list$train_end_date
hor_date_list<-get_horizon_start_end(train_end_date,horizon=hor,freq_type,freq_descrip)
horizon_start_date<-hor_date_list$horizon_start_date
horizon_end_date<-hor_date_list$horizon_end_date
horiz_dates<-get_horizon_dates(horizon_start_date,horizon_end_date,freq_type,freq_descrip)



if (num_var==0){
xreg_df=NULL
newxreg_df=NULL
}

time_series<-ts(actuals, frequency=freq) # number of steps(weeks,months,quarters into start_year)
fit<-auto.arima(time_series, xreg=xreg_df) 
fcast_arima<-forecast(fit,h=hor,xreg=newxreg_df)

# Actual data, returned as it may have been smoothed
actuals_table<-data.frame(
  DATE_ID=DATE_ID,
  TOTAL=actuals
)

# Fit result (the validation period)
fit_result<-data.frame(
  DATE_ID=DATE_ID,
  TOTAL=as.double(fcast_arima$fitted)
)

# horizon result (i.e. the forecast)
horizon_result<-data.frame(
  DATE_ID=horiz_dates,
  TOTAL=as.numeric(fcast_arima$mean),
  TOTAL_UPPER_95=as.numeric(fcast_arima$upper),
  TOTAL_LOWER_95=as.numeric(fcast_arima$lower)
  #TOTAL=as.numeric(fcast2$mean)  
)
END;
