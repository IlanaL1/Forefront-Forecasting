--- Name: missingDates
--- Pre-conditions:
--- Post-conditions:
--- Function: Fill in missing dates 
DROP PROCEDURE autoARIMAWrapper;
CREATE PROCEDURE autoARIMAWrapper(IN timeseries "model", IN param "paramTable", IN xreg "XREG", IN newxreg "NEWXREG", IN future_skip "skiplist", OUT fit_result "forecastFitted", OUT horizon_result "forecastHorizon", OUT actuals_table "actuals", OUT diagnostic_result "diagnosticResult")
LANGUAGE RLANG AS
### diagnostic results

# timeseries
DATE_ID_original=c(timeseries$DATE_ID)
actuals_original=c(timeseries$TOTAL)


xreg_date=c(xreg$DATE_ID)
VAR1=c(xreg$VAR1)
VAR2=c(xreg$VAR2)
VAR3=c(xreg$VAR3)
xreg_df<-data.frame("date"=xreg_date,"X1"=VAR1,"X2"=VAR2,"X3"=VAR3)

newxreg_date=c(newxreg$DATE_ID)
VAR1f=c(newxreg$VAR1)
VAR2f=c(newxreg$VAR2)
VAR3f=c(newxreg$VAR3)
newxreg_df<-data.frame("date"=newxreg_date,"X1"=VAR1f,"X2"=VAR2f,"X3"=VAR3f)

#param
parameters_key<-c(param$KEY)
parameters_value<-c(param$VALUE)
parameters<-data.frame(parameters_key, parameters_value)
colnames(parameters)<-c("key", "value")
hor=parameters$value[parameters$key %in% "HORIZON"]
freq=parameters$value[parameters$key %in% "FREQUENCY"]

time_series<-ts(actuals, frequency=freq) # number of steps(weeks,months,quarters into start_year)
fit<-auto.arima(time_series, xreg=xreg_df[,-c(1)]) 
fcast_arima<-forecast(fit,h=hor,xreg=newxreg_df[,-c(1)])

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