--- Name: smooth_time_series
--- Pre-conditions:
--- Post-conditions:
--- Function: smooth time_series if parameter is set, should be able enter R parameters from HANA
DROP PROCEDURE smoothTimeSeries;
CREATE PROCEDURE smoothTimeSeries(IN timeseries "timeSeriesInput2", IN param "paramTable", OUT smoothed_output "smoothedOutput")
LANGUAGE RLANG AS
--DEFAULT SCHEMA HDITTMER            
BEGIN

#### Get Input Parameters from Tables
# timeseries
DATE_ID=c(timeseries$DATE_ID)
actuals=c(timeseries$TOTAL)

library("zoo")
#param
parameters_key<-c(param$KEY)
parameters_value<-c(param$VALUE)
parameters<-data.frame(parameters_key, parameters_value)
colnames(parameters)<-c("key", "value")
smooth=parameters$value[parameters$key %in% "SMOOTH"]

#### smooth if necessary
if(smooth==1){
	actuals_MA<-rollapply(actuals,FUN=mean,width=4,na.rm=TRUE,align="left") #can use align = left.. #rollmean 
}
last_actual_value<-actuals[length(actuals)] #diagnostic if needed
len_time_series<-length(actuals) 
len_time_series_MA<-length(actuals_MA) 

## return smoothed data
smoothed_output<-data.frame(
  DATE_ID=DATE_ID,
  TOTAL=as.numeric(actuals_MA)
)

END;