--- Name: smooth_time_series
--- Pre-conditions: input_table has 2 columns: date and total
--- Post-conditions: has 2 columns: date and total (smoothed values)
--- Function: smooth time_series 
-- To do: As a wrapper for R funciton zoo::rollapply, should allow input for width, align, function etc... 
-- Note: The specific parameters to rollapply are highly customised to the MG use-case. They must be modified for any future use-case. 

DROP PROCEDURE smoothTimeseries;
CREATE PROCEDURE smoothTimeseries(IN timeseries "model", OUT smoothed_output "model")
LANGUAGE RLANG AS
        
BEGIN

#### Get Input Parameters from Tables
# timeseries
DATE_ID=c(timeseries$DATE_ID)
actuals=c(timeseries$TOTAL)

library("zoo") #rollapply

#WARNING!! - Highly customised function. 
actuals_MA<-rollapply(actuals,FUN=mean,width=4,na.rm=TRUE,align="left",fill="extend") #can use align = left.. #rollmean 

## return smoothed data
smoothed_output<-data.frame(
  DATE_ID=DATE_ID,
  TOTAL=as.numeric(actuals_MA)
)

END;
