--- Name: 
--- Pre-conditions: input_table has 2 columns: date and total
--- Post-conditions: sorted_output has 2 columns: date and total
--- Function: sort any input data frame by date...
DROP PROCEDURE sortTimeseriesByDate;
CREATE PROCEDURE sortTimeseriesByDate(IN timeseries "model",  OUT sorted_output "model")
LANGUAGE RLANG AS
--DEFAULT SCHEMA HDITTMER            
BEGIN

library("utility.forecast")

# timeseries
DATE_ID_original=c(timeseries$DATE_ID)
actuals_original=c(timeseries$TOTAL)

#### sort time series by date
sorted_df<-sort_data_frame_by_date(DATE_ID_original,actuals_original)
DATE_ID<-sorted_df$date 	
actuals<-sorted_df$value

## return sorted  data
sorted_output<-data.frame(
  DATE_ID=DATE_ID,
  TOTAL=as.numeric(actuals)
)

END;
