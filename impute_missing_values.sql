--- Name: missingValues
--- Pre-conditions: input_table has 2 columns: data and total
--- Post-conditions:
--- Function: Fill in missing dates
DROP PROCEDURE imputeMissingValues;
CREATE PROCEDURE imputeMissingValues(IN timeseries "model", OUT imputed_output "model")
LANGUAGE RLANG AS
--DEFAULT SCHEMA HDITTMER            
BEGIN

library("zoo") # na.approx

# timeseries
DATE_ID_original=c(timeseries$DATE_ID)
actuals_original=c(timeseries$TOTAL)
actuals<-na.approx(actuals_original)

## return sorted  data
imputed_output<-data.frame(
  DATE_ID=DATE_ID_original,
  TOTAL=as.numeric(actuals)
)
END;
