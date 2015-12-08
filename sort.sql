--- Name: 
--- Pre-conditions:
--- Post-conditions:
--- Function: sort any input data frame by date...
DROP PROCEDURE sortTableByDate;
CREATE PROCEDURE sortTableByDate(IN variable_matrix "variableMatrix", IN param "paramTable", OUT sorted_table "sorted_table")
LANGUAGE RLANG AS
--DEFAULT SCHEMA HDITTMER            
BEGIN

library("utility.forecast"). 

#param
parameters_key<-c(param$KEY)
parameters_value<-c(param$VALUE)
parameters<-data.frame(parameters_key, parameters_value)
colnames(parameters)<-c("key", "value")
smooth=parameters$value[parameters$key %in% "SMOOTH"]
hor=parameters$value[parameters$key %in% "HORIZON"]
num_var=parameters$value[parameters$key %in% "NUM_VAR"]
freq=parameters$value[parameters$key %in% "FREQUENCY"]
## UI drop-down menu - 1. daily, 2. daily - business days only, 3. weekly, 4. monthly, 5. quarterly, 6. annual
user_input_freq=parameters$value[parameters$key %in% "FREQUENCY_TYPE"] 

#### sort time series by date
sorted_df<-sort_data_frame_by_date(DATE_ID_original,actuals_original)
DATE_ID<-sorted_df$date 	
actuals<-sorted_df$value

## return sorted  data
smoothed_output<-data.frame(
  DATE_ID=DATE_ID,
  TOTAL=as.numeric(actuals)
)

END;
