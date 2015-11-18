--- Name: missingValues
--- Pre-conditions:
--- Post-conditions:
--- Function: Estimate spacing between dates and frequency(ies) of the time-series. 
DROP PROCEDURE findfrequency;
CREATE PROCEDURE findfrequency(IN timeseries "timeSeriesInput2", IN param "paramTable", IN variable_matrix "variableMatrix", IN future_skip "skiplist", OUT fit_result "forecastFitted", OUT horizon_result "forecastHorizon", OUT actuals_table "actuals", OUT diagnostic_result "diagnosticResult", OUT accuracy_result "accuracy")
LANGUAGE RLANG AS
--DEFAULT SCHEMA HDITTMER            
BEGIN

#### Get Input Parameters from Tables
# timeseries
DATE_ID_original=c(timeseries$DATE_ID)
actuals_original=c(timeseries$TOTAL)

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

#### smooth if necessary
if(smooth==1){
	actuals_MA<-rollapply(actuals,FUN=mean,width=4,na.rm=TRUE,align="left") #can use align = left.. #rollmean 
  	actuals<-actuals_MA
}
last_actual_value<-actuals[length(actuals)] #diagnostic
len_time_series<-length(actuals) 

#### Estimate period (spacing) between dates
DATE_ID1<-as.Date(DATE_ID,"%Y-%m-%d")
spacing_estimate<-0 # initialise
spacing_estimate<-guess_period(DATE_ID1)

#### return WARNING if different to user input
spacing_warning<- -1 # default values
spacing_type<-user_input_spacing # what user has guessed
if(length(spacing_estimate)>0){ # guess_period was successful 
spacing_warning<-0 # assume 
if (spacing_estimate != user_input_spacing){
	spacing_warning<-1  # potentially entered wrong frequency
	spacing_type<-spacing_estimate
	}
}

# Get spacing_descrip and freq_ts (default long frequency, assuming an annual cycle)
#Functions: type_to_freq and type_to_descrip

spacing_descrip<-type_to_descrip(spacing_type)
freq_estimate_long_default<--1 # default
freq_estimate_long_default<-type_to_annual_freq(spacing_type)

#### Return estimates for short and long frequency
if (spacing_type==1 | spacing_type==2) {

	freq_estimate_short_default<--1 # default
	freq_estimate_short_default<-type_to_short_freq(spacing_type)

  	if(length(actuals)<(2*freq_estimate_long_default)) { # less than two periods, not going to compare for long (365 day) frequency,auto.arima drops seasonal if no good
    	freq_estimate_short <- findfrequency(actuals)  
    	freq_estimate_long<- -1 # no long frequency 
    	freq_estimate_default_long<- -1    
		
	}else{   ## possibly 2 (or more) frequencies 
    	freq_estimate_short <- findfrequency(actuals[1:182]) ## allow shorter frequency
    	freq_estimate_long <- findfrequency(actuals)
  	}  #end else
  	
}else{ # end if spacing_type..
	freq_estimate_short<- -1
	freq_estimate_short_default <- -1
	freq_estimate_long <- findfrequency(actuals) 
	freq_estimate_long_default
}  

# if not equal, prompt warning if difference between estimate and default less than 20% of default value
prompt_default_short<-0 #defaults
prompt_default_long<-0       
if (freq_estimate_short != freq_estimate_short_default){
	if (abs(freq_estimate_short_default-freq_estimate_short)/freq_estimate_short_default) < 0.2 # error within 
		prompt_default_short <-1
  	}
if (freq_estimate_long != freq_estimate_long_default){
	if (abs(freq_estimate_long_default-freq_estimate_long)/freq_estimate_long_default) < 0.2 # error within 
		prompt_default_long   <-1 
  	}
  	
# if no warning, use estimate... 

diagnostic_result<-data.frame(
SPACING_WARNING=spacing_warning,
FREQ_ESTIMATE_LONG=freq_estimate_long,
FREQ_ESTIMATE_SHORT=freq_estimate_short,
DEFAULT_ESTIMATE_LONG=freq_estimate_long_default,
DEFAULT_ESTIMATE_SHORT=freq_estimate_short_default,
PROMPT_DEFAULT_LONG=prompt_default_long
PROMPT_DEFAULT_SHORT=prompt_default_short
)

END;
