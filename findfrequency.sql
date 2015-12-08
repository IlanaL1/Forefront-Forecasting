--- Name: findfrequency
--- Pre-conditions:
--- Post-conditions:
--- Function: Estimate spacing between dates and frequency(ies) of the time-series. 
--- R utility.forecast functions: guess_period, type_to_descrip, type_to_short_freq, type_to_long_freq
DROP PROCEDURE findfrequency;
CREATE PROCEDURE findfrequency(IN timeseries "timeSeriesInput2", IN param "paramTable", OUT diagnostic_result_findfrequency "diagnosticResultFindFrequency")
LANGUAGE RLANG AS
--DEFAULT SCHEMA HDITTMER            
BEGIN

library("utility.forecast")
library("forecast") # findfrequency

#### Get Input Parameters from Tables
# timeseries
DATE_ID=c(timeseries$DATE_ID)
actuals=c(timeseries$TOTAL)

#param
parameters_key<-c(param$KEY)
parameters_value<-c(param$VALUE)
parameters<-data.frame(parameters_key, parameters_value)
colnames(parameters)<-c("key", "value")
num_var=parameters$value[parameters$key %in% "NUM_VAR"]
## UI drop-down menu - 1. daily, 2. daily - business days only, 3. weekly, 4. monthly, 5. quarterly, 6. annual
user_input_spacing=parameters$value[parameters$key %in% "FREQUENCY_TYPE"] 

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

freq_estimate_short_default<--1 # default
num_freq<-1

#### Return estimates for short and long frequency
if (spacing_type==1 | spacing_type==2) {
	freq_estimate_short_default<-type_to_short_freq(spacing_type) # 5 or 7 day

  	if(length(actuals)<(2*freq_estimate_long_default)) { # less than two periods, not going to compare for long (365 day) frequency,auto.arima drops seasonal if no good
    	freq_estimate_short <- findfrequency(actuals)  
    	freq_estimate_long<- -1 # no second sub-frequency 
    	freq_estimate_long_default<- -1    
		num_freq<-1
	}else{   ## possibly 2 (or more) frequencies 
    	freq_estimate_short <- findfrequency(actuals[1:floor(freq_estimate_long_default/2)]) ## detect frequency in short stretch
    	freq_estimate_long <- findfrequency(actuals)
    	num_freq<-2 
  	}  #end else
  	
}else{ # weekly,monthly,quarterly,annual
	freq_estimate_short<- -1
	freq_estimate_short_default <- -1
	freq_estimate_long <- findfrequency(actuals)
	num_freq<-1
}  

# if not equal, prompt warning if difference between estimate and default less than 20% of default value
prompt_default_short<-0 #defaults, no user prompt that different frequency detect
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

diagnostic_result_findfrequency<-data.frame(
SPACING_WARNING=spacing_warning,
SPACING_ESTIMATE=spacing_estimate,
SPACING_TYPE=spacing_type, 
FREQ_ESTIMATE_SHORT=freq_estimate_short,
FREQ_ESTIMATE_LONG=freq_estimate_long,
DEFAULT_ESTIMATE_SHORT=freq_estimate_short_default,
DEFAULT_ESTIMATE_LONG=freq_estimate_long_default,
PROMPT_DEFAULT_SHORT=prompt_default_short,
PROMPT_DEFAULT_LONG=prompt_default_long,
NUM_FREQ=num_freq
)

END;
