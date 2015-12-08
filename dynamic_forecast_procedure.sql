
-- Preconditions:
-- -- variable_matrix must contain columns: DATE, VAR1, VAR2, VAR3 even if empty

DROP PROCEDURE executeRForecast;
CREATE PROCEDURE executeRForecast(IN timeseries "timeSeriesInput2", IN param "paramTable", IN variable_matrix "variableMatrix", IN future_skip "skiplist", OUT fit_result "forecastFitted", OUT horizon_result "forecastHorizon", OUT actuals_table "actuals", OUT diagnostic_result "diagnosticResult", OUT accuracy_result "accuracy")
LANGUAGE RLANG AS
BEGIN

# libraries
library("timeDate")
library("forecast")
library("utility.forecast")
library("zoo") # rollapply
#library("dplyr")

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


# future_skip
future_skiplist=c(future_skip$DATE_ID) # a list of dates that are set to be holidays in horizon period 
future_skiplist<-as.Date(future_skiplist,format="%d/%m/%Y") # ensure formatting ok
	
#### Deal with missing dates as "holiday" input
future_skiplist<-NULL # default
ncol_xreg_with_holiday<-0
len_holiday<-0
len_holidayf<-0
holiday_flag<-0

# daily data, and either 0 variables (=0), or successful entry of variables (=1)
if (freq_type==1 | freq_type ==2) {

	# fill in missing dates in time-series and create new holiday vector to specify where missing dates were found
	results_ts_df<-set_missing_values(DATE_ID,actuals, freq_type) # sets
	DATE_ID<-results_ts_df$date  # new dates
	actuals<-results_ts_df$value # new actuals (may contain zeros)
	holiday<-results_ts_df$holiday # vector of 1 or 0 for missed values.. 
	len_holiday<-length(holiday[holiday==1]) # diagnostic
	len_holidayf<-length(holidayf[holidayf==1])

	if (length(holiday[holiday==1]) != 0 & length(holiday[holiday==0]) != 0){ # ie not a column of zeros or 1
		# create future holiday vector of 0s, and 1 where holiday expected (skiplist dates)
		holidayf<-c(rep(0,hor))  # initialise to 0
		holidayf[match(future_skiplist,horiz_dates)]<-1 # set to 1 if in horiz_dates
		holiday_flag<-1
		
		# no errors found in input variable_matrix and at least 1 variable
		if(variables_error_flag==0){ 
			xreg_select<-set_missing_values_xreg(xreg_select,freq_type) # fill in missing dates in xreg_select, (recall, already checked xreg dates match timeseries dates)
			xreg_select<-cbind(xreg_select,holiday) ## holiday has FULL Dates, xreg_select has one date missing # think its ok
			new_xreg_select<-cbind(new_xreg_select,holidayf) # add extra variable column as future "skips"
		} else{ # xreg_select = NULL at this point
			xreg_select<-cbind(DATE_ID,holiday)
			new_xreg_select<-cbind(horiz_dates,holidayf)
		}
		ncol_xreg_with_holiday<-(ncol(xreg_select)-1) #diagnostic 
	} # end if holiday clause

} # end if freq_type==1 or 2

# Create forecast
generate_forecast<-function(freq_type,DATE_ID,actuals,hor,xreg_select,new_xreg_select,freq_descrip,freq_ts){
  switch(freq_type,
         "1" = generate_forecast_daily(DATE_ID,actuals,hor,xreg_select,new_xreg_select,freq_type,freq_descrip,freq_ts),
         "2" = generate_forecast_daily(DATE_ID,actuals,hor,xreg_select,new_xreg_select,freq_type,freq_descrip,freq_ts),
         "3" = generate_forecast_weekly_monthly_quarterly(DATE_ID,actuals,hor,xreg_select,new_xreg_select,freq_type,freq_descrip,freq_ts),
         "4" = generate_forecast_weekly_monthly_quarterly(DATE_ID,actuals,hor,xreg_select,new_xreg_select,freq_type,freq_descrip,freq_ts),
         "5" = generate_forecast_weekly_monthly_quarterly(DATE_ID,actuals,hor,xreg_select,new_xreg_select,freq_type,freq_descrip,freq_ts),
         "6" = generate_forecast_annual())
} 


# daily function (cases 1 and 2)
generate_forecast_daily<-function(DATE_ID, actuals,hor,xreg_select,new_xreg_select,freq_type,freq_descrip,freq_ts){

# start parameter
first_train_date=as.Date(DATE_ID[1],"%d/%m/%Y")
start=as.Date(cut(first_train_date,"year")) 
num_days=as.integer(first_train_date-start+1) ## modify for weeks, months, years etc..
start_year=as.numeric(format(first_train_date, "%Y"))

freq_estimate_short=-1  # Default value for flags
freq_estimate_long=-1
aic_option=-1 
aic_value=-1

# Possibly shorter frequencies
if(freq_type==1){
	freq_ts_short<-7
}else{  ## freq_type==2, business days only
	freq_ts_short<-5
}

## FUNCTION: forecast_test_daily: call different R forecast function depending on frequency combinations; return forecast
forecast_test_daily<-function(actuals,freq_test_short,freq_test_long,xreg_select,new_xreg_select){
  if(freq_test_long ==1){ ## no long frequency
    time_series<-ts(actuals, frequency=freq_test_short) # number of steps(weeks,months,quarters into start_year)
    fit<-auto.arima(time_series, xreg=xreg_select[,-c(1)]) 
    fcast<-forecast(fit,h=hor,xreg=new_xreg_select[,-c(1)])
  }else if (freq_test_short ==1 & freq_test_long!=1){
    time_series<-ts(actuals, frequency=freq_test_long) # first value is num_days days into start_year
    fit<-stlm(time_series, method="arima", s.window="periodic", xreg=xreg_select[,-c(1)]) 
    #fit<-stlm(time_series, method="arima", s.window="periodic", xreg=xreg_select[,c(2)])         
    fcast<-forecast.stlm(fit,h=hor,xreg=new_xreg_select[,-c(1)])
  }else if (freq_test_short !=1 & freq_test_long!=1){
    n_train<-length(actuals)
    time_series<-ts(actuals, frequency=freq_test_short) # first value is num_days days into start_year
    k<-12
	if(k > (freq_test_long/2)){
  		k<-floor(freq_test_long/2)
	}
    z <- forecast::fourier(ts(actuals, frequency= freq_test_long), K=k) # f=365.25*5/7
    zf <- forecast::fourierf(ts(actuals, frequency= freq_test_long), K=k, h=hor)
    fit <- auto.arima(time_series, xreg=cbind(xreg_select[,-c(1)],z))
    #new_xreg_select=cbind(new_xreg_select,zf) # update new regressors
    fcast<-forecast(fit, xreg=cbind(new_xreg_select[,-c(1)],zf), h=hor)	    
  }
    fcast # return fcast 
} # end forecast_test_daily

# Calculate MAPE - validation period is equal to horizon, 
compute_accuracy_daily<-function(actuals,freq_short,freq_long,xreg_select,new_xreg_select){
	last_estimation_index<-length(actuals)-hor # remove number of days == horizon 
	validation_train_end_date<-DATE_ID[last_estimation_index]  # length first_training_dates gives the index of the last day of training data
	#validation_train_end_date<-as.Date(validation_train_end_val,"%d/%m/%Y")
	validation_horizon_start_date=DATE_ID[(last_estimation_index+1)] #1 index along
	validation_horizon_end_date=DATE_ID[(last_estimation_index+hor)]
	validation_actuals<-actuals[1:last_estimation_index]
	
	# duplicated code - should move to a separate funciton
	xreg_select_validation<-NULL
	new_xreg_select_validation<-NULL
	if(variables_error_value==1){ # have non-NULL xreg and new_xreg
		xreg_select_validation<-data_frame_select(variable_df, first_train_date, validation_train_end_date,num_var)
		xreg_select_no_dups_validation<-xreg_select_validation[!duplicated(lapply(xreg_select_validation, summary))]
		
		new_xreg_select_validation<-data_frame_select(variable_df, validation_horizon_start_date, validation_horizon_end_date,num_var)
		new_xreg_select_no_dups_validation<-new_xreg_select_validation[!duplicated(lapply(new_xreg_select_validation, summary))]
	
		#set rror flg
		validation_variables_error<-0	
		if(ncol(xreg_select_no_dups_validation)!=ncol(new_xreg_select_no_dups_validation)){
			validation_variables_error<-1
		}else if (!identical(xreg_select_no_dups_validation$date,DATE_ID) | !identical(new_xreg_select_no_dups_validation$date,horiz_dates)){
			validation_variables_error<-1
		}

		if(validation_variables_error==0){
			xreg_select_validation<-xreg_select_no_dups_validation
			new_xreg_select_validation<-new_xreg_select_no_dups_validation
		}else{
			xreg_select_validation<-NULL
			new_xreg_select_validation<-NULL
		}
	}
	
	fcast_arima_validation<-forecast_test_daily(validation_actuals,freq_short,freq_long,xreg_select_validation,new_xreg_select_validation)

	actual_validation_horizon<-actuals[(last_estimation_index+1):length(actuals)] # actual data over validation period
	fcast_validation<-fcast_arima_validation$mean # forecast over validation period
	Mean_APE_function(as.numeric(actual_validation_horizon), as.numeric(fcast_validation))
}

## FUNCTION: forecast_best_freq_daily: call forecast_test_daily with different frequency combinations; select forecast with best fit (aic) to return
forecast_best_freq_daily <- function(actuals,freq_ts_short,freq_ts_long,xreg_select,new_xreg_select){ 
  freq_estimate_short <- findfrequency(actuals[1:182]) ## allow shorter frequency
  freq_estimate_long <- findfrequency(actuals)      
  frequencies_short<-c(freq_estimate_short,freq_ts_short)  # freq_ts_short=7 or 5
  frequencies_long<-c(freq_estimate_long,freq_ts_long)   # freq_ts_long = 365 or 261
  aic <- list(
    "1" = forecast_test_daily(actuals,frequencies_short[1],frequencies_long[1],xreg_select,new_xreg_select),
    "2" = forecast_test_daily(actuals,frequencies_short[1],frequencies_long[2],xreg_select,new_xreg_select),
    "3" = forecast_test_daily(actuals,frequencies_short[2],frequencies_long[1],xreg_select,new_xreg_select),
    "4" = forecast_test_daily(actuals,frequencies_short[2],frequencies_long[2],xreg_select,new_xreg_select)
    #"5" = forecast_test_daily(actuals,1,1,xreg_select,new_xreg_select)    
  )
  aic_option<-which.min(c(aic[[1]]$model$aic,aic[[2]]$model$aic,aic[[3]]$model$aic,aic[[4]]$model$aic))
  best_forecast<-aic[[aic_option]]  # best forecast  
  #freq_min_aic<-frequencies[which.min(aic)]  ## select best frequency
  
  ## compute accuracy with selected aic_option
  result<-expand.grid(a=frequencies_short,b=frequencies_long)
  result <- result[order(result$a,result$b),]
  selected_freq_list<-as.numeric(result[aic_option,])
  select_freq_short<-selected_freq_list[1]
  select_freq_long<-selected_freq_list[2]  	
  Mean_APE_validation<-compute_accuracy_daily(actuals,select_freq_short,select_freq_long,xreg_select,new_xreg_select)
  
  list(best_forecast,freq_estimate_short,freq_estimate_long,aic_option,best_forecast$model$aic,Mean_APE_validation) # return best frequency, and forecast
}

# call function to get forecast from "best" fit ts (ts gives fit with min aic) 

if(length(actuals)<(2*freq_ts)) { # less than two periods, not going to compare for long (365 day) frequency,auto.arima drops seasonal if no good
	freq_estimate <- findfrequency(actuals)  
	time_series<-ts(actuals, frequency=freq_estimate) # number of steps(weeks,months,quarters into start_year)
	fit<-auto.arima(time_series, xreg=xreg_select[,-c(1)]) 
    fcast_arima<-forecast(fit,h=hor,xreg=new_xreg_select[,-c(1)])
    Mean_APE_validation<-compute_accuracy_daily(actuals,freq_estimate,1,xreg_select,new_xreg_select)    
	best_freq<-freq_estimate # used later to compute accuaracy
	freq_estimate_short<-best_freq  
	freq_estimate_long<--1 # N/A
	aic_option<--0 ## 0
	aic_value<-fit$aic
}else{   ## seasonal
	fcast_list<-forecast_best_freq_daily(actuals,freq_ts_short,freq_ts,xreg_select,new_xreg_select) # should pass start_date, num_year
	fcast_arima<-fcast_list[[1]]
	freq_estimate_short<-fcast_list[[2]] # useful for diagnostics
	freq_estimate_long<-fcast_list[[3]]
	aic_option<-fcast_list[[4]]
	aic_value<-fcast_list[[5]]	
	Mean_APE_validation<-fcast_list[[6]] #Mean_APE_validation
}  #end else

# calculate accuracy - will put in later version, for now just use "accuracy" estimate
#fcast_accuracy<-accuracy(fcast_arima)
 
list(fcast_arima=fcast_arima,fcast_accuracy=Mean_APE_validation,freq_estimate_short=freq_estimate_short,freq_estimate_long=freq_estimate_long, aic_option=aic_option,aic_value=aic_value)
} # end generate_forecast_daily

###### Forecast WEEKLY/MONTHLY/QUARTERLY #########
generate_forecast_weekly_monthly_quarterly<-function(DATE_ID,actuals,hor,xreg_select,new_xreg_select,freq_type,freq_descrip,freq_ts){

# start parameter
first_train_date=as.Date(DATE_ID[1],"%d/%m/%Y")
start=as.Date(cut(first_train_date,"year")) 
start_year=as.numeric(format(first_train_date, "%Y"))
 
get_steps<-function(freq_type){
	switch(freq_type,
		"3" = as.numeric(format(first_train_date,"%U")), # weekly
		"4" = as.numeric(format(first_train_date, "%m")), # monthly
		"5" = as.numeric(sub( "Q", "", quarters(first_train_date))) # quarterly
		)
} 
num_steps<-get_steps(as.character(freq_type))

freq_estimate_short=-1  # Default value for flags
freq_estimate_long=-1
aic_option=-1 
aic_value=-1

# runs a forecast for a time-series with a given frequency
forecast_test_weekly_monthly_quarterly<-function(actuals,freq_test,xreg_select,new_xreg_select){
  time_series<-ts(actuals, frequency=freq_test) # number of steps(weeks,months,quarters into start_year)
  time_series<-ts(actuals, frequency=freq_test, start=c(start_year, num_steps)) # number of steps(weeks,months,quarters into start_year) 
  fit<-auto.arima(time_series, xreg=xreg_select[,-c(1)]) 
  forecast(fit,h=hor,xreg=new_xreg_select[,-c(1)])
}

# Calculate MAPE - validation period is equal to horizon, 
compute_accuracy_weekly_monthly_quarterly<-function(actuals,freq_min_aic,xreg_select,new_xreg_select){
	last_estimation_index<-length(actuals)-hor # remove number of days == horizon 
	validation_train_end_date<-DATE_ID[last_estimation_index]  # length first_training_dates gives the index of the last day of training data
	#validation_train_end_date<-as.Date(validation_train_end_val,"%d/%m/%Y")
	validation_horizon_start_date=DATE_ID[(last_estimation_index+1)] #1 index along
	validation_horizon_end_date=DATE_ID[(last_estimation_index+hor)]
	validation_actuals<-actuals[1:last_estimation_index]
	
	# duplicated code - should move to a separate funciton
	xreg_select_validation<-NULL
	new_xreg_select_validation<-NULL
	if(variables_error_value==1){ # have non-NULL xreg and new_xreg
		xreg_select_validation<-data_frame_select(variable_df, first_train_date, validation_train_end_date,num_var)
		xreg_select_no_dups_validation<-xreg_select_validation[!duplicated(lapply(xreg_select_validation, summary))]
		
		new_xreg_select_validation<-data_frame_select(variable_df, validation_horizon_start_date, validation_horizon_end_date,num_var)
		new_xreg_select_no_dups_validation<-new_xreg_select_validation[!duplicated(lapply(new_xreg_select_validation, summary))]
	
		#set error flg
		validation_variables_error<-0	
		if(ncol(xreg_select_no_dups_validation)!=ncol(new_xreg_select_no_dups_validation)){
			validation_variables_error<-1
		}else if (!identical(xreg_select$date,DATE_ID) | !identical(new_xreg_select$date,horiz_dates)){
			validation_variables_error<-1
		}

		if(validation_variables_error==0){
			xreg_select_validation<-xreg_select_no_dups_validation
			new_xreg_select_validation<-new_xreg_select_no_dups_validation
		}else{
			xreg_select_validation<-NULL
			new_xreg_select_validation<-NULL
		}
	}
	
	
	fcast_arima_validation<-forecast_test_weekly_monthly_quarterly(validation_actuals,freq_min_aic,xreg_select_validation,new_xreg_select_validation)

	actual_validation_horizon<-actuals[(last_estimation_index+1):length(actuals)] # actual data over validation period
	fcast_validation<-fcast_arima_validation$mean # forecast over validation period
	Mean_APE_function(as.numeric(actual_validation_horizon), as.numeric(fcast_validation))
}


# returns forecast from model with best fit (from best frequency)
forecast_best_freq <- function(actuals,freq_ts,xreg_select,new_xreg_select){ 
  freq_estimate <- findfrequency(actuals)  
  frequencies<-c(freq_estimate,freq_ts)
  aic <- list(
    "1" = forecast_test_weekly_monthly_quarterly(actuals,frequencies[1],xreg_select,new_xreg_select),
    "2" = forecast_test_weekly_monthly_quarterly(actuals,frequencies[2],xreg_select,new_xreg_select)
    )
  aic_option<-which.min(c(aic[[1]]$model$aic,aic[[2]]$model$aic))    
  best_forecast<-aic[[aic_option]]  # best forecast
  
  # accuracy computations
 freq_min_aic<-frequencies[aic_option]  ## select best frequency	
  Mean_APE_validation<-compute_accuracy_weekly_monthly_quarterly(actuals,freq_min_aic,xreg_select,new_xreg_select)
    
  list(best_forecast,freq_estimate,aic_option,best_forecast$model$aic,Mean_APE_validation) # return best frequency, and forecast  
}

if(length(actuals)<(2*freq_ts)) { # less than two periods, not going to compare for 12 month frequency,auto.arima drops seasonal if no good
	freq_estimate <- findfrequency(actuals)  
	time_series<-ts(actuals, frequency=freq_estimate, start=c(start_year, num_steps)) # number of steps(weeks,months,quarters into start_year)
	fit<-auto.arima(time_series, xreg=xreg_select[,-c(1)]) 
    fcast_arima<-forecast(fit,h=hor,xreg=new_xreg_select[,-c(1)])
    Mean_APE_validation<-compute_accuracy_weekly_monthly_quarterly(actuals,freq_estimate,xreg_select,new_xreg_select)    
	best_freq<-freq_estimate # used later to compute accuaracy
	freq_estimate_short<-best_freq  
	freq_estimate_long<--1 # N/A
	aic_option<-0
	aic_value<-fit$aic
}else{   ## seasonal: return best of frequency estimate and freq_ts
	fcast_list<-forecast_best_freq(actuals,freq_ts,xreg_select,new_xreg_select)
	fcast_arima<-fcast_list[[1]]
	freq_estimate_short<-fcast_list[[2]]
	freq_estimate_long<-fcast_list[[2]] # redundant parameter
	aic_option<-fcast_list[[3]]
	aic_value<-fcast_list[[4]]
	Mean_APE_validation<-fcast_list[[5]] #Mean_APE_validation	
}  # end else
 

#fcast_accuracy<-accuracy(fcast_arima)

list(fcast_arima=fcast_arima,fcast_accuracy=Mean_APE_validation,freq_estimate_short=freq_estimate_short,freq_estimate_long=freq_estimate_long, aic_option=aic_option,aic_value=aic_value)
} # end generate_forecast_weekly_monthly_quarterly

#generate_forecast_annually(){}

fcast_results<-generate_forecast(freq_type,DATE_ID,actuals,hor,xreg_select,new_xreg_select,freq_descrip,freq_ts) # return fcast_arima_object

fcast_arima<-fcast_results$fcast_arima
if(holiday_flag==1){
	fcast_arima$mean[holidayf==1]<-0   ## set to 0 values in skiplist..
}

fcast_accuracy<-fcast_results$fcast_accuracy
freq_estimate_short<-fcast_results$freq_estimate_short
freq_estimate_long<-fcast_results$freq_estimate_long
aic_option<-fcast_results$aic_option
aic_value<-fcast_results$aic_value

## update values for tables
len_new_dates<-length(horiz_dates) #do this outside function
#Mean_APE_validation<-fcast_accuracy['Test set','MAPE'] 
#Mean_APE_validation<-fcast_accuracy[,'MAPE'] 
Mean_APE_validation<-fcast_accuracy


### Tables 
## this table should maybe store all results?


accuracy_result<-data.frame(
MEAN_APE=Mean_APE_validation
#Median_APE=Mean_APE_validation
)

### diagnostic results

diagnostic_result<-data.frame(
HORIZON=hor,
SMOOTH=smooth,
FREQ_TS=freq_ts,
FREQ_WARNING=freq_warning,
LEN_HORIZON_DATES=len_new_dates,
FIRST_TRAIN_DATE=first_train_date,
LAST_TRAIN_DATE=train_end_date,
FIRST_HORIZON_DATE=horizon_start_date,
LAST_HORIZON_DATE=horizon_end_date,
VARIABLES_ERROR_VALUE=variables_error_value,
LEN_TIME_SERIES=len_time_series,
NROW_VARIABLE_DF=nrow_variable_df,
NROW_XREG=nrow_xreg,
NROW_NEW_XREG=nrow_new_xreg,
NCOL_XREG=ncol_xreg,
NCOL_NEW_XREG=ncol_new_xreg,
FREQ_EST_SHORT=freq_estimate_short,
FREQ_EST_LONG=freq_estimate_long,
AIC_OPTION=aic_option,
AIC_VALUE=aic_value,
NCOL_XREG_WITH_HOLIDAY = ncol_xreg_with_holiday,
LEN_HOLIDAY=len_holiday,
LEN_HOLIDAYF=len_holidayf,
LAST_ACTUAL=last_actual_value
)

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


