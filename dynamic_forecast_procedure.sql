
DROP PROCEDURE executeRForecast;
CREATE PROCEDURE executeRForecast(IN timeseries "timeSeriesInput2", IN param "paramTable", IN variable_matrix "variableMatrix", IN future_skip "skiplist", OUT fit_result "forecastFitted", OUT horizon_result "forecastHorizon", OUT actuals_table "actuals", OUT diagnostic_result "diagnosticResult", OUT accuracy_result "accuracy")
LANGUAGE RLANG AS
--DEFAULT SCHEMA HDITTMER            
BEGIN

#utility functions - see dplyr
data_frame_select<-function(input_df,start_date,end_date,num_var){
  subset(input_df,input_df$date >= start_date & input_df$date <= end_date)[,1:(num_var+1)]
}
Mean_APE_function<-function(actual_values,forecast_values){
forecast_error<-abs(forecast_values - actual_values)
APE<-forecast_error/actual_values * 100
sum(APE) /length(actual_values)  ## MAPE
}
# guess likely time step difference in observations 
# http://stackoverflow.com/questions/19217729/check-the-frequency-of-time-series-data?rq=1
guess_period <- function(x) { 
  #average_period <- as.double( mean(diff(x$date)), units="days" )
  #average_period <- as.double( mean(diff(index(x))), units="days" )
  average_period <- as.double( mean(diff(x)), units="days" )
  
  difference <- abs(log( average_period / c(
    daily = 1,
    business_days = 7/5,
    weekly = 7,
    monthly = 30,
    quarterly = 365/4,
    annual = 365
  ) ) )
  #names( which.min( difference ) )
  which.min( difference ) 
}

DATE_ID=c(timeseries$DATE_ID)
actuals=c(timeseries$TOTAL)

# get parameters
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

# Step 1. Estimate period & return WARNING if user input probably incorrect
freq_warning<-0 # assume user_input_period is correct. Insert flag into diagnos
freq_estimate<-0 # initialise
freq_estimate<-guess_period(DATE_ID)

if (freq_estimate != user_input_freq){
freq_warning<-1  # potentially entered wrong frequency
}

# Get freq_descrip and freq_ts (frequency setting, assuming an annual cycle)
#Functions: type_to_freq and type_to_descrip
type_to_descrip<-function(freq_type)
{
  switch(freq_type,
         "1" = "days",
         "2" = "days",
         "3" = "weeks",
         "4" = "months",
         "5" = "quarters",
         "6" = "years")
}

type_to_freq<-function(freq_type)
{
  switch(freq_type,
         "1" = 365.25,
         "2" = 261,
         "3" = 52,
         "4" = 12,
         "5" = 4,
         "6" = 1)
}

freq_type<-user_input_freq
freq_descrip<-type_to_descrip(user_input_freq)
freq_ts<-365 # default
freq_ts<-type_to_freq(user_input_freq)

# Smooth data
# Function: calc_mean_indices - smoothing function
calc_mean_indices<-function(index){
  mean(actuals[(index-3):index])
}

if(smooth==1){ ## smoothing
  actuals_MA<-vector()
  actuals_MA[1:3]<-actuals[1:3]
  index_list<-c(4:length(actuals)) # calc averages
  mean_indices<-sapply(index_list,calc_mean_indices)
  actuals_MA<-c(actuals_MA,mean_indices)
  actuals<-actuals_MA
}
TOTAL=c(timeseries$TOTAL)
len_time_series<-length(actuals)

# set date variables for training and horizon data
first_train_date=as.Date(DATE_ID[1],"%d/%m/%Y")
train_end_date<-as.Date(DATE_ID[length(DATE_ID)] ,"%d/%m/%Y")  # length first_training_dates gives the index of the last day of training data
horizon_start_date=train_end_date+1

# sequence dates for horizon period
#horizon_dates_seq<-seq(from=horizon_start_date, length.out=hor, by=1) #length.out not working
end_date<-horizon_start_date+15*freq_ts # 15 years worth of values (e.g. 15*365, or 15*52 etc
horizon_sequence_dates <- seq(from=horizon_start_date, to=end_date, by=freq_descrip)  ## set "by" to be days, weeks, months etc
horiz_dates<-horizon_sequence_dates[1:hor]
horizon_end_date=horiz_dates[hor] # last date

# set default values for diagnostic flags
variables_error_value=-1  # value informs type of error
variables_error_flag<-0 # assume no errors
nrow_variable_df<-0 # default flag value
nrow_xreg<-0  # for diagnostics
nrow_new_xreg<-0
ncol_xreg<-0
ncol_new_xreg<-0

# set default values for xreg parameters
xreg_select<-NULL # test that works  
new_xreg_select<-NULL 

num_col_var<-(ncol(variable_matrix)-1)  # number of variable columns

if (num_var==0){  # num_var tells us if we want variables or not
  variables_error_value<-0
} else {  
  	# create variable data frame
  	DATES<-c(variable_matrix$DATEI)
  	VAR1<-c(variable_matrix$VAR1) 	
  	VAR2<-c(variable_matrix$VAR2)
  	VAR3<-c(variable_matrix$VAR3)
 	variable_df<-data.frame(DATES, VAR1, VAR2, VAR3)
  	colnames(variable_df)<-c("date", "value1", "value2", "value3")    	
  	nrow_variable_df<-nrow(variable_df)  #diagnostic
  								
    #variable_df_select<-data_frame_select(variable_df, first_train_date, horizon_end_date, num_var) # get all columns       	
	# select rows based on dates matching time series dates & remove duplicates
	xreg_select<-data_frame_select(variable_df, first_train_date, train_end_date, num_var)
	xreg_select_no_dups<-xreg_select[!duplicated(lapply(xreg_select, summary))]
	new_xreg_select<-data_frame_select(variable_df, horizon_start_date, horizon_end_date, num_var)  
	new_xreg_select_no_dups<-new_xreg_select[!duplicated(lapply(new_xreg_select, summary))]
	
	if(ncol(xreg_select_no_dups)!=ncol(new_xreg_select_no_dups)){
		variables_error_flag<-1  # set error
		variables_error_value<-4  # there is a redundant column in either xreg, or new_xreg 
	}else if (!identical(xreg_select$date,DATE_ID) | !identical(new_xreg_select$date,horiz_dates)){
		variables_error_flag<-1
	 	variables_error_value<-3 # dates do not match
	}

	if(variables_error_flag==0){     # variable dates match actuals and horizon dates
		xreg_select<-xreg_select_no_dups
		new_xreg_select<-new_xreg_select_no_dups
    	variables_error_value<-1 	  # success	    	
		nrow_xreg<-nrow(xreg_select)  # for diagnostics
		nrow_new_xreg<-nrow(new_xreg_select)
		ncol_xreg<-(ncol(xreg_select)-1)
		ncol_new_xreg<-(ncol(new_xreg_select)-1)
	}else{
		xreg_select<-NULL # reset to null
		new_xreg_select<-NULL
 	} 
} #end if num_var>0 


## FUNCTION: set_missing_values: sets values for actuals where missing dates to 0
set_missing_values<-function(DATE_ID,actuals, freq_type) {
partial<-data.frame(DATE_ID,actuals,row.names=NULL)
colnames(partial)<-c("date","value")
# sort partial by date
sorted_partial<-partial[order(as.Date(partial$date, format="%d/%m/%Y")),]
first_value<-sorted_partial$date[1] # First time-series point
first_train_date=as.Date(first_value,"%d/%m/%Y")
start_date<-first_train_date
train_end_val<-sorted_partial$date[nrow(sorted_partial)]  
end_date<-as.Date(train_end_val,"%d/%m/%Y")
# create full set of dates in time series
full <- seq(from=start_date, to=end_date, by=1)  ## set "by" to be frequency
if(freq_type==2){   # business days only
full<-full[isWeekday(full)] ## only weekdays
}
#create full data frame, wtih missing dates as NAs
time_series_df<-data.frame(Date=full, value=with(sorted_partial, value[match(full, date)]))
holiday<-as.integer(!complete.cases(time_series_df$value)) # create holiday as vector 1 if skipped value, 0 if value present
#time_series_df$value[!complete.cases(time_series_df$value)==TRUE]<-0 #set to 0
time_series_df$value[!complete.cases(time_series_df$value) & !(time_series_df$Date %in% sorted_partial$date )]<-0 # if date has just been added in this function
time_series_df<-cbind(time_series_df,holiday)
colnames(time_series_df)<-c("Date","value","holiday")
time_series_df
}

## FUNCTION: set_missing_values_xreg: sets values for columns in xreg where missing dates to 0
#only call if xreg_select is not null
set_missing_values_xreg<-function(xreg_select, freq_type) {
partial<-xreg_select
#colnames(partial)<-c("date","value")
# sort partial by date
sorted_partial<-partial[order(as.Date(partial$date, format="%d/%m/%Y")),]
first_value<-sorted_partial$date[1] # First time-series point
first_train_date=as.Date(first_value,"%d/%m/%Y")
start_date<-first_train_date
train_end_val<-sorted_partial$date[nrow(sorted_partial)]  # length first_training_dates gives the index of the last day of training data
end_date<-as.Date(train_end_val,"%d/%m/%Y")
# create full set of dates in time series
full <- seq(from=start_date, to=end_date, by=1)  ## set "by" to be frequency

if(freq_type==2){   # business days only
	full<-full[isWeekday(full)] ## only weekdays
}

# create data frame with full set of dates, and NAs where dates originally missing
xreg_complete_NA<-data.frame(Date=full,xreg_select[match(full, xreg_select$date),-c(1)])   # added Date back in as gets lost

# Function: col_dates: for an input column, sets the value to 0, if value is NA and date was missing
col_dates<-function(col,dates) {col[!complete.cases(col) & !(dates %in% sorted_partial$date)]<-0 ; col}

if (ncol(xreg_complete_NA)>2){
xreg_complete_zero<-apply(xreg_complete_NA[,-c(1)],2, col_dates, dates=xreg_complete_NA$Date)
}else{
xreg_complete_zero<-col_dates(xreg_complete_NA[,-c(1)],xreg_complete_NA$Date)
}

xreg_df<-cbind(xreg_complete_NA$Date,xreg_complete_zero)
colnames(xreg_df)<-colnames(xreg_select)
xreg_df
} # end set_missing_values_xreg


# set future dates to "skip" - only for daily data 
future_skiplist<-NULL # default
ncol_xreg_with_holiday<-0
len_holiday<-0

# daily data, and either 0 variables (=0), or successful entry of variables (=1)
if (freq_type==1 | freq_type ==2) {
future_skiplist=c(future_skip$DATE_ID) # a list of dates to skip in the future.. , regress on these dates with dummy, but remove dates and vals from horizon when returning (future release)
future_skiplist<-as.Date(future_skiplist,format="%d/%m/%Y") # ensure formatting ok
holidayf<-c(rep(0,hor))  # initialise to 0
holidayf[match(future_skiplist,horiz_dates)]<-1 # set to 1 if in horizdates

results_ts_df<-set_missing_values(DATE_ID,actuals, freq_type) # sets
DATE_ID<-results_ts_df$Date  # new dates
actuals<-results_ts_df$value # new actuals (may contain zeros)
holiday<-results_ts_df$holiday # vector of 1 or 0 for missed values.. 
len_holiday<-length(holiday[holiday==1])

if(variables_error_value==1){  # update xreg
xreg_select<-set_missing_values_xreg(xreg_select,freq_type) # update missing dates in xreg_select
}

if (length(holiday[holiday==1]) != 0 & length(holiday[holiday==0]) != 0){ # ie not a column of zeros or 1
if(variables_error_value==1){
	xreg_select<-cbind(xreg_select,holiday) ## holiday has FULL Dates, xreg_select has one date missing # think its ok
	new_xreg_select<-cbind(new_xreg_select,holidayf) # add extra variable column as future "skips"
} else{
	xreg_select<-cbind(DATE_ID,holiday)
	new_xreg_select<-cbind(DATE_ID,holidayf)
}
ncol_xreg_with_holiday<-(ncol(xreg_select)-1) #diagnostic 
} # end if holiday

} # end if freq_type==1 or 2

# Create forecast
library("forecast")
#library("dplyr")

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
forecast_test_daily<-function(actuals,freq_test_short,freq_test_long){
  if(freq_test_long ==1){ ## no long frequency
    time_series<-ts(actuals, frequency=freq_test_short, start=c(start_year, num_days)) # number of steps(weeks,months,quarters into start_year)
    fit<-auto.arima(time_series, xreg=xreg_select[,-c(1)]) 
    fcast<-forecast(fit,h=hor,xreg=new_xreg_select[,-c(1)])
  }else if (freq_test_short ==1 & freq_test_long!=1){
    time_series<-ts(actuals, frequency=freq_test_long, start=c(start_year, num_days)) # first value is num_days days into start_year
    fit<-stlm(time_series, method="arima", s.window="periodic", xreg=xreg_select[,-c(1)]) 
    #fit<-stlm(time_series, method="arima", s.window="periodic", xreg=xreg_select[,c(2)])         
    fcast<-forecast.stlm(fit,h=hor,xreg=new_xreg_select[,-c(1)])
  }else if (freq_test_short !=1 & freq_test_long!=1){
    n_train<-length(actuals)
    time_series<-ts(actuals, frequency=freq_test_short, start=c(start_year, num_days)) # first value is num_days days into start_year
    z <- forecast::fourier(ts(actuals, frequency= freq_test_long), K=12) # f=365.25*5/7
    zf <- forecast::fourierf(ts(actuals, frequency= freq_test_long), K=12, h=hor)
    fit <- auto.arima(time_series, xreg=cbind(xreg_select[,-c(1)],z))
    #new_xreg_select=cbind(new_xreg_select,zf) # update new regressors
    fcast<-forecast(fit, xreg=cbind(new_xreg_select[,-c(1)],zf), h=hor)	    
  }
    fcast # return fcast 
} # end_forecast_test_daily

## FUNCTION: forecast_best_freq_daily: call forecast_test_daily with different frequency combinations; select forecast with best fit (aic) to return
forecast_best_freq_daily <- function(actuals,freq_ts_short,freq_ts_long){ 
  freq_estimate_short <- findfrequency(actuals[1:182]) ## allow shorter frequency
  freq_estimate_long <- findfrequency(actuals)      
  frequencies_short<-c(freq_estimate_short,freq_ts_short)  # freq_ts_short=7 or 5
  frequencies_long<-c(freq_estimate_long,freq_ts_long)   # freq_ts = 365 or 261
  aic <- list(
    "1" = forecast_test_daily(actuals,frequencies_short[1],frequencies_long[1]),
    "2" = forecast_test_daily(actuals,frequencies_short[1],frequencies_long[2]),
    "3" = forecast_test_daily(actuals,frequencies_short[2],frequencies_long[1]),
    "4" = forecast_test_daily(actuals,frequencies_short[2],frequencies_long[2])   
  )
  aic_option<-which.min(c(aic[[1]]$model$aic,aic[[2]]$model$aic,aic[[3]]$model$aic,aic[[4]]$model$aic))
  #freq_min_aic<-frequencies[which.min(aic)]  ## select best frequency
  best_forecast<-aic[[aic_option]]  # best forecast
  list(best_forecast,freq_estimate_short,freq_estimate_long,aic_option,best_forecast$model$aic) # return best frequency, and forecast
}

# call function to get forecast from "best" fit ts (ts gives fit with min aic) 

if(length(actuals)<(2*freq_ts)) { # less than two periods, not going to compare for long (365 day) frequency,auto.arima drops seasonal if no good
	freq_estimate <- findfrequency(actuals)  
	time_series<-ts(actuals, frequency=freq_estimate, start=c(start_year, num_days)) # number of steps(weeks,months,quarters into start_year)
	#fit<-auto.arima(time_series)
	#fcast_arima<-forecast(fit,h=hor)
	fit<-auto.arima(time_series, xreg=xreg_select[,-c(1)]) 
    fcast_arima<-forecast(fit,h=hor,xreg=new_xreg_select[,-c(1)])
	best_freq<-freq_estimate # used later to compute accuaracy
	freq_estimate_short<-best_freq  
	freq_estimate_long<--1 # N/A
	aic_option<--0 ## 0
	aic_value<-fit$aic
}else{   ## seasonal: return best of frequency estimate and freq=12
	fcast_list<-forecast_best_freq_daily(actuals,freq_ts_short,freq_ts) # should pass start_date, num_year
	fcast_arima<-fcast_list[[1]]
	freq_estimate_short<-fcast_list[[2]] # useful for diagnostics
	freq_estimate_long<-fcast_list[[3]]
	aic_option<-fcast_list[[4]]
	aic_value<-fcast_list[[5]]	
}  #end else

# calculate accuracy - will put in later version, for now just use "accuracy" estimate
fcast_accuracy<-accuracy(fcast_arima)
 
list(fcast_arima=fcast_arima,fcast_accuracy=fcast_accuracy,freq_estimate_short=freq_estimate_short,freq_estimate_long=freq_estimate_long, aic_option=aic_option,aic_value=aic_value)
}

###### Forecast WEEKLY/MONTHLY/QUARTERLY #########
generate_forecast_weekly_monthly_quarterly<-function(freq_type,DATE_ID,actuals,hor,xreg_select,new_xreg_select,freq_descrip,freq_ts){

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
num_steps<-get_steps(freq_type)

freq_estimate_short=-1  # Default value for flags
freq_estimate_long=-1
aic_option=-1 
aic_value=-1

# runs a forecast for a time-series with a given frequency
forecast_test_weekly_monthly_quarterly<-function(actuals,freq_test){
  time_series<-ts(actuals, frequency=freq_test, start=c(start_year, num_steps)) # number of steps(weeks,months,quarters into start_year)
  fit<-auto.arima(time_series, xreg=xreg_select[,-c(1)]) 
  forecast(fit,h=hor,xreg=new_xreg_select[,-c(1)])
}

# returns forecast from model with best fit (from best frequency)
forecast_best_freq <- function(actuals,freq_ts){ 
  freq_estimate <- findfrequency(actuals)  
  frequencies<-c(freq_estimate,freq_ts)
  aic <- list(
    "1" = forecast_test_weekly_monthly_quarterly(actuals,frequencies[1]),
    "2" = forecast_test_weekly_monthly_quarterly(actuals,frequencies[2])
    )
  aic_option<-which.min(c(aic[[1]]$model$aic,aic[[2]]$model$aic))    
  best_forecast<-aic[[aic_option]]  # best forecast
  list(best_forecast,freq_estimate,aic_option,best_forecast$model$aic) # return best frequency, and forecast  
}

if(length(actuals)<(2*freq_ts)) { # less than two periods, not going to compare for 12 month frequency,auto.arima drops seasonal if no good
	freq_estimate <- findfrequency(actuals)  
	time_series<-ts(actuals, frequency=freq_estimate, start=c(start_year, num_steps)) # number of steps(weeks,months,quarters into start_year)
	#fit<-auto.arima(time_series)
	#fcast_arima<-forecast(fit,h=hor)
	fit<-auto.arima(time_series, xreg=xreg_select[,-c(1)]) 
    fcast_arima<-forecast(fit,h=hor,xreg=new_xreg_select[,-c(1)])
	best_freq<-freq_estimate # used later to compute accuaracy
	freq_estimate_short<-best_freq  
	freq_estimate_long<--1 # N/A
	aic_option<-0
	aic_value<-fit$aic
}else{   ## seasonal: return best of frequency estimate and freq=12
	fcast_list<-forecast_best_freq(actuals,freq_ts)
	fcast_arima<-fcast_list[[1]]
	freq_estimate_short<-fcast_list[[2]]
	freq_estimate_long<-fcast_list[[2]] # redundant parameter
	aic_option<-fcast_list[[3]]
	aic_value<-fcast_list[[4]]
}  # end else
 
# Forecasting done; generate some statistics and output results	
fcast_accuracy<-accuracy(fcast_arima)

list(fcast_arima=fcast_arima,fcast_accuracy=fcast_accuracy,freq_estimate_short=freq_estimate_short,freq_estimate_long=freq_estimate_long, aic_option=aic_option,aic_value=aic_value)
} # end generate_forecast_weekly_monthly_quarterly

#generate_forecast_annually(){}

fcast_results<-generate_forecast(freq_type,DATE_ID,actuals,hor,xreg_select,new_xreg_select,freq_descrip,freq_ts) # return fcast_arima_object

fcast_arima<-fcast_results$fcast_arima
fcast_accuracy<-fcast_results$fcast_accuracy
freq_estimate_short<-fcast_results$freq_estimate_short
freq_estimate_long<-fcast_results$freq_estimate_long
aic_option<-fcast_results$aic_option
aic_value<-fcast_results$aic_value

## update values for tables
len_new_dates<-length(horiz_dates) #do this outside function
#Mean_APE_validation<-fcast_accuracy['Test set','MAPE'] 
Mean_APE_validation<-fcast_accuracy[,'MAPE'] 

#Median_APE_validation<-calc_MAPE #  test my MAPE is same as calc MAPE for now
#Median_APE_validation<-fcast_accuracy['Test set','MAPE'] ## JUST for now (not really median)
Median_APE_validation<-fcast_accuracy[,'MAPE'] 

### Tables 
## this table should maybe store all results?
accuracy_result<-data.frame(
  MEAN_APE=Mean_APE_validation,
  MEDIAN_APE=Median_APE_validation
)

### diagnostic results

diagnostic_result<-data.frame(
HORIZON=hor,
SMOOTH=smooth,
FREQ_TS=freq_ts,
FREQ_WARNING=freq_warning,
LEN_HORIZON_DATES=len_new_dates,
LAST_TRAIN_DATE=train_end_date,
FIRST_HORIZON_DATE=horizon_start_date,
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
LEN_HOLIDAY=len_holiday
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


