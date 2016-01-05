--- Name: checkXreg
--- Pre-conditions: 
--- Post-conditions: xreg and new_xreg contain same number of columns as each-other, and same dates as training data, and horizon, respectively.
--- If no extra variables selected, or an error occurs, 3 columns of xreg are returned as "NAs"
--- Function: returns xreg and new_xreg as data frames containing extra variables for training and horizon periods, respectively. 
DROP PROCEDURE checkXreg;
CREATE PROCEDURE checkXreg(IN timeseries "timeSeriesInput2",IN param "paramTable", IN variable_matrix "variableMatrix",  OUT xreg "variableMatrix", OUT newxreg "variableMatrix", OUT diagnostic_result_checkxreg "diagnosticResultCheckXreg")
LANGUAGE RLANG AS
--DEFAULT SCHEMA HDITTMER            
BEGIN

library("utility.forecast") # data_frame_select, checkXreg
library("zoo") # na.impute

# timeseries
DATE_ID=c(timeseries$DATE_ID)
actuals=c(timeseries$TOTAL)

#param
parameters_key<-c(param$KEY)
parameters_value<-c(param$VALUE)
parameters<-data.frame(parameters_key, parameters_value)
colnames(parameters)<-c("key", "value")
hor=parameters$value[parameters$key %in% "HORIZON"]
num_var=parameters$value[parameters$key %in% "NUM_VAR"]
freq_type=parameters$value[parameters$key %in% "FREQUENCY_TYPE"]

# variable_matrix (must contain correct columns)
DATES<-c(variable_matrix$DATEI)
VAR1<-c(variable_matrix$VAR1) 	
VAR2<-c(variable_matrix$VAR2)
VAR3<-c(variable_matrix$VAR3)
variable_df<-data.frame(DATES, VAR1, VAR2, VAR3)
colnames(variable_df)<-c("date", "value1", "value2", "value3") 
nrow_variable_df<-nrow(variable_df)  #diagnostic
num_col_var<-(ncol(variable_df)-1)  

#### Set date variables for training and horizon periods
freq_descrip=type_to_descrip(freq_type) #utility.forecast function

train_date_list<-get_train_start_end(DATE_ID)
train_start_date<-train_date_list$train_start_date
train_end_date<-train_date_list$train_end_date
hor_date_list<-get_horizon_start_end(train_end_date,horizon=hor,freq_type,freq_descrip)
horizon_start_date<-hor_date_list$horizon_start_date
horizon_end_date<-hor_date_list$horizon_end_date
horiz_dates<-get_horizon_dates(horizon_start_date,horizon_end_date,freq_type,freq_descrip)

# set default values for diagnostic flags
variables_error_value=-1  # value informs type of error
variables_error_flag<-0 # value = 0 (no error in input) or 1 (error in input)
nrow_variable_df<-0 # default flag value
nrow_xreg<-0  # for diagnostics
nrow_new_xreg<-0
ncol_xreg<-0
ncol_new_xreg<-0

# set default values for xreg parameters
xreg_select<-NULL # test that works  
new_xreg_select<-NULL 
num_col_var<-(ncol(variable_matrix)-1)  # number of variable columns

#### Check and pre-process extra variables
# set default values 
variables_error_flag=-1
variables_error_value=-1
xreg_select<-NULL 
new_xreg_select<-NULL 

if (num_var!=0){  # num_var tells us how many variables user wants   	  
  # select rows based on dates matching time series dates & remove duplicates
  xreg_select_no_dups<-data_frame_select(variable_df, train_start_date, train_end_date, num_var,rem_dups=TRUE)
  new_xreg_select_no_dups<-data_frame_select(variable_df, horizon_start_date, horizon_end_date, num_var,rem_dups=TRUE)  

  # check if xreg data is faulty
  xreg_flags<-checkXreg(xreg_select_no_dups,new_xreg_select_no_dups,DATE_ID,horiz_dates)
  variables_error_flag=xreg_flags$variables_error_flag
  variables_error_value=xreg_flags$variables_error_value
    
  if(variables_error_flag==0){     # no error
    xreg_select<-xreg_select_no_dups
    new_xreg_select<-new_xreg_select_no_dups
    
    ##turn into R function in utlity.forecast
    col_impute<-function(col){
    na.approx(col,na.rm=FALSE);
    na.fill(col,"extend")
    }
  		l<-xreg_select_no_dups[,2:ncol(xreg_select_no_dups)]
  		p<-apply(data.frame(l),2,col_impute)
  		t<-data.frame(matrix(unlist(p),ncol=3,byrow=T))
  		xreg_select<-data.frame(xreg_select_no_dups[,1],t)
  		colnames(xreg_select)<-colnames(xreg_select_no_dups)
  		
  		l2<-new_xreg_select_no_dups[,2:ncol(new_xreg_select_no_dups)]
  		p2<-apply(data.frame(l2),2,col_impute)
  		t2<-data.frame(matrix(unlist(p2),ncol=(ncol(new_xreg_select_no_dups)-1),byrow=T))
  		new_xreg_select<-data.frame(new_xreg_select_no_dups[,1],t2)
		colnames(new_xreg_select)<-colnames(new_xreg_select_no_dups)
  } # end if variables_error_flag==0

} # end if num_var !=0

#diagnostic 
#default
nrow_xreg<-0
ncol_xreg<-0
nrow_new_xreg<-0
ncol_new_xreg<-0

if (!is.null(xreg_select)){
nrow_xreg<-as.numeric(nrow(xreg_select)) 
ncol_xreg<-as.numeric(ncol(xreg_select)-1)
}
if (!is.null(new_xreg_select)){
nrow_new_xreg<-as.numeric(nrow(new_xreg_select))
ncol_new_xreg<-as.numeric(ncol(new_xreg_select)-1)
}

#DATE_ID_zeros<-c(rep(0,length(DATE_ID))

if (is.null(xreg_select)){
xreg<-data.frame(
DATEI=train_start_date,
VAR1=0,
VAR2=0,
VAR3=0
)
}else{
xreg<-data.frame(
DATEI=DATE_ID,
VAR1=if(num_var>=1) {xreg_select$value1} else{c(rep(0,length(DATE_ID)))},
VAR2=if(num_var>=2) {c(xreg_select$value2)} else{c(rep(0,length(DATE_ID)))},
VAR3=if(num_var>=3) {c(xreg_select$value3)} else{c(rep(0,length(DATE_ID)))}
)
}

#VAR1=c(rep(0,length(DATE_ID))),
#VAR2=c(rep(0,length(DATE_ID))),
#VAR3=c(rep(0,length(DATE_ID)))
if (is.null(new_xreg_select)){
newxreg<-data.frame(
DATEI=horizon_start_date,
VAR1=0,
VAR2=0,
VAR3=0
)
}else{
newxreg<-data.frame(
DATEI=horiz_dates,
VAR1=if(num_var>=1) {new_xreg_select$value1} else{c(rep(0,length(horiz_dates)))},
VAR2=if(num_var>=2) {c(new_xreg_select$value2)} else{c(rep(0,length(horiz_dates)))},
VAR3=if(num_var>=3) {c(new_xreg_select$value3)} else{c(rep(0,length(horiz_dates)))}
)
}
#VAR1=c(rep(0,length(horiz_dates))),
#VAR2=c(rep(0,length(horiz_dates))),
#VAR3=c(rep(0,length(horiz_dates)))

diagnostic_result_checkxreg<-data.frame(
VARIABLES_ERROR_FLAG=as.integer(xreg_select$value1[1]),
VARIABLES_ERROR_VALUE=variables_error_value,
NROW_XREG=nrow_xreg,
NROW_NEW_XREG=nrow_new_xreg,
NCOL_XREG=ncol_xreg,
NCOL_NEW_XREG=ncol_new_xreg
)
END;
