
CALL executeRForecast("timeSeriesInput2","paramTable", "variableMatrix","skiplist","forecastFitted","forecastHorizon","actuals","diagnosticResult","accuracy") WITH OVERVIEW;

CALL executeRForecast("timeSeriesInput2","paramTable", "variableMatrix","skiplist","forecastFitted","forecastHorizon","actuals","diagnosticResult","accuracy") WITH OVERVIEW;

# 2 parameters, holiday=0 (no missing dates)
CALL executeRForecast("timeSeriesInput2","paramTable2", "variableMatrix3","skiplist","forecastFitted","forecastHorizon","actuals","diagnosticResult","accuracy") WITH OVERVIEW;

# missing dates, 1 parameters
CALL executeRForecast("timeSeriesInput3","paramTable", "variableMatrix2","skiplist","forecastFitted","forecastHorizon","actuals","diagnosticResult","accuracy") WITH OVERVIEW;

-- variables =2 (timeSeriesInput3 and variableMatrix different dimension), holiday=1 (1 holiday in timeSeriesInput3), len_time_series2=2 (diagnostic)
CALL executeRForecast("timeSeriesInput3","paramTable", "variableMatrix3","skiplist","forecastFitted","forecastHorizon","actuals","diagnosticResult","accuracy") WITH OVERVIEW;

-- missing dates, variables=1 (timeseriesInput3 and variableMatrix4 are same dimension), holiday =1 (1 holiday in timeSeriesInput3) 
CALL executeRForecast("timeSeriesInput3","paramTable2", "variableMatrix4","skiplist","forecastFitted","forecastHorizon","actuals","diagnosticResult","accuracy") WITH OVERVIEW;

SELECT * FROM "timeSeriesInput2";
SELECT * FROM "variableMatrix"  WHERE DATEI > '2014-12-10' ;
SELECT * FROM "forecastFitted" WHERE DATE_ID > '2014-12-10' ;
SELECT * FROM "forecastHorizon";
SELECT * FROM "diagnosticResult";
SELECT * FROM "accuracy";
SELECT * FROM "actuals";
SELECT * FROM "paramTable";

--CALL executeRForecast() WITH OVERVIEW;
