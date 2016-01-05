--TC02
-- DS01_01
-- No missing values, no frequency,no missing dates (holidays), variables exist

--DROP TABLE "variableMatrix_DS01_01";
--CREATE TABLE "variableMatrix_DS01_01" as (SELECT DATE_ID AS DATEI,TEMP as "VAR1",TEMP as "VAR2",TEMP as "VAR3" FROM "ILANA"."North_filtered_weather" WHERE DATE_ID >'2004-10-30' ORDER BY DATE_ID);
--CALL executeRForecast("DS1_01","paramTable1", "variableMatrix_DS01_01","skiplist","forecastFitted","forecastHorizon","actuals","diagnosticResult","accuracy") WITH OVERVIEW;
--SELECT * FROM "forecastHorizon";
--SELECT * FROM "diagnosticResult";
--SELECT * FROM "accuracy";

-- Create output table for actual values
DROP TABLE "sortedOutput";
CREATE COLUMN TABLE "sortedOutput" LIKE "model" WITH NO DATA;
-- Sort data tables by date for input and xreg..
CALL sortTimeseriesByDate("DS1_01","sortedOutput") WITH OVERVIEW;
-- Possibly repeat for each value in input variable_matrix
SELECT * FROM "sortedOutput";

-- impute missing_values 
DROP TABLE "imputedOutput";
CREATE COLUMN TABLE "imputedOutput" LIKE "model" WITH NO DATA;

-- Optional: impute missing values
CALL imputeMissingValues("sortedOutput", "imputedOutput") WITH OVERVIEW;
-- can do also for variable matrix
SELECT * FROM "imputedOutput";

-- Optional: smooth
-- Smooth data
DROP TABLE "smoothedOutput";
CREATE COLUMN TABLE "smoothedOutput" LIKE "model" WITH NO DATA;
CALL smoothTimeseries("sortedOutput", "smoothedOutput") WITH OVERVIEW;

-- Find frequency
DROP TABLE "diagnosticResultFindFrequency";
CREATE COLUMN TABLE "diagnosticResultFindFrequency"(
SPACING_WARNING INTEGER,
SPACING_ESTIMATE INTEGER,
SPACING_TYPE INTEGER, 
FREQ_ESTIMATE_SHORT DOUBLE,
FREQ_ESTIMATE_LONG DOUBLE,
DEFAULT_ESTIMATE_SHORT DOUBLE,
DEFAULT_ESTIMATE_LONG DOUBLE, 	 	
PROMPT_DEFAULT_SHORT DOUBLE,
PROMPT_DEFAULT_LONG DOUBLE,
NUM_FREQ INTEGER
);

CALL findfrequency("smoothedOutput", "paramTable", "diagnosticResultFindFrequency") with OVERVIEW;
SELECT * FROM "diagnosticResultFindFrequency";

-- possibly update paramTable$FREQ based on diagnosticResultFindFrequency frequency estimates

--- Check xreg variables are valid for forecasting; ie -dates match time-series, same # of independent variable columns for train and horizon periods
-- create table diagnosticResultCheckXreg, xreg,newxreg

DROP TABLE "variableMatrix";
CREATE TABLE "variableMatrix" as (SELECT DATE_ID AS DATEI,TEMP as "VAR1",TEMP as "VAR2",TEMP as "VAR3" FROM "ILANA"."North_filtered_weather" ORDER BY DATE_ID);


DROP TABLE "variableMatrix_DS01_01";
CREATE TABLE "variableMatrix_DS01_01" as (SELECT DATE_ID AS DATEI,TEMP as "VAR1",TEMP as "VAR2",TEMP as "VAR3" FROM "ILANA"."North_filtered_weather" WHERE DATE_ID >'2004-10-30' AND DATE_ID < '2005-03-01' ORDER BY DATE_ID);
SELECT * FROM "variableMatrix_DS01_01";
SELECT * FROM "DS1_01";

DROP TABLE "XREG";
CREATE COLUMN TABLE "XREG" LIKE "variableMatrix" WITH NO DATA;
SELECT * FROM "XREG";

DROP TABLE "NEWXREG";
CREATE COLUMN TABLE "NEWXREG" LIKE "variableMatrix" WITH NO DATA;

DROP TABLE "diagnosticResultCheckXreg";
CREATE COLUMN TABLE "diagnosticResultCheckXreg"(
VARIABLES_ERROR_FLAG INTEGER,
VARIABLES_ERROR_VALUE INTEGER,
NROW_XREG INTEGER,
NROW_NEW_XREG INTEGER,
NCOL_XREG INTEGER,
NCOL_NEW_XREG INTEGER
);

SELECT * FROM "paramTable1";
CALL checkXreg("smoothedOutput", "paramTable1", "variableMatrix_DS01_01", "XREG", "NEWXREG","diagnosticResultCheckXreg") WITH OVERVIEW;
SELECT * from "variableMatrix";
SELECT * FROM "variableMatrixEmpty";
SELECT * FROM "XREG";
SELECT * FROM "NEWXREG";
SELECT * FROM "diagnosticResultCheckXreg";
SELECT * FROM "smoothedOutput";

-- Create output table for actual values
DROP TABLE "actuals";
CREATE COLUMN TABLE "actuals" LIKE "model" WITH NO DATA;

-- Create output table for fitted model values
DROP TABLE "forecastFitted";
CREATE COLUMN TABLE "forecastFitted" LIKE "model" WITH NO DATA;

DROP TABLE "horizon_model";
CREATE COLUMN TABLE "horizon_model"(
DATE_ID DATE, 
TOTAL DOUBLE,
TOTAL_UPPER_95 DOUBLE,
TOTAL_LOWER_95 DOUBLE
/*NUM_DAYS INTEGER,*/
/*START_YEAR DOUBLE*/
);

-- Create output table for fitted model values
DROP TABLE "forecastHorizon";
CREATE COLUMN TABLE "forecastHorizon" LIKE "horizon_model" WITH NO DATA;

-- for comparison, num_var=0
CALL autoarimaRWrapper("smoothedOutput","paramTable", "XREG","NEWXREG","forecastFitted","forecastHorizon","actuals") WITH OVERVIEW;
SELECT * FROM "forecastHorizon";
SELECT * FROM "forecastFitted";

CALL autoarimaRWrapper("smoothedOutput","paramTable1", "XREG","NEWXREG","forecastFitted","forecastHorizon","actuals") WITH OVERVIEW;
SELECT * FROM "forecastHorizon";
SELECT * FROM "forecastFitted";

