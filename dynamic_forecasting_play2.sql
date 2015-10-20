-- Instructions: 
-- Run create_tables: create new forecastHorizon and diagnosticResult table before running each test case
--TS01
CALL executeRForecast("DS1_01","paramTable", "variableMatrix","skiplist","forecastFitted","forecastHorizon","actuals","diagnosticResult","accuracy") WITH OVERVIEW;
SELECT * FROM "forecastHorizon";
SELECT * FROM "diagnosticResult";

--TS02
DROP TABLE "variableMatrix_DS01_01";
CREATE TABLE "variableMatrix_DS01_01" as (SELECT DATE_ID AS DATEI,TEMP as "VAR1",TEMP as "VAR2",TEMP as "VAR3" FROM "ILANA"."North_filtered_weather" WHERE DATE_ID >'2004-10-30' ORDER BY DATE_ID);
CALL executeRForecast("DS1_01","paramTable1", "variableMatrix_DS01_01","skiplist","forecastFitted","forecastHorizon","actuals","diagnosticResult","accuracy") WITH OVERVIEW;
SELECT * FROM "forecastHorizon";
SELECT * FROM "diagnosticResult";

--TS03
CALL executeRForecast("DS1_01","paramTable2", "variableMatrix_DS01_01","skiplist","forecastFitted","forecastHorizon","actuals","diagnosticResult","accuracy") WITH OVERVIEW;
SELECT * FROM "forecastHorizon";
SELECT * FROM "diagnosticResult";

--TS04
DROP TABLE "variableMatrix_DS01_02";
CREATE TABLE "variableMatrix_DS01_02" as (SELECT * FROM "variableMatrix_DS01_01" ORDER BY DATEI);
UPSERT "variableMatrix_DS01_02" VALUES ('2005-01-30',3,4,5) where DATEI='2005-01-30';
SELECT * FROM "variableMatrix_DS01_02" where DATEI > '2005-01-20' ORDER BY DATEI;

CALL executeRForecast("DS1_01","paramTable2", "variableMatrix_DS01_02","skiplist","forecastFitted","forecastHorizon","actuals","diagnosticResult","accuracy") WITH OVERVIEW;
SELECT * FROM "forecastHorizon";
SELECT * FROM "diagnosticResult";

--TS05
DROP TABLE "variableMatrix_DS01_03";
CREATE TABLE "variableMatrix_DS01_03" as (SELECT * FROM "variableMatrix_DS01_01" ORDER BY DATEI);
UPSERT "variableMatrix_DS01_03" VALUES ('2005-01-02',3,4,5) where DATEI='2005-01-02';
SELECT * FROM "variableMatrix_DS01_03" where DATEI > '2005-01-01' ORDER BY DATEI;

CALL executeRForecast("DS1_01","paramTable2", "variableMatrix_DS01_03","skiplist","forecastFitted","forecastHorizon","actuals","diagnosticResult","accuracy") WITH OVERVIEW;
SELECT * FROM "forecastHorizon";
SELECT * FROM "diagnosticResult";

--TS06 
DROP TABLE "variableMatrix_DS01_04";
CREATE TABLE "variableMatrix_DS01_04" as (SELECT * FROM "variableMatrix_DS01_01" where DATEI!='2005-01-03' ORDER BY DATEI);
SELECT * FROM "variableMatrix_DS01_04" where DATEI > '2005-01-01' ORDER BY DATEI;

CALL executeRForecast("DS1_01","paramTable2", "variableMatrix_DS01_04","skiplist","forecastFitted","forecastHorizon","actuals","diagnosticResult","accuracy") WITH OVERVIEW;
SELECT * FROM "forecastHorizon";
SELECT * FROM "diagnosticResult";

--TS07 
DROP TABLE "variableMatrix_DS01_05";
CREATE TABLE "variableMatrix_DS01_05" as (SELECT * FROM "variableMatrix_DS01_01" where DATEI!='2005-02-03' ORDER BY DATEI);
SELECT * FROM "variableMatrix_DS01_05" where DATEI > '2005-01-01' ORDER BY DATEI;

CALL executeRForecast("DS1_01","paramTable2", "variableMatrix_DS01_05","skiplist","forecastFitted","forecastHorizon","actuals","diagnosticResult","accuracy") WITH OVERVIEW;
SELECT * FROM "forecastHorizon";
SELECT * FROM "diagnosticResult";

# Longer time-series (Quebec)


# Business Days only Tests...(missing holidays)
DS02

CALL executeRForecast("DS2_01","paramTable", "variableMatrix","skiplist","forecastFitted","forecastHorizon","actuals","diagnosticResult","accuracy") WITH OVERVIEW;
SELECT * FROM "forecastHorizon";
SELECT * FROM "diagnosticResult";


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
