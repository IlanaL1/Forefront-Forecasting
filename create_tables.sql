

DROP TABLE "model";
CREATE COLUMN TABLE "model"(
DATE_ID DATE, 
TOTAL DOUBLE
/*NUM_DAYS INTEGER,*/
/*START_YEAR DOUBLE*/
);
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

DROP TABLE "skiplist_model";
CREATE COLUMN TABLE "skiplist_model"(
DATE_ID DATE
);

-- Create output table for forecast horizon values
DROP TABLE "skiplist";
CREATE COLUMN TABLE "skiplist" LIKE "skiplist_model" WITH NO DATA;

INSERT INTO "skiplist" VALUES ('2010-02-10');
INSERT INTO "skiplist" VALUES ('2015-02-23'); 

DROP TABLE "skiplist1";
CREATE COLUMN TABLE "skiplist1" LIKE "skiplist_model" WITH NO DATA;
INSERT INTO "skiplist1" VALUES ('1998-01-02'); 
INSERT INTO "skiplist1" VALUES ('1998-01-03'); 
INSERT INTO "skiplist1" VALUES ('1998-01-10'); 

-- Create output table for forecast horizon values
DROP TABLE "forecastHorizon";
CREATE COLUMN TABLE "forecastHorizon" LIKE "horizon_model" WITH NO DATA;

DROP TABLE "diagnosticResult";

CREATE COLUMN TABLE "diagnosticResult"(
HORIZON INTEGER,
SMOOTH INTEGER,
FREQ_TS INTEGER,
FREQ_WARNING INTEGER,
LEN_HORIZON_DATES INTEGER,
FIRST_TRAIN_DATE DATE,
LAST_TRAIN_DATE DATE,
FIRST_HORIZON_DATE DATE,
LAST_HORIZON_DATE DATE,
VARIABLES_ERROR_VALUE DOUBLE,
LEN_TIME_SERIES DOUBLE,
NROW_VARIABLE_DF DOUBLE,
NROW_XREG DOUBLE,
NROW_NEW_XREG DOUBLE,
NCOL_XREG DOUBLE,
NCOL_NEW_XREG DOUBLE,
FREQ_EST_SHORT DOUBLE,
FREQ_EST_LONG DOUBLE,
AIC_OPTION INTEGER,
AIC_VALUE DOUBLE,
NCOL_XREG_WITH_HOLIDAY DOUBLE,
LEN_HOLIDAY DOUBLE,
LEN_HOLIDAYF DOUBLE
);

DROP TABLE "accuracy";
CREATE COLUMN TABLE "accuracy"(
MEAN_APE DOUBLE,
MEDIAN_APE DOUBLE
);

DROP TABLE "param_model";
CREATE COLUMN TABLE "param_model"(
KEY 	VARCHAR(16),
VALUE	INTEGER
);


DROP TABLE "paramTable";
-- no variables
CREATE COLUMN TABLE "paramTable" LIKE "param_model" WITH NO DATA;
--INSERT into "paramTable" VALUES (1,365,1);
INSERT INTO "paramTable" VALUES ('HORIZON',30);
INSERT INTO "paramTable" VALUES ('SMOOTH',1);
INSERT INTO "paramTable" VALUES ('NUM_VAR',0); 
INSERT INTO "paramTable" VALUES ('FREQUENCY',365);
INSERT INTO "paramTable" VALUES ('FREQUENCY_TYPE',1);
INSERT INTO "paramTable" VALUES ('HOLIDAY',1);

-- 1variable
CREATE COLUMN TABLE "paramTable1" LIKE "param_model" WITH NO DATA;
--INSERT into "paramTable1" VALUES (1,365,1);
INSERT INTO "paramTable1" VALUES ('HORIZON',30);
INSERT INTO "paramTable1" VALUES ('SMOOTH',1);
INSERT INTO "paramTable1" VALUES ('NUM_VAR',1); 
INSERT INTO "paramTable1" VALUES ('FREQUENCY',365);
INSERT INTO "paramTable1" VALUES ('FREQUENCY_TYPE',1);
INSERT INTO "paramTable1" VALUES ('HOLIDAY',1);

DROP TABLE "paramTable2";
CREATE COLUMN TABLE "paramTable2" LIKE "param_model" WITH NO DATA;
--INSERT into "paramTable" VALUES (1,365,1);
INSERT INTO "paramTable2" VALUES ('HORIZON',30);
INSERT INTO "paramTable2" VALUES ('SMOOTH',1);
INSERT INTO "paramTable2" VALUES ('NUM_VAR',2); 
INSERT INTO "paramTable2" VALUES ('FREQUENCY',365);
INSERT INTO "paramTable2" VALUES ('FREQUENCY_TYPE',1);
INSERT INTO "paramTable2" VALUES ('HOLIDAY',1);

DROP TABLE "paramTable3";
CREATE COLUMN TABLE "paramTable3" LIKE "param_model" WITH NO DATA;
--INSERT into "paramTable" VALUES (1,365,1);
INSERT INTO "paramTable3" VALUES ('HORIZON',30);
INSERT INTO "paramTable3" VALUES ('SMOOTH',1);
INSERT INTO "paramTable3" VALUES ('NUM_VAR',0); 
INSERT INTO "paramTable3" VALUES ('FREQUENCY',365);
INSERT INTO "paramTable3" VALUES ('FREQUENCY_TYPE',2);
INSERT INTO "paramTable3" VALUES ('HOLIDAY',1);

 -- actually in the horizon

DROP TABLE "timeSeriesInput";
CREATE TABLE "timeSeriesInput" as (SELECT DATE AS DATE_ID, TOTAL FROM "HDITTMER"."fa.ppo.build.xs.models::data.milk_data" WHERE REGION = '1' ORDER BY DATE);
--INSERT INTO "timeSeriesInput"

-- forecast forward 30 days
DROP TABLE "timeSeriesInput2";
CREATE TABLE "timeSeriesInput2" as (SELECT * FROM "timeSeriesInput" WHERE DATE_ID < '2015-02-10'); 
--SELECT * FROM "timeSeriesInput2";

DROP TABLE "timeSeriesInput3";
CREATE TABLE "timeSeriesInput3" as (SELECT * FROM "timeSeriesInput2" WHERE DATE_ID != '2015-01-10'); 

DROP TABLE "ILANA"."North_filtered";
CREATE TABLE "ILANA"."North_filtered" as (SELECT "Year" as "YEAR","Month" as "MONTH","Day" as "DAY","Maximum temperature (Degree C)" as "TEMP" from "ILANA"."North_weather");"
ALTER TABLE "ILANA"."North_filtered" ADD (DATE_ID DATE);
UPDATE "ILANA"."North_filtered" SET "DATE_ID" = TO_DATE( "YEAR" || '-' || "MONTH" || '-' || "DAY" , 'YYYY-MM-DD');

DROP TABLE "North_filtered_weather";
CREATE TABLE "North_filtered_weather" as (SELECT DATE_ID,TEMP FROM "ILANA"."North_filtered" ORDER BY DATE_ID);

DROP TABLE "variableMatrix";
CREATE TABLE "variableMatrix" as (SELECT DATE_ID AS DATEI,TEMP as "VAR1",TEMP as "VAR2",TEMP as "VAR3" FROM "ILANA"."North_filtered_weather" ORDER BY DATE_ID);

DROP TABLE "variableMatrix2";
CREATE TABLE "variableMatrix2" as (SELECT * FROM "variableMatrix" WHERE DATEI != '2015-01-10'); 

DROP TABLE "variableMatrix3";
CREATE TABLE "variableMatrix3" as (SELECT * FROM "variableMatrix");
UPSERT "variableMatrix3" VALUES ('2015-01-22',3,4,5) where DATEI='2015-01-22';

DROP TABLE DS1_01;
CREATE TABLE DS1_01 as (SELECT "Date" as DATE_ID, "bits" as TOTAL FROM "DS1_Daily_ISP_Traffic_Bits");

DROP TABLE "DS2";
CREATE TABLE "DS2" as (SELECT "Date" as DATE_ID, "FXRates" as TOTAL FROM "DS2_Daily_FXRates" WHERE "Date" < '1998-01-01' ORDER BY "Date");
DROP TABLE "DS2_01";
CREATE TABLE "DS2_01" as (SELECT * FROM "DS2" ORDER BY DATE_ID);
SELECT * FROM "DS2_01";

DROP TABLE "variableMatrix4";
CREATE TABLE "variableMatrix4" as (SELECT * FROM "variableMatrix2");
UPSERT "variableMatrix4" VALUES ('2015-01-22',3,4,5) where DATEI='2015-01-22';


