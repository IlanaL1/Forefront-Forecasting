-- TC01
-- DS01_01
-- No missing values, no frequency,no missing dates (holidays),

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
CALL smoothTimeseries("sortedOutput","paramTable", "smoothedOutput") WITH OVERVIEW;

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




