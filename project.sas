TITLE "Separate date and time";
DATA weather;
length Formatted_Date $30.;
length summary $30.;
INFILE "weather.csv" DELIMITER =',' MISSOVER FIRSTOBS=2 ;
INPUT Formatted_Date Summary $ Precip_type $ Temperature_c Apparent_Temperature__C_ Humidity Wind_Speed_km_h_ Wind_Bearing_degrees_ Visibility_km_ Loud_Cover Pressure__millibars_ ;
Wmonth=substr(Formatted_Date,6,2);
PTN = ( Precip_type = "snow");
B1 = ( Summary = "Breezy");
B2 = ( Summary = "Breezy and Mostly Cloudy");
B3 = ( Summary = "Breezy and Overcast ");
B4 = ( Summary = "Breezy and Partly Cloudy");
B5 = ( Summary = "Clear");
B6 = (Summary = "Drizzle");
B7 = ( Summary = " Foggy");
B8 = (Summary = "Humid and Overcast");
B9 = (Summary = "Humid and Mostly Cloudy");
B10 = ( Summary = "Light Rain");
B11 = (Summary = "Overcast");
B12 = (Summary = "Partly Cloudy");
B13 = (Summary = "Rain");
*PROC PRINT;
RUN;

TITLE "Select only the required months";
PROC SQL;
DELETE FROM Weather
WHERE Wmonth NOT IN ('10','11','12');
QUIT;
*PROC PRINT;
RUN;

TITLE "EXTRACT TIME AND REPLACE MISSING VALUES";
DATA Weather;
SET WEATHER;
FORMAT time_numeric TIME8.;
time_numeric = input(substr(Formatted_Date,12,2) !! ':' !! substr(Formatted_Date,15,2) !! ':' !! substr(Formatted_Date,18,2),TIME8.);
IF Wmonth = '10' THEN PTN = 0;
IF Wmonth = '11' THEN PTN = 0;
IF Wmonth = '12' THEN PTN = 1;
RUN;

TITLE "SPLITing 80-20";
DATA Weather;
SET Weather;
selected = 0;
n=RANUNI(7777); *here for each record we are generating random value which will be printed at the end of teh table;
*PROC SORT DATA = WEATHER;
*BY n;
DATA  weather;
SET Weather nobs=nobs;
IF _n_<=.8*nobs THEN selected=1; 
RUN;

TITLE"Create interaction terms";
data weather;
set weather;
wswbv = Wind_Bearing_degrees_*Wind_Speed_km_h_*Visibility_km_;
hump = Humidity*Pressure__millibars_;
humptime = Humidity*Pressure__millibars_*Time_numeric;
run;

TITLE "creating a dataset calles as weather_training";
proc sql;
create table weather_training as
select * from weather where selected = 1;
quit;
run;


TITLE "Histogram analysis of Temperature_c";
PROC Univariate normal data=weather;
var Temperature_c;
HISTOGRAM/NORMAL (mu = est sigma = est);
RUN;

TITLE "Histogram analysis of Humidity";
PROC Univariate normal data=weather;
var Humidity;
HISTOGRAM/NORMAL (mu = est sigma = est);
RUN;

TITLE "Histogram analysis of Wind_Speed_km_h_";
PROC Univariate normal data=weather;
var Wind_Speed_km_h_;
HISTOGRAM/NORMAL (mu = est sigma = est);
RUN;

TITLE "Histogram analysis of Visibility_km_";
PROC Univariate normal data=weather;
var Visibility_km_;
HISTOGRAM/NORMAL (mu = est sigma = est);
RUN;

TITLE "Histogram analysis of Pressure__millibars_";
PROC Univariate normal data=weather;
var Pressure__millibars_;
HISTOGRAM/NORMAL (mu = est sigma = est);
RUN;

TITLE "Histogram analysis of Time_numeric";
PROC Univariate normal data=weather;
var Time_numeric;
HISTOGRAM/NORMAL (mu = est sigma = est);
RUN;

*In Regression all the assumptions are made for the dependent variabel and hence the distribution of the independent variable is not required;

TITLE "BOXPLOT OF TIME AND TEMPERATURE";
PROC SORT DATA = weather;
BY time_numeric;
RUN;
PROC BOXPLOT;
PLOT Temperature_c*time_numeric;
RUN;


TITLE "BOXPLOT OF MONTH AND TEMPERATURE";
PROC SORT DATA = weather;
BY Wmonth;
RUN;
PROC BOXPLOT;
PLOT Temperature_c*wmonth;
RUN;


TITLE "BOXPLOT OF Pressure AND TEMPERATURE";
PROC SORT DATA = weather;
BY time_numeric;
RUN;
PROC BOXPLOT;
PLOT Pressure__millibars_*time_numeric;
RUN;

TITLE "BOXPLOT OF VISIBILITY AND TEMPERATURE";
PROC SORT DATA = weather;
BY time_numeric;
RUN;
PROC BOXPLOT;
PLOT Visibility_km_*time_numeric;
RUN;

TITLE "BOXPLOT OF Wind Speed AND TEMPERATURE";
PROC SORT DATA = weather;
BY time_numeric;
RUN;
PROC BOXPLOT;
PLOT Wind_Speed_km_h_*time_numeric;
RUN;


TITLE "Distribution of conditions";
proc freq;
tables summary;
run;


*In the above extract during which time the travel is better and also time for commuting;
TITLE "Correlation with dummy variables";
PROC CORR;
VAR Temperature_c Apparent_Temperature__C_ Humidity Wind_Speed_km_h_ B1-B13 PTN Wind_Bearing_degrees_ Visibility_km_ Time_numeric Pressure__millibars_ ;
RUN;

TITLE "Correlation with dummy variables only significant";
PROC CORR;
VAR Temperature_c Apparent_Temperature__C_ Humidity Wind_Speed_km_h_ B5 B6 B10-B12 PTN Wind_Bearing_degrees_ Visibility_km_ Time_numeric Pressure__millibars_ ;
RUN;


TITLE "Correlation";
PROC CORR;
VAR Temperature_c Apparent_Temperature__C_ Humidity Wind_Speed_km_h_ Wind_Bearing_degrees_ Visibility_km_ Time_numeric Pressure__millibars_ ;
RUN;

TITLE "Scatterplots";
PROC sgscatter;
MATRIX Temperature_c Apparent_Temperature__C_ Humidity Wind_Speed_km_h_ Wind_Bearing_degrees_ Visibility_km_ Time_numeric Pressure__millibars_;
RUN;


TITLE "Basic model:1 with Apparent Temperature";
PROC REG DATA=weather_training;
model Temperature_c = Apparent_Temperature__C_ Humidity B1-B13 PTN Wind_Speed_km_h_ Wind_Bearing_degrees_ Visibility_km_ Time_numeric Pressure__millibars_ / stb;
run;
TITLE "Normality plot";
PROC REG DATA=weather_training;
model Temperature_c = Apparent_Temperature__C_ Humidity B1-B13 PTN Wind_Speed_km_h_ Wind_Bearing_degrees_ Visibility_km_ Time_numeric Pressure__millibars_;
plot npp.*student.;
run;


TITLE "Basic model:2 without Apparent Temperature";
PROC REG DATA=weather_training;
model Temperature_c =  Humidity B1-B13 PTN Wind_Speed_km_h_ Wind_Bearing_degrees_ Visibility_km_ Time_numeric Pressure__millibars_/ stb;
run;
TITLE "Normality plot";
PROC REG DATA=weather_training;
model Temperature_c = Humidity B1-B13 PTN Wind_Speed_km_h_ Wind_Bearing_degrees_ Visibility_km_ Time_numeric Pressure__millibars_;
run;

TITLE "Model:3 without insignificant terms";
PROC REG DATA=weather_training;
model Temperature_c = Humidity B1-B13 PTN Wind_Speed_km_h_ Wind_Bearing_degrees_ Visibility_km_ Time_numeric Pressure__millibars_ / selection = stepwise stb sls = 0.05 sle = 0.05;
run;
TITLE "Normality plot for this model";
PROC REG DATA=weather_training;
model Temperature_c = Humidity B1-B13 PTN Wind_Speed_km_h_ Wind_Bearing_degrees_ Visibility_km_ Time_numeric Pressure__millibars_ / selection = stepwise stb sls = 0.05 sle = 0.05;
plot npp.*student.;
plot student.*predicted.;
plot student.*(Humidity B5 B6 B10 B11 PTN Wind_Speed_km_h_ Wind_Bearing_degrees_ Visibility_km_ Time_numeric Pressure__millibars_);
run;

TITLE "Model 4: Apply sqrt transformation";
data weather_training;
set weather_training;
sqrt_temp = sqrt(Temperature_c);
run;
TITLE "Histogram for the sqrt";
PROC Univariate normal data=weather_training;
var sqrt_temp;
HISTOGRAM/NORMAL (mu = est sigma = est);
RUN;
TITLE "sqrt_temp model";
PROC REG DATA=weather_training;
model sqrt_temp = Humidity B1-B13 PTN Wind_Speed_km_h_ Wind_Bearing_degrees_ Visibility_km_ Time_numeric Pressure__millibars_/ selection = stepwise stb sls = 0.05 sle = 0.05;
run;

TITLE "Model 5: Apply 1/4th root transformation";
data weather_training;
set weather_training;
tempBy4 = temperature_C**(1/4);
run;
TITLE "Histogram for the sqrt";
PROC Univariate normal data=weather_training;
var tempBy4;
HISTOGRAM/NORMAL (mu = est sigma = est);
RUN;
TITLE "1/4 model";
PROC REG DATA=weather_training;
model tempBy4 = Humidity B1-B13 PTN Wind_Speed_km_h_ Wind_Bearing_degrees_ Visibility_km_ Time_numeric Pressure__millibars_/ selection = stepwise stb sls = 0.05 sle = 0.05;
run;
proc reg data=weather_training;
model tempBy4 = Humidity B1-B13 PTN Wind_Speed_km_h_ Wind_Bearing_degrees_ Visibility_km_ Time_numeric Pressure__millibars_/ selection = stepwise stb sls = 0.05 sle = 0.05;
plot npp.*student.;
run;

TITLE "Model 6:GLMSELECT";
ods graphics on;
PROC glmselect DATA = weather_training plots =criteria;
model Temperature_c = B6 B9 B10 B12 PTN Wind_Speed_km_h_|Wind_Bearing_degrees_|Visibility_km_ Humidity|Pressure__millibars_|Time_numeric @3/selection=stepwise(stop=CV);
run;
ods graphics off;

ods graphics on;
TITLE "Normality plot after adding interaction terms";
proc reg data=weather_training;
model temperature_c = B6 B10 PTN wswbv hump humptime time_numeric Pressure__millibars_ Wind_Speed_km_h_ Visibility_km_;
plot npp.*student.;
plot student.*predicted.;
plot student.*(B6 B10 PTN wswbv hump humptime time_numeric Pressure__millibars_ Wind_Speed_km_h_ Visibility_km_);
run;
ods graphics off;


TITLE "setting the newtemp for training set";
data weather;
set weather;
if selected then newtemp=temperature_c;
run;


TITLE "Prediction on test set using model 1";
proc reg data = weather;
model newtemp = Humidity B5 B6 B10 B11 PTN Wind_Speed_km_h_ Wind_Bearing_degrees_ Visibility_km_ Time_numeric Pressure__millibars_ ;
output out = outmodel1(where=(newtemp=.)) p=yhat;
run;

TITLE "Prediction on test set using model 2";
proc reg data = weather;
model newtemp = B6 B10 PTN wswbv hump humptime time_numeric Pressure__millibars_ Wind_Speed_km_h_ Visibility_km_;
output out = outmodel2(where=(newtemp=.)) p=yhat;
run;

TITLE "Difference between observed and Predicted for model 1";
data outmodel1_stat;
set outmodel1;
d = Temperature_c - yhat;
absd = abs(d);
run;
proc summary data = outmodel1_stat;
var d absd;
output out=outm1_stats std(d) = rmse mean(absd)=mae;
run;
proc print data = outm1_stats;
TITLE"Stat validation";
run;
proc corr data=outmodel1;
var temperature_c yhat;
run;

TITLE "Difference between observed and Predicted for model 2";
data outmodel2_stat;
set outmodel2;
d = Temperature_c - yhat;
absd = abs(d);
run;
proc summary data = outmodel2_stat;
var d absd;
output out=outm2_stats std(d) = rmse mean(absd)=mae;
run;
proc print data = outm2_stats;
TITLE"Stat validation";
run;
proc corr data=outmodel2;
var temperature_c yhat;
run;


TITLE "Prediction on test set using model 1 including Apparent temperature";
proc reg data = weather;
model newtemp = Apparent_Temperature__C_ Humidity B5 B6 B10 B11 PTN Wind_Speed_km_h_ Wind_Bearing_degrees_ Visibility_km_ Time_numeric Pressure__millibars_ ;
output out = outmodel11(where=(newtemp=.)) p=yhat;
run;

TITLE "Prediction on test set using model 2 including Apparent Temperature";
proc reg data = weather;
model newtemp = B6 B10 PTN wswbv hump humptime time_numeric Pressure__millibars_ Wind_Speed_km_h_ Visibility_km_ Apparent_Temperature__C_;
output out = outmodel21(where=(newtemp=.)) p=yhat;
run;


TITLE "Difference between observed and Predicted for model 1 after adding apparent temperature";
data outmodel11_stat;
set outmodel11;
d = Temperature_c - yhat;
absd = abs(d);
run;
proc summary data = outmodel11_stat;
var d absd;
output out=outm11_stats std(d) = rmse mean(absd)=mae;
run;
proc print data = outm11_stats;
TITLE"Stat validation";
run;
proc corr data=outmodel11;
var temperature_c yhat;
run;

TITLE "Difference between observed and Predicted for model 2 after adding apparent temperature";
data outmodel21_stat;
set outmodel21;
d = Temperature_c - yhat;
absd = abs(d);
run;
proc summary data = outmodel21_stat;
var d absd;
output out=outm21_stats std(d) = rmse mean(absd)=mae;
run;
proc print data = outm21_stats;
TITLE"Stat validation";
run;
proc corr data=outmodel21;
var temperature_c yhat;
run;
