/*
Gregory Bruich and James Reisinger
Economics 1123, Spring 2020
Harvard University

Send corrections and suggestions to gbruich@fas.harvard.edu

File: 	ps10_s2020_startercode.do

Description:

The following program reads in the data set, sets the variable (time) that denotes
the calendar time period, describes the data in memory, does some transformations, 
produces some graphs, estimates a few models, computes the BIC, calculates and uses
the QLR test to test for a break in the model. 

We give examples of loops to calculate the BIC for various lag lengths in AR(p),
ADL(p,q), and ADL(p,q1,q2) models. 

It also shows how to obtain one step ahead predictions.

Finally, it shows how to calcuate POOS and GARCH RMSPEs.

Inputs:  ps10_data.dta (on Canvas)
		 qlr.ado (on Canvas)
		 outreg2 (ssc install outreg2)
		
Save the data and qlr.ado into the same folder on your computer
		
Outputs: d_unrate_tsline.png
		 d_ln_unrate.png
		 qlr_ar4.png
		 forecasts.png
		 table1.xls
*/


clear all

* Set Working Directory to the location of the data and qlr.ado:
* Easiest to use drop down menu: file --> change working directory
cd "~\Dropbox\1123\ps_2020\ps10"

*Start a log file
capture log close
log using ps10.log, replace

* Import Data
use ps10_data.dta, clear
tsset time // tells Stata the time variable to enable time series operators

*Describe and summarize the data
describe
sum 

*List observations for March and April 2020
list if tin(2020m3,2020m4), noobs

* Create variables
gen d_ln_unrate = 100 * (ln(unrate) - ln(L.unrate))
label var d_ln_unrate "100*change in log unrate"

gen d_unrate = D.unrate 
label var d_unrate "Percentage point change in unrate"

gen baa_gs10 = baa-gs10
label var baa_gs10 "BAA bond to 10 year treasury spread"

gen dlhoust = 100*(log(houst) - log(L.houst))
label var dlhoust "100*change in log Housing Starts"

*Describe and summarize the data
describe unrate d_ln_unrate d_unrate baa_gs10 dlhoust
sum unrate d_ln_unrate d_unrate baa_gs10 dlhoust


* graph
tsline d_unrate
graph export d_unrate_tsline.png, replace

tsline d_ln_unrate
graph export d_ln_unrate.png, replace

*Estimate AR(1) in changes 
reg d_unrate L.d_unrate if tin(1962m1,2020m3), r
dis "BIC = " ln(e(rss)/e(N)) + e(df_m)*ln(e(N))/e(N)
dis "AIC = " ln(e(rss)/e(N)) + e(df_m)*2/e(N)
dis "Adjusted Rsquared = " e(r2_a)

*Estimate AR(1) in 100* log changes 
reg d_unrate L.d_ln_unrate if tin(1962m1,2020m3), r
dis "BIC = " ln(e(rss)/e(N)) + e(df_m)*ln(e(N))/e(N)
dis "AIC = " ln(e(rss)/e(N)) + e(df_m)*2/e(N)
dis "Adjusted Rsquared = " e(r2_a)

*Estimate AR(6) in changes 
reg d_unrate L(1/6).d_unrate if tin(1962m1,2020m3), r
dis "BIC = " ln(e(rss)/e(N)) + e(df_m)*ln(e(N))/e(N)
dis "AIC = " ln(e(rss)/e(N)) + e(df_m)*2/e(N)
dis "Adjusted Rsquared = " e(r2_a)

*Estimate AR(6) in 100* log changes
reg d_unrate L(1/6).d_ln_unrate if tin(1962m1,2020m3), r
dis "BIC = " ln(e(rss)/e(N)) + e(df_m)*ln(e(N))/e(N)
dis "AIC = " ln(e(rss)/e(N)) + e(df_m)*2/e(N)
dis "Adjusted Rsquared = " e(r2_a)

* Basics of choosing lag length for AR(p)
local bicmin= 1e+9
forvalues p = 1/6 {

	reg d_unrate L(1/`p').d_unrate if tin(1962m1,2020m3), r
	
	* BIC and adjusted Rsquared
	scalar bic = ln(e(rss)/e(N)) + e(df_m)*ln(e(N))/e(N)
	scalar ar2 = e(r2_a)
	dis "AR(`p’) BIC =" bic
	dis "AR(`p’) Adjusted Rsquared= " e(r2_a)
	dis "AR(`p’) Rsquared= " e(r2)
	
	if bic < `bicmin' {
		local bicmin = bic
		local phat = `p'
	}
	
	
}
dis "BIC= " `bicmin'
dis "AR BIC lengths: p = " `phat'


* Basics of choosing lag length for ADL(p,q) 
local bicmin= 1e+9
forvalues p = 1/6 {
	forvalues q = 1/6 {

		reg d_unrate L(1/`p').d_unrate L(1/`q').baa_gs10 if tin(1962m1,2020m3), r
		
		* BIC and adjusted Rsquared
		scalar bic = ln(e(rss)/e(N)) + e(df_m)*ln(e(N))/e(N)
		scalar ar2 = e(r2_a)
		dis "AR(`p’) BIC =" bic
		dis "AR(`p’) Adjusted Rsquared= " e(r2_a)
		dis "AR(`p’) Rsquared= " e(r2)
		
		if bic < `bicmin' {
			local bicmin = bic
			local phat = `p'
			local qhat = `q'
		}
	}
	
}

dis "BIC= " `bicmin'
dis "ADL BIC lengths: p = " `phat' " q = " `qhat'

* Basics of choosing lag length for ADL(p,q1,q2) 
*Try with change in spread and housing starts
set more off

local bicmin = 1e+9

forvalues p = 1/8 {
forvalues q1 = 1/8 {
forvalues q2 = 0/8 {
qui reg d_unrate L(1/`p').d_unrate L(1/`q1').baa_gs10 L(0/`q2').dlhoust if tin(1978m1,2019m4), r
sca bic = ln(e(rss)/e(N)) + e(df_m)*ln(e(N))/e(N)
dis "ADL(`p',`q1',`q2') BIC = " bic
dis "ADL(`p',`q1',`q2') Adjusted Rsquared = " e(r2_a)

if bic < `bicmin' {
local bicmin = bic
local qhat = `q1'
local phat = `p'
local q2hat = `q2'
}
}
}
}


dis "BIC= " `bicmin'
dis "BIC lengths: p = " `phat' " q_baa_gs10 = " `qhat' " q_dlhoust = " `q2hat' 


* Generate predictions 
reg d_unrate L.d_unrate if tin(1962m1,2020m3), r
predict yhat_ar1_6220

reg d_unrate L(1/4).d_unrate if tin(1962m1,2020m3), r
predict yhat_ar4_6220

reg d_unrate L.d_unrate L.baa_gs10 if tin(1962m1,2020m3), r
predict yhat_adl11_6220

reg d_unrate L(1/4).d_unrate L(1/2).baa_gs10 if tin(1962m1,2020m3), r
predict yhat_adl42_6220

* Compare predictions from AR1 and AR4 to actual
list time d_unrate yhat_ar1_6220 yhat_ar4_6220 yhat_adl11_6220 yhat_adl42_6220 if tin(2020m2, 2020m4)

* Convert predictions from first difference to levels
* This will be different if you are working with logs!
foreach var in ar1 ar4 adl11 adl42 {
	gen unrate_`var'_6220 = L.unrate + yhat_`var'_6220
}

/*
*Example of how to do it if you are working in logs
foreach var in ar1 ar4 adl11 adl42{
gen demp_`var'_6220 = L.unrate *(yhat_`var'_6220/100)
}
*/


list time unrate unrate_ar1_6220 unrate_ar4_6220 unrate_adl11_6220 unrate_adl42_6220 if tin(2020m2, 2020m4)

*Plot forecasts of the unemployment rates
#delimit ;
twoway (tsline unrate  if tin(2018m1,2020m4), lwidth(thick) lcolor(red%50)) 
(tsline unrate_ar1_6220 if tin(2018m1,2020m4), lcolor(navy)) 
(tsline unrate_adl42_6220 if tin(2018m1,2020m4), lcolor(blue) lwidth(thick) lpattern(dash)) , 
 title("Forecasts from Models", color(black) size(medsmall)) 
 legend(order(1 "Actual" 2 "AR(1)" 3 "ADL(4,2)") rows(1) size(small)) 
 ytitle("Unemployment Rate", margin(medlarge)) xtitle("Year and month") 
 graphregion(color(white)) bgcolor(white) xlabel(, grid)  ; 
 #delimit cr
 
graph export forecasts.png, replace

* QLR test for AR(4)  
* mlabpos(#) moves the label on the graph, numbers are 1-12 like hands on a clock
* Note: the QLR test takes a long time to run because you are running a separate regression for every date
qlr d_unrate L(1/4).d_unrate if tin(1962m1,2020m1), graph type(png) mlabpos(10)
display "break date = " r(maxdate) 
display "QLR statistic = " r(qlr) 
display "Number of restrictions = " r(restrict)

graph export qlr_ar4.png, replace

*Save QLR statistics as locals to export into a table
local breakdate =  r(maxdate) 
local qlr =  r(qlr) 
local restrictions =  r(restrict)

* POOS estimate of root mean squared prediction error for AR(4)
reg d_unrate L(1/4).d_unrate if tin(1962m1,2014m12), r
predict yhat_ar4_6214

*Calculate the root mean squared prediction error via POOS
*Prediction
gen unrate_yhat_ar4_6214 = L.unrate + yhat_ar4_6214

*Squared Error
gen error2_ar4_6214 = (unrate - unrate_yhat_ar4_6214)^2

*Mean error over POOS period
sum error2_ar4_6214 if tin(2015m1,2020m3)

*Save square root of mean error
scalar rmspe_ar4_6214 = sqrt(r(mean))
disp rmspe_ar4_6214

* GARCH(1,1) estimate of root mean squared prediction error for AR(4)
arch  d_unrate L(1/4).d_unrate if tin(1962m1,2020m3), arch(1) garch(1)
predict cv6220, variance
sum cv6220 if tin(2020m4,2020m4)
local garch6220_rmsfe = sqrt(r(mean))

*Note that using 100*log changes for unrate would require converting garch6220_rmsfe back to levels

*Store the forecast in levels for 2020m4 from the AR(4)
sum unrate_ar4_6220 if tin(2020m4,2020m4)
local forecast = r(mean)

*Store the forecast in changes for 2020m4 from the AR(4)
sum yhat_ar4_6220 if tin(2020m4,2020m4)
local forecastchanges = r(mean)

*Store the BIC, AIC, and RMSE from the AR(4)
reg d_unrate L(1/4).d_unrate if tin(1962m1,2020m3), r
local BIC = ln(e(rss)/e(N)) + e(df_m)*ln(e(N))/e(N)
local AIC = ln(e(rss)/e(N)) + e(df_m)*2/e(N)
local rmse = e(rmse)

*Output all the results to a table
outreg2 using table1.xls, replace addstat(qlr, `qlr', qlr rest, `restrictions', BIC,  `BIC', AIC, `AIC', Adj R2, `e(r2_a)', RMSE, `rmse', POOS RMSE, rmspe_ar4_6214, GARCH RMSE, `garch6220_rmsfe', forecast levels, `forecast', forecast change, `forecastchanges') addtext(Estimation, 1962m1-2020m3, breakdate, `breakdate')

*Close log file
log close

