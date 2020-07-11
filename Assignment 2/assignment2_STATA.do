/*
Nabib Ahmed
Economics 1016, Summer 2020
Harvard University
nahmed@college.harvard.edu

File: 	ps10_s2020_startercode.do

Description:	STATA code for assignment 2
*/


clear all

* Set Working Directory to the location of the data and qlr.ado:
* Easiest to use drop down menu: file --> change working directory
cd "/Users/nabibahmed/Desktop/ECON1016/Assignment 2"

*Start a log file
capture log close
log using assignment2_log.log, replace

* Import Data
use marcps_w.dta, clear

* Install binscatter command
ssc install binscatter

* Adjusting the variables to reflect work year, not survey year
replace year = year -1

* Limit analysis to age group 21-39
keep if age>=21 & age<=39

* New variable for log(weekly earnings)
generate lnwkwage=log(wsal_val/wkswork)


* Question 1b
* Figure 1
binscatter wkswork year if inrange(year, 86, 96), by(disabl1) linetype(connect) title("Figure 1: Weeks Worked by Disability Status") xtitle("Year") ytitle("Weeks Worked") legend(rows(1) order(1 “Non-disabled” 2 “Disabled”) label(1 "Non-Disabled") label(2 "Disabled")) xline(91)
graph export fig1.png, replace

* Figure 2
binscatter lnwkwage year if inrange(year, 86, 96), by(disabl1) linetype(connect) title("Figure 2: log(Weekly Earnings) by Disability Status") xtitle("Year") ytitle("log(Weekly Earnings)") legend(rows(1) order(1 “Non-disabled” 2 “Disabled”) label(1 "Non-Disabled") label(2 "Disabled")) xline(91)
graph export fig2.png, replace


* Question 2
* Generating the relevant variables
generate post92 = (year >= 92)
generate disabl1_post92 = disabl1*post92

* Regression A
regress wkswork disabl1 post92 disabl1_post92, robust

* Regression B
regress lnwkwage disabl1 post92 disabl1_post92, robust


* Question 3
* Install coefplot
ssc instal coefplot

* Part A
* Generate the relevant variables
generate y88 = (year == 88)
generate y89 = (year == 89)
generate y90 = (year == 90)
generate y91 = (year == 91)
generate y92 = (year == 92)
generate y93 = (year == 93)
generate y94 = (year == 94)
generate y95 = (year == 95)
generate y96 = (year == 96)

generate disabl1_y88 = disabl1*y88
generate disabl1_y89 = disabl1*y89
generate disabl1_y90 = disabl1*y90
generate disabl1_y91 = disabl1*y91
generate disabl1_y92 = disabl1*y92
generate disabl1_y93 = disabl1*y93
generate disabl1_y94 = disabl1*y94
generate disabl1_y95 = disabl1*y95
generate disabl1_y96 = disabl1*y96

regress wkswork disabl1 y88 y89 y90 y91 y92 y93 y94 y95 y96 disabl1_y88 disabl1_y89 disabl1_y90 disabl1_y91 disabl1_y92 disabl1_y93 disabl1_y94 disabl1_y95 disabl1_y96, robust

* Part B
coefplot, recast(connected) ciopts(recast(rline) lpattern(dash)) vertical /// 
keep(disabl1_y*) xline(4.5) yline(0) ///
coeflabel(disabl1_y88 = "1988" disabl1_y89 = "1989" ///
disabl1_y90 = "1990" disabl1_y91 = "1991" disabl1_y92 = "1992" /// 
disabl1_y93 = "1993" disabl1_y94 = "1994" disabl1_y95 = "1995" /// 
disabl1_y96 = "1996") xtitle(Year) ///
ytitle("Coefficient and 95% CI") ///
title("Figure 3: Weeks Worked by Disabled vs. Non-disabled by Year")
graph export "fig3.png", replace


*Close log file
log close

