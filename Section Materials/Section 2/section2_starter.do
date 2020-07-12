*Gregory Bruich, Ph.D.
*Economics 1016, Summer 2021
*Harvard University
*Send suggestions and corrections to gbruich@fas.harvard.edu

clear

*Load in data
use cig_taxes.dta, replace

*-------------------------------------------------------------------------------
* Data set up
*-------------------------------------------------------------------------------

*Natural log of packs per capita
gen log_packs_pc = log( pack_sales)

*Indicator variable for Arizona
gen az = 0
replace az = 1 if state == "AZ"


*-------------------------------------------------------------------------------
* Graphical analyes
*-------------------------------------------------------------------------------


*Example code: Binned scatter plots to modify to complete coding exercise

/* Description of the options used in the long command below

			1.  #delimit ; <- The delimit command tells stata that the command continues until the ;

			2. binscatter log_packs_pc year if inrange(year, 1970, 2017), <- first two variables are what gets plotted. if statement restricts the years

			3.    by(az) <- this line plots a different line for each value of az

			4.	linetype(connect) discrete <- these options give a connected plot with a separate point for each year ("discrete")
				
			5.	colors(gs11 blue) <- these are the colors
				
			6.	msymbols(circle_hollow square)  <- these are the style of connectors for the lines
				
			7.	legend(rows(1) order(1 "Rest of U.S." 2 "Arizona") ) <- this is for the legend
				
			8.	xline(1994)  <- this adds a line at 1994
				
			9.	; <- this tells stata that this is the end of the line

			10.	#delimit cr <- this goes back to using a new line to tell stata where a command ends
*/

*Try it in the data for Arizona to see what it prodices
#delimit ;
binscatter log_packs_pc year if inrange(year, 1970, 2017), 
    by(az) 
	linetype(connect) discrete
	colors(gs11 blue) 
	msymbols(circle_hollow square) 
	legend(rows(1) order(1 "Rest of U.S." 2 "Arizona") )
	xline(1994)  
	;
#delimit cr

*Save the graph
graph export arizona.wmf, replace

*Example 2
*Indicator variable for Arizona vs. surrounding states (California, Nevada, Utah, New Mexico, and Colorado)
gen az_neighbors = 0 if inlist(state, "CA", "NV", "UT", "NM", "CO")
replace az_neighbor = 1 if state == "AZ"

*Example Binscatter Command, simply changing by(az) to by(az_neighbors)
#delimit ;
binscatter log_packs_pc year if inrange(year, 1970, 2017), 
    by(az_neighbor) 
	linetype(connect) discrete
	colors(gs11 blue) 
	msymbols(circle_hollow square) 
	legend(rows(1) order(1 "Surrounding States" 2 "Arizona") )
	xline(1994)  
	;
#delimit cr

*Save the graph
graph export arizona_neighbhors.wmf, replace



*-------------------------------------------------------------------------------
* Regression analysis to quantify what we see in the graphs
*-------------------------------------------------------------------------------

*Example for regression part of the coding exercise
*Regression comparing AZ vs. neighbors
*Generate post variable
gen post = 0
replace post = 1 if year >= 1994

*Generate interaction term
gen dd = post*az

*Estimate simple difference in difference regression
regress log_packs_pc dd post az_neighbor if inrange(year, 1993, 1995), robust


*Estimate difference in difference regression extended to multiple control states and many years
regress log_packs_pc dd i.year i.state_fips if inrange(year, 1987, 2000), robust



*-------------------------------------------------------------------------------
* Plot coefficients from non-parametric regression
*-------------------------------------------------------------------------------


*Example to estimate regression and plot coefficients

*** generate year indicator variables and interaction terms to plot
forvalues j = 1988/2000{
	*Generate year indicators
	generate y_`j' = (year == `j')
	
	*Generate interaction terms
	generate az_y_`j' = az * y_`j'
	
}


*** run the regression
regress log_packs_pc az_y_*  y_* i.state_fips if inrange(year, 1987, 2000), vce(cluster state_fips)


*Method 1 to plot coefficients
*** create the coefficient plot
*To install, type "ssc install coefplot" without the quotes
*For instructions, type "help coefplot" without the quotes

#delimit ;
coefplot, 
recast(connected) ciopts(recast(rline) lpattern(dash)) vertical 
keep(az_y_*) xline(4.5) yline(0) 
coeflabel(az_y_1988  = "1988" 
az_y_1989  = "1989" 
az_y_1990  = "1990" 
az_y_1991  = "1991" 
az_y_1992  = "1992" 
az_y_1993 = "1993" 
az_y_1994  = "1994" 
az_y_1995  = "1995" 
az_y_1996  = "1996"  
az_y_1997  = "1997" 
az_y_1998  = "1998" 
az_y_1999  = "1999"  
az_y_2000  = "2000" ) 
xtitle(Year) 
ytitle("Coefficient and 95% CI") 
title("Log Cigarette Consumption in Arizona vs. Rest of U.S.") 
graphregion(color(white)) bgcolor(white)
;
#delimit cr

*Save the graph
graph export "Figure3.png", replace


**********************************

*Method 2 to plot coefficients

*Create file to store coefficients
capture postclose data_for_graph
postfile data_for_graph year beta se using "data_for_graph.dta", replace
post data_for_graph (1987) (0) (0) 

*Run regression
regress log_packs_pc az_y_*  y_* i.state_fips if inrange(year, 1987, 2000), vce(cluster state_fips)

*Store coefficients
forvalues j = 1988/2000{
post data_for_graph (`j') (_b[az_y_`j']) (_se[az_y_`j']) 
}

*Now create a dataset of the coefficients
postclose data_for_graph


*Now load in the coefficients and plot them as usual
preserve

*Load in data with coefficients
use data_for_graph.dta, clear

*Generate 95% Confidence Interval end points
gen ub = beta + 1.96*se
gen lb = beta - 1.96*se

*Draw connected graph with rcap red bars for 95% Confidence Intervals
#delimit ;
twoway 
(rcap ub lb year, lcolor(red))
(connected beta year, mcolor(black) msymbol(circle) lcolor(black)) , 
graphregion(color(white)) bgcolor(white) 
 xline(1993.5) 
 yline(0, lcolor(black)) 
 xlabel(1988(2)2000) 
xtitle(" " "Year") 
ytitle("Coefficient and 95% CI" " " ) 
title("Log Cigarette Consumption in Arizona vs. Rest of U.S." " ")
legend(off)
;
#delimit cr

*Save graphs
graph save figure2.gph, replace 
graph export figure2.png, replace 

restore















