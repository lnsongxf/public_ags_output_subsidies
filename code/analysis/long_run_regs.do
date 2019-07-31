* LOOK AT HOW SELECTION CHANGES OVER COURSE OF 1603 PROGRAM
********************************************************************************
local fname long_run_regs

clear
global repodir = substr("`c(pwd)'",1,length("`c(pwd)'") - (strpos(reverse("`c(pwd)'"),reverse("ags_capital_vs_output"))) + 1)
do "$repodir/code/setup.do"

tempsetup
* this code uses tabout
//ssc install tabout

capture log close
log using "$outdir/logs/`fname'.txt", replace text
********************************************************************************

/* SETUP DATA  */

use $repodir/generated_data/panel_reg_data, clear

keep if year > 2012 // restrict so wind vars etc are same period 
collapse (mean) ptnl_* wind* reg_dummy capacity_factor , by(facilityid)
merge 1:1 facilityid using $repodir/generated_data/static_reg_data, nogen keep(match)
keep if insample
keep if insample_covars
save crossdata, replace 

use crossdata, clear

*DEFINE VARIABLES 

gen awea_pct = awea_geo_MW / first_nameplate_capacity
gen footprintMW = awea_area / awea_geo_MW 
gen footprintTurb = awea_area / awea_geo_turbines

lab var footprintMW "Footprint (SqMile/MW)"
lab var footprintTurb "Footprint (SqMile/Turbine)"

*gen flag_1603_2009 = cond(firstyear == 2009 & flag_1603==1,1,0)
gen flag_1603_2010 = cond(firstyear == 2010 & flag_1603==1,1,0)
gen flag_1603_2011 = cond(firstyear == 2011 & flag_1603==1,1,0)
gen flag_1603_2012 = cond(firstyear == 2012 & flag_1603==1,1,0)

lab var flag_1603_2010 "1603 - Enter 2010"
lab var flag_1603_2011 "1603 - Enter 2011"
lab var flag_1603_2012 "1603 - Enter 2012"

gen log_name = log(first_name)
gen turbinesize = powercurve_max_cap/1000

lab var turbinesize "Turbine Size (MW)"

save regdat, replace 


use regdat, clear
*RESTRICT TO SAMPLE WE ACTUALLY USE 
keep if firstyear > 2004
xi i.state i.iso_rto_code i.entnum i.firstyear, prefix(_D) noomit
* ANCHOR ESTIMATES TO 2005 
drop _Dfirstyear_2005

*FOR FOOTPRINT VARIABLES DROP OBSERVATIONS WE DON'T OBSERVE AREA WELL ON
	* - SINGLE TURBINES 
	* - PLANTS THAT GREW A LOT OVER TIME (AWEA DATABASE CONTAINS MANY MORE TURBINES THAN WE HAVE IN NAMEPLATE)
foreach v of varlist footprint* {
	replace `v' = . if awea_area == 0
	sum `v', detail
	*di `r(p99)'
	replace `v' = . if `v' > `r(p99)' // winsorize some plants with obviously erronous areas
	replace `v' = . if awea_pct > 2 // drop plants were AWEA contains more than 2x initial capacity
}

save tempdat, replace

capture program drop estimateregs
program define estimateregs
	eststo: reg $yvar _Dfirst* flag_1603*, robust
			estadd local yvar "$ylab", replace
			estadd local state "N", replace
			estadd local covars "N", replace
	eststo: reg $yvar _Dfirst* flag_1603* $xvar, robust
			estadd local yvar "$ylab", replace
			estadd local state "N", replace
			estadd local covars "Y", replace
	eststo: reg $yvar _Dfirst* flag_1603* $xvar _Dst* , robust
			estadd local yvar "$ylab", replace
			estadd local state "Y", replace
			estadd local covars "Y", replace
end


eststo clear

global xvar log_name ipp_dummy reg_dummy design_windspeed_eia 

global yvar turbinesize 
global ylab "Turbine Size"
qui {
estimateregs
}

global yvar footprintTurb 
global ylab "Footprint"

qui {
estimateregs
}


esttab, keep(_Dfi* flag_* )	///
	s(yvar covars state r2 N, ///
		label("Outcome" "Controls" "State FE" "R-sq." "N")) ///
	se noconstant label star(* 0.10 ** 0.05 *** 0.01) nomtitles 

*EXPORT TABLE
esttab using "$outdir/tables/input_trend_regressions.tex" , ///
	se noconstant label star(* 0.10 ** 0.05 *** 0.01) replace  ///
	keep(flag_*) nonotes compress booktabs b(a2) ///
	s(yvar covars state r2 N, ///
		label("Outcome" "Controls" "State FE" "R-sq." "N"))  nomtitles 

*EXPORT TABLE FOR PRESENTATION
esttab est1 est2 est4 est5 using "$outdir/tables/input_trend_regressions_prez.tex" , ///
	se noconstant label star(* 0.10 ** 0.05 *** 0.01) replace  ///
	keep(flag_*) nonotes compress booktabs b(a2) ///
	mtitles("Turbine Size" "Turbine Size" "Footprint" "Footprint") nonumbers ///
	s(covars state r2 N, ///
		label("Controls" "State FE" "R-sq." "N"))

		
*CREATE GRAPHS 
use tempdat, clear
local varname turbinesize

collapse (mean) y = `varname' (semean) se_y = `varname', by(firstyear flag_1603)

gen yu = y + 1.96*se_y
gen yl = y - 1.96*se_y	

replace firstyear = firstyear + .2 if flag_1603

twoway (scatter y firstyear if flag_1603 == 0, msymbol(S) msize(medium)) ///
       (rcap yu yl firstyear if flag_1603 == 0) ///
	   (scatter y firstyear if flag_1603 == 1, msymbol(D) msize(medium)) ///
       (rcap yu yl firstyear if flag_1603 == 1, ///
	   legend(order(1 "PTC" 3 "1603")) xlab(2005(1)2012) ///
	   saving(turbsize, replace)  xtitle("")  ytitle("Turbine Size (MW)"))
	   
use tempdat, clear	   
*local varname footprintMW
local varname footprintTurb 

collapse (mean) y = `varname' (semean) se_y = `varname', by(firstyear flag_1603)

gen yu = y + 1.96*se_y
gen yl = y - 1.96*se_y	

replace firstyear = firstyear + .2 if flag_1603

twoway (scatter y firstyear if flag_1603 == 0, msymbol(S) msize(medium)) ///
       (rcap yu yl firstyear if flag_1603 == 0) ///
	   (scatter y firstyear if flag_1603 == 1, msymbol(D) msize(medium)) ///
       (rcap yu yl firstyear if flag_1603 == 1, ///
	   legend(order(1 "PTC" 3 "1603")) xlab(2005(1)2012) ///
	   saving(footprint, replace) xtitle("") ytitle("Sq.Mile / Turbine"))
	
grc1leg  turbsize.gph footprint.gph , /// 
	legendfrom(turbsize.gph)  rows(2) imargin(zero)
	 
graph export "$repodir/output/figures/input_trends.png", replace

********************************************************************************
cap graph close
tempsetup
capture log close
exit
