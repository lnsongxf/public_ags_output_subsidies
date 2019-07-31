* ESTIMATE IMPACT OF NEGATIVE PRICES ON 1603 PLANTS
********************************************************************************
local fname plants_negative_prices

clear
global repodir = substr("`c(pwd)'",1,length("`c(pwd)'") - (strpos(reverse("`c(pwd)'"),reverse("ags_capital_vs_output"))) + 1)
do "$repodir/code/setup.do"

tempsetup

capture log close
log using "$outdir/logs/`fname'.txt", replace text
********************************************************************************

use $repodir/generated_data/panel_reg_data, clear
keep if insample_covars

clonevar date = ymdate
xi i.state i.windclass_eia i.date i.nercnum i.off_cat_num ///
	i.ott i.iso_rto_code i.entnum, prefix(_D) noomit
drop  _Ddate_659 // drop same period in each reg
drop if age == 0 // output could be for partial month at time 0
replace log_netgen = log(netgen + 1)
gen log_ptnl_output_adj = log(ptnl_output_adj)
lab var log_ptnl_output_adj "log(Potential Output)"
lab var ptnl_cf_adj "Potential Capacity Factor"
lab var design_windspeed_eia "Design Wind Speed"

drop if year < 2011
keep if flag_in_lmp == 1 & in_iso_rto== 1

save regdat, replace

use regdat, clear
xtset facilityid ymdate

gen frac0_1603 = flag_1603 * frac0

label var frac0	"Share $<$ 0"
label var frac0_1603	"Share $<$ 0 - 1603 Grant"

eststo clear

sum capacity_factor if flag_1603 == 1
local tv = trim("`: display %10.2fc `r(mean)''")
di `tv'

qui: eststo: xtreg capacity_factor  _Dd* ptnl_cf_adj windvar /// 
	frac0 frac0_1603, fe cluster(facilityid)
estadd local my = `tv'

esttab, keep(frac0 frac0_1603 ptnl_cf_adj windvar) /// 
		order(frac0 frac0_1603 ptnl_cf_adj windvar)	///
		s(my r2_a N, ///
		label("Mean(Y) - 1603 Grant" "R-sq." "N")) ///
		se noconstant nonumbers label star(* 0.10 ** 0.05 *** 0.01)

*EXPORT TABLE
esttab using "$outdir/tables/frac0_1603reg.tex", replace ///
		keep(frac0 frac0_1603) /// 
		order(frac0 frac0_1603)	///
		s(my r2_a N, ///
		label("Mean(Y) - 1603 Grant" "R-sq." "N")) ///
		nonotes compress booktabs b(a2) label se nonumbers
		

********************************************************************************
* CREATE A TABLE WITH NEGATIVE PRICE CORRECTIONS 
************************************************************
		
eststo clear

*IV estimate
estimates use  "$outdir/estimates/rd_main_spec.ster" 
	eststo
	estadd local regtype "IV", replace
	estadd local negative "No", replace
	
estimates use  "$outdir/estimates/rd_robust_fracLMP" 
	eststo
	estadd local regtype "IV", replace
	estadd local negative "Yes", replace

*Matching estimate
estimates use  "$outdir/estimates/match_main_spec.ster" 
	eststo
	estadd local regtype "Match", replace
	estadd local negative "No", replace
	
estimates use  "$outdir/estimates/match_robust_fracLMP.ster" 
	eststo
	estadd local regtype "Match", replace
	estadd local negative "Yes", replace

esttab , ///
	se noconstant label star(* 0.10 ** 0.05 *** 0.01) ///
	keep(*1603) ///
	s(regtype negative N, ///
		label("Estimator" "Negative Price Adjustment" "N")) ///
	nomtitles

esttab using "$outdir/tables/regs_negative_prices.tex", ///
	se noconstant label star(* 0.10 ** 0.05 *** 0.01) replace  ///
	keep(*1603) ///
	s(regtype negative N, ///
		label("Estimator" "Negative Price Adjustment" "N")) ///
	nomtitles booktabs nonotes
	
********************************************************************************
tempsetup
capture log close
exit
	
