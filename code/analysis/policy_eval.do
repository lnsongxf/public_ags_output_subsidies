/*******************************************************************************
THIS FILE ESTIMATES LIFETIME PRODUCTION
- PURPOSE IS TO GET DISCOUNTED PROFITS UNDER 1603 AND PREDICT PROFITS UNDER PTC
********************************************************************************/
local fname 1606_policy_eval

clear
global repodir = substr("`c(pwd)'",1,length("`c(pwd)'") - (strpos(reverse("`c(pwd)'"),reverse("ags_capital_vs_output"))) + 1)
do "$repodir/code/setup.do"

tempsetup

capture log close
log using "$outdir/logs/`fname'.txt", replace text
********************************************************************************

/*******************************************************************************
DEFINE SAMPLE AND CLEAN PRICE DATA 
********************************************************************************/
*RESTRICT SAMPLE
use $repodir/generated_data/panel_reg_data, clear
keep if insample_covars 
egen maxyear = max(year), by(facilityid)
keep if maxyear == 2014

*FILL IN MISSING PRICES WITH FACILITY AVERAGE PRICE IF AVAILABLE
****PRICE IS GOING TO BE RETAIL PRICE FOR RETAIL SALES AND RESALE PRICE OTHERWISE 

egen tk = mean(price_resale_EIA), by(facilityid)
replace price_resale_EIA = tk if price_resale_EIA == .

*SOME FIRMS HAVE NON-RESALE SALES. 
**GIVING THEM THE AVERAGE RETAIL PRICE IN STATE
gen frac_resale = salesforresale /  totaldisposition
replace frac_resale = 0 if totaldisp > 0 & totaldisp != . & frac_res == .
replace frac_resale = 1 if frac_resale > 1 & frac_resale != .
capture drop tk 
egen tk = mean(frac_resale), by(facilityid)
replace frac_resale = tk if frac_resale == .
drop tk 

*RETAIL PRICES ARE CENTS PER KWH
replace state_avg_price = state_avg_price*10

gen avg_price = price_resale*frac_resale + (1-frac_resale)* state_avg_price
replace avg_price = state_avg_price if frac_resale==0

gen pmiss = cond(avg_price == . | avg_price == 0,1,0)
egen npmiss = sum(pmiss), by(facilityid)
bys facilityid: gen ni = _N
gen pct_p_missing = npmiss/ni 
drop pmiss npmiss 

*PARSE PPA PRICES
gen price_disp_EIA = avg_price
split pparate, parse(",") gen(tp_)
destring tp_*, force replace
egen pparate_max = rowmax(tp_*)
egen pparate_med = rowmedian(tp_*)
drop tp_*

order facilityid year month avg_price price_resale frac_resale salesforresale totaldis pparate
sort facilityid year month
save tempdat, replace

*GRAPHICALLY COMPARE EIA REVENUE ESTIMATES TO MATCHED PPAS
***MOST ARE ON THE 45 DEGREE LINE, SUGGESTING CONSISTENT. 
use tempdat, clear
collapse (mean) pparate_* price_* capacity_factor frac_resale, by(facilityid year)
gen inppa = cond(pparate_max > 0 & pparate_max !=. , 1,0)
tab inppa

la var price_disp_EIA "Average Price (EIA)"
la var pparate_max "PPA Price" 
twoway scatter price_resale pparate_max if inppa & year == 2014, ///
	ytitle("Average Resale Price")
graph export "$repodir/output/figures/resale_vs_ppa_price.png", replace
graph close

/*******************************************************************************
PREDICT LIFETIME GENERATION
- CARRY 2014 WIND AND PRICE FORWARD (IN REAL TERMS)
- ESTIMATE DECLINE CURVE ON OBSERVED DATA
- THEN CALCULATE LIFETIME PROFITS BY COLLAPSING 
********************************************************************************/
global nyears = 25 // expected lifetime

*EXPAND DATASET OUT TO FULL EXPECTED LIFETIME
clear 
local te = 12 * $nyears
set obs `te'
gen year = ceil(_n/12 -.01)
gen month = _n -(year-1)*12
replace year = year + 2014
gen fper = _n
save tmdat, replace

use tempdat, clear
egen avg_ptnl_cf_adj = mean(ptnl_cf_adj), by(facilityid)
gen td = ptnl_cf_adj - avg_ptnl_cf_adj if year == 2014
sum td, detail

keep if year == 2014
egen maxage = max(age), by(facilityid)
drop year 
joinby month using tmdat
replace age = maxage + fper
drop if age >= 12*$nyears
gen future = 1

append using tempdat
replace future = 0 if future == . 

replace age = age + 1
gen yage = ceil(age/12)
save regdat, replace

**PREDICT FUTURE GENERATION
use regdat, clear
eststo clear

qui{
	gen firstind = cond(age <= 12,1,0)	

	capture drop Fid 
	gen Fid = facilityid
	
	local covars log_nameplate ptnl_cf_adj i.ymdate 
*linear decay
	local tv yage
	eststo: reg capacity_factor `covars' `tv' i.Fid if future == 0

	estimates save "$outdir/estimates/policy_eval_linear", replace

*exponential decay
	predict cf_hat_regcf
	capture drop tk
	eststo: reg log_netgen `covars'  `tv' i.Fid if future == 0
	
	predict tk 
	gen cf_hat_regloggen = exp(tk)/monthcap*100
}
label var yage "Age (years)"
esttab, keep(*age* ) r2  lab

*EXPORT SUBSET FOR PRESENTATION
esttab using "$outdir/tables/output_decay.tex" , ///
	se noconstant nomtitles label star(* 0.10 ** 0.05 *** 0.01) replace  ///
	keep(*age*)  nonotes compress booktabs b(a2) 

		
*TABLE OF THE AVERAGE CAPACITY FACTORS PREDICTED BY AGE
capture drop tk 	
gen tk = capacity_factor if future == 0
egen cf_hat_avg = mean(tk), by(facilityid)
replace cf_hat_avg = capacity_factor if future == 0
drop tk
tabstat cf_hat* , by(yage) stat(mean)

*PUT ALL CAPACITY FACTORS IN PERCENT
foreach v of varlist cf* capacity_factor {
	replace `v' = `v' / 100
	replace `v' = 0 if `v' < 0 // linear spec drives some of these negative
}
save est_gen_data, replace

/*******************************************************************************
*POLICY COUNTERFACTUALS FUNCTION TO:
- PREDICT OUTPUT AND REVENUE EACH MONTH WITH AND WITHOUT PTC
- ESTIMATE PROFITABILITY
********************************************************************************/

*FIRST DEFINE PROGRAM TO DO WORK, THEN SET ASSUMPTION PARAMETERS AND CALL THE PROGRAM
capture program drop getprofits
program define getprofits
	*DEFINE LOCALS USING ARGUMENTS
	local teffect		`1'
	local r_public		`2'
	local r_firm		`3'
	local r_ptc			`4'
	local ptc_deflator	`5'

	use est_gen_data, clear
	sort year month
	order year month *price* pparate *rec*

	*DEFINE PRICE VARIABLES 
	egen p_max = rowmax(price_disp_EIA pparate_max)
	drop if p_max == . 
	egen p_med = rowmax(price_disp_EIA pparate_med)

	*PICK WHICH PRICE TO USE
	gen p = p_med 

	gen p_ptc = cond(yage <=10,`ptc_deflator'*23,0)

	*REC PRICES 
	gen p_rec = cond(rec_price != .,rec_price,0)
	gen p_rec_exp = cond(expected_rec != .,expected_rec,0)
	* IOWA ALSO HAS A $15/mwh state level tax credit
		** https://iub.iowa.gov/renewable-energy-tax-credits **
	replace p_rec = p_rec + 15 if state =="IA" & yage <=10
	replace p_rec_exp = p_rec_exp + 15 if state =="IA" & yage <=10

	*DEFINE CF, Q AND REVENUE VARIABLES (0=1603, 1 = PTC)
	gen cf_1603 = capacity_factor
	replace cf_1603 = cf_hat_regcf if year > 2014 

	gen cf_ptc = cf_1603 
	replace cf_ptc = cf_1603 + `teffect' if yage <=10

	*CONVERT CF TO QUANTITIES
		**MONTHCAP IS TOTAL POSSIBLE HOURS IN MONTH AT FULL CAP
	gen q_1603 = cf_1603 * monthcap
	gen q_ptc = cf_ptc * monthcap

	*FIRM DISCOUNT FACTOR
	gen df = 1/((1+`r_firm')^((age-1)/12)) // monthly
	gen df_ptc = 1/((1+`r_ptc')^((age-1)/12)) // monthly

	*GET DISCOUNTED Q
	gen dq_1603 = q_1603*df
	gen dq_ptc = q_ptc*df

	*GET DISCOUNTED REVENUE BY STREAM
	gen dr_op_1603 = p*q_1603*df
	gen dr_op_ptc = p*q_ptc*df
	gen dr_ptc_pmt = p_ptc*q_ptc*df_ptc
	gen dr_rec_1603 = p_rec*q_1603*df
	gen dr_rec_ptc = p_rec*q_ptc*df
	gen dr_rec_exp_1603 = p_rec_exp*q_1603*df
	gen dr_rec_exp_ptc = p_rec_exp*q_ptc*df

	*DISCOUNTED PUBLIC PTC OUTLAY
	gen pubexp_ptc = q_ptc*(p_ptc/`ptc_deflator')/((1+`r_public')^((age-1)/12)) // monthly


	**https://www.nrel.gov/docs/fy16osti/64281.pdf
	* USING LBNL NUMBERS 
	*2014 O&M baseline estimate of $9/MWh (Wiserand Bolinger 2015)
	gen domcost_avg_1603 = 9*q_1603*df
	gen domcost_avg_ptc = 9*q_ptc*df

	save longdata, replace

	/*******************************************************************************
	*ESTIMATE PROFITABILITY
	********************************************************************************/
	use longdata, clear
	gen p_2014 = p if year == 2014
	collapse (mean) p p_* cf_1603 cf_ptc df (firstnm) initial_cap = nameplate ///
		(sum) dr* domcost* dq_1603 dq_ptc q_1603 q_ptc pubexp_ptc, by(facilityid)

	gen dr_1603_tot = dr_op_1603 + dr_rec_exp_1603
	gen dr_ptc_tot = dr_op_ptc + dr_ptc_pmt + dr_rec_exp_ptc

	gen dp_1603_tot = dr_1603_tot/ q_1603
	gen dp_ptc_tot = dr_ptc_tot/ q_ptc

	*MERGE IN PLANT CHARACTERISTICS
	merge 1:1 facilityid using $repodir/generated_data/static_reg_data, nogen keep(match)

	keep if flag_1603
	di _N

	*GET CAPITAL COSTS FROM 1603 AWARD AMOUNTS
		*amount is in USD
		*so lcoe = $/MWh
	gen investcost= amount_funded/.3 
	*reduce invest cost by 10% for accelerated depreciation
	gen capcost_ptc = investcost*.9
	gen capcost_ptc_mw = capcost_ptc/initial_cap
	gen capcost_1603 = investcost*.9*.7
	gen capcost_1603_mw = capcost_ptc_mw*.7

	gen lcoe_1603 = capcost_1603/q_1603
	gen lcoe_1603_om = (capcost_1603 + domcost_avg_1603)/q_1603

	gen profits_1603 = dr_1603_tot - capcost_1603 - domcost_avg_1603
	gen profitable_1603 = cond(profits_1603 >=0 ,1,0)

	gen profits_ptc = dr_ptc_tot - capcost_ptc - domcost_avg_ptc
	gen profitable_ptc = cond(profits_ptc >=0 ,1,0)

	capture drop sgroup
	gen sgroup = "1603" if profitable_1603 ==1
	replace sgroup = "1603 only" if profitable_1603 ==1 & profitable_ptc == 0
	*replace sgroup = "PTC only" if profitable_1603 ==0 & profitable_ptc == 1
	replace sgroup = "neither" if profitable_1603 ==0 & profitable_ptc == 0

	save profits_by_windfarm, replace
end

*SET GLOBAL ASSUMPTION PARAMETERS
eststo clear
estimates use  "$outdir/estimates/rd_main_spec.ster" //IV estimate
local te_iv = _b[flag_1603]
estimates use  "$outdir/estimates/match_main_spec.ster" //IV estimate
local te_match = _b[flag_1603]
global teffect = -1*(`te_iv' + `te_match')/200

/*******************************************************************************
-MAKE TABLE FOR PAPER
*******************************************************************************/

*arguments: teffect, r_public, r_firm, r_ptc, ptc_deflator
getprofits 	$teffect .03 .05 .08 1		// baseline assumptions
use profits_by_windfarm, clear

*GET COUNTS IN EACH GROUP FOR TEXT
count if sgroup=="" // plants marginal to the PTC in always group
local tv = r(N)
if (`tv'==1) local tv = "one"
else if (`tv'==2) local tv = "two"
else if (`tv'==3) local tv = "three"
else if (`tv'==4) local tv = "four"
else if (`tv'==5) local tv = "five"
else if (`tv'==6) local tv = "six"
else if (`tv'==7) local tv = "seven"
else if (`tv'==8) local tv = "eight"
else if (`tv'==9) local tv = "nine"
file open myfile using "$repodir/output/estimates/ptcmarginal_group_num.tex", write text replace 
	file write myfile "`tv'"
file close myfile

replace sgroup="1603" if sgroup=="" // include plants marginal to the PTC in always group

count if sgroup=="1603"
local tv = r(N)
file open myfile using "$repodir/output/estimates/always_group_num.tex", write text replace 
	file write myfile "`tv'"
file close myfile

summ q_1603 if sgroup=="1603 only"
local tv = round(r(N)*r(mean)/1000000)
disp `tv'
file open myfile using "$repodir/output/estimates/marginal_group_q.tex", write text replace 
	file write myfile "`tv'"
file close myfile

gen never = cond(sgroup == "neither",1,0)
sum never, detail
local tv = round(r(mean)*100)
file open myfile using "$repodir/output/estimates/never_group_pct.tex", write text replace 
	file write myfile "`tv'"
file close myfile

count if sgroup=="1603 only"
local tv = r(N)
if (`tv'==1) local tv = "one"
else if (`tv'==2) local tv = "two"
else if (`tv'==3) local tv = "three"
else if (`tv'==4) local tv = "four"
else if (`tv'==5) local tv = "five"
else if (`tv'==6) local tv = "six"
else if (`tv'==7) local tv = "seven"
else if (`tv'==8) local tv = "eight"
else if (`tv'==9) local tv = "nine"
file open myfile using "$repodir/output/estimates/marginal_group_num.tex", write text replace 
	file write myfile "`tv'"
file close myfile

gen marginal = cond(sgroup == "1603 only",1,0)
sum marginal, detail
local tv = round(r(mean)*100)
if (`tv'==1) local tv = "one"
else if (`tv'==2) local tv = "two"
else if (`tv'==3) local tv = "three"
else if (`tv'==4) local tv = "four"
else if (`tv'==5) local tv = "five"
else if (`tv'==6) local tv = "six"
else if (`tv'==7) local tv = "seven"
else if (`tv'==8) local tv = "eight"
else if (`tv'==9) local tv = "nine"
file open myfile using "$repodir/output/estimates/marginal_group_pct.tex", write text replace 
	file write myfile "`tv'"
file close myfile

*MAKE TABLE
*DEFINE PROGRAM HERE TO USE LATER FOR SENSITIVITY
capture program drop makepolicyevaltable
program define makepolicyevaltable
	local filename `1'
	use profits_by_windfarm, clear
	replace sgroup="1603" if sgroup==""
	capture drop nobs
	gen nobs =1

	*NEED TO CALCULATE AVERAGE PUB EXP HERE (OTHERWISE ITS SIMPLE AVERAGE IN TABLE)
	egen ggen = sum(dq_1603), by(sgroup)
	egen gdol = sum(amount), by(sgroup)
	gen plcoe_1603 = gdol/ggen
	drop ggen gdol
	egen ggen = sum(dq_ptc), by(sgroup)
	egen gdol = sum(pubexp_ptc), by(sgroup)
	gen plcoe_ptc = gdol/ggen
	drop ggen gdol

	*CREATE LABELS THAT MATCH TEXT
	gen slabel  = "Always Profitable"
	replace slabel = "Marginal" if sgroup == "1603 only"
	replace slabel = "Never Profitable" if sgroup == "neither"

	*convert units for table
	foreach v of varlist q_* dq_* amount pubexp {
		replace `v' = `v'/1000000
	}
	tabout slabel using $repodir/output/tables/`filename'.tex, ///
	c(sum nobs  sum q_1603 sum amount_funded mean plcoe_1603 ///
		sum q_ptc sum pubexp_ptc  mean plcoe_ptc) ///
	f(0c 0c 0c 2 0c 0c 2) ///
	rep sum ///
	style(tex) bt ///
	topf($repodir/code/analysis/top.tex) topstr(\textwidth) ptotal(none) total(none) botf($repodir/code/analysis/bot.tex) ///
	h1(nil) ///
	cl2(3-5 6-8) h2( & & \multicolumn{3}{c}{1603} & \multicolumn{3}{c}{PTC} \\ ) ///
	h3(Group & N & Output (MMWh) & Subsidy (\textdollar M) & Subsidy (\textdollar/MWh) ///
		& Output (MMWh) & Subsidy (\textdollar M) & Subsidy (\textdollar/MWh) \\ )
end

*CALL FUNCTION TO EXPORT TABLE
makepolicyevaltable policyevaltab

collapse (sum) nobs (sum) q_1603 (sum) dq_1603 (sum) amount_funded (mean) plcoe_1603 (sum) q_ptc (sum) dq_ptc (sum) pubexp_ptc (mean) plcoe_ptc, by(sgroup)
save tempdat, replace

*CALCULATE SUMMARY MEASURES FOR DRAFT TEXT
use tempdat, clear
gen assumption = "Marginal"

foreach v of varlist q_ptc dq_ptc pubexp_ptc {
	replace `v' = . if sgroup == "1603 only"
	replace `v' = . if sgroup == "neither"
}
save sdat, replace

use tempdat, clear
gen assumption = "Always"

foreach v of varlist q_ptc dq_ptc pubexp_ptc {
	replace `v' = . if sgroup == "1603 only"
}
append using sdat

collapse (sum) q_* dq_* amount pubexp, by(assumption)

gen Q_diff = q_1603 - q_ptc
gen Q_diff_pct = Q_diff/ q_ptc

gen C_diff = amount - pubexp_ptc
gen C_diff_pct = C_diff/ pubexp_ptc

gen lcoe_1603 = amount/dq_1603
gen lcoe_ptc = pubexp/dq_ptc

gen lcoe_diff = lcoe_1603 - lcoe_ptc
gen lcoe_diff_pct = lcoe_diff/lcoe_ptc

replace Q_diff = round(Q_diff)
replace C_diff = round(C_diff)

replace Q_diff_pct = round(Q_diff_pct*100)
replace C_diff_pct = round(C_diff_pct*100)

replace lcoe_diff = round(lcoe_diff, .01)
replace lcoe_1603 = round(lcoe_1603, .01)
replace lcoe_ptc = round(lcoe_ptc, .01)

replace lcoe_diff_pct = round(lcoe_diff_pct*100)

save policy_sums, replace

*EXPORT TEX FILES WITH NUMBERS FOR DRAFT
use policy_sums, clear
sort assumption
assert Q_diff[1] < 0

foreach v of varlist Q_* {
	replace `v' = -`v' if _n == 1
}

*VARS WITH NO DECIMALS IN TEXT
foreach v of varlist Q_diff* C_diff* lcoe_diff_pct {
	local tlab = "Always"
	local tv = trim("`: display %10.0fc `v'[1]'") // FC ADDS COMMAS
	di `tv'

	di "`v' - `tlab'  =   `tv'"

	file open myfile using "$repodir/output/estimates/pe_`tlab'_`v'.tex", write text replace 
	file write myfile "`tv'"
	file close myfile

	local tlab = "Marginal"
	local tv = trim("`: display %10.0fc `v'[2]'")
	di `tv'
	di "`v' - `tlab'  =   `tv'"

	file open myfile using "$repodir/output/estimates/pe_`tlab'_`v'.tex", write text replace 
	file write myfile "`tv'"
	file close myfile
}

*VARS WITH 2 DECIMALS IN TEXT
foreach v of varlist lcoe_diff lcoe_1603 lcoe_ptc {
	local tlab = "Always"
	local tv = trim("`: display %10.2fc `v'[1]'") // FC ADDS COMMAS
	di `tv'

	di "`v' - `tlab'  =   `tv'"

	file open myfile using "$repodir/output/estimates/pe_`tlab'_`v'.tex", write text replace 
	file write myfile "`tv'"
	file close myfile

	local tlab = "Marginal"
	local tv = trim("`: display %10.2fc `v'[2]'")
	di `tv'
	di "`v' - `tlab'  =   `tv'"

	file open myfile using "$repodir/output/estimates/pe_`tlab'_`v'.tex", write text replace 
	file write myfile "`tv'"
	file close myfile
}


/*******************************************************************************
-MAKE TABLES FOR SENSITIVITY
*******************************************************************************/
* getprofits arguments: teffect, r_public, r_firm, r_ptc, ptc_deflator

* baseline assumptions
*qui: getprofits 	$teffect .03 .05 .08  1

* peak tax equity rate
qui: getprofits 	$teffect .03 .05 .105 1
qui: makepolicyevaltable policyevaltab_rptchigh // argument is filename for output

* scale nominal ptc value by 0.85 per johnston (2019), use 0.05 to discount ptc
qui: getprofits 	$teffect .03 .05 .105 0.85
tab sgroup, miss
qui: makepolicyevaltable policyevaltab_ptcdeflated // argument is filename for output

********************************************************************************
tempsetup
capture log close
exit
