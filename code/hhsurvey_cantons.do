****************************************** 
*** HH Income & Inequality: Stata Code ***
******************************************

***********
*** README: 
***********

/*
README: 
This file uses household survey data from El Salvador 2008-2016 to compare worker
earnings and earning inequality across cantons with a higher vs. lower exposure to
the 1980 land reform.

To run the file:
(i) Set the appropriate workspace
(ii) Access the required HH Survey waves, all available at 
http://digestyc.microdatahub.com/, and place them in the appropriate data folder 
location
(iii) Ensure all commands and packages referenced in the next section are installed 
(iv) Run the do file.
*/


********************************************
*** PREP: Workspace, Settings & Requirements 
********************************************

capture log close
version 17
clear
set matsize 3000
set more off
set scheme s2color
cap graph set window fontface "Palatino"

** Set Workspace **
cd /Users/`c(username)'/Dropbox/Research_ElSalvador_LandReform/HHSurvey_Inequality/

** ssc install shp2dta; estout; cmogram; dm88_1; grqreg; gr0002_3; binscatter; distinct


************************************
*** PREP: Load and Append HH Surveys
************************************

** Open all the hh survey waves available online, define key variables, and append them together:
* All data from http://digestyc.microdatahub.com/

global vars_to_keep COD_CTO dept_code INGRE R106 R104 survey_year

* 2008:
use "./data/SLV_2008_EHPM_v01_M_STATA8/EPHM_2008.dta", clear
gen dept_code = string(R004,"%02.0f")
gen R005 = MUNIC_AUTO
gen muni_code = string(R005,"%02.0f")
gen canton_code = string(R006,"%02.0f")
gen COD_CTO = dept_code + muni_code + canton_code
gen survey_year=2008
keep $vars_to_keep
tempfile ehpm_2008
save `ehpm_2008'

* 2009:
use "./data/SLV_2009_EHPM_v01_M_STATA8/EHPM2009.dta", clear
gen dept_code = string(R004,"%02.0f")
gen muni_code = string(R005,"%02.0f")
gen canton_code = string(R006,"%02.0f")
gen COD_CTO = dept_code + muni_code + canton_code
gen survey_year=2009
keep $vars_to_keep
tempfile ehpm_2009
save `ehpm_2009'

* 2010:
use "./data/SLV_2010_EHPM_v01_M_STATA8/EHPM_2010.dta", clear
gen dept_code = string(R004,"%02.0f")
gen muni_code = string(R005,"%02.0f")
gen canton_code = string(R006,"%02.0f")
gen COD_CTO = dept_code + muni_code + canton_code
gen survey_year=2010
keep $vars_to_keep
tempfile ehpm_2010
save `ehpm_2010'

* 2011:
use "./data/SLV_2011_EHPM_v01_M_STATA8/EHPM_2011.dta", clear
gen dept_code = string(R004,"%02.0f")
gen muni_code = string(R005,"%02.0f")
gen canton_code = string(R006,"%02.0f")
gen COD_CTO = dept_code + muni_code + canton_code
gen survey_year=2011
gen NIVAPROB=R217A 
keep $vars_to_keep
tempfile ehpm_2011
save `ehpm_2011'

* 2012:
use "./data/SLV_2012_EHPM_v01_M_STATA8/EHPM_2012.dta", clear
gen dept_code = string(R004,"%02.0f")
gen muni_code = string(R005,"%02.0f")
gen canton_code = string(R006,"%02.0f")
gen COD_CTO = dept_code + muni_code + canton_code
gen survey_year=2012
keep $vars_to_keep
tempfile ehpm_2012
save `ehpm_2012'

* 2013:
use "./data/SLV_2013_EHPM_v01_M_STATA8/EHPM_2013.dta", clear
gen dept_code = string(R004,"%02.0f")
gen muni_code = string(R005,"%02.0f")
gen canton_code = string(R006,"%02.0f")
gen COD_CTO = dept_code + muni_code + canton_code
gen survey_year=2013
keep $vars_to_keep
tempfile ehpm_2013
save `ehpm_2013'

* 2014:
use "./data/SLV_2014_EHPM_v01_M_STATA8/EHPM_2014.dta", clear
gen dept_code = string(R004,"%02.0f")
gen muni_code = string(R005,"%02.0f")
gen canton_code = string(R006,"%02.0f")
gen COD_CTO = dept_code + muni_code + canton_code
gen survey_year=2014
keep $vars_to_keep
tempfile ehpm_2014
save `ehpm_2014'

* 2015:
use "./data/SLV_2015_EHPM_v01_M_STATA8/EHPM_2015.dta", clear
rename *, upper
gen dept_code = string(R004,"%02.0f")
gen muni_code = string(R005,"%02.0f")
gen canton_code = string(R006,"%02.0f")
gen COD_CTO = string(R006,"%06.0f")  
gen survey_year=2015
keep $vars_to_keep
tempfile ehpm_2015
save `ehpm_2015'



* 2016:
use "./data/SLV_2016_EHPM_v01_M_STATA8/EHPM_2016.dta", clear
rename *, upper
gen dept_code = string(R004,"%02.0f")
gen muni_code = string(R005,"%02.0f")
gen canton_code = string(R006,"%02.0f")
gen COD_CTO = string(R006,"%06.0f")  
gen survey_year=2016
keep $vars_to_keep
tempfile ehpm_2016
save `ehpm_2016'



** Append:
use `ehpm_2008'
append using `ehpm_2009'
append using `ehpm_2010'
append using `ehpm_2011'
append using `ehpm_2012'
append using `ehpm_2013'
append using `ehpm_2014'
append using `ehpm_2015'
append using `ehpm_2016'
tempfile ehpm
save `ehpm'

****************************************************************
*** PREP: Load Land Reform Property and El Salvador Cantons Data
****************************************************************


** Load Reform Property Data & Collapse to Canton Level:
preserve
use "./data/prop_data_wcodigos.dta", clear
keep if !missing(CODIGO)
bys CODIGO: egen num_prop_canton = count(own_amt)
bys CODIGO: egen prop_above_canton = count(own_amt) if own_amt>500
replace prop_above_canton=0 if missing(prop_above_canton)
bys CODIGO: egen prop_below_canton = count(own_amt) if own_amt<500
replace prop_below_canton=0 if missing(prop_below_canton)
bys CODIGO: egen area_above_canton = sum(area) if own_amt>500
replace area_above_canton=0 if missing(area_above_canton)
bys CODIGO: egen area_below_canton = count(area) if own_amt<500
replace area_below_canton=0 if missing(area_below_canton)
gen share_own_above500 = prop_above_canton/(prop_above_canton+prop_below_canton)
gen share_above500 = area_above_canton/(area_above_canton+area_below_canton)
bys CODIGO: egen area_above_canton_w300 = sum(area) if own_amt>500  & own_amt<800
replace area_above_canton_w300=0 if missing(area_above_canton_w300)
bys CODIGO: egen area_below_canton_w300 = count(area) if own_amt<500 & own_amt>200
replace area_below_canton_w300=0 if missing(area_below_canton_w300)
gen share_above500_w300 = area_above_canton_w300/(area_above_canton_w300+area_below_canton_w300)
bys CODIGO: egen max_holdings = max(own_amt)
bys CODIGO: egen mean_holdings = mean(area)
collapse (mean) share_above500 share_above500_w300 num_prop_canton max_holdings mean_holdings, by(CODIGO)
gen COD_CTO = string(CODIGO, "%06.0f")
tempfile prop_data
save `prop_data'
restore


*******************************************************
*** MERGE: Merge HH Survey data with reform canton info
*******************************************************

* EHPM:
use `ehpm', clear
merge m:1 COD_CTO using `prop_data', gen(merge_cantons)
keep if merge_cantons==3

* Merge Canton-Level Covs:
preserve
shp2dta using "./data/cantons_wCodigos.shp", data(./data/cantons_wCodigos) coord(./data/cantons_coords) replace
use "./data/cantons_wCodigos.dta", clear
duplicates drop CODIGO, force 
tempfile cantons_data
save `cantons_data'
restore
merge m:1 CODIGO using  `cantons_data', gen(merge_canton_covs)
preserve
use "./data/cantons_wGeoCovariates.dta", clear
duplicates drop CODIGO, force  
tempfile cantons_geodata
save `cantons_geodata'
restore
merge m:1 CODIGO using `cantons_geodata', gen(merge_canton_geocovs)

***************************************
*** PREP: Define Canton-Level Variables
***************************************

*Real earnings measure
* Deflated using World Bank consumer price data from El Salvador: https://data.worldbank.org/indicator/FP.CPI.TOTL?locations=US%E2%89%A4/SEURLD-SV
gen hh_income_pc = INGRE
rename survey_year year
gen hh_inc_pc_real = (year==2000)*hh_income_pc*71.57/100 + ///
                 (year==2001)*hh_income_pc*74.25/100 + (year==2004)*hh_income_pc*80.68/100 + ///
                 (year==2005)*hh_income_pc*84.47/100 + (year==2006)*hh_income_pc*87.88/100 + ///
                 (year==2007)*hh_income_pc*91.90/100 + (year==2008)*hh_income_pc*98.06/100 + ///
                 (year==2009)*hh_income_pc*99.10/100 + (year==2011)*hh_income_pc*105.13/100 + ///
                 (year==2012)*hh_income_pc*106.95/100 + (year==2013)*hh_income_pc*107.79/100  + ///
				 (year==2010)*hh_income_pc + (year==2014)*hh_income_pc*109/100 + ///
				 (year==2015)*hh_income_pc*108.2/100 + (year==2016)*hh_income_pc*108.8/100
gen ln_hh_inc_pc_real = log(hh_inc_pc_real)

rename R106 age
rename R104 sex
gen age2=age*age
destring dept_code, replace
drop if age > 97

** Keep Only Employed Workers of Adult Age:
keep if age >= 18 
keep if !missing(INGRE) & INGRE>0
label var share_above500 "Share Above 500 ha"
label var share_above500_w300 "Share Above 500 ha"

******************************************************************************
*** ANALYSIS: Relationship b/w Income, Inequality, and Phase I of Land Reform:
******************************************************************************

** All Properties:

preserve
reg hh_inc_pc_real share_above500  i.year, cluster(NOM_MUN)
		qui sum hh_inc_pc_real if e(sample)
		qui estadd scalar ymean = r(mean), replace
		qui estadd scalar ar2 = e(r2_a), replace
		qui estadd scalar n_clust = e(N_clust), replace
		qui local sd1 = `r(sd)'
		qui sum share_above500 if e(sample)
		qui estadd scalar beta = e(b)[1,1]*`r(sd)'/`sd1'
		qui distinct COD_CTO if e(sample)
		qui estadd scalar n_cantons = `r(ndistinct)'
		qui eststo a1
		
reg hh_inc_pc_real share_above500  i.year i.dept_code, cluster(NOM_MUN)
		qui sum hh_inc_pc_real if e(sample)
		qui estadd scalar ymean = r(mean), replace
		qui estadd scalar ar2 = e(r2_a), replace
		qui estadd scalar n_clust = e(N_clust), replace
		qui local sd1 = `r(sd)'
		qui sum share_above500 if e(sample)
		qui estadd scalar beta = e(b)[1,1]*`r(sd)'/`sd1'
		qui distinct COD_CTO if e(sample)
		qui estadd scalar n_cantons = `r(ndistinct)'
		qui eststo a2
		

collapse (mean) hh_inc_pc_real ln_hh_inc_pc_real share_above500 (iqr) iqr_hh_inc = hh_inc_pc_real, by(dept_code NOM_MUN COD_CTO year)

reg iqr_hh_inc share_above500 i.year, cluster(NOM_MUN)
		qui sum iqr_hh_inc if e(sample)
		qui estadd scalar ymean = r(mean), replace
		qui estadd scalar ar2 = e(r2_a), replace
		qui estadd scalar n_clust = e(N_clust), replace
		qui local sd1 = `r(sd)'
		qui sum share_above500 if e(sample)
		qui estadd scalar beta = e(b)[1,1]*`r(sd)'/`sd1'
		qui distinct COD_CTO if e(sample)
		qui estadd scalar n_cantons = `r(ndistinct)'
		qui eststo a3
		
reg iqr_hh_inc share_above500  i.year i.dept_code , cluster(NOM_MUN) 
		qui sum iqr_hh_inc if e(sample)
		qui estadd scalar ymean = r(mean), replace
		qui estadd scalar ar2 = e(r2_a), replace
		qui estadd scalar n_clust = e(N_clust), replace
		qui local sd1 = `r(sd)'
		qui sum share_above500 if e(sample)
		qui estadd scalar beta = e(b)[1,1]*`r(sd)'/`sd1'
		qui distinct COD_CTO if e(sample)
		qui estadd scalar n_cantons = `r(ndistinct)'
		qui eststo a4
		
restore

estout a1 a2 a3 a4 , cells(b(fmt(3) star) se(fmt(3) par)) stats(ymean ar2 beta N n_cantons n_clust, fmt(2 3 3 %12.0fc %12.0fc) labels("Outcome Mean" "Adjusted R2" "Beta Coef." "Observations" "Cantons" "Clusters")) starlevels(* 0.1 ** 0.05 *** 0.01) keep(share_above500)
 
	* Output for latex
	esttab a1 a2 a3 a4 using "./output/hhsurvey_incomes_share_above500_a.tex", frag cells(b(fmt(2) star) se(fmt(2) par)) ///
		stats() keep(share_above500) ///
		starlevels(`"\sym{*}"' 0.1 `"\sym{**}"' 0.05 `"\sym{***}"' 0.01, label(" \(p<@\)")) ///
		varwidth(20) modelwidth(12) delimiter(&) end(\\) ///
		noobs collabels(none) booktabs gaps nonumber label  nonote replace nomtitles nodepvars nolines

	esttab a1 a2 a3 a4 using "./output/hhsurvey_incomes_share_above500_b.tex", frag	cells(b(fmt(2) star)) ///
		drop(*)  stats(ymean ar2 beta N n_cantons n_clust, fmt(2 3 3 %12.0fc %12.0fc) labels("Outcome Mean" "Adjusted R2" "Beta Coef." "Observations" "Cantons" "Clusters") ///
		layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")) ///
		varwidth(20) modelwidth(12) delimiter(&) end(\\) collabels(none) nolines ///
		nonumber label  nonote replace nomtitles nodepvars noisily 

		
		
** Properties within 300 ha of threshold:

preserve
reg hh_inc_pc_real share_above500_w300  i.year, cluster(NOM_MUN)
		qui sum hh_inc_pc_real if e(sample)
		qui estadd scalar ymean = r(mean), replace
		qui estadd scalar ar2 = e(r2_a), replace
		qui estadd scalar n_clust = e(N_clust), replace
		qui local sd1 = `r(sd)'
		qui sum share_above500_w300 if e(sample)
		qui estadd scalar beta = e(b)[1,1]*`r(sd)'/`sd1'
		qui distinct COD_CTO if e(sample)
		qui estadd scalar n_cantons = `r(ndistinct)'
		qui eststo a1
		
reg hh_inc_pc_real share_above500_w300  i.year i.dept_code, cluster(NOM_MUN)
		qui sum hh_inc_pc_real if e(sample)
		qui estadd scalar ymean = r(mean), replace
		qui estadd scalar ar2 = e(r2_a), replace
		qui estadd scalar n_clust = e(N_clust), replace
		qui local sd1 = `r(sd)'
		qui sum share_above500_w300 if e(sample)
		qui estadd scalar beta = e(b)[1,1]*`r(sd)'/`sd1'
		qui distinct COD_CTO if e(sample)
		qui estadd scalar n_cantons = `r(ndistinct)'
		qui eststo a2
		

collapse (mean) hh_inc_pc_real ln_hh_inc_pc_real share_above500_w300 (iqr) iqr_hh_inc = hh_inc_pc_real, by(dept_code NOM_MUN COD_CTO year)

reg iqr_hh_inc share_above500_w300  i.year, cluster(NOM_MUN)
		qui sum iqr_hh_inc if e(sample)
		qui estadd scalar ymean = r(mean), replace
		qui estadd scalar ar2 = e(r2_a), replace
		qui estadd scalar n_clust = e(N_clust), replace
		qui local sd1 = `r(sd)'
		qui sum share_above500_w300 if e(sample)
		qui estadd scalar beta = e(b)[1,1]*`r(sd)'/`sd1'
		qui distinct COD_CTO if e(sample)
		qui estadd scalar n_cantons = `r(ndistinct)'
		qui eststo a3
		
reg iqr_hh_inc share_above500_w300  i.year i.dept_code, cluster(NOM_MUN) 
		qui sum iqr_hh_inc if e(sample)
		qui estadd scalar ymean = r(mean), replace
		qui estadd scalar ar2 = e(r2_a), replace
		qui estadd scalar n_clust = e(N_clust), replace
		qui local sd1 = `r(sd)'
		qui sum share_above500_w300 if e(sample)
		qui estadd scalar beta = e(b)[1,1]*`r(sd)'/`sd1'
		qui distinct COD_CTO if e(sample)
		qui estadd scalar n_cantons = `r(ndistinct)'
		qui eststo a4
		
restore

estout a1 a2 a3 a4 , cells(b(fmt(3) star) se(fmt(3) par)) stats(ymean ar2 beta N n_cantons n_clust, fmt(2 3 3 %12.0fc %12.0fc) labels("Outcome Mean" "Adjusted R2" "Beta Coef." "Observations" "Cantons" "Clusters")) starlevels(* 0.1 ** 0.05 *** 0.01) keep(share_above500_w300)

	* Output for latex
	esttab a1 a2 a3 a4 using "./output/hhsurvey_incomes_share_above500_w300_a.tex", frag cells(b(fmt(2) star) se(fmt(2) par)) ///
		stats() keep(share_above500_w300) ///
		starlevels(`"\sym{*}"' 0.1 `"\sym{**}"' 0.05 `"\sym{***}"' 0.01, label(" \(p<@\)")) ///
		varwidth(20) modelwidth(12) delimiter(&) end(\\) ///
		noobs collabels(none) booktabs gaps nonumber label  nonote replace nomtitles nodepvars nolines

	esttab a1 a2 a3 a4 using "./output/hhsurvey_incomes_share_above500_w300_b.tex", frag	cells(b(fmt(2) star)) ///
		drop(*)  stats(ymean ar2 beta N n_cantons n_clust, fmt(2 3 3 %12.0fc %12.0fc) labels("Outcome Mean" "Adjusted R2" "Beta Coef." "Observations" "Cantons" "Clusters") ///
		layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")) ///
		varwidth(20) modelwidth(12) delimiter(&) end(\\) collabels(none) nolines ///
		nonumber label  nonote replace nomtitles nodepvars noisily 
	

**** Quantile Regression Estimates
preserve
gen Above500_QPlot = share_above500
label var Above500_QPlot "Quantile Estimates for: Share Above 500 ha"
qui tab year, gen(i_year)
drop i_year1
bsqreg hh_inc_pc_real  Above500_QPlot  i_year*,  q(.50)
set scheme lean1
grqreg Above500_QPlot,  ci reps(40) qstep(.2) seed(821)
	graph export "./output/quantile_hh_inc_pc_real.pdf", replace
restore


