*-----------------------------------------------------------------------------
* Estimation of income process - Problem set 6
*-------------------------------------------------------------------------------

/*
     HOW TO USE THID .DO FILE:
	 
     - set your working directory in line 20
	 - in your directory there has to be PANEL6812_HH.dta
	 - in your directory create a folder called "Annex" (every export will be saved there)
*/

clear 
set more off
cls

* Set up the data --------------------------------------------------------------

* load data
cd "C:\Users\lollo\Documents\uni\QEM\[3] Barcelona_Autonoma\Academics Barcelona\Social  insurance\project 6\STATA" 
use PANEL6812_HH.dta

* declare panel 
xtset persnr year

* restrict household head age
drop if age_head < 25 | age_head > 60

* real earnings using consumer price index 
summarize yhh7
gen y_postax =yhh7/cpi

***negative values
drop if  yhh7<0 |  yhh7==. 


* restrict earnings > 0



* controls 
gen age2 = age_head^2

* education dummies 
gen coll = 0
replace coll = 1 if edu == 4 
gen hs = 0
replace hs = 1 if  edu == 2 | edu == 3
* interactions 
gen agecoll = age_head*coll
gen agecoll2 = age2*coll

gen agehs = age_head*hs
gen agehs2 = age2*hs

rename age_head age

gen hourly_income= y_postax/Hrs_hh
gen ln_wage = log(hourly_income)


*==========================================
* first stage estimates ----------------------------------------------------------------------------

xtreg ln_wage age age2 coll hs famsize married ///
agecoll agecoll2 agehs agehs2 i.year, fe cluster(persnr)
estimates store model1

esttab model1 using Annex\01.tex, se ar2 keep(age age2 coll hs famsize married ///
agecoll agecoll2 agehs agehs2 _cons) replace tex ///
mtitle("ln(wage/hour)") ///
title(Fixed Effects Regression with Year dummies\label{reg01})

* predicted income
predict hat_ln_wage

scatter hat_ln_wage age, title(Predicted Income over Age) yti(predicted ln wage) ///
xti(age)
graph export Annex\predicted_income.png, replace

*==================================================
* smoothed age earnings profile
reg hat_ln_wage age age2 married
estimates store model2

predict sm_inc
scatter sm_inc age, title(Smoothed Age Earnings profile) yti(predicted ln wage) ///
xti(age)
graph export Annex\smoothed.png, replace
* correlation between hours and age
reg Hrs_hh age
estimates store model3

predict hat_hours
scatter hat_hours age, title(Correlation Hours Wage) yti(predicted hours) ///
xti(age)
graph export Annex\corr_hours_age.png, replace

*export model2 and model3
esttab model2 using Annex\02.tex, se ar2 replace tex ///
mtitle("predicted ln(wage/hour)") ///
title(Smoothed Age Earnings profile\label{reg02})

esttab model3 using Annex\03.tex, se ar2 replace tex ///
mtitle("predicted hours worked by HH") ///
title(Correaltion between Hours and Age\label{reg03})


* residual income
gen res_ln_wage = ln_wage - hat_ln_wage

summarize res_ln_wage , d
		   
* second stage estimates -----------------------------------------------------------------

*****_____________________________PART 2________________________________
 
 * second stage regression of residual on its lag

* Generate lead variables:
gen res_ln_wage1 = F.res_ln_wage
gen res_ln_wage2 = F2.res_ln_wage
 
********bysort persnr:  egen minage= min(age)**!!!!!!!!!!!!!!!!
* Generate empirical moments.
gen m00_1 = res_ln_wage^2 
gen m10_1 = res_ln_wage1^2
gen m01_1 = res_ln_wage*res_ln_wage1 
gen m02_1 = res_ln_wage*res_ln_wage2


collapse (mean) m00_1 m10_1 m01_1 m02_1 [w=fwgt], by(age year) 


***************************************************
* Carry out GMM estimation.

	gmm ///
		(mj0: m00_1 - {rho}^(2*(age-25+1))*{var_z} - {var_nu}*(1-{rho}^(2*(age-25+1)))/(1-{rho}^2) - {var_epsi} ) ///
		(mjp10: m10_1 - {rho}^(2*(age-25+2))*{var_z} - {var_nu}*(1-{rho}^(2*(age-25+2)))/(1-{rho}^2) - {var_epsi} ) ///
		(mj1: m01_1 - {rho}*({rho}^(2*(age-25+1))*{var_z} + {var_nu}*(1-{rho}^(2*(age-25+1)))/(1-{rho}^2)) ) ///
		(mj2: m02_1 - {rho}^2*({rho}^(2*(age-25+1))*{var_z} + {var_nu}*(1-{rho}^(2*(age-25+1)))/(1-{rho}^2)) ), ///
		instruments(age, noconstant) instruments(mj0: m00_1, noconstant) ///
		instruments(mjp10: m10_1, noconstant) ///
		instruments(mj1: m01_1, noconstant) ///
		instruments(mj2: m02_1, noconstant) ///
		winitial(identity) from(rho 0.9 var_z 0.3 var_nu 0.03 var_epsi 0.03) onestep nolog

mat rt_1 = e(b) 
	eststo gmm
	gen rho=_b[/rho]
	gen var_nu=_b[/var_nu]
	gen var_epsi = _b[/var_epsi] 
	gen Var_Ytilda=  var_nu/(1-(rho)^2) + var_epsi

*Exporting the table
	esttab using Annex/gmm.tex, se ar2 replace tex ///
	mtitle("Variance") ///
	title(GMM estimates unconditional variance \label{gmm})
	eststo clear
