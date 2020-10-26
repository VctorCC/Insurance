*-----------------------------------------------------------------------------
* Estimation of income process - Problem set 6
*-------------------------------------------------------------------------------

clear 
set more off
cls

* Set up the data --------------------------------------------------------------

* load data
use "C:\Users\USUARIO\Desktop\Master\2º Año\Social Insurance\Projects\P4\PANEL6812_HH.dta" 


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

xtreg ln_wage age age2 coll hs famsize married /*
               */ agecoll agecoll2 agehs agehs2 i.year, fe cluster(persnr)

estimates store model1



* predicted income
predict hat_ln_wage

scatter hat_ln_wage age

*==================================================
* smoothed age earnings profile
reg hat_ln_wage age age2 married
predict sm_inc
scatter sm_inc age
* correlation between hours and age
reg Hrs_hh age
predict hat_hours
scatter hat_hours age

* residual income
gen res_ln_wage = ln_wage - hat_ln_wage

summarize res_ln_wage , d

* save data 

		   
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
	eststo gmm1
	gen rho=_b[/rho]
	gen var_nu=_b[/var_nu]
	gen var_epsi = _b[/var_epsi] 
	gen Var_Ytilda=  var_nu/(1-(rho)^2) + var_epsi



