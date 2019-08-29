

set more off

** Setup: 
** To run, please specify local directory containing Benin files in line 12
** This code also requires user written function outreg2 which can be 
** installed with the line below
** ssc inst outreg2

** Set Working Directory to root directory containing 
cd "## INSERT DIRECTORY OF BENIN FILES ##"

* Table 1 - experimental design


/* Main results on common arm */

*****************************************************
* Table 2 - Summary Stats in Common Arm and Control

** Survey data
* Use individual level data provided to metaketa for metaanalysis

* Individual level using survey
cd "data"
use benin_survey.dta, clear
drop if condition=="Control in Private"
gen treatment = 0
replace treatment = 1 if condition=="T1T3"

* Means in Good news
mean m1 if treatment==1&m10==1
mean m1 if treatment==0&m10==1

* Means in Bad news
mean m1 if treatment==1&m10==0
mean m1 if treatment==0&m10==0




* Administrative data
use benin_official.dta, clear

gen treatment = commonarm
gen m10 = goodnews
rename m2 m1
rename m4 m3

* Means in Good news
mean m1 if treatment==1&m10==1
mean m1 if treatment==0&m10==1

* Means in Bad news
mean m1 if treatment==1&m10==0
mean m1 if treatment==0&m10==0


*****************************************************
* Table 3 - Common arm treatment effects, incumbent vote



* will include specification with distance from prior in survey analysis

* survey
use benin_survey.dta, clear
drop if condition=="Control in Private"

gen treatment = 0
replace treatment = 1 if condition=="T1T3"

label variable treatment "Treatment"

*gen N =   performance - m9
* Normalize N in good and bad news groups, separately

sum N if m10==1 // get mean and SD of Q-P in good news
gen N_good = (N - r(mean))/r(sd) if m10==1  // create normalized var

sum N if m10==0 // get mean and SD of Q-P in bad news
gen N_bad = (N - r(mean))/r(sd) if m10==0  // create normalized var

gen Ng_inter = treatment * N_good
gen Nb_inter = treatment * N_bad

label variable N_good "Distance between Prior and Performance (good news)"
label variable N_bad "Distance between Prior and Performance (bad news)"

label variable Ng_inter "Treatment x Distance (good news)"
label variable Nb_inter "Treatment x Distance (bad news)"

* remember, all don't knows on priors (M9) are not in the analysis below
tab m9 m10, missing // shows how much missingness is created



* WD for saving tables
 cd "../tables"
* good news
areg m1 treatment if m10==1, absorb(blockid) vce(cluster quartier)
	outreg2 using table3a.tex, tex(fra) replace ctitle("Good News", "Survey") label  dec(2)

	
* bad news
areg m1 treatment if m10==0, absorb(blockid) vce(cluster quartier)
	outreg2 using table3a.tex, tex(fra) ctitle("Bad News" ,"Survey") label append dec(2)

* Create bar graph
preserve

collapse (mean) meanvote= m1 (sd) sdvote=m1 (count) n=m1, by(treatment m10)

generate hivote = meanvote + invttail(n-1,0.025)*(sdvote / sqrt(n))
generate lowvote = meanvote - invttail(n-1,0.025)*(sdvote / sqrt(n))

tostring m10, replace
replace m10="Bad News" if m10=="0"
replace m10="Good News" if m10=="1"

graph twoway (bar meanvote treatment, barwidth(.8)) (rcap hivote lowvote treatment), by(m10) ///
xlabel(0 "Control" 1 "Treatment", noticks) xscale(r(-1 2)) /// 
scheme(s1mono)  ytitle("Mean Incumbent Voteshare") yscale(r(0 1)) ylabel(0(.1)1) ///
xtitle("") legend(order(2 "95% Confidence Interval"))
graph export "../figures/common_survey_BW.pdf", replace

restore	

/***************
 REVIEW REQUIRED: cprivate variable not found **GM: the variable definition was commented out above and condition of control in private dropped. 
 GM: I've included definition of cprivate variable below and reiterated other relevant variable definitions after recovering dataset without cprivate dropped.
*******************/

cd "../data"
use benin_survey.dta, clear

gen treatment = 0
replace treatment = 1 if condition=="T1T3"

label variable treatment "Treatment"

gen cprivate = 0
	replace cprivate = 1 if condition=="Control in Private"

	sum N if m10==1 // get mean and SD of Q-P in good news
gen N_good = (N - r(mean))/r(sd) if m10==1  // create normalized var

sum N if m10==0 // get mean and SD of Q-P in bad news
gen N_bad = (N - r(mean))/r(sd) if m10==0  // create normalized var

gen Ng_inter = treatment * N_good
gen Nb_inter = treatment * N_bad

label variable N_good "Distance between Prior and Performance (good news)"
label variable N_bad "Distance between Prior and Performance (bad news)"

label variable Ng_inter "Treatment x Distance (good news)"
label variable Nb_inter "Treatment x Distance (bad news)"


* good news, with distance	
areg m1 treatment N_good  Ng_inter if m10==1&cprivate==0, absorb(blockid) vce(cluster quartier)
	outreg2 using table3a.tex,tex(fra) ctitle("Good News" ,"Survey") label append dec(2)


* bad news, with distance	
areg m1 treatment N_bad  Nb_inter if m10==0&cprivate==0, absorb(blockid) vce(cluster quartier)
		outreg2 using table3a.tex,tex(fra)  ctitle("Bad News" ,"Survey") label append dec(2)

		


* common arm results using admin data
* Administrative data
cd "../data"
use benin_official.dta, clear

gen treatment = commonarm
gen m10 = goodnews
rename m2 m1
rename m4 m3

label variable treatment "Treatment"

* clustering on commune-treatment

 cd "../tables"
* good news
areg m1 treatment if m10==1, absorb(blockid) vce(cluster newid)
	outreg2 using table3b.tex, tex(fra) ctitle("Good News" ,"Official") label replace dec(2)

* bad news
areg m1 treatment if m10==0, absorb(blockid) vce(cluster newid)
	outreg2 using table3b.tex, tex(fra) ctitle("Bad News", "Official") label append dec(2)

* Create bar graph
preserve

collapse (mean) meanvote= m1 (sd) sdvote=m1 (count) n=m1, by(treatment m10)

generate hivote = meanvote + invttail(n-1,0.025)*(sdvote / sqrt(n))
generate lowvote = meanvote - invttail(n-1,0.025)*(sdvote / sqrt(n))

tostring m10, replace
replace m10="Bad News" if m10=="0"
replace m10="Good News" if m10=="1"

graph twoway (bar meanvote treatment, barwidth(.8)) (rcap hivote lowvote treatment), by(m10) ///
xlabel(0 "Control" 1 "Treatment", noticks) xscale(r(-1 2)) /// 
scheme(s1mono)  ytitle("Mean Incumbent Voteshare") yscale(r(0 1)) ylabel(0(.1)1) ///
xtitle("") legend(order(2 "95% Confidence Interval"))

graph export "../figures/common_admin_BW.pdf", replace

restore	

	
*************************************************************
* Table 4 - Common arm treatment on turnout 

* will include specification with distance from prior in survey analysis

* survey
cd "../data"
use benin_survey.dta, clear
drop if condition=="Control in Private"
gen treatment = 0
replace treatment = 1 if condition=="T1T3"

label variable treatment "Treatment"

*gen N =   performance - m9
* Normalize N in good and bad news groups, separately

sum N if m10==1 // get mean and SD of Q-P in good news
gen N_good = (N - r(mean))/r(sd) if m10==1  // create normalized var

sum N if m10==0 // get mean and SD of Q-P in bad news
gen N_bad = (N - r(mean))/r(sd) if m10==0  // create normalized var

gen Ng_inter = treatment * N_good
gen Nb_inter = treatment * N_bad

label variable N_good "Distance between Prior and Performance (good news)"
label variable N_bad "Distance between Prior and Performance (bad news)"

label variable Ng_inter "Treatment x Distance (good news)"
label variable Nb_inter "Treatment x Distance (bad news)"

* remember, all don't knows on priors (M9) are not in the analysis below
tab m9 m10, missing // shows how much missingness is created


 cd "../tables"
* good news
areg m3 treatment if m10==1, absorb(blockid) vce(cluster quartier)
	outreg2 using table4a.tex, tex(fra) replace ctitle("Good News", "Survey") label  dec(2)

	
* bad news
areg m3 treatment if m10==0, absorb(blockid) vce(cluster quartier)
	outreg2 using table4a.tex, tex(fra) ctitle("Bad News" ,"Survey") label append dec(2)

* good news, with distance	
areg m3 treatment N_good  Ng_inter if m10==1, absorb(blockid) vce(cluster quartier)
	outreg2 using table4a.tex, tex(fra) ctitle("Good News" ,"Survey") label append dec(2)


* bad news, with distance	
areg m1 treatment N_bad  Nb_inter if m10==0, absorb(blockid) vce(cluster quartier)
		outreg2 using table4a.tex, tex(fra) ctitle("Bad News" ,"Survey") label append dec(2)


* admin
* Administrative data
cd "../data"

use benin_official.dta, clear

gen treatment = commonarm
gen m10 = goodnews
rename m2 m1
rename m4 m3

label variable treatment "Treatment"

* clustering on commune-treatment
 cd "../tables"
* good news
areg m3 treatment if m10==1, absorb(blockid) vce(cluster newid)
	outreg2 using table4b.tex, tex(fra) ctitle("Good News" ,"Official") label replace dec(2)

* bad news
areg m3 treatment if m10==0, absorb(blockid) vce(cluster newid)
	outreg2 using table4b.tex, tex(fra) ctitle("Bad News", "Official") label append dec(2)




*************************************************************
* Table 5 - HTES

* M14 (age), M17 (education), M18 (wealth), M20 (vote previous)
* M21 (support incumbent last), M22 (clientelism future)
* , M26 (secret ballot), M27 (elections will be free and fair)
* M15 -- coethnic


set more off

* survey
cd "../data"

use benin_survey.dta, clear
drop if condition=="Control in Private"




gen treatment = 0
replace treatment = 1 if condition=="T1T3"

label variable treatment "Treatment"

label variable m14 "Age"
label variable m17 "Yrs of Education"
label variable m18 "Wealth (relative)"
label variable m20 "Turnout in Previous Election"
label variable m21 "Vote Incumbent in Previous Election"
label variable m15 "Coethnic with Incumbent"
label variable m22 "Expect Handout from Incumbent in Future"
label variable m26 "Perception of Ballot Secrecy"
label variable m27 "Expects Free and Fair Elections"
/***************
 REVIEW REQUIRED: clientelistlinkage variable not found 
 GM: I've sent separately the .dta file with clientelistlinkage in it. I'm not sure how the variable got dropped from the version saved in the Metaketa folder.
 GM: I've sent the complete .dta file as "benin_survey_withclientelistlinkage.dta" so it needs to be resaved as benin_survey.dta to run this .do file.
***************/

label variable clientelistlinkage "Personal Assistance in the Past"


gen tcl = treatment*clientelistlinkage 
	label variable tcl "Treatment x Personal Assistance in the Past"

gen tm14 = treatment*m14
	label variable tm14 "Treatment x Age"
	
gen tm17 = treatment*m17
	label variable tm17 "Treatment x Education"
	
gen tm18 = treatment*m18
	label variable tm18 "Treatment x Wealth"
	
gen tm20 = treatment*m20
	label variable tm20 "Treatment x Turnout Previous"

gen tm21 = treatment*m21
	label variable tm21 "Treatment x Incumbent Vote Previous"

gen tm15 = treatment*m15
	label variable tm15 "Treatment x Coethnic"

gen tm22 = treatment*m22
	label variable tm22 "Treatment x Handouts"
	
gen tm26 = treatment*m26
	label variable tm26 "Treatment x Ballot Secret"	

gen tm27 = treatment*m27
	label variable tm27 "Treatment x Free and Fair"	
	
 cd "../tables"


 ** Good news
areg m1 treatment tm14 m14 if m10==1, absorb(blockid) vce(cluster quartier)
	outreg2 using table5a.tex, tex(fra) replace label dec(2) ctitle(" ")

areg m1 treatment tm17 m17 if m10==1, absorb(blockid) vce(cluster quartier)
	outreg2 using table5a.tex, tex(fra) append label dec(2) ctitle(" ")

areg m1 treatment tm18 m18 if m10==1, absorb(blockid) vce(cluster quartier)
	outreg2 using table5a.tex, tex(fra) append label dec(2) ctitle(" ")

areg m1 treatment tm20 m20 if m10==1, absorb(blockid) vce(cluster quartier)
	outreg2 using table5a.tex, tex(fra) append label dec(2) ctitle(" ")

areg m1 treatment tm21 m21 if m10==1, absorb(blockid) vce(cluster quartier)
	outreg2 using table5a.tex, tex(fra) append label dec(2) ctitle(" ")

*areg m1 treatment tm15 m15 if m10==1, absorb(blockid) vce(cluster quartier)
*	outreg2 using table5a.tex, tex(fra) append label dec(2) ctitle(" ")
	
areg m1 treatment tm22 m22 if m10==1, absorb(blockid) vce(cluster quartier)
	outreg2 using table5a.tex, tex(fra) append label dec(2) ctitle(" ")

areg m1 treatment tm26 m26 if m10==1, absorb(blockid) vce(cluster quartier)
	outreg2 using table5a.tex, tex(fra) append label dec(2) ctitle(" ")

areg m1 treatment tm27 m27 if m10==1&m27<10, absorb(blockid) vce(cluster quartier)
	outreg2 using table5a.tex, tex(fra) append label dec(2) ctitle(" ")

	
** Bad news
areg m1 treatment tm14 m14 if m10==0, absorb(blockid) vce(cluster quartier)
	outreg2 using table5b.tex, tex(fra) replace label dec(2) ctitle(" ")

areg m1 treatment tm17 m17 if m10==0, absorb(blockid) vce(cluster quartier)
	outreg2 using table5b.tex, tex(fra) append label dec(2) ctitle(" ")

areg m1 treatment tm18 m18 if m10==0, absorb(blockid) vce(cluster quartier)
	outreg2 using table5b.tex, tex(fra) append label dec(2) ctitle(" ")

areg m1 treatment tm20 m20 if m10==0, absorb(blockid) vce(cluster quartier)
	outreg2 using table5b.tex, tex(fra) append label dec(2) ctitle(" ")

areg m1 treatment tm21 m21 if m10==0, absorb(blockid) vce(cluster quartier)
	outreg2 using table5b.tex, tex(fra) append label dec(2) ctitle(" ")

*areg m1 treatment tm15 m15 if m10==0, absorb(blockid) vce(cluster quartier)
*	outreg2 using table5b.tex, tex(fra) append label dec(2) ctitle(" ")
	
areg m1 treatment tm22 m22 if m10==0, absorb(blockid) vce(cluster quartier)
	outreg2 using table5b.tex, tex(fra) append label dec(2) ctitle(" ")

areg m1 treatment tm26 m26 if m10==0, absorb(blockid) vce(cluster quartier)
	outreg2 using table5b.tex, tex(fra) append label dec(2) ctitle(" ")

areg m1 treatment tm27 m27 if m10==0&m27<10, absorb(blockid) vce(cluster quartier)
	outreg2 using table5b.tex, tex(fra) append label dec(2) ctitle(" ")
	

*************************************************************************
*************************************************************************
** Interactions with coethnicity and clientelism and yrs of education

 cd "../tables"


areg m1 treatment tm15 m15 if m10==1, absorb(blockid) vce(cluster quartier)
	outreg2 using table7.tex, tex(fra) replace label dec(2) ctitle(" Good", "News")

areg m1 treatment tm15 m15 if m10==0, absorb(blockid) vce(cluster quartier)
	outreg2 using table7.tex, tex(fra) append label dec(2) ctitle(" Bad", "News")

areg m1 treatment tcl clientelistlinkage if m10==1, absorb(blockid) vce(cluster quartier)
	outreg2 using table7.tex, tex(fra) append label dec(2) ctitle(" Good", "News")

areg m1 treatment tcl clientelistlinkage if m10==0, absorb(blockid) vce(cluster quartier)
	outreg2 using table7.tex, tex(fra) append label dec(2) ctitle(" Bad", "News")

areg m1 treatment tm17 m17 if m10==1, absorb(blockid) vce(cluster quartier)
	outreg2 using table7.tex, tex(fra) append label dec(2) ctitle(" Good", "News")

areg m1 treatment tm17 m17 if m10==0, absorb(blockid) vce(cluster quartier)
	outreg2 using table7.tex, tex(fra) append label dec(2) ctitle(" Bad", "News")	
