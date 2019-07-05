cap cd "D:\Dropbox\Metaketa_replication_shared\chapter_replications\mx\code\"
cap log close

set more off


qui {

use "..\data\Base QFD Intervencion eleccion 2015_27082015_rev.dta", clear

set more off

*** Cleaning data
compress

rename ENT state
rename SECD section
destring section, replace
rename MANZ manzana
destring manzana, replace
order state section MUN LOC

* Merge in electoral outcomes
sort state section
merge m:1 state section using "..\data\ANALYSIS.dta"
keep if _merge==3
drop _merge


* Edit survey variables
qui do SURVEY_VARIABLES_chapter.do



*************************************************************************;
* ANALYSES
*************************************************************************;

*RESTRICTING SAMPLE TO COMPARATIVE ARM
keep if Tc== 1 | T==0

cap drop good_news
g good_news = newchange_i4<=0 if newchange_i4!=.
cap drop bad_news
g bad_news = newchange_i4>0 if newchange_i4!=.

g poor = (corruption==0)

}

*************************************************************************;
* TABLE 2
* Effect of information treatment on voter posterior beliefs about 
* incumbent party malfeasance
*************************************************************************;

*PANEL A (POOLED)
quietly foreach y of varlist posterior_i {
  eststo clear
  
  eststo, title("`y'"): areg `y' 1.T if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' 1.T##c.newchange_i4 if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("`y'"): areg `y' 1.Tn 1.Ts if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' 1.T#1.corruption 1.T#1.poor if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
}
  noisily estout, style(tex) cells(b(star fmt(3)) se(par)) ///
    stats(N r2 Outcome_Mean Outcome_SD Treatment_Mean Treatment_SD , fmt(0 2 2 2 2 2) ///
    labels("Observations" "$ R^2$" "Control outcome mean" "Control outcome std. dev." "Treatment mean" "Treatment std. dev." )) ///
    starlevels(* .1 ** .05 *** .01) label ///
	keep (1.T 1.T#c.newchange_i4 1.Tn 1.Ts 1.T#1.corruption 1.T#1.poor ) ///
	varlabel(1.T "Information treatment" ///
	1.T#c.newchange_i4 "$\times$ Negative incumbent updating" ///
	1.T#1.good_news "Good News" 1.T#1.bad_news "Bad News" ///
	1.T#1.corruption "Unauthorized spending" 1.T#1.poor "Not-spending on the poor" ///
	1.Ts "Public information treatment" 1.Tn "Private information treatment") 


	
*PANEL B (BAD NEWS) AND PANEL C (GOOD NEWS)
foreach x of numlist 0 1 {
preserve
keep if good_news==`x'
eststo clear

quietly foreach y of varlist posterior_i {
  eststo clear
  
  eststo, title("`y'"): areg `y' 1.T if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' 1.T##c.newchange_i4 if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("`y'"): areg `y' 1.Tn 1.Ts if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' 1.T#1.corruption 1.T#1.poor if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
}
  noisily estout, style(tex) cells(b(star fmt(3)) se(par)) ///
    stats(N r2 Outcome_Mean Outcome_SD Treatment_Mean Treatment_SD , fmt(0 2 2 2 2 2) ///
    labels("Observations" "$ R^2$" "Control outcome mean" "Control outcome std. dev." "Treatment mean" "Treatment std. dev." )) ///
    starlevels(* .1 ** .05 *** .01) label ///
	keep (1.T 1.T#c.newchange_i4 1.Tn 1.Ts 1.T#1.corruption 1.T#1.poor ) ///
	varlabel(1.T "Information treatment" ///
	1.T#c.newchange_i4 "$\times$ Negative incumbent updating" ///
	1.T#1.good_news "Good News" 1.T#1.bad_news "Bad News" ///
	1.T#1.corruption "Unauthorized spending" 1.T#1.poor "Not-spending on the poor" ///
	1.Ts "Public information treatment" 1.Tn "Private information treatment") 
restore
}
*


*************************************************************************;
* TABLE 5 
* Effect of information treatment on political party responses
*************************************************************************;

qui {

*** Politician reactions
g i_ref_leaflet = PP24A_1==1
g i_ref_campaign = PP24A_2==1
g i_ref_visit = PP24A_3==1 
g i_ref_advert = PP24A_4==1
g i_ref_media = PP24A_5==1

g c_ref_leaflet = PP25A_1==1
g c_ref_campaign = PP25A_2==1
g c_ref_visit = PP25A_3==1 
g c_ref_advert = PP25A_4==1
g c_ref_media = PP25A_5==1

rename incumbent_reaction i_reaction 
rename challenger_reaction c_reaction

 
  eststo clear
  
  eststo, title("i_reaction"): areg i_reaction 1.T, a(id_block) cluster(cluster2)
  sum i_reaction if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("i_reaction"): areg i_reaction T##c.intensity_own, a(id_block) cluster(cluster2)
  sum i_reaction if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  sum intensity_own if e(sample)
  estadd scalar Interaction_Mean=`r(mean)'
  estadd scalar Interaction_SD=`r(sd)'

  eststo, title("c_reaction"): areg c_reaction 1.T, a(id_block) cluster(cluster2)
  sum c_reaction if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("c_reaction"): areg c_reaction T##c.intensity_own, a(id_block) cluster(cluster2)
  sum c_reaction if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  sum intensity_own if e(sample)
  estadd scalar Interaction_Mean=`r(mean)'
  estadd scalar Interaction_SD=`r(sd)'
}
  noisily estout, style(tex) cells(b(star fmt(3)) se(par)) ///
    stats(N r2 Outcome_Mean Outcome_SD Treatment_Mean Treatment_SD Interaction_Mean Interaction_SD, fmt(0 2 2 2 2 2 2 2 2) ///
    labels("Observations" "$ R^2$" "Control outcome mean" "Control outcome std. dev." "Treatment mean" "Treatment std. dev." "Interaction mean" "Interaction std. dev.")) ///
    starlevels(* .1 ** .05 *** .01) label keep (1.T 1.T#c.intensity_own) varlabel(1.T "Information treatment" 1.T#c.intensity_own "$\times$ Incumbent malfeasant spending" 1.T#c.newchange_i4 "$\times$ Negative incumbent updating") 
  eststo clear
  

  
*************************************************************************;
* 							APPENDIX
*************************************************************************;


*************************************************************************;
* TABLE A1
* Effect of 8 individual-level pre-treatment variables
*************************************************************************;

eststo clear
capture g turnout2012 = voted_incumbent2012!=.
qui foreach y of varlist female age education income logincome employed turnout2012 voted_incumbent2012 {
  eststo, title("`y'"): areg `y' 1.T, a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Control_Mean=`r(mean)'
  sum `y' if e(sample) & T==1
  estadd scalar Treatment_Mean=`r(mean)'
}
estout using "balance_survey.txt", cells(b(star fmt(3)) se(par)) ///
stats(Control_Mean Treatment_Mean N, fmt( 2 2 0) ///
labels("Control mean" "Treatment mean" "Observations" )) ///
starlevels(* .1 ** .05 *** .01) label keep (1.T) ///
varlabel(1.T "Information treatment") replace



*************************************************************************;
* TABLE A5
* Effect of information treatment on self-reported vote and turnout
*************************************************************************;

*PANEL A (POOLED)
quietly foreach y of varlist incumbent_vote_survey {
  eststo clear
  
  eststo, title("`y'"): areg `y' 1.T , a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' T##c.newchange_i4 , a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
    
  eststo, title("`y'"): areg `y' 1.Tn 1.Ts , a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' 1.T#1.corruption 1.T#1.poor , a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("turnout"): areg iturnout  1.T , a(id_block) cluster(cluster2)
  sum turnout if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  }
  noisily estout, style(tex) cells(b(star fmt(3)) se(par)) ///
    stats(N r2 Outcome_Mean Outcome_SD Treatment_Mean Treatment_SD , fmt(0 2 2 2 2 2) ///
    labels("Observations" "$ R^2$" "Control outcome mean" "Control outcome std. dev." "Treatment mean" "Treatment std. dev." )) ///
    starlevels(* .1 ** .05 *** .01) label ///
	keep (1.T 1.T#c.newchange_i4 1.T#c.newchange_i4 1.Tn 1.Ts 1.T#1.corruption 1.T#1.poor ) ///
	varlabel(1.T "Information treatment" ///
	1.T#c.newchange_i4 "Treatment $\times$ Negative incumbent updating" ///
	1.T#1.good_news "Good News" 1.T#1.bad_news "Bad News" ///
	1.T#1.corruption "Unauthorized spending" 1.T#1.poor "Not-spending on the poor" ///
	1.Ts "Public information treatment" 1.Tn "Private information treatment") 

	
*PANEL B (BAD NEWS) AND PANEL C (GOOD NEWS)
foreach x of numlist 0 1 {
preserve
keep if good_news==`x'
eststo clear

quietly foreach y of varlist incumbent_vote_survey {
  eststo clear
  
  eststo, title("`y'"): areg `y' 1.T , a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' T##c.newchange_i4 , a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
    
  eststo, title("`y'"): areg `y' 1.Tn 1.Ts , a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' 1.T#1.corruption 1.T#1.poor , a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("turnout"): areg iturnout  1.T , a(id_block) cluster(cluster2)
  sum turnout if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  }
  
  noisily estout, style(tex) cells(b(star fmt(3)) se(par)) ///
    stats(N r2 Outcome_Mean Outcome_SD Treatment_Mean Treatment_SD , fmt(0 2 2 2 2 2) ///
    labels("Observations" "$ R^2$" "Control outcome mean" "Control outcome std. dev." "Treatment mean" "Treatment std. dev." )) ///
    starlevels(* .1 ** .05 *** .01) label ///
	keep (1.T 1.T#c.newchange_i4 1.T#c.newchange_i4 1.Tn 1.Ts 1.T#1.corruption 1.T#1.poor ) ///
	varlabel(1.T "Information treatment" ///
	1.T#c.newchange_i4 "Treatment $\times$ Negative incumbent updating" ///
	1.T#1.good_news "Good News" 1.T#1.bad_news "Bad News" ///
	1.T#1.corruption "Unauthorized spending" 1.T#1.poor "Not-spending on the poor" ///
	1.Ts "Public information treatment" 1.Tn "Private information treatment") 
restore
}
*


*************************************************************************;
* TABLE A6: Effect of information treatment on voter posterior beliefs 
* about incumbent party malfeasance (Relative Measures)
*************************************************************************;

g d_posteriorICa = posterior_i - posterior_cA
g d_posteriorICb = posterior_i - posterior_cB
g d_posteriorICc = posterior_i - posterior_cC

g d_newchangeICa = newchange_i4 - newchangeCA
g d_newchangeICb = newchange_i4 - newchangeCB
g d_newchangeICc = newchange_i4 - newchangeCC

g good_newsICa = d_newchangeICa<=0 if d_newchangeICa!=.
g good_newsICb = d_newchangeICb<=0 if d_newchangeICb!=.
g good_newsICc = d_newchangeICc<=0 if d_newchangeICc!=.


*PANEL A (POOLED)
quietly foreach y of varlist d_posteriorICa {
  eststo clear
  
  eststo, title("`y'"): areg `y' 1.T if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' 1.T##c.d_newchangeICa if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("`y'"): areg `y' 1.Tn 1.Ts if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' 1.T#1.corruption 1.T#1.poor if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
}
  noisily estout, style(tex) cells(b(star fmt(3)) se(par)) ///
    stats(N r2 Outcome_Mean Outcome_SD Treatment_Mean Treatment_SD , fmt(0 2 2 2 2 2) ///
    labels("Observations" "$ R^2$" "Control outcome mean" "Control outcome std. dev." "Treatment mean" "Treatment std. dev." )) ///
    starlevels(* .1 ** .05 *** .01) label ///
	keep (1.T 1.T#c.d_newchangeICa 1.Tn 1.Ts 1.T#1.corruption 1.T#1.poor ) ///
	varlabel(1.T "Information treatment" ///
	1.T#c.d_newchangeICa "$\times$ Negative incumbent updating relative to the challenger party" ///
	1.T#1.corruption "Unauthorized spending" 1.T#1.poor "Not-spending on the poor" ///
	1.Ts "Public information treatment" 1.Tn "Private information treatment") 



*PANEL B (RELATIVE BAD NEWS) AND PANEL C (RELATIVE GOOD NEWS)
foreach x of numlist 0 1 {
preserve
keep if good_newsICa==`x'
eststo clear

quietly foreach y of varlist d_posteriorICa {
  eststo clear
  
  eststo, title("`y'"): areg `y' 1.T if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' 1.T##c.d_newchangeICa if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("`y'"): areg `y' 1.Tn 1.Ts if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' 1.T#1.corruption 1.T#1.poor if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
}
  noisily estout, style(tex) cells(b(star fmt(3)) se(par)) ///
    stats(N r2 Outcome_Mean Outcome_SD Treatment_Mean Treatment_SD , fmt(0 2 2 2 2 2) ///
    labels("Observations" "$ R^2$" "Control outcome mean" "Control outcome std. dev." "Treatment mean" "Treatment std. dev." )) ///
    starlevels(* .1 ** .05 *** .01) label ///
	keep (1.T 1.T#c.d_newchangeICa 1.Tn 1.Ts 1.T#1.corruption 1.T#1.poor ) ///
	varlabel(1.T "Information treatment" ///
	1.T#c.d_newchangeICa "$\times$ Negative incumbent updating relative to the challenger party" ///
	1.T#1.corruption "Unauthorized spending" 1.T#1.poor "Not-spending on the poor" ///
	1.Ts "Public information treatment" 1.Tn "Private information treatment") 
restore
}
*



*************************************************************************;
* TABLE A3: Effect of information treatment on voter posterior beliefs 
* about incumbent party malfeasance (Comparative-Private as Treatment)
*************************************************************************;

preserve
*RESTRICTING SAMPLE TO COMPARATIVE ARM
keep if Tc== 1 | T==0
*RESTRICTING SAMPLE TO COMPARATIVE-PRIVATE ARM
keep if Tn== 1 | T==0

*PANEL A (POOLED)
quietly foreach y of varlist posterior_i {
  eststo clear
  
  eststo, title("`y'"): areg `y' 1.T if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' 1.T##c.newchange_i4 if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("`y'"): areg `y' 1.T#1.corruption 1.T#1.poor if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
}
  noisily estout, style(tex) cells(b(star fmt(3)) se(par)) ///
    stats(N r2 Outcome_Mean Outcome_SD Treatment_Mean Treatment_SD , fmt(0 2 2 2 2 2) ///
    labels("Observations" "$ R^2$" "Control outcome mean" "Control outcome std. dev." "Treatment mean" "Treatment std. dev." )) ///
    starlevels(* .1 ** .05 *** .01) label ///
	keep (1.T 1.T#c.newchange_i4 1.T#1.corruption 1.T#1.poor ) ///
	varlabel(1.T "Information treatment" ///
	1.T#c.newchange_i4 "$\times$ Negative incumbent updating" ///
	1.T#1.good_news "Good News" 1.T#1.bad_news "Bad News" ///
	1.T#1.corruption "Unauthorized spending" 1.T#1.poor "Not-spending on the poor" ///
	1.Ts "Public information treatment" 1.Tn "Private information treatment") 


*PANEL B (BAD NEWS) AND PANEL C (GOOD NEWS)
foreach x of numlist 0 1 {
preserve
keep if good_news==`x'
eststo clear

quietly foreach y of varlist posterior_i {
  eststo clear
  
  eststo, title("`y'"): areg `y' 1.T if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' 1.T##c.newchange_i4 if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("`y'"): areg `y' 1.T#1.corruption 1.T#1.poor if muni!="APASEO EL ALTO", a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
}
  noisily estout, style(tex) cells(b(star fmt(3)) se(par)) ///
    stats(N r2 Outcome_Mean Outcome_SD Treatment_Mean Treatment_SD , fmt(0 2 2 2 2 2) ///
    labels("Observations" "$ R^2$" "Control outcome mean" "Control outcome std. dev." "Treatment mean" "Treatment std. dev." )) ///
    starlevels(* .1 ** .05 *** .01) label ///
	keep (1.T 1.T#c.newchange_i4 1.T#1.corruption 1.T#1.poor ) ///
	varlabel(1.T "Information treatment" ///
	1.T#c.newchange_i4 "$\times$ Negative incumbent updating" ///
	1.T#1.good_news "Good News" 1.T#1.bad_news "Bad News" ///
	1.T#1.corruption "Unauthorized spending" 1.T#1.poor "Not-spending on the poor" ///
	1.Ts "Public information treatment" 1.Tn "Private information treatment") 
restore
}
*
restore






*************************************************************************;
* END
*************************************************************************;
cap log close
*************************************************************************;
