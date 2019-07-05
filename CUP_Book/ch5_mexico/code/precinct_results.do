cap cd "D:\Dropbox\Metaketa_replication_shared\chapter_replications\mx\code\"
cap log close

set more off

qui {

use "..\data\ANALYSIS.dta", clear

* Merge in survey data priors and direction of updating
sort id_block
merge m:1 id_block using "..\data\surveypriors4precinct_chapter.dta"
drop _merge

* Merge in runner up/municipal data
sort uniqueid
merge m:1 uniqueid using "..\data\Muni_runnerup_2012.dta"
drop _merge


* Types of treatment
gen Ti = treatment==2 | treatment==4 if treatment!=.
gen Tc = treatment==3 | treatment==5 if treatment!=.
gen Tn = treatment==2 | treatment==3 if treatment!=.
gen Ts = treatment==4 | treatment==5 if treatment!=.

g Tin = treatment==2 if treatment!=.
g Tcn = treatment==3 if treatment!=.
g Tis = treatment==4 if treatment!=.
g Tcs = treatment==5 if treatment!=.

* Good or bad news
capture g good_i4 = newchange_i4<=-0.5 if newchange_i4!=.
capture g bad_i4 = newchange_i4>=0.5 if newchange_i4!=.
capture g good_i3 = newchange_i3<=-0.5 if newchange_i3!=.
capture g bad_i3 = newchange_i3>=0.5 if newchange_i3!=.

foreach x in A B C {
capture g good_C`x' = newchangeC`x'<=-0.5 if newchangeC`x'!=.
capture g bad_C`x' = newchangeC`x'>=0.5 if newchangeC`x'!=.
}

cap drop good_news
g good_news = newchange_i4<=0 if newchange_i4!=.
cap drop bad_news
g bad_news = newchange_i4>0 if newchange_i4!=.


g d_newchangeICa = newchange_i4 - newchangeCA
g d_newchangeICb = newchange_i4 - newchangeCB
g d_newchangeICc = newchange_i4 - newchangeCC

g good_newsICa = d_newchangeICa<=0 if d_newchangeICa!=.
g good_newsICb = d_newchangeICb<=0 if d_newchangeICb!=.
g good_newsICc = d_newchangeICc<=0 if d_newchangeICc!=.


*************************************************************************;
*	ANALYSIS
*************************************************************************;

*RESTRICTING SAMPLE TO COMPARATIVE ARM
keep if Tc== 1 | T==0

g poor = (corruption==0)

}

*************************************************************************;
* TABLE 3 
* Effect of information treatment on incumbent party vote share and 
* turnout (weighted estimates)
*************************************************************************;

*PANEL A (POOLED)
quietly foreach y of varlist incumbent_vote_share {
  eststo clear
  
  eststo, title("`y'"): areg `y' 1.T [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' T##c.newchange_i4 [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
    
  eststo, title("`y'"): areg `y' 1.Tn 1.Ts [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' 1.T#1.corruption 1.T#1.poor  [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("turnout"): areg turnout  1.T  [aweight=share_received], a(id_block) cluster(cluster2)
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

quietly foreach y of varlist incumbent_vote_share {
  eststo clear
  
  eststo, title("`y'"): areg `y' 1.T [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' T##c.newchange_i4 [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
    
  eststo, title("`y'"): areg `y' 1.Tn 1.Ts [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' 1.T#1.corruption 1.T#1.poor  [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("turnout"): areg turnout  1.T  [aweight=share_received], a(id_block) cluster(cluster2)
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
* TABLE 4 
* Effect of information treatment on incumbent party vote share
* by context (weighted estimates)
*************************************************************************;

*PARTISAN IDENTITIES
sum incumbent_ideology_b partisan_id_block partisan_id_i_block
egen ideologyI = std(incumbent_ideology_b)
*OTHERS
g competitiveness = 1 - winner_margin
foreach var of varlist pol_know_block media_avg_block trust_fair_block vote_buying_block competitiveness {
egen `var'Z = std(`var')
}
*

*PANEL A (POOLED)
eststo clear
qui foreach y of varlist incumbent_vote_share  {

  eststo, title("`y'"): areg `y' 1.T [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' T##c.ideologyI [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("`y'"): areg `y' T##c.vote_buying_blockZ [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("`y'"): areg `y' T##c.competitivenessZ [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("`y'"): areg `y' T##c.pol_know_blockZ [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("`y'"): areg `y' T##c.media_avg_blockZ [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
   
  eststo, title("`y'"): areg `y' T##c.trust_fair_blockZ [aweight=share_received], a(id_block) cluster(cluster2)
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
	keep (1.T 1.T#c.ideologyI 1.T#c.pol_know_blockZ 1.T#c.media_avg_blockZ 1.T#c.trust_fair_blockZ ///
	1.T#c.vote_buying_blockZ 1.T#c.competitivenessZ) ///
	varlabel(1.T "Information treatment" 1.T#c.ideologyI "$\times$ Inc. Ideology" 1.T#c.pol_know_blockZ "$\times$ Political Knowledge" 1.T#c.media_avg_blockZ "$\times$ Media Consumption" ///
	1.T#c.vote_buying_blockZ "$\times$ Vote Buying" 1.T#c.competitivenessZ "$\times$ Competitiveness" 1.T#c.trust_fair_blockZ "$\times$ Trust in Elections") 


*PANEL B (BAD NEWS) AND PANEL C (GOOD NEWS)
foreach x of numlist 0 1 {
preserve
keep if good_news==`x'
eststo clear
qui foreach y of varlist incumbent_vote_share  {

  eststo, title("`y'"): areg `y' 1.T [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' T##c.ideologyI [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("`y'"): areg `y' T##c.vote_buying_blockZ [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("`y'"): areg `y' T##c.competitivenessZ [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("`y'"): areg `y' T##c.pol_know_blockZ [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("`y'"): areg `y' T##c.media_avg_blockZ [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
   
  eststo, title("`y'"): areg `y' T##c.trust_fair_blockZ [aweight=share_received], a(id_block) cluster(cluster2)
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
	keep (1.T 1.T#c.ideologyI 1.T#c.pol_know_blockZ 1.T#c.media_avg_blockZ 1.T#c.trust_fair_blockZ ///
	1.T#c.vote_buying_blockZ 1.T#c.competitivenessZ) ///
	varlabel(1.T "Information treatment" 1.T#c.ideologyI "$\times$ Inc. Ideology" 1.T#c.pol_know_blockZ "$\times$ Political Knowledge" 1.T#c.media_avg_blockZ "$\times$ Media Consumption" ///
	1.T#c.vote_buying_blockZ "$\times$ Vote Buying" 1.T#c.competitivenessZ "$\times$ Competitiveness" 1.T#c.trust_fair_blockZ "$\times$ Trust in Elections") 
restore
}
*



*************************************************************************;
* 							APPENDIX
*************************************************************************;


*************************************************************************;
* TABLE A1
* Effect of 37 precinct-level pre-treatment variables
*************************************************************************;

global balance "area pop census_households	priv_dwellings share_working_age	share_old	share_married	ave_children_per_woman	share_male_head	share_born_out_of_state	indigenous_speakers	ave_schooling	share_economically_active	share_without_health_care	share_state_workers_health	ave_occupants_dwelling	ave_occupants_per_room	share_2more_bedrooms	share_3more_rooms	share_nondirt_floor	share_toilet	share_water	share_drainage	share_electricity	share_elec_water_drainage	share_washing_machine	share_landline	share_radio	share_fridge	share_cell_phone	share_television	share_auto	share_computer	share_internet	turnout_lag	comp_precinct_lag	incumbent_vote_share_lag"
eststo clear
quietly foreach y of varlist $balance {
  eststo, title("`:var l `y''"): areg `y' 1.T if incumbent_vote_share!=., a(id_block) cluster(cluster)
  sum `y' if e(sample) & T==0
  estadd scalar Control_Mean=`r(mean)'
  sum `y' if e(sample) & T==1
  estadd scalar Treatment_Mean=`r(mean)'
}
estout using "balance_precinct.txt", cells(b(star fmt(3)) se(par)) ///
stats(Control_Mean Treatment_Mean N, fmt( 2 2 0) ///
labels("Control mean" "Treatment mean" "Observations" )) ///
starlevels(* .1 ** .05 *** .01) label keep (1.T) ///
varlabel(1.T "Information treatment") replace


*************************************************************************;
* TABLE A2
* Effect of information treatment on incumbent party vote share 
* and turnout (Unweighted)
*************************************************************************;

*PANEL A (POOLED)
eststo clear
quietly foreach y of varlist incumbent_vote_share {
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
  
  eststo, title("turnout"): areg turnout  1.T , a(id_block) cluster(cluster2)
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
quietly foreach y of varlist incumbent_vote_share {
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
  
  eststo, title("turnout"): areg turnout  1.T , a(id_block) cluster(cluster2)
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
* TABLE A4
* Effect of information treatment on incumbent party vote share and 
* turnout (Comparative-Private as Treatment)
*************************************************************************;

*RESTRICTING SAMPLE TO COMPARATIVE ARM
keep if Tc== 1 | T==0
*RESTRICTING SAMPLE TO COMPARATIVE-PRIVATE ARM
keep if Tn== 1 | T==0

*PANEL A (POOLED)
quietly foreach y of varlist incumbent_vote_share {
  eststo clear
  
  eststo, title("`y'"): areg `y' 1.T [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' T##c.newchange_i4 [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
    
  eststo, title("`y'"): areg `y' 1.T#1.corruption 1.T#1.poor  [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("turnout"): areg turnout  1.T  [aweight=share_received], a(id_block) cluster(cluster2)
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
	keep (1.T 1.T#c.newchange_i4 1.T#c.newchange_i4 1.T#1.corruption 1.T#1.poor ) ///
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

quietly foreach y of varlist incumbent_vote_share {
  eststo clear
  
  eststo, title("`y'"): areg `y' 1.T [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' T##c.newchange_i4 [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' 1.T#1.corruption 1.T#1.poor  [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("turnout"): areg turnout  1.T  [aweight=share_received], a(id_block) cluster(cluster2)
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
	keep (1.T 1.T#c.newchange_i4 1.T#c.newchange_i4 1.T#1.corruption 1.T#1.poor ) ///
	varlabel(1.T "Information treatment" ///
	1.T#c.newchange_i4 "Treatment $\times$ Negative incumbent updating" ///
	1.T#1.good_news "Good News" 1.T#1.bad_news "Bad News" ///
	1.T#1.corruption "Unauthorized spending" 1.T#1.poor "Not-spending on the poor" ///
	1.Ts "Public information treatment" 1.Tn "Private information treatment") 
restore
}
*


*************************************************************************;
* TABLE A7
* Effect of information treatment on incumbent party vote share and 
* turnout (Relative Measures)
*************************************************************************;

*PANEL A (POOLED)
eststo clear

quietly foreach y of varlist incumbent_vote_share {
  eststo clear
  
  eststo, title("`y'"): areg `y' 1.T [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' T##c.d_newchangeICa [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
    
  eststo, title("`y'"): areg `y' 1.Tn 1.Ts [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' 1.T#1.corruption 1.T#1.poor  [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("turnout"): areg turnout  1.T  [aweight=share_received], a(id_block) cluster(cluster2)
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
	keep (1.T 1.T#c.d_newchangeICa 1.T#c.d_newchangeICa 1.Tn 1.Ts 1.T#1.corruption 1.T#1.poor ) ///
	varlabel(1.T "Information treatment" ///
	1.T#c.d_newchangeICa "Treatment $\times$ Negative incumbent updating" ///
	1.T#1.good_news "Good News" 1.T#1.bad_news "Bad News" ///
	1.T#1.corruption "Unauthorized spending" 1.T#1.poor "Not-spending on the poor" ///
	1.Ts "Public information treatment" 1.Tn "Private information treatment") 


*PANEL B (BAD NEWS) AND PANEL C (GOOD NEWS)
foreach x of numlist 0 1 {
preserve
keep if good_newsICa==`x'
eststo clear

quietly foreach y of varlist incumbent_vote_share {
  eststo clear
  
  eststo, title("`y'"): areg `y' 1.T [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' T##c.d_newchangeICa [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
    
  eststo, title("`y'"): areg `y' 1.Tn 1.Ts [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'

  eststo, title("`y'"): areg `y' 1.T#1.corruption 1.T#1.poor  [aweight=share_received], a(id_block) cluster(cluster2)
  sum `y' if e(sample) & T==0
  estadd scalar Outcome_Mean=`r(mean)'
  estadd scalar Outcome_SD=`r(sd)'
  sum T if e(sample)
  estadd scalar Treatment_Mean=`r(mean)'
  estadd scalar Treatment_SD=`r(sd)'
  
  eststo, title("turnout"): areg turnout  1.T  [aweight=share_received], a(id_block) cluster(cluster2)
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
	keep (1.T 1.T#c.d_newchangeICa 1.T#c.d_newchangeICa 1.Tn 1.Ts 1.T#1.corruption 1.T#1.poor ) ///
	varlabel(1.T "Information treatment" ///
	1.T#c.d_newchangeICa "Treatment $\times$ Negative incumbent updating" ///
	1.T#1.good_news "Good News" 1.T#1.bad_news "Bad News" ///
	1.T#1.corruption "Unauthorized spending" 1.T#1.poor "Not-spending on the poor" ///
	1.Ts "Public information treatment" 1.Tn "Private information treatment") 
restore
}
*


*************************************************************************;
* END
*************************************************************************;
cap log close
*************************************************************************;
