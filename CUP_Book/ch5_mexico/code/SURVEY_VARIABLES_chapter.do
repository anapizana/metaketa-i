*************************************************************************;
*  File-Name: 	SURVEY_VARIABLES_chapter.do								*;
*  Purpose: 	Create and edit variables for survey analyses (EGAP Ch.)*;									 	 *;
*  Used in: 	Individual Level Results.do								*;
*  				Individual Level Results.do								*;
*************************************************************************;

*************************************************************************;
* CLEAN DATA MORE
*************************************************************************;
drop IDENC IDENTEXT HRINJ HRTES HOGINT CREDVIG P54 P56

* DROP block of Chimalhuacan due to issues
drop if id_block==35

* FIX INCUMBENT
replace incumbent="PAN" if state==11 & section==1938


*************************************************************************;
* TREATMENT CODES
*************************************************************************;

* Types of treatment
gen Ti = treatment==2 | treatment==4 if treatment!=.
gen Tc = treatment==3 | treatment==5 if treatment!=.
gen Tn = treatment==2 | treatment==3 if treatment!=.
gen Ts = treatment==4 | treatment==5 if treatment!=.

g Tin = treatment==2 if treatment!=.
g Tcn = treatment==3 if treatment!=.
g Tis = treatment==4 if treatment!=.
g Tcs = treatment==5 if treatment!=.

* Clustering
cap drop cluster2
egen cluster2 = group(T uniqueid)

*************************************************************************;
* Individual Characteristics
*************************************************************************;


* Attribute Preferences: 
/*We collect measures on the importance that respondents attach
to the main candidate attributes associated with our informational treatments: honesty
and interest in the poor.*/
g honesty = P14_2 if P14_2<88 /*[larger is higher]*/

* Political Knowledge: 

*Antes de la elección, ¿qué tan frecuentemente habló usted sobre política,
*por ejemplo, con su familia o conocidos? [higher is more]
g pol_talk = P33 if P33<88

*Muni-level measure [to test H40]
bysort uniqueid: egen avgposterior = mean(pol_talk) if T==0
bysort uniqueid: egen avgposterior2 = mean(avgposterior)
g pol_talk_muni = avgposterior2
drop avgposterior*
*Block-level measure [to test H40]
bysort id_block: egen avgposterior = mean(pol_talk) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g pol_talk_block = avgposterior2
drop avgposterior*

g polknow_term = (P34==1)
g polknow_incum = (P35==1)
g polknow_winner = (P36==1)

g polknow = polknow_term + polknow_incum + polknow_winner

*Block-level measure [to test H9 EGAP CHAPTER]
g polknow2 = polknow / 3
bysort id_block: egen avgposterior = mean(polknow2) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g pol_know_block = avgposterior2
drop avgposterior*





* 3. Access (Consumption) Information: [higher is more]
gen media_tv = 5 - P38_1 if P38_1!=9
gen media_radio = 5 - P38_2 if P38_2!=9
gen media_paper = 5 - P38_3 if P38_3!=9
gen media_web = 5 - P38_4 if P38_4!=9

*Block-level measure [to test H9 EGAP CHAPTER]
g media_avg = (media_tv + media_radio + media_paper + media_web) / 4
bysort id_block: egen avgposterior = mean(media_avg) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g media_avg_block = avgposterior2
drop avgposterior*

bysort id_block: egen avgposterior = mean(media_tv) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g media_tv_block = avgposterior2
drop avgposterior*

bysort id_block: egen avgposterior = mean(media_radio) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g media_radio_block = avgposterior2
drop avgposterior*


* 4. Trust Institutions: [higher is more trust]
*(i) elections are helpful in electing honest and competent politicians; 
gen trust_helpful = P39 if P39<88
*(ii) the ballot is secret in Mexico and 
gen trust_secret = 6 - P40 if P40<88
*(iii) June elections were free and fair.
gen trust_fair = P41 if P41<88

*Block-level measure [to test H11 EGAP CHAPTER]
bysort id_block: egen avgposterior = mean(trust_fair) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g trust_fair_block = avgposterior2
drop avgposterior*


* Voted for the Incumbent Party in 2012?
g voted_incumbent2012 = 0
replace voted_incumbent2012 = 1 if incumbent=="PAN" & P5B1==1
replace voted_incumbent2012 = 1 if incumbent=="PAN_PANAL" & P5B1==1
replace voted_incumbent2012 = 1 if incumbent=="PAN_PANAL" & P5B1==6
replace voted_incumbent2012 = 1 if incumbent=="PRD" & P5B1==3
replace voted_incumbent2012 = 1 if incumbent=="PRI" & P5B1==2
replace voted_incumbent2012 = 1 if incumbent=="PRI_PVEM" & P5B1==2
replace voted_incumbent2012 = 1 if incumbent=="PRI_PVEM" & P5B1==4
replace voted_incumbent2012 = 1 if incumbent=="PRI_PVEM_PANAL" & P5B1==2
replace voted_incumbent2012 = 1 if incumbent=="PRI_PVEM_PANAL" & P5B1==4
replace voted_incumbent2012 = 1 if incumbent=="PRI_PVEM_PANAL" & P5B1==6
replace voted_incumbent2012 = 1 if incumbent=="PC" & P5B1==7
replace voted_incumbent2012 = . if P5B1==.
replace voted_incumbent2012 = . if P5B1==88
replace voted_incumbent2012 = . if P5B1==99



* 6. Demographics:
g female = (P47==2)
g age = 2015 - P48 if P48>1900
g education = P49 if P49<=16
g income = P52 if P52<88
g logincome = log(income+1)

g employed = .
replace employed = 1 if P50 == 1
replace employed = 1 if P50 == 2
replace employed = 0 if P50!=1 & P50!=2 & P50!=. & P50<88



*************************************************************************;
* EDIT KEY VARIABLES
*************************************************************************;

*** Outcome = Incumbent vote
gen incumbent_vote_survey = 0
replace incumbent_vote_survey = 1 if incumbent=="PAN" & P1B==1
replace incumbent_vote_survey = 1 if incumbent=="PAN_PANAL" & P1B==1
replace incumbent_vote_survey = 1 if incumbent=="PAN_PANAL" & P1B==6
replace incumbent_vote_survey = 1 if incumbent=="PRD" & P1B==3
replace incumbent_vote_survey = 1 if incumbent=="PRI" & P1B==2
replace incumbent_vote_survey = 1 if incumbent=="PRI_PVEM" & P1B==2
replace incumbent_vote_survey = 1 if incumbent=="PRI_PVEM" & P1B==4
replace incumbent_vote_survey = 1 if incumbent=="PRI_PVEM_PANAL" & P1B==2
replace incumbent_vote_survey = 1 if incumbent=="PRI_PVEM_PANAL" & P1B==4
replace incumbent_vote_survey = 1 if incumbent=="PRI_PVEM_PANAL" & P1B==6
replace incumbent_vote_survey = 1 if incumbent=="PC" & P1B==7
replace incumbent_vote_survey = . if P1B==.
replace incumbent_vote_survey = . if P1B==88
replace incumbent_vote_survey = . if P1B==99

*** Outcome = Challenger vote
gen challenger_vote_survey = 1 - incumbent_vote_survey

*** Outcome = Turnout [only 2 respondents missing for this one]
gen iturnout = (P1A==1) if P1A<88


gen notpartyvote2015 = .
replace notpartyvote2015 = 1 if P1B==12
replace notpartyvote2015 = 1 if P1B==13
replace notpartyvote2015 = 1 if P1B==14

gen notpartyvote2012 = .
replace notpartyvote2012 = 1 if P5B1==13
replace notpartyvote2012 = 1 if P5B1==14
replace notpartyvote2012 = 1 if P5B_1=="NINGUNO"


gen nochange1215 = .
replace nochange1215 = 1 if P1B==1 & P5B1==1
replace nochange1215 = 1 if P1B==2 & P5B1==2
replace nochange1215 = 1 if P1B==3 & P5B1==3
replace nochange1215 = 1 if P1B==4 & P5B1==4
replace nochange1215 = 1 if P1B==5 & P5B1==5
replace nochange1215 = 1 if P1B==6 & P5B1==6
replace nochange1215 = 1 if P1B==7 & P5B1==7


*************************************************************************;
* 5.3 Other Individual-Level Outcomes
*************************************************************************;

*** Non-PAP: Ideology
* Can add ideology aligned with incumbent party
gen incumbent_ideology = 0
replace incumbent_ideology = 1 if incumbent=="PAN" & P7A==1
replace incumbent_ideology = 1 if incumbent=="PAN_PANAL" & P7A==1
replace incumbent_ideology = 1 if incumbent=="PAN_PANAL" & P7A==6
replace incumbent_ideology = 1 if incumbent=="PRD" & P7A==3
replace incumbent_ideology = 1 if incumbent=="PRI" & P7A==2
replace incumbent_ideology = 1 if incumbent=="PRI_PVEM" & P7A==2
replace incumbent_ideology = 1 if incumbent=="PRI_PVEM" & P7A==4
replace incumbent_ideology = 1 if incumbent=="PRI_PVEM_PANAL" & P7A==2
replace incumbent_ideology = 1 if incumbent=="PRI_PVEM_PANAL" & P7A==4
replace incumbent_ideology = 1 if incumbent=="PRI_PVEM_PANAL" & P7A==6
replace incumbent_ideology = 1 if incumbent=="PC" & P7A==7
*replace incumbent_ideology = . if P7A==13
replace incumbent_ideology = . if P7A==99
replace incumbent_ideology = . if P7A_1=="ES SECRETO"
replace incumbent_ideology = . if P7A_1=="NO QUISO"
replace incumbent_ideology = . if P7A_1=="SECRETO"

*CREATE MUNI-LEVELS
bysort id_block: egen avgposterior = mean(incumbent_ideology) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g incumbent_ideology_m = avgposterior2
drop avgposterior*

*CREATE BLOCK-LEVELS
bysort id_block: egen avgposterior = mean(incumbent_ideology) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g incumbent_ideology_b = avgposterior2
drop avgposterior*

g ideology_m = (P7A==99)

*STRENGHT OF PARTISAN IDENTITY
g partisan_id = .
replace partisan_id = P7B
replace partisan_id = . if partisan_id==88
replace partisan_id = . if partisan_id==99

*Block-level measure
bysort id_block: egen avgposterior = mean(partisan_id) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g partisan_id_block = avgposterior2
drop avgposterior*

*complete continuum (strong id non-incumbent -- strong id incumbent)
g partisan_id_i = partisan_id
replace partisan_id_i = partisan_id_i * (-1) if incumbent_ideology!=1

*Block-level measure
bysort id_block: egen avgposterior = mean(partisan_id_i) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g partisan_id_i_block = avgposterior2
drop avgposterior*


*************************************************************************;
* INCUMBENT PARTISANSHIP
*************************************************************************;
g partisanship_i = .
replace partisanship_i = P7B if incumbent_ideology==1
replace partisanship_i = . if partisanship_i==88
replace partisanship_i = . if partisanship_i==99

replace partisanship_i = 0 if incumbent=="PAN" & P8B==1
replace partisanship_i = 0 if incumbent=="PAN_PANAL" & P8B==1
replace partisanship_i = 0 if incumbent=="PAN_PANAL" & P8B==6
replace partisanship_i = 0 if incumbent=="PRD" & P8B==3
replace partisanship_i = 0 if incumbent=="PRI" & P8B==2
replace partisanship_i = 0 if incumbent=="PRI_PVEM" & P8B==2
replace partisanship_i = 0 if incumbent=="PRI_PVEM" & P8B==4
replace partisanship_i = 0 if incumbent=="PRI_PVEM_PANAL" & P8B==2
replace partisanship_i = 0 if incumbent=="PRI_PVEM_PANAL" & P8B==4
replace partisanship_i = 0 if incumbent=="PRI_PVEM_PANAL" & P8B==6
replace partisanship_i = 0 if incumbent=="PC" & P8B==7

replace partisanship_i = -1 if incumbent_ideology!=1 & partisanship_i!=0
replace partisanship_i = partisanship_i + 2

g partisanship012 = partisanship_i - 1
replace partisanship012 = 2 if partisanship012>2 & partisanship012!=.

g partisanship01 = partisanship_i - 1
replace partisanship01 = 1 if partisanship01>1 & partisanship01!=.


*************************************************************************;
* CREATING VARS TO IDENTIFY THE CHALLENGER PARTY
*************************************************************************;
{
***************
* CHALLENGER_A
***************

/*
A) 
First, preferred party from "¿usted con qué partido político simpatiza?" 
	if preferred party is not the incumbent.
Second, if preferred party is the incumbent, 
	2012 vote if different from incumbent.
Third, if the 2012 vote is also the incumbent, 
	the alternative party they would have voted for in 2012.
Fourth, if they answer none to the the alternative party 
	they would have voted for in 2012, 
	the 2nd largest party in the municipality in terms of votes.
*/

g challenger_A = ""
#delimit ;
replace challenger_A = "PAN" 
	if P7A==1 & 
	(incumbent!="PAN" | incumbent!="PAN_PANAL");
replace challenger_A = "PRI" 
	if P7A==2 & 
	(incumbent!="PRI" | incumbent!="PRI_PVEM" | incumbent!="PRI_PVEM_PANAL");
replace challenger_A = "PRD" 
	if P7A==3 & 
	(incumbent!="PRD");
replace challenger_A = "PVEM" 
	if P7A==4 & 
	(incumbent!="PRI_PVEM" | incumbent!="PRI_PVEM_PANAL");
replace challenger_A = "PT" 
	if P7A==5 ;
replace challenger_A = "PANAL" 
	if P7A==6 & 
	(incumbent!="PAN_PANAL" | incumbent!="PRI_PVEM_PANAL");
replace challenger_A = "PC" 
	if P7A==7 & 
	(incumbent!="PC");
replace challenger_A = "MORENA" 
	if P7A==8 ;
replace challenger_A = "Encuentro Social" 
	if P7A==9 ;
replace challenger_A = "Partido Humanista" 
	if P7A==10 ;
replace challenger_A = "Partido Futuro Democraticp" 
	if P7A==11 ;
#delimit cr

#delimit ;
replace challenger_A = "PAN" 
	if P5B1==1 & incumbent_ideology==0 & voted_incumbent2012==0;
replace challenger_A = "PRI" 
	if P5B1==2 & incumbent_ideology==0 & voted_incumbent2012==0;
replace challenger_A = "PRD" 
	if P5B1==3 & incumbent_ideology==0 & voted_incumbent2012==0;
replace challenger_A = "PVEM" 
	if P5B1==4 & incumbent_ideology==0 & voted_incumbent2012==0;
replace challenger_A = "PT" 
	if P5B1==5 & incumbent_ideology==0 & voted_incumbent2012==0;
replace challenger_A = "PANAL" 
	if P5B1==6 & incumbent_ideology==0 & voted_incumbent2012==0;
replace challenger_A = "PC" 
	if P5B1==7 & incumbent_ideology==0 & voted_incumbent2012==0;
#delimit cr

#delimit ;
replace challenger_A = "PAN" 
	if P6==1 & incumbent_ideology==0 & voted_incumbent2012==1;
replace challenger_A = "PRI" 
	if P6==2 & incumbent_ideology==0 & voted_incumbent2012==1;
replace challenger_A = "PRD" 
	if P6==3 & incumbent_ideology==0 & voted_incumbent2012==1;
replace challenger_A = "PVEM" 
	if P6==4 & incumbent_ideology==0 & voted_incumbent2012==1;
replace challenger_A = "PT" 
	if P6==5 & incumbent_ideology==0 & voted_incumbent2012==1;
replace challenger_A = "PANAL" 
	if P6==6 & incumbent_ideology==0 & voted_incumbent2012==1;
replace challenger_A = "PC" 
	if P6==7 & incumbent_ideology==0 & voted_incumbent2012==1;
replace challenger_A = "PCP" 
	if P6==8 & incumbent_ideology==0 & voted_incumbent2012==1;
replace challenger_A = "MORENA" 
	if P6==13 & incumbent_ideology==0 & voted_incumbent2012==1;
replace challenger_A = "MORENA" 
	if P6A=="HUBIERA SIDO MORENA" & incumbent_ideology==0 & voted_incumbent2012==1;
#delimit cr

*Fourth, if they answer none to the the alternative party 
*	they would have voted for in 2012, 
*	the 2nd largest party in the municipality in terms of votes.

replace challenger_A = "PAN" if uniqueid==11004 & challenger_A==""
replace challenger_A = "PAN" if uniqueid==11014 & challenger_A==""
replace challenger_A = "PAN" if uniqueid==11015 & challenger_A==""
replace challenger_A = "PAN" if uniqueid==11023 & challenger_A==""
replace challenger_A = "PRI" if uniqueid==11025 & challenger_A==""
replace challenger_A = "PRI" if uniqueid==11027 & challenger_A==""
replace challenger_A = "PRI" if uniqueid==11035 & challenger_A==""
replace challenger_A = "PRI_PVEM_PANAL" if uniqueid==15013 & challenger_A==""
replace challenger_A = "PAN" if uniqueid==15014 & challenger_A==""
replace challenger_A = "PRD" if uniqueid==15031 & challenger_A==""
replace challenger_A = "PRD" if uniqueid==15121 & challenger_A==""
replace challenger_A = "PRD_PT_PC" if uniqueid==15033 & challenger_A==""
replace challenger_A = "PAN" if uniqueid==15039 & challenger_A==""
replace challenger_A = "PRD" if uniqueid==15070 & challenger_A==""
replace challenger_A = "PRI_PVEM_PANAL" if uniqueid==15058 & challenger_A==""
replace challenger_A = "PAN" if uniqueid==15060 & challenger_A==""
replace challenger_A = "PAN" if uniqueid==15087 & challenger_A==""
replace challenger_A = "PRD_MC" if uniqueid==15104 & challenger_A==""
replace challenger_A = "PAN" if uniqueid==15106 & challenger_A==""
replace challenger_A = "PRD" if uniqueid==15109 & challenger_A==""
replace challenger_A = "PRI_PVEM_PANAL" if uniqueid==15122 & challenger_A==""
replace challenger_A = "PAN" if uniqueid==15114 & challenger_A==""
replace challenger_A = "PRI_PANAL" if uniqueid==22011 & challenger_A==""
replace challenger_A = "PAN" if uniqueid==24053 & challenger_A==""
replace challenger_A = "PAN" if uniqueid==24032 & challenger_A==""
replace challenger_A = "PC" if uniqueid==24036 & challenger_A==""
replace challenger_A = "PAN" if uniqueid==24054 & challenger_A==""

	
***************
* CHALLENGER_B
***************
*B) The 2nd largest party in the municipality in terms of votes.
*%Q: here the vote share is from 2012 
*--otw, 2nd party might be the incumbent

g challenger_B = ""
replace challenger_B = "PAN" if uniqueid==11004
replace challenger_B = "PAN" if uniqueid==11014 
replace challenger_B = "PAN" if uniqueid==11015 
replace challenger_B = "PAN" if uniqueid==11023 
replace challenger_B = "PRI" if uniqueid==11025 
replace challenger_B = "PRI" if uniqueid==11027 
replace challenger_B = "PRI" if uniqueid==11035 
replace challenger_B = "PRI_PVEM_PANAL" if uniqueid==15013 
replace challenger_B = "PAN" if uniqueid==15014
replace challenger_B = "PRD" if uniqueid==15031 
replace challenger_B = "PRD" if uniqueid==15121 
replace challenger_B = "PRD_PT_PC" if uniqueid==15033
replace challenger_B = "PAN" if uniqueid==15039 
replace challenger_B = "PRD" if uniqueid==15070 
replace challenger_B = "PRI_PVEM_PANAL" if uniqueid==15058 
replace challenger_B = "PAN" if uniqueid==15060 
replace challenger_B = "PAN" if uniqueid==15087
replace challenger_B = "PRD_MC" if uniqueid==15104 
replace challenger_B = "PAN" if uniqueid==15106 
replace challenger_B = "PRD" if uniqueid==15109 
replace challenger_B = "PRI_PVEM_PANAL" if uniqueid==15122 
replace challenger_B = "PAN" if uniqueid==15114 
replace challenger_B = "PRI_PANAL" if uniqueid==22011 
replace challenger_B = "PAN" if uniqueid==24053 
replace challenger_B = "PAN" if uniqueid==24032 
replace challenger_B = "PC" if uniqueid==24036 
replace challenger_B = "PAN" if uniqueid==24054 


***************
* CHALLENGER_C
***************

*C) Average
}
*


*************************************************************************;
* OTHER OUTCOMES
*************************************************************************;


*** Outcome = Vote Confidence [higher more confidence]
gen vconfidence2015 = 5 - P1C if P1C<=4 & P1C!=.
gen vconfidence2012 = 5 - P5C if P5C<=4 & P5C!=.

gen d_confidence_same = .
replace d_confidence_same = vconfidence2015 - vconfidence2012 if nochange1215==1

g d_conf_same_pos = (d_confidence_same>0) if !missing(d_confidence_same)
g d_conf_same_neg = (d_confidence_same<0) if !missing(d_confidence_same)

gen d_confidence_change = .
replace d_confidence_change = vconfidence2015 - vconfidence2012 if nochange1215!=1

g d_conf_change_pos = (d_confidence_change>0) if !missing(d_confidence_change)
g d_conf_change_neg = (d_confidence_change<0) if !missing(d_confidence_change)



*** Outcome = Party Perceptions

*[higher worst]
g perception_level_PRI = P9PRI if P9PRI<88
replace perception_level_PRI = 6 - perception_level_PRI if QSTIPP=="P"
g perception_level_PAN = P9PAN if P9PAN<88
replace perception_level_PAN = 6 - perception_level_PAN if QSTIPP=="P"
g perception_level_PRD = P9PRD if P9PRD<88
replace perception_level_PRD = 6 - perception_level_PRD if QSTIPP=="P"

g perception_level_PRI_m = (P9PRI>=88)
g perception_level_PAN_m = (P9PAN>=88)
g perception_level_PRD_m = (P9PRD>=88)

*STRENGTH OF POSTERIOR, INCUMBENT [higher: more secure]
g perception_sure_PRI = 5 - P10PRI if P10PRI<88
g perception_sure_PAN = 5 - P10PAN if P10PAN<88
g perception_sure_PRD = 5 - P10PRD if P10PRD<88

g strength_i = .
replace strength_i = perception_sure_PRI if incumbent=="PRI"
replace strength_i = perception_sure_PRI if incumbent=="PRI_PVEM"
replace strength_i = perception_sure_PRI if incumbent=="PRI_PVEM_PANAL"
replace strength_i = perception_sure_PAN if incumbent=="PAN"
replace strength_i = perception_sure_PAN if incumbent=="PAN_PANAL"
replace strength_i = perception_sure_PRD if incumbent=="PRD"
replace strength_i = P10EXT if P10EXT<88 & P10EXT!=. & incumbent=="PC" & P7A==7
replace strength_i = P10EXT if P10EXT<88 & P10EXT!=. & incumbent=="PC" & P8B==7


*PERCEPTION CORRUPTION/LACK OF INTEREST [posterior] INCUMBENT [higher:worst]
g perception_level_incumbent = .
replace perception_level_incumbent = perception_level_PRI if incumbent=="PRI"
replace perception_level_incumbent = perception_level_PRI if incumbent=="PRI_PVEM"
replace perception_level_incumbent = perception_level_PRI if incumbent=="PRI_PVEM_PANAL"
replace perception_level_incumbent = perception_level_PAN if incumbent=="PAN"
replace perception_level_incumbent = perception_level_PAN if incumbent=="PAN_PANAL"
replace perception_level_incumbent = perception_level_PRD if incumbent=="PRD"
replace perception_level_incumbent = P9EXT if P9EXT<88 & P9EXT!=. & incumbent=="PC" & P7A==7 & QSTIPP=="C"
replace perception_level_incumbent = P9EXT if P9EXT<88 & P9EXT!=. & incumbent=="PC" & P8B==7 & QSTIPP=="C"
replace perception_level_incumbent = 6 - P9EXT if P9EXT<88 & P9EXT!=. & incumbent=="PC" & P7A==7 & QSTIPP=="P"
replace perception_level_incumbent = 6 - P9EXT if P9EXT<88 & P9EXT!=. & incumbent=="PC" & P8B==7 & QSTIPP=="P"
*Rescale to -2 to 2
replace perception_level_incumbent = perception_level_incumbent - 3
rename perception_level_incumbent posterior_i

*GEN NEWS (based on P11)
g perception_change_PRI = P11PRI if P11PRI<88
replace perception_change_PRI = 6 - perception_change_PRI if QSTIPP=="P"
g perception_change_PAN = P11PAN if P11PAN<88
replace perception_change_PAN = 6 - perception_change_PAN if QSTIPP=="P"
g perception_change_PRD = P11PRD if P11PRD<88
replace perception_change_PRD = 6 - perception_change_PRD if QSTIPP=="P"

g perception_change_PRI_m = (P11PRI>=88)
g perception_change_PAN_m = (P11PAN>=88)
g perception_change_PRD_m = (P11PRD>=88)

*CHANGE OF CORRUPTION/LACK OF INTEREST INCUMBENT [higher worst]
g perception_change_incumbent = .
replace perception_change_incumbent = perception_change_PRI if incumbent=="PRI"
replace perception_change_incumbent = perception_change_PRI if incumbent=="PRI_PVEM"
replace perception_change_incumbent = perception_change_PRI if incumbent=="PRI_PVEM_PANAL"
replace perception_change_incumbent = perception_change_PAN if incumbent=="PAN"
replace perception_change_incumbent = perception_change_PAN if incumbent=="PAN_PANAL"
replace perception_change_incumbent = perception_change_PRD if incumbent=="PRD"
replace perception_change_incumbent = P9EXT if P9EXT<88 & P9EXT!=. & incumbent=="PC" & P7A==7 & QSTIPP=="C"
replace perception_change_incumbent = P9EXT if P9EXT<88 & P9EXT!=. & incumbent=="PC" & P8B==7 & QSTIPP=="C"
replace perception_change_incumbent = 6 - P9EXT if P9EXT<88 & P9EXT!=. & incumbent=="PC" & P7A==7 & QSTIPP=="P"
replace perception_change_incumbent = 6 - P9EXT if P9EXT<88 & P9EXT!=. & incumbent=="PC" & P8B==7 & QSTIPP=="P"

*Rescale to -2 to 2
replace perception_change_incumbent = perception_change_incumbent - 3
rename perception_change_incumbent change_i

*	Different measures
g good_i = (change_i<0) if !missing(change_i)
g bad_i = (change_i>0) if !missing(change_i)

*	[bad -1, neutral 0, good +1]
g news_bng_i = .
replace news_bng_i = 1 if change_i<0 & change_i!=.
replace news_bng_i = 0 if change_i==0 & change_i!=.
replace news_bng_i = -1 if change_i>0 & change_i!=.

*PRIORs
g prior_i = . /*[higher is worse]*/
replace prior_i = posterior_i - change_i
*MAKING PRIORS INTO A -2+2 SCALE
replace prior_i = -2 if prior_i<-2 & prior_i!=.
replace prior_i = 2 if prior_i>2 & prior_i!=.





*************************************************************************;
*PERCEPTION CORRUP/LACK OF INTEREST [posterior] CHALLENGER [higher:worst]
*************************************************************************;


***************
* CHALLENGER_A == posterior_cA
***************
g posterior_cA = .
replace posterior_cA = perception_level_PRI if challenger_A=="PRI"
replace posterior_cA = perception_level_PRI if challenger_A=="PRI_PVEM_PANAL"
replace posterior_cA = perception_level_PRI if challenger_A=="PRI_PANAL"
replace posterior_cA = perception_level_PAN if challenger_A=="PAN"
replace posterior_cA = perception_level_PRD if challenger_A=="PRD"
replace posterior_cA = perception_level_PRD if challenger_A=="PRD_MC"
replace posterior_cA = perception_level_PRD if challenger_A=="PRD_PT_PC"
replace posterior_cA = P9EXT if P9EXT<88 & P9EXT!=. & QSTIPP=="C" & incumbent!="PC"
replace posterior_cA = P9EXT if P9EXT<88 & P9EXT!=. & QSTIPP=="C" & incumbent!="PC"
replace posterior_cA = 6 - P9EXT if P9EXT<88 & P9EXT!=. & QSTIPP=="P" & incumbent!="PC"
replace posterior_cA = 6 - P9EXT if P9EXT<88 & P9EXT!=. & QSTIPP=="P" & incumbent!="PC"
*Rescale to -2 to 2
replace posterior_cA = posterior_cA - 3
*Turning missing posterior to zero
replace posterior_cA = 0 if posterior_cA==.

*strenght
g strength_cA = .
replace strength_cA = perception_sure_PRI if challenger_A=="PRI"
replace strength_cA = perception_sure_PRI if challenger_A=="PRI_PVEM_PANAL"
replace strength_cA = perception_sure_PRI if challenger_A=="PRI_PANAL"
replace strength_cA = perception_sure_PAN if challenger_A=="PAN"
replace strength_cA = perception_sure_PRD if challenger_A=="PRD"
replace strength_cA = perception_sure_PRD if challenger_A=="PRD_MC"
replace strength_cA = perception_sure_PRD if challenger_A=="PRD_PT_PC"
replace strength_cA = P10EXT if P10EXT<88 & P10EXT!=. & incumbent!="PC"



***************
* CHALLENGER_B == posterior_cB
***************
g posterior_cB = .
replace posterior_cB = perception_level_PRI if challenger_B=="PRI"
replace posterior_cB = perception_level_PRI if challenger_B=="PRI_PVEM_PANAL"
replace posterior_cB = perception_level_PRI if challenger_B=="PRI_PANAL"
replace posterior_cB = perception_level_PAN if challenger_B=="PAN"
replace posterior_cB = perception_level_PRD if challenger_B=="PRD"
replace posterior_cB = perception_level_PRD if challenger_B=="PRD_MC"
replace posterior_cB = perception_level_PRD if challenger_B=="PRD_PT_PC"
replace posterior_cB = P9EXT if P9EXT<88 & P9EXT!=. & QSTIPP=="C" & incumbent!="PC"
replace posterior_cB = P9EXT if P9EXT<88 & P9EXT!=. & QSTIPP=="C" & incumbent!="PC"
replace posterior_cB = 6 - P9EXT if P9EXT<88 & P9EXT!=. & QSTIPP=="P" & incumbent!="PC"
replace posterior_cB = 6 - P9EXT if P9EXT<88 & P9EXT!=. & QSTIPP=="P" & incumbent!="PC"
*Rescale to -2 to 2
replace posterior_cB = posterior_cB - 3
*Turning missing posterior to zero
replace posterior_cB = 0 if posterior_cB==.

*strength
g strength_cB = .
replace strength_cB = perception_sure_PRI if challenger_B=="PRI"
replace strength_cB = perception_sure_PRI if challenger_B=="PRI_PVEM_PANAL"
replace strength_cB = perception_sure_PRI if challenger_B=="PRI_PANAL"
replace strength_cB = perception_sure_PAN if challenger_B=="PAN"
replace strength_cB = perception_sure_PRD if challenger_B=="PRD"
replace strength_cB = perception_sure_PRD if challenger_B=="PRD_MC"
replace strength_cB = perception_sure_PRD if challenger_B=="PRD_PT_PC"
replace strength_cB = P10EXT if P10EXT<88 & P10EXT!=. & incumbent!="PC"

***************
* CHALLENGER_C == posterior_cC
***************
#delimit ;
egen v1 = rowmean(perception_level_PAN perception_level_PRD)
	if incumbent=="PRI"	| incumbent=="PRI_PVEM" | incumbent=="PRI_PVEM_PANAL";
egen v2 = rowmean(perception_level_PRI perception_level_PRD)
	if incumbent=="PAN" | incumbent=="PAN_PANAL";
egen v3 = rowmean(perception_level_PRI perception_level_PAN)
	if incumbent=="PRD";
egen v4 = rowmean(perception_level_PRI perception_level_PAN perception_level_PRD)
	if incumbent=="PC";
#delimit cr

g posterior_cC = .
replace posterior_cC = v1 if incumbent=="PRI"	| incumbent=="PRI_PVEM" | incumbent=="PRI_PVEM_PANAL"
replace posterior_cC = v2 if incumbent=="PAN" | incumbent=="PAN_PANAL"
replace posterior_cC = v3 if incumbent=="PRD"
replace posterior_cC = v4 if incumbent=="PC"
drop v1 v2 v3 v4
*rescale to -2 - 2
replace posterior_cC = posterior_cC - 3
*posterior to missing
replace posterior_cC = 0 if posterior_cC==.

*strenght
#delimit ;
egen v1 = rowmean(perception_sure_PAN perception_sure_PRD)
	if incumbent=="PRI"	| incumbent=="PRI_PVEM" | incumbent=="PRI_PVEM_PANAL";
egen v2 = rowmean(perception_sure_PRI perception_sure_PRD)
	if incumbent=="PAN" | incumbent=="PAN_PANAL";
egen v3 = rowmean(perception_sure_PRI perception_sure_PAN)
	if incumbent=="PRD";
egen v4 = rowmean(perception_sure_PRI perception_sure_PAN perception_sure_PRD)
	if incumbent=="PC";
#delimit cr

g strength_cC = .
replace strength_cC = v1 if incumbent=="PRI" | incumbent=="PRI_PVEM" | incumbent=="PRI_PVEM_PANAL"
replace strength_cC = v2 if incumbent=="PAN" | incumbent=="PAN_PANAL"
replace strength_cC = v3 if incumbent=="PRD"
replace strength_cC = v4 if incumbent=="PC"
drop v1 v2 v3 v4







*************************************************************************;
*Turning missing posterior/change/prior to zero, 
/*which makes sense since not knowing is closer to a flat posterior.*/
*************************************************************************;

gen posterior_i2 = posterior_i
replace posterior_i2 = 0 if posterior_i2==.

gen change_i2 = change_i
replace change_i2 = 0 if change_i==.

gen prior_i2 = prior_i
replace prior_i2 = 0 if prior_i==.
*************************************************************************;




*** Outcome = Corr Type Same [larger is higher]
gen corr_same = . 
replace corr_same = PC12 if PC12<88 & PC12!=.
replace corr_same = PP12 if PP12<88 & PP12!=.

*** Outcome = Corr Type Other [larger is higher]
gen corr_other = . 
replace corr_other = PC13 if PC13<88 & PC13!=.
replace corr_other = PP13 if PP13<88 & PP13!=.

*** Outcome = Info Consumption
*16a.¿Usted recuerda si usted o alguien de su hogar recibió esta información
*en un tríptico como este?
g leaflet_remember = 0
replace leaflet_remember = 1 if PC16A==1
replace leaflet_remember = 1 if PP16A==1

g leaflet_remember_read = 0
replace leaflet_remember_read = 1 if P17A==1

g leaflet_benchmark = 0 
replace leaflet_benchmark = 1 if P19==2

*Other people received the leaflet [higher is more]
g leaflet_others = P22 if P22<88

g loudspeaker_remember = 0
replace loudspeaker_remember = 1 if P23A==1

*[higher is more] LOTS OF MISSINGNESS
g loudspeaker_others = P23B if P23B<88

*¿Cambió su decisión de voto como consecuencia de la información presentada
*en los trípticos?
g leaflet_vote = 0
replace leaflet_vote = 1 if P20==1

*** Outcome = Vote Buying
*[higher is more frequent]
g vote_buying = P42 if P42<88

g vote_buying_m = (P42>=88)

*Block-level measure [to test H8 EGAP CHAPTER]
bysort id_block: egen avgposterior = mean(vote_buying) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g vote_buying_block = avgposterior2
drop avgposterior*




*** Outcome = Politician Reactions
recode PC24A_1 PC24A_2 PC24A_3 PC24A_4 PC24A_5 PP24A_1 PP24A_2 PP24A_3 PP24A_4 PP24A_5 PC25A_1 PC25A_2 PC25A_3 PC25A_4 PC25A_5 PP25A_1 PP25A_2 PP25A_3 PP25A_4 PP25A_5 (9 = .) (2 = 0)

egen incumbent_reaction = rowtotal(PC24A_1 PC24A_2 PC24A_3 PC24A_4 PC24A_5 PP24A_1 PP24A_2 PP24A_3 PP24A_4 PP24A_5)

egen challenger_reaction = rowtotal(PC25A_1 PC25A_2 PC25A_3 PC25A_4 PC25A_5 PP25A_1 PP25A_2 PP25A_3 PP25A_4 PP25A_5)

gen incumbent_reaction_neg = .
replace incumbent_reaction_neg = 1 if P24B>=1 & P24B<=4
replace incumbent_reaction_neg = 0 if P24B==2

gen challenger_reaction_pos = .
replace challenger_reaction_pos = 0 if P25B>=1 & P24B<=4
replace challenger_reaction_pos = 1 if P25B==2
 
*** Outcome = Social Transmission
g social_discussion = .
replace social_discussion = 1 if P26==1
replace social_discussion = 1 if P26==2
replace social_discussion = 0 if P26==3


g social_coordination = 0
replace social_coordination = 1 if P27==1

*Change vote as a result of these discussions?
g social_vote = 0
replace social_vote = 1 if P28A==1

*Porque pensó que otros cambiarían su decisión de voto
g social_otherschange = .
replace social_otherschange = 1 if P28B_3==1
replace social_otherschange = 0 if P28B_3==2


*************************************************************************;
***	Qs FROM THE END OF THE SURVEY
*************************************************************************;

g posterior_incumbent43 = . /* [higher is worse]*/
replace posterior_incumbent43 = PC43 if PC43<88 & PC43!=.
replace posterior_incumbent43 = 6 - PP43 if PP43<88 & PP43!=.
replace posterior_incumbent43 = posterior_incumbent43 - 3
*as per flat argument: replace . with 0
replace posterior_incumbent43 = 0 if posterior_incumbent43==.


gen change_incumbent44 = . /*[higher is worse]*/
replace change_incumbent44 = 3 - PC44 if PC44<88 & PC44!=.
replace change_incumbent44 = PP44 - 3 if PP44<88 & PP44!=.
*as per flat argument: replace . with 0
replace change_incumbent44 = 0 if change_incumbent44==.


g prior_incumbent4344 = 0 /*[higher is worse]*/
replace prior_incumbent4344 = posterior_incumbent43 - change_incumbent44
*MAKING PRIORS INTO A -2+2 SCALE
g prior_incumbent43445 = prior_incumbent4344
replace prior_incumbent43445 = -2 if prior_incumbent4344<-2 & prior_incumbent4344!=.
replace prior_incumbent43445 = 2 if prior_incumbent4344>2 & prior_incumbent4344!=.
*as per flat argument: replace . with 0
replace prior_incumbent43445 = 0 if prior_incumbent43445==.


g good_inc44 = (change_incumbent44<0) if !missing(change_incumbent44)
g bad_inc44 = (change_incumbent44>0) if !missing(change_incumbent44)

*[bad -1, neutral 0, good +1]
g news_bng44 = .
replace news_bng44 = 1 if change_incumbent44<0 & change_incumbent44!=.
replace news_bng44 = 0 if change_incumbent44==0 & change_incumbent44!=.
replace news_bng44 = -1 if change_incumbent44>0 & change_incumbent44!=.


***	CHALLENGER
g posterior_challenger45 = . /*[higher is worse]*/
replace posterior_challenger45 = PC45 if PC45<88 & PC45!=.
replace posterior_challenger45 = 6 - PP45 if PP45<88 & PP45!=.
replace posterior_challenger45 = posterior_challenger45 - 3

gen change_challenger46 = . /*[higher is worse]*/
replace change_challenger46 = 3 - PC46 if PC46<88 & PC46!=.
replace change_challenger46 = PP46 - 3 if PP46<88 & PP46!=.

g prior_challenger4546 = .
replace prior_challenger4546 = posterior_challenger45 - change_challenger46
*MAKING PRIORS INTO A -2+2 SCALE
g prior_challenger45465 = prior_challenger4546
replace prior_challenger45465 = -2 if prior_challenger4546<-2 & prior_challenger4546!=.
replace prior_challenger45465 = 2 if prior_challenger4546>2 & prior_challenger4546!=.


g good_chal46 = (change_challenger46<0) if !missing(change_challenger46)
g bad_chal46 = (change_challenger46>0) if !missing(change_challenger46)


*************************************************************************;
*CREATING PRIORS FOR FROM CONTROL AREAS
*Avg Posterior of Control -> Avg Prior of Treated
*************************************************************************;

*Prior 3 -- using municipality data
bysort uniqueid: egen avgposterior = mean(posterior_i2) if T==0
bysort uniqueid: egen avgposterior2 = mean(avgposterior)
g prior_i3 = avgposterior2
drop avgposterior*
label var prior_i3 "Prior"

*Prior 4 -- using block data
bysort id_block: egen avgposterior = mean(posterior_i2) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g prior_i4 = avgposterior2
drop avgposterior*
label var prior_i4 "Prior"

*Prior 5 --using block data and Q43
bysort id_block: egen avgposterior = mean(prior_incumbent43445) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g prior_i5 = avgposterior2
drop avgposterior*
label var prior_i5 "Prior"

*Priros using individual level predicted values
global covariates "female age education employed turnout_lag"

*Prior i3
areg posterior_i2 $covariates if T==0, a(uniqueid) cluster(cluster2)
predict yhat2 if T!=0
g prior_i3i = posterior_i2 if T==0
replace prior_i3i = yhat2 if prior_i3i==. & T!=0
drop yhat*
bysort id_block: egen prior_i3i3 = mean(prior_i3i)

*Prior i4
areg posterior_i2 $covariates if T==0, a(id_block) cluster(cluster2)
predict yhat2 if T!=0
g prior_i4i = posterior_i2 if T==0
replace prior_i4i = yhat2 if prior_i4i==. & T!=0
drop yhat*
bysort id_block: egen prior_i4i4 = mean(prior_i4i)

*Prior i5
areg prior_incumbent43445 $covariates if T==0, a(id_block) cluster(cluster2)
predict yhat2 if T!=0
g prior_i5i = prior_incumbent43445 if T==0
replace prior_i5i = yhat2 if prior_i5i==. & T!=0
drop yhat*
bysort id_block: egen prior_i5i5 = mean(prior_i5i)




*Strength 3 -- using municipal data
bysort uniqueid: egen avgposterior = mean(strength_i) if T==0
bysort uniqueid: egen avgposterior2 = mean(avgposterior)
g strength_i3 = avgposterior2
drop avgposterior*

*Strength 4 -- using block data
bysort id_block: egen avgposterior = mean(strength_i) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g strength_i4 = avgposterior2
drop avgposterior*

*NEW CHANGE -- using municipal data
bysort uniqueid: egen avgposterior = mean(posterior_incumbent43) if T==0
bysort uniqueid: egen avgposterior2 = mean(avgposterior)
g newchange_i3 = avgposterior2 - prior_i3
drop avgposterior*
label var newchange_i3 "Change"

*NEW CHANGE -- using block data
bysort id_block: egen avgposterior = mean(posterior_incumbent43) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g newchange_i4 = avgposterior2 - prior_i4
drop avgposterior*
label var newchange_i4 "Change"

*NEW CHANGE -- using municipal data
bysort uniqueid: egen avgposterior = mean(posterior_incumbent43) if T==0
bysort uniqueid: egen avgposterior2 = mean(avgposterior)
g newchange_i3i3 = avgposterior2 - prior_i3i3
drop avgposterior*

*NEW CHANGE -- using block data
bysort id_block: egen avgposterior = mean(posterior_incumbent43) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g newchange_i4i4 = avgposterior2 - prior_i4i4
drop avgposterior*

*NEW CHANGE -- using block data
bysort id_block: egen avgposterior = mean(posterior_incumbent43) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g newchange_i5i5 = avgposterior2 - prior_i5i5
drop avgposterior*



*PARTISANSHIP
bysort id_block: egen avgposterior = mean(partisanship_i) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g partisanship_i4 = avgposterior2
drop avgposterior*
label var partisanship_i4 "Block Level Incumbent Partisanship"

*Other measures of partisanship -- less continous
bysort id_block: egen avgposterior = mean(partisanship012) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g partisanship_i012 = avgposterior2
drop avgposterior*

bysort id_block: egen avgposterior = mean(partisanship01) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g partisanship_i01 = avgposterior2
drop avgposterior*




*************************************************************************;
* CREATE prior, posterior, newchange FOR CHALLENGER
*************************************************************************;

*Avg Posterior of Control -> Avg Prior of Treated (using block data)

*Prior A
bysort id_block: egen avgposterior = mean(posterior_cA) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g prior_cA = avgposterior2
drop avgposterior*

*Prior B
bysort id_block: egen avgposterior = mean(posterior_cB) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g prior_cB = avgposterior2
drop avgposterior*

*Prior C 
bysort id_block: egen avgposterior = mean(posterior_cC) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g prior_cC = avgposterior2
drop avgposterior*

*New Change C
bysort id_block: egen avgposterior = mean(posterior_challenger45) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g newchangeCA = avgposterior2 - prior_cA
g newchangeCB = avgposterior2 - prior_cB
g newchangeCC = avgposterior2 - prior_cC
drop avgposterior*


*Strength for challengers -- using block data
*Challenger A
bysort id_block: egen avgposterior = mean(strength_cA) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g strength_c4A = avgposterior2
drop avgposterior*

*Challenger B
bysort id_block: egen avgposterior = mean(strength_cB) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g strength_c4B = avgposterior2
drop avgposterior*

*Challenger C
bysort id_block: egen avgposterior = mean(strength_cC) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g strength_c4C = avgposterior2
drop avgposterior*




*************************************************************************;
* GOOD OR BAD NEWS
*************************************************************************;

capture g good_i4 = newchange_i4<=-0.5 if newchange_i4!=.
capture g bad_i4 = newchange_i4>=0.5 if newchange_i4!=.
capture g good_i3 = newchange_i3<=-0.5 if newchange_i3!=.
capture g bad_i3 = newchange_i3>=0.5 if newchange_i3!=.

foreach x in A B C {
capture g good_C`x' = newchangeC`x'<=-0.5 if newchangeC`x'!=.
capture g bad_C`x' = newchangeC`x'>=0.5 if newchangeC`x'!=.
}

cap drop good_news
g good_news_i4 = newchange_i4<0 if newchange_i4!=.




*************************************************************************;
* PROXY FOR PEOPLE CARING ABOUT THE INFORMATION THAT IS PROVIDED
*************************************************************************;

g importance2info = .
replace importance2info = P14_2 if QSTIPP=="C"
replace importance2info = P14_5 if QSTIPP=="P"
replace importance2info = . if importance2info==88
replace importance2info = . if importance2info==99

*importance2infoblock -- using block data
bysort id_block: egen avgposterior = mean(importance2info) if T==0
bysort id_block: egen avgposterior2 = mean(avgposterior)
g importance2infoblock = avgposterior2
drop avgposterior*
label var importance2infoblock "Importance to Information Provided"








*************************************************************************;
* SOME LABELS
*************************************************************************;

label var incumbent_vote_survey "Vote (I)"
label var vconfidence2015 "Vote Conf (I)"

label var iturnout "Turnout"

label var incumbent_ideology "Ideology (I)"
label var d_confidence_same "Conf same"
label var d_confidence_change "Conf chg"

label var posterior_i "Posterior (I)"

label var strength_i "Strength (I)"

label var change_i "Change/News (I)"

label var news_bng_i "BNG News (I)"
label var good_i "Good News (I)"
label var bad_i "Bad News (I)"

label var prior_i "Prior (I)"

label var incumbent_reaction "React (I)"
label var incumbent_reaction_neg "React Neg (I)"

label var challenger_reaction "Chal React"
label var challenger_reaction_pos "Chal React Pos"

label var d_conf_same_pos "Conf same pos"
label var d_conf_same_neg "Conf same neg"
label var d_conf_change_pos "Conf chg pos"
label var d_conf_change_neg "Conf chg neg"

label var female "Female"
label var age "Age"
label var education "Education"
label var income "Income"
label var employed "Employed"

label var intensity_own "Intensity Own"
label var intensity_others "Intensity Others"
label var voted_incumbent2012 "Vote 12 (I)"


*SURVEY_VARIABLES DONE
*************************************************************************;
