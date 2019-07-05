
### TABLE 2: DEMOGRAPHICS OF THE STUDY POPULATION AND TESTS OF BALANCE BETWEEN TREATMENT AND CONTROL GROUPS

#Age
age<-signif(summary(lm(age~treatment.info,weights=weight,data=subset(survey,is.na(weight)==FALSE)))$coefficients,2)

#Female
survey$female<-survey$gender=="Woman"
fem<-signif(summary(lm(female~treatment.info,weights=weight,data=subset(survey,is.na(weight)==FALSE)))$coefficients,2)

#Years of education
educ<-signif(summary(lm(edyears~treatment.info,weights=weight,data=subset(survey,is.na(weight)==FALSE)))$coefficients,2)

#Literacy
lit<-signif(summary(lm(as.numeric(literate=="Yes")~treatment.info,weights=weight,data=subset(survey,is.na(weight)==FALSE)))$coefficients,2)

#Voted in 2012
survey$voted.2012<-survey$vote_municipal_elections_2012=="Yes"
voted<-signif(summary(lm(voted.2012~treatment.info,weights=weight,data=subset(survey,is.na(weight)==FALSE)))$coefficients,2)

#Voted for incumbent in 2012
#Indicator of whether respondent voted for the incumbent party in 2012
survey$vote.incumbent.2012<-as.numeric(survey$vote_incumbent_party_mun_2012=="Yes")
#Set vote.incumbent.2012=0 if respondent did not vote in 2012
survey$vote.incumbent.2012[survey$vote_municipal_elections_2012!="Yes"]<-0
#Indicator of voting eligibility in 2012
survey$votingage.2012<-as.numeric(survey$age>=21)
voted.inc<-signif(summary(lm(vote.incumbent.2012~treatment.info,weights=weight,data=subset(survey,is.na(weight)==FALSE)))$coefficients,2)

#Relative living conditions [-2,2]
survey$relative.living.conditions<-as.numeric(survey$living_conditions)-3
livc<-signif(summary(lm(relative.living.conditions~treatment.info,weights=weight,data=subset(survey,is.na(weight)==FALSE)))$coefficients,2)

# OUTPUT TEX TABLE

tex("
    \\begin{center}
    \\begin{tabular}{l c c c}
    \\hline
    &Treatment Mean&Control Mean&P-value Difference\\\\
    \\hline
    Age (years)&<<age[1,1]+age[2,1]>>&<<age[1,1]>>&<<age[2,4]>>\\\\
    Female&<<fem[1,1]+fem[2,1]>>&<<fem[1,1]>>&<<fem[2,4]>>\\\\
    Years of education&<<educ[1,1]+educ[2,1]>>&<<educ[1,1]>>&<<educ[2,4]>>\\\\
    Literate&<<lit[1,1]+lit[2,1]>>&<<lit[1,1]>>&<<lit[2,4]>>\\\\
    Voted in 2012&<<voted[1,1]+voted[2,1]>>&<<voted[1,1]>>&<<voted[2,4]>>\\\\
    Voted for incumbent in 2012&<<voted.inc[1,1]+voted.inc[2,1]>>&<<voted.inc[1,1]>>&<<voted.inc[2,4]>>\\\\
    Relative living conditions $[-2,2]$&<<livc[1,1]+livc[2,1]>>&<<livc[1,1]>>&<<livc[2,4]>>\\\\
    \\hline\\hline
    \\end{tabular}
    \\end{center}
    \\caption{Demographics of the study population and tests of balance between treatment and control group. }
    ", file = "Tables/BF_TableBalance.tex")


