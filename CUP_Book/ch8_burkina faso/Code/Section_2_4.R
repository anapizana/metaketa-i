### SECTION 2.4 UNDERSTANDING OF THE INFORMATION CONTENT

## SURVEYOR'S ASSESSMENT OF RESPONDENTS' ATTENTION
# Scale: from 1 (completely bored) to 10 (extremely attentive)
mean(survey$focus) #mean
sd(survey$focus)   #standard deviation
length(survey$focus[is.na(survey$focus)]) #number of missing values

## STUDY PARTICIPANTS' EVALUATION OF THE INFORMATION TREATMENT

# Credibility of the information treatment
credibility<-table(survey$credible_information)
round(100*credibility/sum(credibility),2) #Tabulate percentages

# Usefulness of the information treatment
usefulness<-table(survey$useful_information)
round(100*usefulness/sum(usefulness),2) #Tabulate percentages

# Was the information interesting or boring?
interesting<-table(survey$useful_information)
round(100*interesting/sum(interesting),2) #Tabulate percentages

# Understandability of the information treatment
understandable<-table(survey$understandable_information)
round(100*understandable/sum(understandable),2) #Tabulate percentages

# How many subects find the information useful?
useful<-table(survey$useful_information)
round(100*useful/sum(useful),2) #Tabulate percentages
