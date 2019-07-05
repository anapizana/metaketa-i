
### TABLE 1: COMPREHENSION OF PERFORMANCE INDICATORS & CORRECT RECALL OF INFORMATION CONTENT

indicators <- c("School Water Sources", "School Latrines", "School Supplies", "CEP Admission", 
                "Functioning Water Points", "CSPS Gas Stockouts", "Infant Vaccination Rates", "Assisted Deliveries", 
                "Birth Certificates")


# COLUMN 1: PROPORTION OF RESPONDENTS WHO CORRECTLY UNDERSTOOD PICTOGRAMS

props <- matrix(nrow = 9, ncol = 2)
props[ ,1]<- indicators

props[1, 2] <- round(length(survey$school_water_source_text[survey$school_water_source_text=="Yes"])/sum(!is.na(survey$school_water_source_text)), 2)
props[2, 2] <- round(length(survey$school_latrine_text[survey$school_latrine_text=="Yes"])/sum(!is.na(survey$school_latrine_text)), 2)
props[3, 2] <- round(length(survey$school_supplies_text[survey$school_supplies_text=="Yes"])/sum(!is.na(survey$school_supplies_text)), 2)
props[4, 2] <- round(length(survey$cep_admission_text[survey$cep_admission_text=="Yes"])/sum(!is.na(survey$cep_admission_text)), 2)
props[5, 2] <- round(length(survey$functional_water_source_text[survey$functional_water_source_text=="Yes"])/sum(!is.na(survey$functional_water_source_text)), 2)
props[6, 2] <- round(length(survey$csps_gas_text[survey$csps_gas_text=="Yes"])/sum(!is.na(survey$csps_gas_text)), 2)
props[7, 2] <- round(length(survey$vaccinated_infants_text[survey$vaccinated_infants_text=="Yes"])/sum(!is.na(survey$vaccinated_infants_text)), 2)
props[8, 2] <- round(length(survey$assisted_deliveries_text[survey$assisted_deliveries_text=="Yes"])/sum(!is.na(survey$assisted_deliveries_text)), 2)
props[9, 2] <- round(length(survey$birth_certificate_text[survey$birth_certificate_text=="Yes"])/sum(!is.na(survey$birth_certificate_text)), 2)

colnames(props) <- c("Indicators", "Proportion Correct")
props

# COLUMN 2: PROPORTION OF RESPONDENTS WHO CORRECTLY RECALLED INFORMATION CONTENT AT FIRST ATTEMPT

correct.recall<-NULL
correct.recall[1] <- round(summary(survey$attentiveness_comp_q1)[1]/sum(!is.na(survey$attentiveness_comp_q1)), 2)
correct.recall[2] <- round(summary(survey$attentiveness_comp_q2)[1]/sum(!is.na(survey$attentiveness_comp_q2)), 2)
correct.recall[3] <- round(summary(survey$attentiveness_comp_q3)[1]/sum(!is.na(survey$attentiveness_comp_q3)), 2)
correct.recall[4] <- round(summary(survey$attentiveness_comp_q4)[1]/sum(!is.na(survey$attentiveness_comp_q4)), 2)
correct.recall[5] <- round(summary(survey$attentiveness_comp_q5)[1]/sum(!is.na(survey$attentiveness_comp_q5)), 2)
correct.recall[6] <- round(summary(survey$attentiveness_comp_q6)[1]/sum(!is.na(survey$attentiveness_comp_q6)), 2)
correct.recall[7] <- round(summary(survey$attentiveness_comp_q7)[1]/sum(!is.na(survey$attentiveness_comp_q7)), 2)
correct.recall[8] <- round(summary(survey$attentiveness_comp_q8)[1]/sum(!is.na(survey$attentiveness_comp_q8)), 2)
correct.recall[9] <- round(summary(survey$attentiveness_comp_q9)[1]/sum(!is.na(survey$attentiveness_comp_q9)), 2)

grouped<-cbind(indicators,correct.recall)
grouped

# OUTPUT TEX TABLE

tex("
    \\begin{center}
    \\begin{tabular}{l c | c }
    \\hline
    Indicator&\\multicolumn{2}{c}{Proportion of study participants}\\\\
    &correctly interpreting&correctly recalling\\\\
    &illustrations without&performance information\\\\
    &verbal explanation&at first attempt\\\\
    \\hline
    <<props[1,1]>>&<<props[1,2]>>&<<grouped[1,2]>>\\\\
    <<props[2,1]>>&<<props[2,2]>>&<<grouped[2,2]>>\\\\
    <<props[3,1]>>&<<props[3,2]>>&<<grouped[3,2]>>\\\\
    <<props[4,1]>>&<<props[4,2]>>&<<grouped[4,2]>>\\\\
    <<props[5,1]>>&<<props[5,2]>>&<<grouped[5,2]>>\\\\
    <<props[6,1]>>&<<props[6,2]>>&<<grouped[6,2]>>\\\\
    <<props[7,1]>>&<<props[7,2]>>&<<grouped[7,2]>>\\\\
    <<props[8,1]>>&<<props[8,2]>>&<<grouped[8,2]>>\\\\
    <<props[9,1]>>&<<props[9,2]>>&<<grouped[9,2]>>\\\\
    Observations  &<<length(survey$treatment.info)>>&<<length(survey$treatment.info[survey$treatment.info=='Information'])>>\\\\
    \\hline\\hline
    \\end{tabular}
    \\end{center}
    ", file = "Tables/BF_TableComprehension.tex")

