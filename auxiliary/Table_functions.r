Table_6<-function(data) {

child_controls = c("control_fu_child_age", "control_fu_female", "missing_fu_child_age", "missing_fu_female")

    
hh_controls = c("control_fu_adults", "control_fu_hh_head_edu","control_fu_hh_head_occ_farmer" ,"control_fu_total_land", 
             "control_fu_household_size" ,"missing_fu_adults", "missing_fu_hh_head_edu" ,"missing_fu_hh_head_occ_farmer",  
             "missing_fu_total_land", "missing_fu_household_size")

independent_vars = c("treatment_1", "treat_1_female" ,"treatment_2","treat_2_female")
district_control =c("factor(bl_district)")


child_data_subset= data %>% filter(fu_child_level == 1 & fu_young_child == 1)

x_vars <- c(independent_vars,child_controls,hh_controls,district_control)


full.formula <- as.formula(paste('fu_child_enrolled', paste(x_vars,collapse = ' + '),sep='~'))

lm_enroll<-lm(full.formula,data=child_data_subset,weights=child_data_subset$hh_weight)

full.formula <- as.formula(paste('total_score_dev', paste(x_vars,collapse = ' + '),sep='~'))

lm_score<-lm(full.formula,data=child_data_subset,weights=child_data_subset$hh_weight)

full.formula <- as.formula(paste('fu_child_highest_grade', paste(x_vars,collapse = ' + '),sep='~'))

lm_grades<-lm(full.formula,data=child_data_subset,weights=child_data_subset$hh_weight)

rob_se <- list(sqrt(diag(vcovHC(lm_enroll, type = "HC1"))),
               sqrt(diag(vcovHC(lm_score, type = "HC1"))),
               sqrt(diag(vcovHC(lm_grades, type = "HC1"))))


display_vars<- c("treatment_1","treat_1_female","treatment_2","treat_2_female")
    
outreg <- capture.output( 
    
    
    stargazer(lm_enroll,lm_grades,lm_score, 
          type = "html", 
          se = rob_se,
          keep=display_vars,
          column.sep.width = "3pt",
          covariate.labels = c("Treatment gender uniform","Treatment gender uniform x Female ",
                               "Treatment gender differentiated","Treatment gender differentiated x Female"
                                          ),
          title = "Gender differential impacts by the subsidy treatment",
          dep.var.labels   = c("Reported Enrollment","Highest Grade attained","Test Scores"),
          model.numbers = FALSE)
    
)
    
    
display_html(toString(outreg))

}



Table_4<-function(data){

child_controls = c("control_fu_child_age", "control_fu_female", "missing_fu_child_age", "missing_fu_female")

hh_controls = c("control_fu_adults", "control_fu_hh_head_edu","control_fu_hh_head_occ_farmer" ,"control_fu_total_land", 
             "control_fu_household_size" ,"missing_fu_adults", "missing_fu_hh_head_edu" ,"missing_fu_hh_head_occ_farmer",  
             "missing_fu_total_land", "missing_fu_household_size")


independent_vars = c("pooled_treatment")
district_control =c("factor(bl_district)")

child_data_subset= child_data %>% filter(fu_child_level == 1 & fu_young_child == 1)

x_vars <- c(child_controls,hh_controls,independent_vars)


score_markers <- c('math_score_dev','urdu_score_dev','total_score_dev')

lm_list <- NULL




i=0 #Model1
    

for (score in score_markers) {
    

i=i+1
x_vars <- c(independent_vars)
full.formula <- as.formula(paste(score, paste(x_vars,collapse = ' + '),sep='~')) 

lm_list[[i]]<-lm(full.formula,data=child_data_subset,weights=child_data_subset$hh_weight)
 
i= i+1 #Model 2


x_vars <- c(independent_vars,child_controls)
full.formula <- as.formula(paste(score, paste(x_vars,collapse = ' + '),sep='~')) 

lm_list[[i]]<-lm(full.formula,data=child_data_subset,weights=child_data_subset$hh_weight)
 
i= i+1  #Model 3



x_vars <- c(independent_vars,child_controls,hh_controls)
full.formula <- as.formula(paste(score, paste(x_vars,collapse = ' + '),sep='~')) 
lm_list[[i]]<-lm(full.formula,data=child_data_subset,weights=child_data_subset$hh_weight)
          
 
i= i+1  #Model 4

x_vars <- c(independent_vars,child_controls,hh_controls,district_control)
full.formula <- as.formula(paste(score, paste(x_vars,collapse = ' + '),sep='~')) 
lm_list[[i]]<-lm(full.formula,data=child_data_subset,weights=child_data_subset$hh_weight)

}

rob_se<-NULL
for (r in 1:i)
    {
    rob_se[[r]]<- sqrt(diag(vcovHC(lm_list[[r]], type = "HC1")))
    }
                    
 
                    
display_vars<- c("pooled_treatment")

outreg <- capture.output( 
        
    stargazer(lm_list,
          type = "html",
          keep=display_vars,
          se=rob_se,
          column.sep.width = "3pt",
          title = "Program Impacts on Test Scores",
          column.labels=c("(1)","(2)","(3)","(4)","(1)","(2)","(3)","(4)","(1)","(2)","(3)","(4)","(1)","(2)","(3)","(4)"),
          dep.var.labels=c("Math Score","Urdu Score","Total Score"),
          align= TRUE,
          add.lines=list(c('Child Controls', 'no','yes','yes','yes','no','yes','yes','yes','no','yes','yes',
                           'yes','no','yes','yes','yes'),
                         c('HH Controls','no','no','yes','yes','no','no','yes','yes',
                            'no','no','yes','yes','no','no','yes','yes'),
                         c('District Fixed Effects','no','no','no','yes','no','no','no','yes',
                            'no','no','no','yes','no','no','no','yes')),
          covariate.labels = c("Pooled Treatment"),
          model.numbers = FALSE
          
             )
    
        )
    
    




display_html(toString(outreg))
}

