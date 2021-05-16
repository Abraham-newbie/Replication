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


Table_8 <-function(data){

    
school_data_subset<-data
    
child_controls = c("control_fu_child_age", "control_fu_female", "missing_fu_child_age", "missing_fu_female")

hh_controls = c("control_fu_adults", "control_fu_hh_head_edu","control_fu_hh_head_occ_farmer" ,"control_fu_total_land", 
             "control_fu_household_size" ,"missing_fu_adults", "missing_fu_hh_head_edu" ,"missing_fu_hh_head_occ_farmer",  
             "missing_fu_total_land", "missing_fu_household_size")


independent_vars = c("pooled_treatment")
district_control =c("factor(bl_district)")




school_data_subset= school_data_subset %>% filter(!(pooled_treatment == 0 & school_type_pprs ==1))


school_data_subset$student_teacher_ratio = (school_data_subset$ss5_num_boys+school_data_subset$ss5_num_girls)/
             school_data_subset$ss3_num_teachers
       
char_school_survey <-c("ss2_num_days_operational", "ss2_open_admission"," ss6_uniform_required", "ss6_school_tuition_required", 
           "ss2_language_sindhi", "ss2_language_english")

#char_teacher_school_survey <- c("ss3_num_teachers", "pct_teachers_female", "pct_teachers_postsecondary", "pct_teachers_5years" ,
                   #"pct_teachers_5to10years" ,"pct_teachers_10years", "ss3_avg_absent_2more")
   
char_teacher_school_survey <-c("ss3_num_teachers","ss3_num_teachers_female", "ss3_num_teachers_postsecondary",
                               "ss3_num_teachers_5years", 
                                "ss3_num_teachers_5to10years" ,
                   "ss3_num_teachers_10years","ss3_avg_absent_2more")


char_buildings <-c("ss4_building", "ss4_num_classrooms" ,"ss4_enough_desks" ,"ss4_drinking_water" ,"ss4_electricity" ,
                   "ss4_toilet")
                
char_students <-c( "ss5_num_boys" ,"ss5_num_girls" ,"ss5_pct_students_female"," student_teacher_ratio")

char_teacher_survey <-c("ss9_days_absent"," ss9_female" ,"ss9_age ","ss9_education", "ss9_monthly_salary", "ss9_years_teaching", 
                        "ss9_years_teaching_thischool")

char_hours_teaching <-c("ss9_hours_teaching", "ss9_hrs_teaching_wholeclass" ,"ss9_hrs_teaching_smallgroup" ,
                        "ss9_hrs_teaching_individual",
                        "ss9_hrs_teaching_notes" ,"ss9_hrs_teaching_discipline", "ss9_hrs_teaching_testing" ,
                        "ss9_hrs_teaching_admin")


vars <-c(char_school_survey,char_teacher_school_survey,char_buildings,char_students,char_teacher_survey,char_hours_teaching)




results.df<-NULL

i=1
for (var in vars){
    
    #Calculating means for private schools
    school_data_subset_pprs= school_data_subset %>% filter(child_school_level==1 & fu_young_child==1 &school_type_pprs==1)

       
    full.formula <- as.formula(paste(var, paste("school_type_pprs -1",collapse = ' + '),sep='~')) 
                                           
    obj<-lm(full.formula,data=school_data_subset_pprs,weights=school_data_subset_pprs$hh_weight)
    
    result<-tidy(obj)
    results.df$program[i]<-result$estimate
    
    #Calculating means for goverment schools
    
    school_data_subset_govt= school_data_subset %>% filter(child_school_level==1 & fu_young_child==1 &school_type_govt==1)

       
    full.formula <- as.formula(paste(var, paste("school_type_govt -1",collapse = ' + '),sep='~')) 
                                           
    obj<-lm(full.formula,data=school_data_subset_govt,weights=school_data_subset_govt$hh_weight)
    
    result<-tidy(obj)
    results.df$govt[i]<-result$estimate
    
     #Calculating means for private schools
    
    school_data_subset_private= school_data_subset %>% filter(child_school_level==1 & fu_young_child==1 &school_type_private==1)

       
    full.formula <- as.formula(paste(var, paste("school_type_private -1",collapse = ' + '),sep='~')) 
                                           
    obj<-lm(full.formula,data=school_data_subset_private,weights=school_data_subset_private$hh_weight)
    
    result<-tidy(obj)
    results.df$private[i]<-result$estimate
    
    
    i=i+1
  

}



results.df$prog_minus_priv= results.df$program-results.df$private
results.df$prog_minus_gov= results.df$program-results.df$govt

results.df$Characteristics<-c("Days operational","Open admission","Uniform required","Tuition required","Medium:Sindhi",
                          "Medium:English",
                              
                            "Total Teachers","Female Teachers","Post secondary","<5 yrs Experience",
                          "5-10 yrs Experience",">10 yrs Experience","Avg teacher absent >= 2 days/month",
                              
                          "Building","Number Classrooms",
                          "Sufficient desks","Drinking water","Electricity","Toilet","Number boys","Number girls",
                          "Percent female students","Student Teacher Ratio","Teacher absent days/month","Teacher female ",
                          "Teacher age","Education","Salary(1000 rupees)","Years teaching","Years teaching same school",
                          "Total","Teaching whole class","Teaching small group","Teaching indiviual","Blackboard/dictation",
                           "Classroom management","Testing","Administrative")




length(results.df$prog_minus_priv)

length(results.df$Characteristics)

display_html(toString(
data.frame(results.df$Characteristics,results.df$program,results.df$prog_minus_gov,results.df$prog_minus_priv)%>%
    mutate_if(is.numeric, round, digits = 3 )%>%
    knitr::kable(., col.names = c("Characteristics",
                           "Program Schools",
                           "Program - Govt. Schools",
                           "Program - Pvt. Schools")) %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

))

}
