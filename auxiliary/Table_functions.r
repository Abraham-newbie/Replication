Table_1<-function(data){
    



df <- data.frame(matrix(ncol = 6, nrow = 0))
x <- c("Number of:","Control","Pooled Treat.","Gender Uniform Treat." ,"Gender Differentiated Treat.","Total")
colnames(df) <- x



#control

df[1,2]<-child_data %>% filter(village_level==1 & pooled_treatment == 0)%>%nrow()

df[2,2]<-child_data %>% filter(census_household_w_child==1 & pooled_treatment == 0)%>% nrow()
df[3,2]<-child_data %>% filter(fu1c_child==1 & fu1c_child_level ==1 & pooled_treatment == 0)%>% nrow()
df[4,2]<-child_data %>% filter(followup_household==1 & pooled_treatment == 0)%>% nrow()
df[5,2]<-child_data %>% filter(fu_child==1 & fu_child_level ==1 & pooled_treatment == 0)%>% nrow()

#pooled treatment
df[1,3]<-child_data %>% filter(village_level==1 & pooled_treatment ==1)%>%nrow()
df[2,3]<-child_data %>% filter(census_household_w_child==1 & pooled_treatment ==1)%>% nrow()
df[3,3]<-child_data %>% filter(fu1c_child==1 & fu1c_child_level ==1 & pooled_treatment == 1)%>% nrow()
df[4,3]<-child_data %>% filter(followup_household==1 & pooled_treatment == 1)%>% nrow()
df[5,3]<-child_data %>% filter(fu_child==1 & fu_child_level ==1 & pooled_treatment == 1)%>% nrow()

#treatment 1

df[1,4]<-child_data %>% filter(village_level==1 & treatment_1 == 1)%>%nrow()
df[2,4]<-child_data %>% filter(census_household_w_child==1 & treatment_1 == 1)%>% nrow()
df[3,4]<-child_data %>% filter(fu1c_child==1 & fu1c_child_level ==1 & treatment_1 == 1)%>% nrow()
df[4,4]<-child_data %>% filter(followup_household==1 & treatment_1 == 1)%>% nrow()
df[5,4]<-child_data %>% filter(fu_child==1 & fu_child_level ==1 & treatment_1  == 1)%>% nrow()

#treatment 2

df[1,5]<-child_data %>% filter(village_level==1 & treatment_2 == 1)%>%nrow()
df[2,5]<-child_data %>% filter(census_household_w_child==1 & treatment_2 == 1)%>% nrow()
df[3,5]<-child_data %>% filter(fu1c_child==1 & fu1c_child_level ==1 & treatment_2 == 1)%>% nrow()
df[4,5]<-child_data %>% filter(followup_household==1 & treatment_2 == 1)%>% nrow()
df[5,5]<-child_data %>% filter(fu_child==1 & fu_child_level ==1 & treatment_2  == 1)%>% nrow()


#all
df[1,6]<-child_data %>% filter(village_level==1)%>%nrow()
df[2,6]<-child_data %>% filter(fu1c_child==1 & fu1c_child_level ==1)%>% nrow()
df[3,6]<-child_data %>% filter(census_household_w_child==1)%>% nrow()
df[4,6]<-child_data %>% filter(followup_household==1)%>% nrow()
df[5,6]<-child_data %>% filter(fu_child_level == 1 & fu_child == 1)%>% nrow

df[,1]<-c("Villages","Baseline Households","Young Children","Households","Young children")





display_html(toString(df %>% 
                    knitr::kable() %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

))
#Outputs do not match output in paper - but exactly matches output from author's own stata codes.
}     
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
data <- data.frame(
  individual=results.df$Characteristics,
 group=c( rep('A', 10), rep('B', 10), rep('C', 10), rep('D', 8)) ,
  value1=results.df$program,
  value2=results.df$private,
  value3=results.df$gov
)

return(data)

}



Table_2 <-function(data)
{

child_data<-data


make_stars <- function(pval) {
  stars = ""
  if(pval <= 0.001)
    stars = "***"
  if(pval > 0.001 & pval <= 0.01)
    stars = "**"
  if(pval > 0.01 & pval <= 0.05)
    stars = "*"
  if(pval > 0.05 & pval <= 0.1)
     stars = "."
  stars
}

i=1

vars<-c("bl_child_age","bl_child_female", "bl_child_in_school","bl_household_size",
        "bl_number_child", "bl_education","bl_farmer")


results.df<-NULL
for (var in vars){
    
     child_data_subset= child_data %>% filter(bl_child_level == 1 & baseline_household == 1 & pooled_treatment == 0)
    
    child_data_subset$control=1
 
         
    full.formula <- as.formula(paste(var, paste("control -1",collapse = ' + '),sep='~')) 
                                           
    obj<-lm(full.formula,data=child_data_subset,weights=child_data_subset$hh_weight)
    
    
    result<-tidy(obj) %>% mutate(signif = sapply(p.value, function(x) make_stars(x)))
    results.df$control[i]<-result$estimate
    results.df$control_se[i]<-result$std.error
    results.df$control_signif[i]<-result$signif
    
    child_data_subset= child_data %>% filter(bl_child_level == 1 & baseline_household == 1)
    
        
    full.formula <- as.formula(paste(var, paste("pooled_treatment",collapse = ' + '),sep='~')) 
                                           
    obj<-lm(full.formula,data=child_data_subset,weights=child_data_subset$hh_weight)
    
    result<-tidy(obj) %>% mutate(signif = sapply(p.value, function(x) make_stars(x)))
    results.df$treat[i]<-result$estimate[2]
    results.df$treat_se[i]<-result$std.error[2]
    results.df$treat_signif[i]<-result$signif[2]
    
    
     i=i+1
    
    }
    
vars<-c("fu_child_age", "fu_female", "bl_child_in_school" ,"fu_child_of_hh_head", "fu_household_size", 
        "fu_num_children", "fu_hh_head_edu" ,"fu_hh_head_occ_farmer","fu_total_land",
        "fu1c_ht_pukka" ,"fu1c_ht_semi_pukka", "fu1c_ht_kaccha" ,"fu1c_ht_thatched_huts",  
        "fu_num_goats" ,"fu_muslim_fiqa_sunni" ,"fu_lang_urdu" ,"fu_lang_sindhi")




r=1
for (var in vars){
    
    child_data_subset= child_data %>% filter(fu_young_child == 1 & fu_child_level == 1 & pooled_treatment == 0)
    
    child_data_subset$control=1
 
         
    full.formula <- as.formula(paste(var, paste("control",collapse = ' + '),sep='~')) 
                                           
    obj<-lm(full.formula,data=child_data_subset,weights=child_data_subset$hh_weight)
    
    
    result<-tidy(obj) %>% mutate(signif = sapply(p.value, function(x) make_stars(x)))
    results.df$fu_control[r]<-result$estimate
    results.df$fu_control_se[r]<-result$std.error
    results.df$fu_control_signif[r]<-result$signif
   
    child_data_subset= child_data %>% filter(fu_young_child == 1 & fu_child_level == 1)
    
        
    full.formula <- as.formula(paste(var, paste("pooled_treatment",collapse = ' + '),sep='~')) 
                                           
    obj<-lm(full.formula,data=child_data_subset,weights=child_data_subset$hh_weight)
    
    result<-tidy(obj) %>% mutate(signif = sapply(p.value, function(x) make_stars(x)))
    results.df$fu_treat[r]<-result$estimate[2]
    results.df$fu_treat_se[r]<-result$std.error[2]
    results.df$fu_treat_signif[r]<-result$signif[2]
    
    
     
    if(r>7){ 
    results.df$control[i]<-NA
    results.df$control_se[i]<-NA
    results.df$control_pvalue[i]<-NA
    
    result<-tidy(obj)
    results.df$treat[i]<-NA
    results.df$treat_se[i]<-NA
    results.df$treat_pvalue[i]<-NA
    
     i=i+1
              
    }
     r=r+1
    
    
    
    }
    


results.df$Characteristics<-c("Child age","Female","Child in School","Child of hh head","Household size","Number of Children",
                               "Household head education","Household head farmer","Total land","Pukka house","Semi-Pukka house",
                               "Kaccha house","Thatched hut","Goat","Sunni","Urdu","Sindhi")




options(knitr.kable.NA = '')

display_html(toString(
data.frame(results.df$Characteristics,results.df$control,results.df$treat,results.df$fu_control,results.df$fu_treat)%>%
    mutate_if(is.numeric, round, digits = 3 )%>%
    mutate(results.df.fu_treat=paste(results.df.fu_treat, results.df$fu_treat_signif))%>%

    mutate(results.df.fu_control=paste(results.df.fu_control,""))%>% 

    knitr::kable(., col.names = c("Characteristics","Baseline Control",
                           "Baseline Treatment - Control",
                           "Followup Control",
                           "Followup Treatment - Control"),align = "l") %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

))




}







Table_7 <-function(data)
{

child_data <-data
make_stars <- function(pval) {
  stars = ""
  if(pval <= 0.001)
    stars = "***"
  if(pval > 0.001 & pval <= 0.01)
    stars = "**"
  if(pval > 0.01 & pval <= 0.05)
    stars = "*"
  if(pval > 0.05 & pval <= 0.1)
     stars = "."
  stars
}




child_data$fu_future_job_civil <- ifelse(child_data$fu_future_job== "Civil servant", 1, 0)

child_data$fu_future_job_doctor <- ifelse(child_data$fu_future_job== "Doctor", 1, 0)

child_data$fu_future_job_pvt <- ifelse(child_data$fu_future_job== "Employed in Private enterprise", 1, 0)


child_data$fu_future_job_engineer <- ifelse(child_data$fu_future_job== "Engineer", 1, 0)

child_data$fu_future_job_farmer <- ifelse(child_data$fu_future_job== "Farmer", 1, 0)

child_data$fu_future_job_housewife <- ifelse(child_data$fu_future_job== "Housewife", 1, 0)


child_data$fu_future_job_laborer<- ifelse(child_data$fu_future_job == "Laborer", 1, 0)

child_data$fu_future_job_landlord<- ifelse(child_data$fu_future_job == "Landlord", 1, 0)

child_data$fu_future_job_police_army<- ifelse(child_data$fu_future_job ==  "Police/army/security", 1, 0)

child_data$fu_future_job_teacher<- ifelse(child_data$fu_future_job == "Teacher", 1, 0)

child_data$fu_future_job_lawyer<- ifelse(child_data$fu_future_job == "Lawyer", 1, 0)

child_data$fu_future_job_raise_livestock<- ifelse(child_data$fu_future_job == "Raise livestock", 1, 0)

child_data$control<- ifelse(child_data$pooled_treatment==1, 0, 1)


vars<-c("fu_future_job_civil", "fu_future_job_doctor", "fu_future_job_pvt" ,"fu_future_job_engineer", "fu_future_job_farmer", 
        "fu_future_job_housewife","fu_future_job_laborer","fu_future_job_landlord","fu_future_job_police_army",
        "fu_future_job_teacher","fu_future_job_lawyer","fu_future_job_raise_livestock","fu_ideal_marriage_age")
        

    

        
results.df<-NULL
lk<-NULL

r=1
for (var in vars){
    child_data_subset =child_data %>% filter(fu_child_level==1 & fu_young_child==1 & control==1)
    
 
         
    full.formula <- as.formula(paste(var, paste("control-1",collapse = ' + '),sep='~')) 
                                           
    obj<-lm(full.formula,data=child_data_subset,weights=child_data_subset$hh_weight)
    
    
    result<-tidy(obj) %>% mutate(signif = sapply(p.value, function(x) make_stars(x)))
    results.df$fu_control[r]<-result$estimate
    results.df$fu_control_se[r]<-result$std.error
    results.df$fu_control_signif[r]<-result$signif
    
                                                 
                                              
    child_data_subset =child_data %>% filter(fu_child_level==1 & fu_young_child==1)
    full.formula <- as.formula(paste(var, paste("pooled_treatment",collapse = ' + '),sep='~')) 
    obj<-lm(full.formula,data=child_data_subset,weights=child_data_subset$hh_weight )
                                                 
    result<-tidy(obj) %>% mutate(signif = sapply(p.value, function(x) make_stars(x)))
    results.df$fu_treat[r]<-result$estimate[2]
    results.df$fu_treat_se[r]<-result$std.error[2]
    results.df$fu_treat_signif[r]<-result$signif[2]
                                                 
    full.formula <- as.formula(paste(var, paste("fu_female + pooled_treatment + treatment_female",collapse = ' + '),sep='~')) 
    obj<-lm(full.formula,data=child_data_subset,weights=child_data_subset$hh_weight )
                                                 
    result<-tidy(obj) %>% mutate(signif = sapply(p.value, function(x) make_stars(x)))
    results.df$fu_female[r]<-result$estimate[2]
    results.df$fu_female_se[r]<-result$std.error[2]
    results.df$fu_female_signif[r]<-result$signif[2]
                                                 
    results.df$fu_treat2[r]<-result$estimate[3]
    results.df$fu_treat2_se[r]<-result$std.error[3]
    results.df$fu_treat2_signif[r]<-result$signif[3]
                                                 
    results.df$fu_treat_female[r]<-result$estimate[4]
    results.df$fu_treat_female_se[r]<-result$std.error[4]
    results.df$fu_treat_female_signif[r]<-result$signif[4]
                                                 
     r=r+1
    }

                          



vars<-c("fu_future_job_civil", "fu_future_job_doctor", "fu_future_job_pvt" ,"fu_future_job_engineer", "fu_future_job_farmer", 
        "fu_future_job_housewife","fu_future_job_laborer","fu_future_job_landlord","fu_future_job_police_army",
        "fu_future_job_teacher","fu_future_job_lawyer","fu_future_job_raise_livestock","fu_ideal_marriage_age")
                                                 
        
results.df$Characteristics<-c("Civil Servant", "Doctor", "Private Enterprise" ,"Engineer", "Farmer", 
        "Housewife","Laborer","Landlord","Police/army/security",
        "Teacher","Lawyer","Raise Livestock","Ideal marriage age")





display_html(toString(
data.frame(results.df$Characteristics,results.df$fu_control,results.df$fu_treat,results.df$fu_female,results.df$fu_treat2,
           results.df$fu_treat_female)%>%
            mutate_if(is.numeric, round, digits = 3 )%>%
            mutate(results.df.fu_treat=paste(results.df.fu_treat,results.df$fu_treat_signif))%>%
            mutate(results.df.fu_female=paste(results.df.fu_female,results.df$fu_female_signif))%>%
            mutate(results.df.fu_treat2=paste(results.df.fu_treat2,results.df$fu_treat2_signif))%>%
            mutate(results.df.fu_treat_female=paste(results.df.fu_treat_female,results.df$fu_treat_female_signif))%>%
            mutate(results.df.fu_control=paste(results.df.fu_control, ""))%>% 
            knitr::kable(., col.names = c("Characteristics","Control",
                           "Treatment - Control",
                           "Female",
                           "Treatment","Treatment x Female"),caption = "Table 7 : Program Impacts on Aspirations") %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

))  
                                                 
                                            
data <-data.frame(results.df$Characteristics,results.df$fu_control,results.df$fu_treat,results.df$fu_female,results.df$fu_treat2,
           results.df$fu_treat_female)
                                            
return(data)
                                                 
}

