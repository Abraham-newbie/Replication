##############################################

#IV Robustness Check

########################################





iv_robust_check <-function(data) {
    
child_data=data
child_data_subset= child_data %>% filter(fu_child_level == 1 & fu_young_child == 1)

child_controls = c("control_fu_child_age", "control_fu_female", "missing_fu_child_age", "missing_fu_female")

hh_controls = c("control_fu_adults", "control_fu_hh_head_edu","control_fu_hh_head_occ_farmer" ,"control_fu_total_land", 
             "control_fu_household_size" ,"missing_fu_adults", "missing_fu_hh_head_edu" ,"missing_fu_hh_head_occ_farmer",  
             "missing_fu_total_land", "missing_fu_household_size")



district_control =c("factor(bl_district)")


controls<- c(child_controls,hh_controls,district_control)

score_markers <- c('math_score_dev','urdu_score_dev','total_score_dev')

lm_list <- NULL



i=0 #Model1
i=i+1

ivreg_model<- ivreg(math_score_dev~  fu_child_enrolled | 
                    pooled_treatment, data =child_data_subset)
    
    
lm_list[[i]]<-coeftest(ivreg_model, vcov = vcovHC, type = "HC1")  

i=i+1

ivreg_model<- ivreg(math_score_dev~  fu_child_enrolled + control_fu_child_age+control_fu_female+missing_fu_child_age
                    +missing_fu_female |pooled_treatment + control_fu_child_age+control_fu_female+missing_fu_child_age
                    +missing_fu_female, data =child_data_subset)
    
    
lm_list[[i]]<-coeftest(ivreg_model, vcov = vcovHC, type = "HC1")
    
    
    

i=i+1


ivreg_model<- ivreg(math_score_dev~  fu_child_enrolled + control_fu_child_age+control_fu_female+missing_fu_child_age
                    +missing_fu_female+control_fu_adults+control_fu_hh_head_edu+control_fu_hh_head_occ_farmer+
                    control_fu_total_land+control_fu_household_size+missing_fu_adults+missing_fu_hh_head_edu+
                    missing_fu_hh_head_occ_farmer+missing_fu_total_land+missing_fu_household_size | 
                    pooled_treatment + control_fu_child_age+control_fu_female+missing_fu_child_age
                    +missing_fu_female+control_fu_adults+control_fu_hh_head_edu+control_fu_hh_head_occ_farmer+
                    control_fu_total_land+control_fu_household_size+missing_fu_adults+missing_fu_hh_head_edu+
                    missing_fu_hh_head_occ_farmer+missing_fu_total_land+missing_fu_household_size, data =child_data_subset)
    
    
lm_list[[i]]<-coeftest(ivreg_model, vcov = vcovHC, type = "HC1")




i=i+1


ivreg_model<- ivreg(math_score_dev~  fu_child_enrolled + control_fu_child_age+control_fu_female+missing_fu_child_age
                    +missing_fu_female+control_fu_adults+control_fu_hh_head_edu+control_fu_hh_head_occ_farmer+
                    control_fu_total_land+control_fu_household_size+missing_fu_adults+missing_fu_hh_head_edu+
                    missing_fu_hh_head_occ_farmer+missing_fu_total_land+missing_fu_household_size+factor(bl_district) | 
                    pooled_treatment + control_fu_child_age+control_fu_female+missing_fu_child_age
                    +missing_fu_female+control_fu_adults+control_fu_hh_head_edu+control_fu_hh_head_occ_farmer+
                    control_fu_total_land+control_fu_household_size+missing_fu_adults+missing_fu_hh_head_edu+
                    missing_fu_hh_head_occ_farmer+missing_fu_total_land+missing_fu_household_size+factor(bl_district), data =child_data_subset)
    
    
lm_list[[i]]<-coeftest(ivreg_model, vcov = vcovHC, type = "HC1")





i=i+1

ivreg_model<- ivreg(urdu_score_dev~  fu_child_enrolled | 
                    pooled_treatment, data =child_data_subset)
    
    
lm_list[[i]]<-coeftest(ivreg_model, vcov = vcovHC, type = "HC1")  

i=i+1

ivreg_model<- ivreg(urdu_score_dev~  fu_child_enrolled + control_fu_child_age+control_fu_female+missing_fu_child_age
                    +missing_fu_female |pooled_treatment + control_fu_child_age+control_fu_female+missing_fu_child_age
                    +missing_fu_female, data =child_data_subset)
    
    
lm_list[[i]]<-coeftest(ivreg_model, vcov = vcovHC, type = "HC1")
    
    
    

i=i+1


ivreg_model<- ivreg(urdu_score_dev~  fu_child_enrolled + control_fu_child_age+control_fu_female+missing_fu_child_age
                    +missing_fu_female+control_fu_adults+control_fu_hh_head_edu+control_fu_hh_head_occ_farmer+
                    control_fu_total_land+control_fu_household_size+missing_fu_adults+missing_fu_hh_head_edu+
                    missing_fu_hh_head_occ_farmer+missing_fu_total_land+missing_fu_household_size | 
                    pooled_treatment + control_fu_child_age+control_fu_female+missing_fu_child_age
                    +missing_fu_female+control_fu_adults+control_fu_hh_head_edu+control_fu_hh_head_occ_farmer+
                    control_fu_total_land+control_fu_household_size+missing_fu_adults+missing_fu_hh_head_edu+
                    missing_fu_hh_head_occ_farmer+missing_fu_total_land+missing_fu_household_size, data =child_data_subset)
    
    
lm_list[[i]]<-coeftest(ivreg_model, vcov = vcovHC, type = "HC1")


i=i+1


ivreg_model<- ivreg(urdu_score_dev~  fu_child_enrolled + control_fu_child_age+control_fu_female+missing_fu_child_age
                    +missing_fu_female+control_fu_adults+control_fu_hh_head_edu+control_fu_hh_head_occ_farmer+
                    control_fu_total_land+control_fu_household_size+missing_fu_adults+missing_fu_hh_head_edu+
                    missing_fu_hh_head_occ_farmer+missing_fu_total_land+missing_fu_household_size+factor(bl_district) | 
                    pooled_treatment + control_fu_child_age+control_fu_female+missing_fu_child_age
                    +missing_fu_female+control_fu_adults+control_fu_hh_head_edu+control_fu_hh_head_occ_farmer+
                    control_fu_total_land+control_fu_household_size+missing_fu_adults+missing_fu_hh_head_edu+
                    missing_fu_hh_head_occ_farmer+missing_fu_total_land+missing_fu_household_size+factor(bl_district), data =child_data_subset)
    
    
lm_list[[i]]<-coeftest(ivreg_model, vcov = vcovHC, type = "HC1")

i=i+1

ivreg_model<- ivreg(total_score_dev~  fu_child_enrolled | 
                    pooled_treatment, data =child_data_subset)
    
    
lm_list[[i]]<-coeftest(ivreg_model, vcov = vcovHC, type = "HC1")  

i=i+1

ivreg_model<- ivreg(total_score_dev~  fu_child_enrolled + control_fu_child_age+control_fu_female+missing_fu_child_age
                    +missing_fu_female |pooled_treatment + control_fu_child_age+control_fu_female+missing_fu_child_age
                    +missing_fu_female, data =child_data_subset)
    
    
lm_list[[i]]<-coeftest(ivreg_model, vcov = vcovHC, type = "HC1")
    
    
    

i=i+1


ivreg_model<- ivreg(total_score_dev~  fu_child_enrolled + control_fu_child_age+control_fu_female+missing_fu_child_age
                    +missing_fu_female+control_fu_adults+control_fu_hh_head_edu+control_fu_hh_head_occ_farmer+
                    control_fu_total_land+control_fu_household_size+missing_fu_adults+missing_fu_hh_head_edu+
                    missing_fu_hh_head_occ_farmer+missing_fu_total_land+missing_fu_household_size | 
                    pooled_treatment + control_fu_child_age+control_fu_female+missing_fu_child_age
                    +missing_fu_female+control_fu_adults+control_fu_hh_head_edu+control_fu_hh_head_occ_farmer+
                    control_fu_total_land+control_fu_household_size+missing_fu_adults+missing_fu_hh_head_edu+
                    missing_fu_hh_head_occ_farmer+missing_fu_total_land+missing_fu_household_size, data =child_data_subset)
    
    
lm_list[[i]]<-coeftest(ivreg_model, vcov = vcovHC, type = "HC1")



i=i+1


ivreg_model<- ivreg(total_score_dev~  fu_child_enrolled + control_fu_child_age+control_fu_female+missing_fu_child_age
                    +missing_fu_female+control_fu_adults+control_fu_hh_head_edu+control_fu_hh_head_occ_farmer+
                    control_fu_total_land+control_fu_household_size+missing_fu_adults+missing_fu_hh_head_edu+
                    missing_fu_hh_head_occ_farmer+missing_fu_total_land+missing_fu_household_size+factor(bl_district) | 
                    pooled_treatment + control_fu_child_age+control_fu_female+missing_fu_child_age
                    +missing_fu_female+control_fu_adults+control_fu_hh_head_edu+control_fu_hh_head_occ_farmer+
                    control_fu_total_land+control_fu_household_size+missing_fu_adults+missing_fu_hh_head_edu+
                    missing_fu_hh_head_occ_farmer+missing_fu_total_land+missing_fu_household_size+factor(bl_district), data =child_data_subset)
    
    
lm_list[[i]]<-coeftest(ivreg_model, vcov = vcovHC, type = "HC1")

display_vars<- c("fu_child_enrolled","control_fu_child_age", "control_fu_female"," control_fu_total_land","control_fu_hh_head_edu")

outreg <- capture.output( 
        
    stargazer(lm_list,
          type = "html",
          keep=display_vars,
          column.sep.width = "30pt",
          
          title = "Program Impacts on Test Scores",
          column.labels=c("Math Score","Math Score","Math Score","Math Score","Urdu Score","Urdu Score","Urdu Score","Urdu Score",
                          "Total Score Deviation","Total Score Deviation","Total Score Deviation","Total Score Deviation"),
          align= TRUE,
          covariate.labels = c("Child Enrollment (Instrumented on Pooled Treatment)","Child Age","Female","Education Head of Family"),
           add.lines=list(c('Child Controls', 'no','yes','yes','yes','no','yes','yes','yes','no','yes','yes',
                           'yes','no','yes','yes','yes'),
                         c('HH Controls','no','no','yes','yes','no','no','yes','yes',
                            'no','no','yes','yes','no','no','yes','yes'),
                         c('District Fixed Effects','no','no','no','yes','no','no','no','yes',
                            'no','no','no','yes','no','no','no','yes')),
          model.numbers = FALSE, flip = TRUE
          
             )
    
        )
    





display_html(toString(outreg))

    }
##############################################

#Doube LASSO Robustness Check

########################################





double_lasso_penalization_function<-function(data){
    
    
set.seed(123)

child_data=data

child_controls = c("control_fu_child_age", "control_fu_female", "missing_fu_child_age", "missing_fu_female")

hh_controls = c("control_fu_adults", "control_fu_hh_head_edu","control_fu_hh_head_occ_farmer" ,"control_fu_total_land", 
             "control_fu_household_size" ,"missing_fu_adults", "missing_fu_hh_head_edu" ,"missing_fu_hh_head_occ_farmer",  
             "missing_fu_total_land", "missing_fu_household_size")


dependent_vars = c("pooled_treatment")
district_control =c("factor(bl_district)")

child_data_subset= child_data %>% filter(fu_child_level == 1 & fu_young_child == 1)




x_vars <- c(dependent_vars,child_controls,hh_controls,district_control)


col_vars<-c("fu_child_enrolled",x_vars)
child_data_subset_1 <-child_data_subset [,colnames(child_data_subset )%in%col_vars] 
child_data_subset_1  <-as.data.frame(child_data_subset_1 ) %>% na.omit()

c<-glmnet (as.matrix (child_data_subset_1[ -1] ) , child_data_subset_1[ ,1],standardize=TRUE, alpha =1,label=TRUE)

cv<-cv.glmnet (as.matrix (child_data_subset_1[ -1] ), child_data_subset_1 [,1] , standardize=TRUE, type.measure='mse', nfold  =5,alpha =1)
CF <- as.matrix(coef(cv, cv$lambda.1se))
#print("First Stage : Regressing all independent variables on Outcome")
rel_vars<-NULL
rel_vars[[1]]<- CF[CF!=0,] %>% tidy(1:4) %>% as_tibble()  



x_vars <- c(dependent_vars,child_controls,hh_controls,district_control)

col_vars<-c(x_vars)
child_data_subset_2 <-child_data_subset [,colnames(child_data_subset )%in%col_vars] 

child_data_subset_2 <-as.data.frame(child_data_subset_2 ) %>% na.omit()
c<-glmnet (as.matrix (child_data_subset_2[ -1] ) , child_data_subset_2[ ,1],standardize=TRUE, alpha =1,label=TRUE)
k<-plot(c)

cv<-cv.glmnet (as.matrix (child_data_subset_2[ -11] ), child_data_subset_2 [,1] , standardize=TRUE, type.measure='mse', nfold  =5,alpha =1)
CF <- as.matrix(coef(cv, cv$lambda.1se))

#print("Second Stage : Regressing all independent variables on Main Independent variable of concern (Enrollment Probability)")
rel_vars[[2]]<-CF %>% tidy(1:5,) %>% as_tibble() 
            

return(rel_vars)
    
#If Xi is an effectively randomized treatment, no covariates should be selected in this step.(https://home.uchicago.edu/ourminsky/Variable_Selection.pdf)


    
    
}

##############################################

#Doube LASSO Robustness Check Regression Table

########################################



double_lasso_robustness_check_table<-function(data){
    
    
child_data=data

child_data_subset= child_data %>% filter(fu_child_level == 1 & fu_young_child == 1)



child_controls = c("control_fu_child_age", "control_fu_female", "missing_fu_child_age", "missing_fu_female")

hh_controls = c("control_fu_adults", "control_fu_hh_head_edu","control_fu_hh_head_occ_farmer" ,"control_fu_total_land", 
             "control_fu_household_size" ,"missing_fu_adults", "missing_fu_hh_head_edu" ,"missing_fu_hh_head_occ_farmer",  
             "missing_fu_total_land", "missing_fu_household_size")

independent_vars = c("pooled_treatment")
district_control =c("factor(bl_district)")



double_lasso_x_vars <- c("control_fu_hh_head_edu","control_fu_household_size")

score <- c("fu_child_enrolled")

lm_list <- NULL




i=0 #Model1
    

    


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

 
i= i+1  #Model 5

x_vars <- c(independent_vars,double_lasso_x_vars)
full.formula <- as.formula(paste(score, paste(x_vars,collapse = ' + '),sep='~')) 
lm_list[[i]]<-lm(full.formula,data=child_data_subset,weights=child_data_subset$hh_weight)



rob_se<-NULL
for (r in 1:i)
    {
    rob_se[[r]]<- sqrt(diag(vcovHC(lm_list[[r]], type = "HC1")))
    }
                    
 
                    
display_vars<- c("pooled_treatment","control_fu_child_age", "control_fu_female","control_fu_adults", "control_fu_hh_head_edu"
                 ,"control_fu_total_land", "control_fu_household_size")

outreg <- capture.output( 
        
    stargazer(lm_list,
          type = "html",
          keep=display_vars,
          se=rob_se,
          column.sep.width = "3pt",
          title = "Program Impacts on Test Scores",
          dep.var.labels=c("Child Enrollment"),
          align= TRUE,
             covariate.labels = c("Pooled Treatment","Child Age","Child Female","Adults in Household","Household education",
                               "Total Land","Household Size"),
          add.lines=list(c('Child Controls', 'no','yes','yes','yes','no'),
                         c('HH Controls','no','no','yes','yes','no'),
                         c('District Fixed Effects','no','no','no','yes','no'),
          
          model.numbers = FALSE
          
             )
    
        ))
    
    




display_html(toString(outreg))


}