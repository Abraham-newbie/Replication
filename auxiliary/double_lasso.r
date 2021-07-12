
child_data<- read_dta("C:/Users/abrah/Desktop/ose_project/ose-data-science-course-project-Abraham-newbie/data/ReStat_children.dta")


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
