


###########################################################
#### Figure: Randomized Allocation Visualization
###########################################################





rct_plot<-function(data){
    
    



child_data_subset= data %>% filter(bl_child_level == 1 & baseline_household == 1)%>%select(bl_education,bl_household_size,bl_number_child,pooled_treatment)%>%na.omit()

child_data_subset$pooled_treatment=as_factor(child_data_subset$pooled_treatment)
child_data_subset_pool<-child_data_subset[ sample( which(child_data_subset$pooled_treatment==1), round(0.2*length(which(child_data_subset$pooled_treatment==1)))), ]
child_data_subset_control<-child_data_subset[ sample( which(child_data_subset$pooled_treatment==0), round(1*length(which(child_data_subset$pooled_treatment==0)))), ]
child_data_subset<-rbind(child_data_subset_pool,child_data_subset_control)
child_data_subset%<>% filter(bl_household_size<30)

fig <- plot_ly(child_data_subset, x = ~ bl_household_size , y = ~bl_education, z = ~bl_number_child, color = ~pooled_treatment, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Household Size'),
                     yaxis = list(title = 'No. of Children'),
                     zaxis = list(title = 'Education Household head')))%>%layout(title = "Randomized Treatment Allocation")

return(fig)
}








###########################################################
#### Figure 1.1 POly plot generation function
###########################################################

poly_plot<-function(data,x,y) {
    
school_data_subset_control<-data %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==0) 
school_data_subset_treat<-  data %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==1)
size=0.75
    
ggplot(NULL,aes_string(x=x , y=y))+  theme_minimal()+ 
           geom_smooth(data=school_data_subset_control,method = "loess", formula = y ~ poly(x) ,se=FALSE,size = size, col="#291F7FFF",linetype="dashed") + 
           geom_smooth(data=school_data_subset_treat,method = "loess", formula = y ~ poly(x), size =size, se=FALSE, col = "#B8DE29FF") +
           geom_smooth(data=school_data_subset_control,method = "lm", formula = y ~ x ,se=FALSE,size = size, col ="#33638DFF",linetype="dashed") + 
           geom_smooth(data=school_data_subset_treat,method =  "lm", formula = y ~ x, size = size, se=FALSE, col = "#DCE319FF")+
                 geom_smooth(data=school_data_subset_control,method = "lm", formula = y ~ poly(x^5) ,se=FALSE,size = size,               col="#55C667FF",linetype="dashed") + 
           geom_smooth(data=school_data_subset_treat,method =  "lm", formula = y ~ poly(x^5), size = size, se=FALSE, col = "#FDE725FF")+
    scale_color_manual(name = "légende",labels = c("Data", "Moy", "loess regression \n with confidence interval"), 
                     values = c("darkgray", "royalblue", "red"))
}



###########################################################
#### Figure 1.2 Marginal plot generation function
###########################################################

marginal_plot<-function(data)
{

data<-school_data %>% filter(min_school_dist_sef < 1)  %>% 
            filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==1) 

p=ggplot(data =data,
          aes(x =min_school_dist_sef, y = fu_child_enrolled)) +
      geom_point() + geom_smooth(method = 'loess', formula = 'y ~ x+x^2',se=FALSE,size = 0.5)+
     theme_minimal()+ labs(x ="Minimum Distance to School < 1 ",y= "Probability of Enrollment")
p1 <- ggMarginal(p, fill = "chocolate3",type="histogram")


data<-school_data %>% filter(min_school_dist_sef < 1.5)  %>% 
            filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==1) 

p=ggplot(data =data,
          aes(x =min_school_dist_sef, y = fu_child_enrolled)) +
      geom_point() + geom_smooth(method = 'loess', formula = 'y ~ x+x^2',se=FALSE,size = 0.5)+
     theme_minimal()+ labs(x ="Minimum Distance to School < 1.5 ",y= "Probability of Enrollment")
p2 <- ggMarginal(p, fill = "gold1",type="histogram")

data<-school_data %>% filter(min_school_dist_sef < 1.5)  %>% 
            filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==1 &fu_female == 1) 

p=ggplot(data =data,
          aes(x =min_school_dist_sef, y = fu_child_enrolled)) +
      geom_point() + geom_smooth(method = 'loess', formula = 'y ~ x+x^2',se=FALSE,size = 0.5)+
     theme_minimal()+ labs(x ="Minimum Distance to School (Girls)",y= "Probability of Enrollment")
p3 <- ggMarginal(p, fill = "pink",type="histogram")


data<-school_data %>% filter(min_school_dist_sef < 1.5)  %>% 
            filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==1 &fu_female == 0) 

p=ggplot(data =data,
          aes(x =min_school_dist_sef, y = fu_child_enrolled)) +
      geom_point() + geom_smooth(method = 'loess', formula = 'y ~ x+x^2',se=FALSE,size = 0.5)+scale_fill_viridis_d()+
     theme_minimal()+ labs(x ="Minimum Distance to School (Boys)",y= "Probability of Enrollment")
p4 <- ggMarginal(p, fill = "slateblue",type="histogram")
    
return(grid.arrange(p1, p2, p3,p4, ncol=2,nrow=2))

}





###########################################################
#### Map 1 generation function
###########################################################

map_enrollment<- function(data){
#data(admin1.map)
#pakistan = subset(admin1.map, admin == "pakistan")
#saveRDS(pakistan, file="data/Pakistan.Rda")  #File saved locally and provided in repository

pakista<-data

pakistan= pakistan %>% mutate(region = case_when(region == "azad kashmir" ~ "AJ&K", 
                                             region == "islamabad capital territory" ~"ICT",
                                             region == "punjab" ~ "Punjab",
                                            region == "balochistan" ~ "Balochistan" ,
                                            region == "khyber pakhtunkhwa province" ~ "KPK" ,
                                            region == "sindh" ~ "Sindh",
                                            region == "federally administered tribal areas" ~ "FATA",
                                            region == "northern areas" ~ "GB",
                                            TRUE ~ "" 
                                                  ))

enrollment<-c(530957,215756,11736099,907350,4883943,4485692,681226,155230)
region<-c( "AJ&K","ICT","Punjab","Balochistan","KPK","Sindh", "FATA", "GB")
population<-c(4045366,2006572,110012442,12344408,35525047,47886051,5001676,3500000)
enrollment <- (enrollment/population) *100

enrollment_df.pakistan <- as.data.frame(cbind(region,enrollment))

pakistan <- merge(pakistan,enrollment_df.pakistan)
pakistan$enrollment <-as.numeric(levels(pakistan$enrollment))[pakistan$enrollment]

cnames <- aggregate(cbind(long, lat) ~ region, data=pakistan, 
                    FUN=function(x)mean(range(x)))
  
 
cnames$long[cnames$region == "FATA"] <- 70   
cnames$lat[cnames$region == "KPK"] <- 35
cnames$long[cnames$region == "KPK"] <- 72.5                  
p<-ggplot(pakistan, aes(x = long, y = lat, group = region)) + geom_polygon(aes(fill = enrollment ))+ 
     geom_path(data = pakistan, aes(x = long, y = lat, group = region), 
            color = "black", size = 0.055) +
      geom_label(data=cnames, aes(long, lat, label = region), size=2.5,fontface = "bold") +
         labs(x = NULL, 
         y = NULL, 
         title = " Pakistan Student Enrollment by Population", 
         subtitle = "  2017", 
         caption = "Data : Annual School Census 2017-18,Goverment of Punjab; Spatial Data : r - Choroplethr")+ 
         theme_map() +
         scale_fill_viridis(option = "viridis", direction = -1,name = "enrollment %",guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      # some shifting around
      title.hjust = 0.5,
      label.hjust = 0.5
  ))
                    
                    
                    
print(p)
 }

                    
                    
                    
                    
#######################################################################################
 ##### Circle Plot Function                  
####################################################################################
                    
circle_plot<-function(data){
    

data %<>% filter(value1<25)
# Transform data in a tidy format (long format)
data <- data %>% tidyr::gather(key = "observation", value="value", -c(1,2)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 5
nObsType <- nlevels(as.factor(data$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar*nObsType )
data <- rbind(data, to_add)
data <- data %>% arrange(group, individual)
data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)




# Get the name and the y position of each label
label_data <- data %>% group_by(id, individual) %>% summarize(tot=sum(value)/2)
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation,label = value), stat="identity",alpha=1) +
  scale_fill_viridis(discrete=TRUE,name="", labels=c("Program", "Private", "Govt."))+
  # Add text showing the value of each 100/75/50/25 line
  ggplot2::annotate("text", x = rep(max(data$id),5), y = c(0, 50, 100, 150, 200), label = c("", "", "", "", "") , color="grey", size=6 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-25,max(label_data$tot, na.rm=T)*2) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  theme(legend.position = c(.5, .5)) +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  
  
print(p)
# Save at png
    
}
                    
#################################################### 
# Plot Impact on Number of Days Operational and Hours Teaching
##################################################
  
impact_hours_days_teaching<-function(data)
    { 
school_data<-data
total_cntrl <- school_data %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==0 ) %>%select (ss2_num_days_operational)
total_treat<- school_data %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==1  )%>% select (ss2_num_days_operational)
combdat <- dplyr::bind_rows(list(control=total_cntrl,treatment=total_treat),.id="treat_status") %>% na.omit()
f_1 <-ggplot(combdat, aes(ss2_num_days_operational, fill =treat_status )) + geom_density(alpha = 0.2) + theme_minimal()+scale_fill_viridis_d()+ labs(x ="Number of Days Operational",y= "Density")


total_score_pct_cntrl <- school_data %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==0 ) %>%select (ss9_hours_teaching)
total_score_pct_treat<- school_data %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==1  )%>% select (ss9_hours_teaching)
combdat <- dplyr::bind_rows(list(control=total_score_pct_cntrl,treatment=total_score_pct_treat),.id="treat_status") %>% na.omit()
f_2 <-ggplot(combdat, aes(ss9_hours_teaching, fill =treat_status )) + geom_density(alpha = 0.2) + theme_minimal()+scale_fill_viridis_d()+ labs(x ="Number of Teaching Hours",y= "Density")


print(f_1/f_2)
    
}                  
       
#################################################### 
# Plot Impact on Number of Teachers and Score
##################################################
                     
impact_score_teachers<-function(data){
school_data=data

total_score_pct_cntrl <- school_data %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==0 ) %>%select (total_score_pct)%>%na.omit()
total_score_pct_treat<- school_data %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==1  )%>% select (total_score_pct)%>%na.omit()
combdat <- dplyr::bind_rows(list(control=total_score_pct_cntrl,treatment=total_score_pct_treat),.id="treat_status")
f_1<-ggplot(combdat, aes(total_score_pct, fill =treat_status )) + theme_minimal()+
   geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity',color = "black") +scale_fill_viridis_d(name = "")+
  labs(x ="Percentage Score",y= "Density")





total_score_pct_cntrl <- school_data %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==0 ) %>%select (ss3_num_teachers)%>%na.omit()
total_score_pct_treat<- school_data %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==1 )%>% select (ss3_num_teachers)%>%na.omit()
combdat <- dplyr::bind_rows(list(control=total_score_pct_cntrl,treatment=total_score_pct_treat),.id="treat_status")

f_2<-ggplot(combdat, aes(ss3_num_teachers, x=treat_status,y=ss3_num_teachers,fill =treat_status )) + theme_minimal()+
       geom_violin(trim = FALSE)+theme(legend.position = "none")+scale_fill_viridis_d()+
        labs(x ="Number of Teachers",y= "Density")


total_score_pct_cntrl <- school_data %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==0 & fu_female==1) %>%select (total_score_pct)%>%na.omit()
total_score_pct_treat<- school_data %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==1 & fu_female==1 )%>% select (total_score_pct)%>%na.omit()
combdat <- dplyr::bind_rows(list(control=total_score_pct_cntrl,treatment=total_score_pct_treat),.id="treat_status")
f_3<-ggplot(combdat, aes(total_score_pct, fill =treat_status )) + theme_minimal()+
   geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity',color = "black") +scale_fill_viridis_d(name = "")+
  labs(x ="Percentage Score (Females)",y= "Density")



total_score_pct_cntrl <- school_data %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==0 & fu_female==1) %>%select (ss3_num_teachers)%>%na.omit()
total_score_pct_treat<- school_data %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==1 & fu_female==1)%>% select (ss3_num_teachers)%>%na.omit()
combdat <- dplyr::bind_rows(list(control=total_score_pct_cntrl,treatment=total_score_pct_treat),.id="treat_status")

f_4<-ggplot(combdat, aes(ss3_num_teachers, x=treat_status,y=ss3_num_teachers,fill =treat_status )) + theme_minimal()+
       geom_violin(trim = FALSE)+theme(legend.position = "none")+scale_fill_viridis_d()+
        labs(x ="Number of Teachers",y= "Density")


print((f_1|f_2)/(f_3|f_4))
    
    
}      
        
                    
                    
                    
        
###################################################################
# Plot ReLationship between female enrollment and distance to school
##################################################################                   
                    
                    
enroll_girls_dist<-function(data){
    
    
school_data<-data   
m2<-school_data %>% filter(min_school_dist_sef < 1.5) 
m2$fu_female=as_factor(m2$fu_female)
school_data_subset_control<-m2 %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==0) %>% 
        select(min_school_dist_sef,ss5_pct_students_female) %>% na.omit()
school_data_subset_treat<-  m2 %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==1)%>%
         select(min_school_dist_sef,ss5_pct_students_female) %>% na.omit()
school_data_subset<-  m2 %>% filter(fu_child_level == 1 & fu_young_child == 1)%>%
              select(min_school_dist_sef,ss5_pct_students_female) %>% na.omit()
b_1<- ggplot(school_data_subset_treat, aes(x =min_school_dist_sef, y = ss5_pct_students_female))+ 
     geom_hex() +
  scale_fill_viridis_c(option="mako",direction=-1) +theme_classic()+ labs(x ="Minimum Distance to School (Treat)",y= "Percentage Female Students")

b_2<-ggplot(school_data_subset_control, aes(x =min_school_dist_sef, y = ss5_pct_students_female))+ 
     geom_hex() +
  scale_fill_viridis_c(option="inferno",direction=-1) +theme_classic()+ labs(x ="Minimum Distance to School (Control)",y= "Percentage Female Students")

b_3<-ggplot(school_data_subset, aes(x =min_school_dist_sef, y = ss5_pct_students_female))+ 
     geom_hex() +
  scale_fill_viridis_c(option="rocket",direction=-1) +theme_classic()+ labs(x ="Minimum Distance to School (All)",y= "Percentage Female Students")
b_4<-ggplot(school_data_subset, aes(x =min_school_dist_sef, y = ss5_pct_students_female))
k<-((b_1|b_2)/(b_3|plot_spacer()) + plot_layout(guides = "collect")  & theme(legend.position = "bottom"))
print(k)

    
}
                    
        
###################################################################
# Double randomization plot 
##################################################################                       
                    
 double_randomization_plot <-function(data)
    {
    
child_data=data   

child_controls = c("control_fu_child_age", "control_fu_female", "missing_fu_child_age", "missing_fu_female")

hh_controls = c("control_fu_adults", "control_fu_hh_head_edu","control_fu_hh_head_occ_farmer" ,"control_fu_total_land", 
             "control_fu_household_size" ,"missing_fu_adults", "missing_fu_hh_head_edu" ,"missing_fu_hh_head_occ_farmer",  
             "missing_fu_total_land", "missing_fu_household_size")


independent_vars = c("pooled_treatment")
district_control =c("bl_district")
dependent_vars<-c("fu_child_enrolled","fu_child_highest_grade")

rel_vars<-c(child_controls,hh_controls,independent_vars,district_control,dependent_vars,"bl_education","bl_household_size","bl_number_child")

child_data_subset= child_data %>% filter(fu_child_level == 1 & fu_young_child == 1)
child_data_subset = child_data_subset[,rel_vars]

child_data_subset <- transform(child_data_subset, pooled_treatment_random = sample(pooled_treatment))

district_control =c("factor(bl_district)")

child_data_subset =child_data_subset%>% mutate(
     pooled_treatment = case_when(
         pooled_treatment==pooled_treatment_random  ~2,
         TRUE~ pooled_treatment
     )
)


child_data_subset$pooled_treatment=as_factor(child_data_subset$pooled_treatment)

child_data_subset_pool<-child_data_subset %>% filter(pooled_treatment==1|pooled_treatment==2) %>% sample_frac(., 0.5)


child_data_subset_control<-child_data_subset[ sample( which(child_data_subset$pooled_treatment==0), round(1*length(which(child_data_subset$pooled_treatment==0)))), ]

child_data_subset<-rbind(child_data_subset_pool,child_data_subset_control)%>% na.omit()


child_data_subset_pool<-child_data_subset[ sample( which(child_data_subset$pooled_treatment==1), round(0.2*length(which(child_data_subset$pooled_treatment==1)))), ]


child_data_subset%<>% select("bl_household_size","bl_education","bl_number_child","pooled_treatment") %>% na.omit() %>% filter(bl_household_size <30)
child_data_subset$pooled_treatment=as_factor(child_data_subset$pooled_treatment)
fig <- plot_ly(child_data_subset, x = ~ bl_household_size , y = ~bl_education, z = ~bl_number_child, color = ~pooled_treatment, colors = c('#BF382A', '#0C4B8E',"#32CD32"))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Household Size'),
                     yaxis = list(title = 'No. of Children'),
      zaxis = list(title = 'Education Household head')))%>%layout(title = "Double Randomized Placebo Treatment Allocation")



#child_data_subset_regression= child_data %>% filter(fu_child_level == 1 & fu_young_child == 1) %>%
                # select(all_of(rel_vars)) 


#child_data_subset_regression <- transform(child_data_subset, pooled_treatment_random = sample(pooled_treatment))
return(fig)
    
}           
                    
#####################################################
## Marginal Impact of Test scores on different ages             
####################################################
                    
                    
                    
 marginal_impact_test_score<-function(data)
    {


child_controls = c("Age", "fu_female", "missing_fu_child_age", "missing_fu_female")

    
hh_controls = c("control_fu_adults", "control_fu_hh_head_edu","control_fu_hh_head_occ_farmer" ,"control_fu_total_land", 
             "control_fu_household_size" ,"missing_fu_adults", "missing_fu_hh_head_edu" ,"missing_fu_hh_head_occ_farmer",  
             "missing_fu_total_land", "missing_fu_household_size")

independent_vars = c("treatment_1", "treat_1_female" ,"treatment_2","treat_2_female")
district_control =c("factor(bl_district)")


child_data_subset= data %>% filter(fu_child_level == 1 & fu_young_child == 1)
child_data_subset$Age=child_data_subset$control_fu_child_age

child_data_subset$treatment_1<- factor(child_data_subset$treatment_1, levels=c('0','1'))
labels=c('Pooled Treatment','Control')

x_vars <- c(independent_vars,child_controls,hh_controls,district_control)



full.formula <- as.formula(paste('total_score_dev', paste(x_vars,collapse = ' + '),sep='~'))

lm_score<-lm(full.formula,data=child_data_subset,weights=child_data_subset$hh_weight)

x<-ggpredict(lm_score, terms = c("treatment_1", "fu_female","Age"), ci = FALSE, add.data = TRUE)
plot(x,connect.lines = TRUE,facet = TRUE, ci.style = "errorbar", dot.size = 1.5) + labs(
    x = "Pooled Treatment Dummy", 
    y = "Total Test Score Z-Score", 
    title = "Predicted Mean Score"
  )+ labs(colour = "Child = Female")+theme_bw()
  
}                   
                    
                    
                    
                    
                    
                    
                    
                    