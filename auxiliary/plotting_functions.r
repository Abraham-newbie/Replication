
poly_plot<-function(data,x,y) {
    
school_data_subset_control<-data %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==0) 
school_data_subset_treat<-  data %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==1)
size=0.75
    
ggplot(NULL,aes_string(x=x , y=y))+ theme_classic()+
           geom_smooth(data=school_data_subset_control,method = "loess", formula = y ~ poly(x) ,se=FALSE,size = size, col = "red",bw=0.15,linetype="dashed") + 
           geom_smooth(data=school_data_subset_treat,method = "loess", formula = y ~ poly(x), size =size, se=FALSE, col = "blue",bw=0.15) +
           geom_smooth(data=school_data_subset_control,method = "lm", formula = y ~ x ,se=FALSE,size = size, col = "green",bw=0.15,linetype="dashed") + 
           geom_smooth(data=school_data_subset_treat,method =  "lm", formula = y ~ x, size = size, se=FALSE, col = "violet",bw=0.15) +
             geom_smooth(data=school_data_subset_control,method = "lm", formula = y ~ poly(x^5) ,se=FALSE,size = size, col = "black",bw=0.15,linetype="dashed") + 
           geom_smooth(data=school_data_subset_treat,method =  "lm", formula = y ~ poly(x^5), size = size, se=FALSE, col = "yellow",bw=0.15) 
           
}

