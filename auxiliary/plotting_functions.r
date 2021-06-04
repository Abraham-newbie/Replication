###########################################################
#### Figure 1 POly plot generation function
###########################################################

poly_plot<-function(data,x,y) {
    
school_data_subset_control<-data %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==0) 
school_data_subset_treat<-  data %>% filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==1)
size=0.75
    
ggplot(NULL,aes_string(x=x , y=y))+  theme_minimal()+
           geom_smooth(data=school_data_subset_control,method = "loess", formula = y ~ poly(x) ,se=FALSE,size = size, col = "red",bw=0.15,linetype="dashed") + 
           geom_smooth(data=school_data_subset_treat,method = "loess", formula = y ~ poly(x), size =size, se=FALSE, col = "blue",bw=0.15) +
           geom_smooth(data=school_data_subset_control,method = "lm", formula = y ~ x ,se=FALSE,size = size, col = "green",bw=0.15,linetype="dashed") + 
           geom_smooth(data=school_data_subset_treat,method =  "lm", formula = y ~ x, size = size, se=FALSE, col = "violet",bw=0.15) +
             geom_smooth(data=school_data_subset_control,method = "lm", formula = y ~ poly(x^5) ,se=FALSE,size = size, col = "black",bw=0.15,linetype="dashed") + 
           geom_smooth(data=school_data_subset_treat,method =  "lm", formula = y ~ poly(x^5), size = size, se=FALSE, col = "yellow",bw=0.15) 
           
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
         scale_fill_viridis(option = "brewer blues", direction = -1,name = "enrollment %",guide = guide_colorbar(
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
