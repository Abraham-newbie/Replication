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
      geom_point() + geom_smooth(se=FALSE,size = 0.5)+
     theme_minimal()+ labs(x ="Minimum Distance to School < 1 ",y= "Probability of Enrollment")
p1 <- ggMarginal(p, fill = "slateblue",type="histogram")


data<-school_data %>% filter(min_school_dist_sef < 1.5)  %>% 
            filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==1) 

p=ggplot(data =data,
          aes(x =min_school_dist_sef, y = fu_child_enrolled)) +
      geom_point() + geom_smooth(se=FALSE,size = 0.5)+
     theme_minimal()+ labs(x ="Minimum Distance to School < 1.5 ",y= "Probability of Enrollment")
p2 <- ggMarginal(p, fill = "slateblue",type="histogram")

data<-school_data %>% filter(min_school_dist_sef < 1.5)  %>% 
            filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==1 &fu_female == 1) 

p=ggplot(data =data,
          aes(x =min_school_dist_sef, y = fu_child_enrolled)) +
      geom_point() + geom_smooth(se=FALSE,size = 0.5)+
     theme_minimal()+ labs(x ="Minimum Distance to School (Girls)",y= "Probability of Enrollment")
p3 <- ggMarginal(p, fill = "slateblue",type="histogram")


data<-school_data %>% filter(min_school_dist_sef < 1.5)  %>% 
            filter(fu_child_level == 1 & fu_young_child == 1 & pooled_treatment==1 &fu_female == 0) 

p=ggplot(data =data,
          aes(x =min_school_dist_sef, y = fu_child_enrolled)) +
      geom_point() + geom_smooth(se=FALSE,size = 0.5)+
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
                    
                    
                    
                    