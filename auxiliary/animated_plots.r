
schooling_mortality_graph<-function(){
  
library(viridis)
library(tidyr)
library(dplyr)
library(tidyverse)
library(processx)
library(gganimate)
  




my_data <- read.csv("C:/Users/abrah/Downloads/correlation-between-child-mortality-and-mean-years-of-schooling-for-those-aged-15-and-older.csv")
head(my_data)

target <- c("Niger","India","Pakistan","Indonesia","South Africa","Vietnam",
            "Ghana","Honduras","Haiti","Egypt","Afghanistan","Andorra","Bangladesh","Colombia","Ghana","Mali","Nepal",
            "Bolivia","Gabon","Honduras","Jordan","Namibia","Philippines","Turkey","Germany")
my_data<-my_data %>% filter(Entity %in% target)


colnames(my_data)[4] <- "child_mortality"

colnames(my_data)[5]<- "education_attainment" 
colnames(my_data)[6]<- "net_population"


clean_data <-my_data  %>%filter(Year>=1985 & Year <=2010) %>% select (Year,Entity,child_mortality,education_attainment,net_population) %>% filter(Year>=1950 & Year <=2010)

clean_data %<>% group_by(Entity) %>% filter(any(!is.na(child_mortality)))
clean_data %<>% group_by(Entity) %>% filter(any(!is.na(education_attainment)))
clean_data %<>% arrange(Entity,Year) %>% dplyr::group_by(Entity) %>% tidyr::fill(education_attainment,.direction="down")

clean_data<-clean_data%>% drop_na(child_mortality,education_attainment,net_population,Year) 



plot<-clean_data %>%
  arrange(desc(net_population)) %>%
  ggplot(aes(x=education_attainment, y=child_mortality, size=net_population, fill=Entity,label=Entity))+
  geom_point(alpha=0.5, shape=21, color="black") +
  geom_text(hjust = 1, size =3.5, nudge_x = -0.5 , vjust=-0) +
  scale_size(range = c(5,25), name="Population (M)") +
  theme_minimal()+
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="viridis") +
  theme(legend.position="none") +
  labs(title = 'Inverse correlation between years of schooling and mortality - Year : {frame_time}', x = 'Mean Years of Schooling', y = 'Child Mortality') +
  transition_time(Year) +
  ease_aes('linear')


animate(plot , height = 600, width =600)




}





attendance_vs_enrollment_graph<-function(){

my_data <- read.csv("C:/Users/abrah/Desktop/ose_project/ose-data-science-course-project-Abraham-newbie/data/attendance-vs-enrolment-rates-primary-education.csv")

colnames(my_data)[4] <- "net_attendance_rate"

colnames(my_data)[5]<- "net_enrollment_rate" 
colnames(my_data)[6]<- "net_population"


target <- c("Niger","Mozambique","India","Pakistan","Indonesia","South Africa","Vietnam",
            "Ghana","Honduras","Haiti","Egypt","Afghanistan","Andorra","Bangladesh","Colombia","Germany","Ghana","Mali","Nepal",
            "Armenia","Bolivia","Gabon","Honduras","Jordan","Namibia","Philippines","Turkey")

my_data<-my_data %>% filter(Entity %in% target)




clean_data <-my_data  %>% filter(Year>=1995 & Year <=2010) %>% select (Year,Entity,net_attendance_rate,net_population,net_enrollment_rate)
clean_data %<>% group_by(Entity) %>% filter(any(!is.na(net_attendance_rate)))
clean_data %<>% group_by(Entity) %>% filter(any(!is.na(net_enrollment_rate)))
clean_data %<>% arrange(Entity,Year) %>% dplyr::group_by(Entity) %>% tidyr::fill(net_attendance_rate,.direction="down")
clean_data %<>% arrange(Entity,Year) %>% dplyr::group_by(Entity) %>% tidyr::fill(net_enrollment_rate,.direction="down")

clean_data<-clean_data%>% drop_na(net_enrollment_rate,net_attendance_rate) 

clean_data  <-clean_data %>% drop_na(net_attendance_rate,net_enrollment_rate)



plot<-clean_data %>%
  arrange(desc(net_population)) %>%
  ggplot(aes(x=net_attendance_rate, y=net_enrollment_rate, size=net_population, fill=Entity,label=Entity)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  geom_text(hjust = 1, size = 3.5, nudge_x = -2.5, vjust=-0) +
  scale_size(range = c(5, 25), name="Population (M)") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="viridis") +
  theme(legend.position="bottom") +
  theme_minimal()+
  theme(legend.position = "none") +
  labs(title = 'Primary education enrollment and attendance - Year : {frame_time}', x = 'Net Attendance Rate', y = 'Net Enrollment Rate') +
  transition_time(Year) +
  ease_aes('linear')



animate(plot , height = 600, width =600)

}