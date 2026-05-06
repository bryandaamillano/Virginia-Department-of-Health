library(tidyverse)            # QUALITY OF LIFE IMPROVEMENTS
library(readxl)               # IMPORT EXCEL DOCUMENTS
library(writexl)              # EXPORT EXCEL DOCUMENT
library(dplyr)                # DATA MANIPULATION 
library(tidyr)                # DATA MANIPULATION
library(ggplot2)              # GRAPHING


############ FILE PATHS ############


# ******** INSERT FILE PATHS HERE ********

# INDICATING WHERE OUR FILES ARE FROM  
# FOLDER CONTAINING EXCEL DOCUMENT
import.path<-"C:/Users/mhg62973/Desktop/R Projects/DLC/R Outbreak Response Analysis/DLC_Outbreak_Line_List.xlsx"

# FOLDER THAT YOU WANT GRAPHS/DOCS EXPORTED TO 
export.path<-"C:/Users/mhg62973/Desktop/R Projects/DLC/R Outbreak Response Analysis"


############ DATA IMPORT ############

# PATH OF EXCEL DOC
doc.path<-"C:/Users/mhg62973/Desktop/R Projects/DLC/R Outbreak Response Analysis/DLC_Outbreak_Line_List.xlsx"

# GET LIST/NAMES OF SHEETS IN THE EXCEL DOC
excel_sheets(doc.path)

             
# IMPORT INDIVIDUAL SHEETS AS DATASETS 
List_1<-read_xlsx(doc.path, sheet="List_1") 
          
List_2<-read_xlsx(doc.path, sheet="List_2") 
                  
List_3<-read_xlsx(doc.path, sheet="List_3") 

Testing_log<-read_xlsx(doc.path, sheet="Testing_log") 
                         
VEDSS<-read_xlsx(doc.path, sheet="VEDSS") 
               

############ DATA PROCESSING FOR LINE LISTS ############

# LOOK AT THE STRUCTURE OF THE THREE DATASETS (ARE ALL THE COLUMNS CONSISTENT? WHAT IS NOT?)
str(List_1)
str(List_2)
str(List_3)

# FIRST TASKS IS TO COMBINE ALL THREE LISTS INTO A SINGLE ONE
Line_list_combined <- List_1 %>%
  bind_rows(List_2) %>%
  bind_rows(List_3) %>%
  # SEPARATE FULL NAME COLUMN INTO LAST NAME AND FIRST NAME BY THE COMMA
  separate(Full_name,c("Last","First_middle"),sep=",") %>% 
  # TRIM WHITE SPACES OFF CHARACTER VARIABLES 
  mutate_if(is.character,trimws) %>% 
  # SEPARATE FIRST NAME COL INTO FIRST NAME AND MIDDLE INITIAL BY THE SPACE 
  separate(First_middle,c("First","Middle"),sep=" ") %>% 
  # TRIM WHITE SPACE AND CAPITALIZE ALL CHARACTER VARIABLES 
  mutate_if(is.character,toupper) %>% 
  mutate_if(is.character,trimws) %>% 
  # FORMAT DATES 
  mutate(Week_end=as.Date(Week_end),
         Week_start= as.Date(Week_start)) %>% 
  # ORGANIZE BY DATE
  arrange(Week_end)

# REMEMBER THERE MIGHT BE DUPLICATES SO LET'S LOOK AT FREQUENCY OF SYMPTOMS.  CAN ANYTHING BE COMBINED? 
# (THERE MIGHT NOT ACTUALLY BE AS MANY REPORTED SYMPTOMS AS THERE ARE DUE TO POTENTIAL MULTIPLE VISITS)
Line_list_combined %>% 
  group_by(Symptom) %>% 
  summarize(Total=n())

# CREATE A MASTER LINE LIST FROM THE LIST THAT WAS GROUPED BY SYMPTOM
Line_list_master<-Line_list_combined %>% 
  # COMBINE REDUNDANT SYMPTOMS INTO SINGLE CATEGORIES 
  # IF YOU SEE THE WORD COUGH IN THE SYMPTOM COLUMN, LABEL IT COUGH
  mutate(Symptom2= ifelse(grepl("COUGH",Symptom),"COUGH",
                          
                    # IF YOU SEE THE WORD FEVER IN THE SYMPTOM COLUMN, LABEL IT FEVER
                    ifelse(grepl("FEVER",Symptom),"FEVER",
                           
                    # IF YOU SEE THE WORD PHARYNGITIS OR THROAT IN THE SYMPTOM COLUMN, LABEL IT SORE THROAT
                    ifelse(grepl("PHARYNGITIS|THROAT",Symptom),"SORE THROAT",     
                           
                    # IF NEITHER CONDITION IS MET, LEAVE IT AS THE ORIGINAL SYMPTOM
                    Symptom)))) %>% 
  
  # GET RID OF DUPLICATES BASED ON LAST, FIRST, AGE, SYMPTOM, AND WEEK ENDING
  distinct(Last,First, Age, Symptom2, Week_end, .keep_all = TRUE)

         
############ ANALYSIS ############

# CREATE A FREQUENCY TABLE FOR SYMPTOMS LISTED
# RECORD EACH SYMPTOM PER INDIVIDUAL ONCE 
Symptom_frequency<-Line_list_master %>% 
  # KEEP ONLY ROW FOR LAST, FIRST, AGE, AND SYMPTOM
  distinct(Last,First,Age,Symptom2, .keep_all = TRUE) %>% 
  # GET A TOTAL COUNT OF EACH SYMPTOM MENTIONED
  group_by(Symptom2) %>% 
  summarize(Total = n()) %>%
  arrange(desc(Total))

# GRAPH THE FREQUENCY OF SYMPTOMS 
Symptom_frequency %>% 
  # START OF GRAPH
  ggplot(aes(Symptom2, Total, fill =  Symptom2))+
  geom_bar(stat="identity",color="black")+
  # FLIP COORDINATES FOR EASIER READING 
  coord_flip()
  # EXPORTING GRAPH INTO A FOLDER
ggsave(path =export.path,height=5,width=20, filename ="Symptom_frequency.png")

# COUNT OF TOTAL UNIQUE INDIVIDUALS 
Total_ind<-Line_list_master %>% 
  distinct(Last,First,Age, .keep_all = TRUE) 

# ADDING A STATEMENT FOR THE CHARTS, ETC.
# CREATE MESSAGE FOR TOTAL COUNT 
Total_ind_caption<-paste0(nrow(Total_ind)," total individuals")

# CREATE EPI CURVE OF FIRST RECORDED VISIT OF PATIENT
# USING THE COMBINED LINE LIST DATA SET
Line_list_master %>%
  # KEEP ONLY FIRST RECORDED VISIT PER INDIVIDUAL.  REMOVE OTHER DUPLICATES!  
  distinct(Last,First,Age, .keep_all = TRUE) %>% 
  group_by(Week_end) %>% 
  summarize(Total= n()) %>% 
  # CONVERTING THAT TO A GRAPH
  ggplot(aes(Week_end,Total))+
  geom_bar(stat="identity",color= "black", fill = "orange")  +
  ggtitle("Total Individuals Visiting Student Health by Week Ending")+
  labs(x="Week Ending",
       y = "Total Individuals",
       subtitle=Total_ind_caption)+
  # FONT SIZE AND BORDERS
  theme(axis.line = element_line(colour= "black", size = .5, linetype = "solid"),
        plot.title = element_text(hjust = 0, size = 10),
        plot.subtitle =  element_text(hjust = 0, size = 8))+
  # DATE FORMATTING 
  scale_x_date(date_labels = "%b/%Y")
ggsave(path =export.path,height=5,width=20, filename ="Epi_curve_total_individuals.png")


# LOOKING AT IT BY GRADUATING CLASS
Line_list_master %>% 
  distinct(Last,First,Age, .keep_all = TRUE) %>% 
  group_by(Week_end,Class) %>% 
  summarize(Total= n()) %>% 
  # START OF GRAPH
  ## THE "FILL=" REFERS TO WHAT YOU WANT IT COLORED BY IN THE GRAPH
  ggplot(aes(Week_end,Total, fill= Class))+
  geom_bar(stat="identity",color= "black")+
  ggtitle("Total Individuals Visiting Student Health by Graduating Class and Week Ending")+
  labs(x="Week Ending",
       y = "Total Individuals",
       subtitle=Total_ind_caption)+
  # FONT SIZE AND BORDERS
  theme(axis.line = element_line(colour= "black", size = .5, linetype = "solid"),
        plot.title = element_text(hjust = 0, size = 10),
        plot.subtitle =  element_text(hjust = 0, size = 8))+
  # DATE FORMATTING 
  scale_x_date(date_labels = "%b/%Y")
ggsave(path =export.path,height=5,width=20, filename ="Epi_curve_graduating_class.png")

# LOOKING AT IT BY SEX
Line_list_master %>% 
  distinct(Last,First,Age, .keep_all = TRUE) %>% 
  group_by(Week_end,Sex) %>% 
  summarize(Total= n()) %>% 
  # START OF GRAPH
  ggplot(aes(Week_end,Total, fill=Sex))+
  geom_bar(stat="identity",color= "black")+
  ggtitle("Total Individuals Visiting Student Health by Sex and Week Ending")+
  labs(x="Week Ending",
       y = "Total Individuals",
       subtitle=Total_ind_caption)+
  # FONT SIZE AND BORDERS
  theme(axis.line = element_line(colour= "black", size = .5, linetype = "solid"),
        plot.title = element_text(hjust = 0, size = 10),
        plot.subtitle =  element_text(hjust = 0, size = 8))+
  # DATE FORMATTING 
  scale_x_date(date_labels = "%b/%Y")
ggsave(path =export.path,height=5,width=20, filename ="Epi_curve_sex.png")                              


# FROM THE GRAPHS WE CREATED, IT LOOKS LIKE TOWARDS THE BEGINNING THE PEOPLE GETTING SICK WERE FRESHMEN AND AS THE OUTBREAK PROGRESSED IT SPREAD AMONGST THE OTHER CLASSES. 
# IT ALSO LOOKS LIKE THIS AFFECTED MALES MORE THAN FEMALES. THIS LEADS TO MORE ADVANCED QUESTIONS. 


############ DATA PROCESSING FOR TESTING LOG ############               
str(Testing_log)

Testing_log2<-Testing_log %>% 
  mutate_if(is.character,toupper) %>% 
  # SEPARATE FULL NAME INTO LAST AND FIRST NAME BY THE COMMA 
  separate(Full_name,c("Last", "First"), sep = ",") %>% 
  # TRIM WHITE SPACES OFF OF VARIABLES
  mutate_if(is.character,trimws)

# COUNT OF TOTAL UNIQUE INDIVIDUALS 
Total_tested<-Testing_log2 %>% 
  distinct(Last,First,Age, .keep_all = TRUE) 

# CREATE MESSAGE FOR TOTAL COUNT 
Total_tested_caption<-paste0(nrow(Total_tested), " total individuals tested")


# TO SEE WHAT ORGANISM IS PRESENT. IS THERE A SINGLE ETIOLOGIC AGENT OR ARE THERE MULTIPLE?
Testing_log2 %>% 
  group_by(Test) %>% 
  summarise(Total=n()) %>% 
  # START OF GRAPH
  ggplot(aes(Test,Total,fill=Test))+
  geom_bar(stat="identity",color="black")+
  ggtitle("Total Individuals Tested by DCLS")+
  coord_flip()+
  labs(x="Lab Test",
       y = "Total Individuals",
       subtitle=Total_tested_caption)+
  
  # FONT SIZE AND BORDERS
  theme(axis.line = element_line(colour= "black", size = .5, linetype = "solid"),
        plot.title = element_text(hjust = 0, size = 10),
        plot.subtitle =  element_text(hjust = 0, size = 8))
ggsave(path =export.path,height=5,width=20, filename ="Lab_tests.png")  


############ DATA PROCESSING FOR VEDSS VERIFICATION ############ 


# CREATE COPY OF VEDSS RECORDS
VEDSS_Records<-VEDSS %>% 
  mutate_if(is.character,toupper) %>% 
  mutate_if(is.character,trimws) 

# MATCH LINE LIST RECORDS TO VEDSS
Line_list_VEDSS<-Line_list_combined %>% 
  # THIS IS SAYING I WANT TO MERGE WHAT'S ON LINE_LIST_COMBINED TO WHAT'S ON VEDSS 
  left_join(VEDSS_Records, by = c("Last"= "Patient.Last.Name",
                          "First" = "Patient.First.Name",
                          "Age" = "Patient.Age")) %>% 
  # COUNT EACH INDIVIDUAL ONLY ONCE 
  distinct(Last,First,Age, .keep_all = TRUE)
                          
# VEDSS MATCHES ON LINE LIST THAT ALREADY HAVE A VEDSS ID                         
VEDSS_matches<-Line_list_VEDSS %>% 
  filter(!is.na(Person.Search.ID))

# MISSING VEDSS ID (FILTER FOR ONES THAT DON'T HAVE A VEDSS ID)
No_VEDSS<-Line_list_VEDSS %>% 
  filter(is.na(Person.Search.ID))
    
# CURRENT VEDSS RECORDS WITH OUTBREAK IDS
Outbreak_ID<-Line_list_VEDSS %>% 
  filter(!is.na(Investigation.Outbreak.ID))

# ARE THERE ANY VEDSS RECORDS MISSING FROM THE LINE LIST?
VEDSS_missing_line<-VEDSS_Records %>% 
  anti_join(Line_list_master, by = c("Patient.Last.Name"= "Last",
                                     "Patient.First.Name"= "First",
                                     "Patient.Age"= "Age"))

# YOU WANT TO MAKE SURE EVERYTHING IS PRESENT AND ACCOUNTED FOR WHICH IS WHY WE RAN ALL OF THESE FILTERING CODES.


############ EXPORT ############ 

write_xlsx(list("Master" = Line_list_VEDSS,
                # VEDSS MATCHES                           
                "VEDSS_matches"=VEDSS_matches,
                # MISSING VEDSS ID 
                "No_VEDSS"=No_VEDSS,
                # CURRENT VEDSS RECORDS WITH OUTBREAK IDS
                "Outbreak_ID"=Outbreak_ID,
                # ARE THERE ANY VEDSS RECORDS MISSING FROM THE LINE LIST?
                "VEDSS_missing_line"=VEDSS_missing_line,
                # SYMPTOM FREQUENCY
                "Symptom_frequency" = Symptom_frequency),
           file.path(export.path,"Outbreak_records.xlsx"))

