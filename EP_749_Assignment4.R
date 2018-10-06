##### EP 749 Assignment 4 #####
##  HW Description  ##
# 1.	Create a shell for the Table 1 that you plan to use for your data analysis project.  
# a.	At a minimum, please include information about the following: 
#   sex, age, race/ethnicity, marital status, education, employment, 
#   annual income, health coverage, shingles vaccination, tetanus vaccination. 
#   You may include additional variables if you like, but this is not required.
# b.	It is your choice as to whether or not you want to include prevalence ratios in your Table 1. 
#   You will not be penalized if you decide not to do this. 
#   If you do report prevalence ratio measures, 
#   please be intentional in how you select and report reference categories.   
# c.	You are welcome to update your Table 1 even after you turn it in as part of this homework assignment. 
#   The purpose of including Table 1 in this assignment is to give everyone one last opportunity to 
#   receive feedback if you need it, and to help you get ¼ of the deliverables for your data analysis 
#   project out of the way. We will not be discussing this finalized version of Table 1 in class next week.
# 
# 2.	Create table shells for Tables 2 and 3 of the data analysis project. 
# a.	These shells can be rough drafts –you are not required to have settled 
#   on the final content or format of your tables for this assignment. 
#   I want you to turn something in, however, so I can see who needs 
#   some help to think through their analysis. 
# b.	We will spend time in class discussing your table shells from the perspective 
#   of how to plan an analysis (we will not be discussing formatting).

##### load libraries  #####
library(dplyr)
library(tidyr)
library(xlsx)

##### table 1 function  #####
##  define groups by cancer status
##  summarize, at minimum, demographics
##    sex, age, race/ethnicity, marital status, education, employment, annual income, 
##    health coverage, shingles vacc, tetanus vacc
make.table.1 <- function(df,group.var,demog.var){
  ##  group.var is variable to group
  ##  demog.var is character vector of variables to summarize across groups
  ##  calculate n in each group
  group.count <- df %>% select_at(vars(one_of(group.var))) %>% group_by_all() %>% count() %>% spread(group.var,n)
  ##  loop through demographic variables to make table of patient counts
  count.df <- data.frame()
  for(i in 1:length(demog.var)){
    cat('\nbinding',demog.var[i])
    count.df <- bind_rows(count.df,
                          cancer.df %>%
                            select_at(vars(one_of(group.var,demog.var[i]))) %>%
                            group_by_all() %>% count() %>%
                            group_by_at(group.var) %>% spread(group.var,n) %>%
                            mutate(Factor = colnames(.)[1]) %>% select(Factor,everything()) %>%
                            rename_at(vars(one_of(demog.var[i])),funs(sub(demog.var[i],"value",.))) %>%
                            mutate(value = as.character(value)))
  }
  cat('\ncreating percent df')
  #TODO# make this dynamically defined
  perc.df <- count.df %>%
    mutate(DoN = DoN / group.count$DoN,
           No = No / group.count$No,
           Yes = Yes / group.count$Yes)
  return(list(count.df = count.df,
              perc.df = perc.df))
}
demog.var <- c("SEX.fac","FLUSHOT3","X_AGE_G","X_RACE_G",
               "MARITAL","X_EDUCAG","X_INCOMG","HLTHPLAN",
               "SHINGLES","TNSARCV")
group.var <- "PRIORCNCR"
cancer.df %>%
  select_at(vars(one_of(group.var,demog.var[1]))) %>%
  group_by_all() %>% count() %>%
  group_by_at(group.var) %>% spread(group.var,n) %>%
  mutate(Factor = colnames(.)[1]) %>% select(Factor,everything()) %>%
  rename_at(vars(one_of(demog.var[1])),funs(sub(demog.var[1],"value",.)))

table1 <- make.table.1(cancer.df,"PRIORCNCR",demog.var)

##### table 2 function  #####
##### table 3 function  #####

##### read in file  #####
cat('\nchoose file')
fp <- file.choose()
cat('\nreading excel file')
cancer.df <- read.xlsx(file = fp,sheetName = "Raw Data",as.data.frame = T,header = T,stringsAsFactors = F)
##### process df  #####
##  make factors for categorization
##    since age included non-age numbers, DON'T USE AGE
cancer.df.factors <- list(sex = factor(levels = c("Male","Female")))
cancer.df <- cancer.df %>% mutate(IDATE = as.Date(IDATE,format = "%m%d%Y"),
                                  SEX.fac = as.factor(levels(cancer.df.factors$sex)[SEX]))
##### create tables #####
tables <- list()
##### write tables  #####