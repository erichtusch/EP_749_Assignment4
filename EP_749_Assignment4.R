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
  ##  make count table
  ##  loop through demographic variables to make table of patient counts
  count.df <- data.frame()
  for(i in 1:length(demog.var)){
    cat('\nbinding',demog.var[i])
    count.df <- bind_rows(count.df,
                          df %>%
                            select_at(vars(one_of(group.var,demog.var[i]))) %>%
                            group_by_all() %>% count() %>%
                            group_by_at(group.var) %>% spread(group.var,n) %>%
                            mutate(Factor = colnames(.)[1]) %>% select(Factor,everything()) %>%
                            rename_at(vars(one_of(demog.var[i])),funs(sub(demog.var[i],"value",.))) %>%
                            mutate(value = as.character(value)))
  }
  # change NA to 0
  count.df <- count.df %>%
    mutate_if(is.numeric,funs(if_else(is.na(.),as.numeric(0),as.numeric(.))))
  ##  make percent table
  cat('\ncreating percent df')
  perc.df <- count.df
  for(perc.df.col in colnames(perc.df)[3:length(colnames(perc.df))]){
    perc.df <- perc.df %>% mutate_at(vars(one_of(perc.df.col)),funs(./group.count[[perc.df.col]]))
  }
  ##  make print-formatted table
  cat('\ncreating print-formatted df')
  print.df <- count.df
  for(print.df.col in colnames(print.df)[3:length(colnames(print.df))]){
    print.df <- print.df %>% mutate_at(vars(one_of(print.df.col)),
                                       funs(paste0(.," (",round(perc.df[[print.df.col]]*100,digits=2),"%)")))
    }
  ##  return
  return(list(count.df = count.df,
              perc.df = perc.df,
              print.df = print.df))
}

##### table 2 function  #####
##### table 3 function  #####

##### read in file  #####
cat('\nchoose file')
fp <- file.choose()
cat('\nreading excel file')
cancer.df <- read.xlsx(file = fp,sheetName = "Raw Data",as.data.frame = T,header = T,stringsAsFactors = F)
##### process df  #####
##    since age included non-age numbers, DON'T USE AGE
cancer.df <- cancer.df %>% mutate(IDATE = as.Date(IDATE,format = "%m%d%Y"))
##### create tables #####
demog.var <- c("SEX","FLUSHOT3","X_AGE_G","X_RACE_G",
               "MARITAL","X_EDUCAG","X_INCOMG","HLTHPLAN",
               "SHINGLES","TNSARCV")
group.var <- "PRIORCNCR"
table1 <- make.table.1(cancer.df %>% mutate_at(vars(one_of(group.var)),funs(paste("Prior Cancer:",.,sep=" "))),
                       group.var,demog.var)
##  replace factor values with meaningful info
#     make data frame for each factor, 
#     then bind all data frames into a factor values df
#     then join factor values df to print table
cat('\ncreating factor.value.label.list')
factor.value.label.list = list(
  FLUSHOT3.values.df = data.frame(value = c(1,2,7,9),
                                  label = c("Yes","No","Don't know / Refused","Refused"),
                                  stringsAsFactors = F) %>%
    mutate(Factor = "FLUSHOT3",Factor.desc = "Flu shot in past 12 mo."),
  X_AGE_G.values.df = data.frame(value = c(1,2,3,4,5,6),
                                 label = c("18-24 years",
                                           "25-34 years",
                                           "35-44 years",
                                           "45-54 years",
                                           "55-64 years",
                                           "65+ years"),stringsAsFactors = F) %>%
    mutate(Factor = "X_AGE_G",Factor.desc = "Age group"),
  SEX.values.df = data.frame(value = c(1,2),label=c("Male","Female"),stringsAsFactors = F) %>%
    mutate(Factor = "SEX",Factor.desc = "Sex"),
  #TODO# when I get labels for these values, fill them in and add them to the method.
  X_RACE_G.values.df = data.frame(value = c(1,2,3,4,5,NA)) %>%
    mutate(Factor = "X_RACE_G",Factor.desc = "Race Group"),
  MARITAL.values.df = data.frame(value = c(1,2,3,4,5,6,9)) %>%
    mutate(Factor = "MARITAL",Factor.desc = "Marital Status"),
  X_EDUCAG.values.df = data.frame(value = c(1,2,3,4,9)) %>%
    mutate(Factor = "X_EDUCAG",Factor.desc = "Education level"),
  X_INCOMG.values.df = data.frame(value = c(1,2,3,4,5,9)) %>%
    mutate(Factor = "X_INCOMG",Factor.desc = "Income Group"),
  HLTHPLAN.values.df = data.frame(value = c(1,2,7,9),
                                  label=c("Yes","No","Don't know / Refused","Refused")) %>%
    mutate(Factor = "HLTHPLAN",Factor.desc = "Health plan"),
  SHINGLES.values.df = data.frame(value = c(1,2,7,NA),
                                  label=c("Yes","No","Don't know / Refused","N/A")) %>%
    mutate(Factor = "SHINGLES",Factor.desc = "Shingles vaccine"),
  TNSARCV.values.df = data.frame(value = c(1,2,7,9,NA),
                                 label=c("Yes","No","Don't know / Refused",'Refused',"N/A")) %>%
    mutate(Factor = "TNSARCV",Factor.desc = "Tetanus vacc. within 10 y.")
)
##  combine values with labels into a factor values table
cat('\nbinding list dfs into single factor.value.label.df')
factor.value.label.df <- bind_rows(factor.value.label.list[[1]],factor.value.label.list[[2]])
for(i in 3:length(factor.value.label.list)){
  factor.value.label.df <- bind_rows(factor.value.label.df,factor.value.label.list[[i]])
}
table1$print.df.formatted <- left_join(table1$print.df,
                                       factor.value.label.df %>% mutate(value = as.character(value)),
                                       by=c("Factor"="Factor","value"="value")) %>% 
  mutate(Factor.desc = coalesce(Factor.desc,Factor),
         label = coalesce(label,value)) %>%
  select(Factor,Factor.desc,label,everything(),-value)
##### write tables  #####