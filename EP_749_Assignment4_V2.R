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

##  V2  ##
##  2018 10 13 :: ET
##  change excel writing library to writexl

##### load libraries  #####
library(dplyr)
library(tidyr)
library(writexl)
library(readxl)

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

##### table 3 function  #####
make.2x2.table <- function(df,exposure,outcome){
  df %>% select_at(vars(one_of(exposure,outcome))) %>% 
    # only Yes and No for both Prior Cancer and Flu Shot Outcome
    filter_at(vars(one_of(exposure)), any_vars(. %in% c("Yes","No"))) %>%
    filter_at(vars(one_of(outcome)),any_vars(. %in% 1:2)) %>%
    # count per cell
    group_by_all() %>% count() %>%
    # join flu shot value labels
    left_join(table1.factor.value.label.df %>% filter_at(vars(one_of("Factor")),any_vars(grepl(outcome,.))),
              by=setNames("value",outcome)) %>%
    # clean up
    ungroup() %>% select_at(vars(-one_of("Factor",outcome))) %>% 
    unite(vacc_outcome,c("label","Factor.desc"),sep=" ") %>%
    mutate_at(vars(one_of(exposure)),funs(paste0(.,"_Prior_Cancer"))) %>%
    # spread for 2x2
    group_by_at(vars(one_of(exposure))) %>% spread(exposure,n) %>%
    return()
}

##### read in file  #####
cat('\nchoose file')
fp <- file.choose()
cat('\nreading excel file')
cancer.df <- read_excel(path = fp,sheet = "Raw Data")

##### process df  #####
##    since age included non-age numbers, DON'T USE AGE
cancer.df <- cancer.df %>% mutate(IDATE = as.Date(IDATE,format = "%m%d%Y")) %>%
  # add 'X' to beginning of variables that start with '_'
  rename_at(vars(starts_with("_")),funs(paste0("X",.)))
##### create table 1 #####
demog.var <- c("SEX","FLUSHOT3","PNEUVAC3",
               "X_AGE_G","X_RACE_G",
               "MARITAL","X_EDUCAG",
               "X_INCOMG","HLTHPLAN",
               "SHINGLES","TNSARCV")
group.var <- "PRIORCNCR"
table1 <- make.table.1(cancer.df %>% mutate_at(vars(one_of(group.var)),funs(paste("Prior Cancer:",.,"n (%)",sep=" "))),
                       group.var,demog.var)
##  replace factor values with meaningful info
#     make list of data frames for each factor containing values and descriptions
#     then join factor values df to print table
cat('\ncreating table1.factor.value.label.list')
table1.factor.value.label.list = list(
  FLUSHOT3.values.df = data.frame(value = c(1,2,7,9),
                                  label = c("Yes","No","Don't know / Refused","Refused"),
                                  stringsAsFactors = F) %>%
    mutate(Factor = "FLUSHOT3",Factor.desc = "Flu shot in past 12 mo."),
  PNEUVAC3.values.df = data.frame(value = c(1,2,7,9),label = c("Yes","No","Don't know / Refused","Refused"),
                                  stringsAsFactors = F) %>%
    mutate(Factor = "PNEUVAC3",Factor.desc = "Pneumonia shot ever"),
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
  X_RACE_G.values.df = data.frame(value = c(1,2,3,4,5,NA),
                                  label = c("White, non-Hispanic",
                                            "Black, non-Hispanic",
                                            "Hispanic",
                                            "Other race, non-Hispanic",
                                            "Multi-racial, non-Hispanic","Don't know / Refused"),
                                  stringsAsFactors = F) %>%
    mutate(Factor = "X_RACE_G",Factor.desc = "Race Group"),
  MARITAL.values.df = data.frame(value = c(1,2,3,4,5,6,9),
                                 label = c("Married","Divorced","Widowed","Separated","Never Married",
                                           "A member of an unmarried couple","Refused"),
                                 stringsAsFactors = F) %>%
    mutate(Factor = "MARITAL",Factor.desc = "Marital Status"),
  X_EDUCAG.values.df = data.frame(value = c(1,2,3,4,9),
                                  label = c("Did not graduate High School",
                                            "Graduated High School",
                                            "Attended College or Technical School",
                                            "Graduated from College or Technical School",
                                            "Don't know/Not sure/Missing"),
                                  stringsAsFactors = F) %>%
    mutate(Factor = "X_EDUCAG",Factor.desc = "Education level"),
  X_INCOMG.values.df = data.frame(value = c(1,2,3,4,5,9),
                                  label = c("Less than $15,000",
                                            "$15,000 to less than $25,000",
                                            "$25,000 to less than $35,000",
                                            "$35,000 to less than $50,000",
                                            "$50,000 or more",
                                            "Don’t know/Not sure/Missing"),
                                  stringsAsFactors = F) %>%
    mutate(Factor = "X_INCOMG",Factor.desc = "Income Group"),
  HLTHPLAN.values.df = data.frame(value = c(1,2,7,9),
                                  label=c("Yes","No","Don't know / Refused","Refused"),stringsAsFactors = F) %>%
    mutate(Factor = "HLTHPLAN",Factor.desc = "Health plan"),
  SHINGLES.values.df = data.frame(value = c(1,2,7,NA),
                                  label=c("Yes","No","Don't know / Refused","no data"),stringsAsFactors = F) %>%
    mutate(Factor = "SHINGLES",Factor.desc = "Shingles vaccine"),
  TNSARCV.values.df = data.frame(value = c(1,2,7,9,NA),
                                 label=c("Yes","No","Don't know / Refused",'Refused',"no data"),stringsAsFactors = F) %>%
    mutate(Factor = "TNSARCV",Factor.desc = "Tetanus vacc. within 10 y.")
)
##  combine values with labels into a factor values table
cat('\nbinding list dfs into single table1.factor.value.label.df')
table1.factor.value.label.df <- bind_rows(table1.factor.value.label.list[[1]],table1.factor.value.label.list[[2]])
for(i in 3:length(table1.factor.value.label.list)){
  table1.factor.value.label.df <- bind_rows(table1.factor.value.label.df,table1.factor.value.label.list[[i]])
}
table1$print.df.formatted <- left_join(table1$print.df,
                                       table1.factor.value.label.df %>% mutate(value = as.character(value)),
                                       by=c("Factor"="Factor","value"="value")) %>% 
  mutate(Factor.desc = coalesce(Factor.desc,Factor),
         label = coalesce(label,value)) %>%
  select(Factor,Factor.desc,label,everything(),-value)
##### create table 2  #####
##  info about cancer survivors ONLY
#     e.g. type of cancer
##  features: CNCRAGE,CNCRTYPE,
##  create CNCRAGE_G
cancer.survivor.df <- cancer.df %>% filter(PRIORCNCR=="Yes") %>%
  mutate(CNCRAGE_G = paste(if_else(is.na(CNCRAGE),"Don't know / Refused",
                             if_else(CNCRAGE >=65,"65+",
                                     if_else(CNCRAGE >= 55,"55-64",
                                             if_else(CNCRAGE >= 45,"45-54",
                                                     if_else(CNCRAGE >= 35,"35-44",
                                                             if_else(CNCRAGE >= 25,"25-34",
                                                                     if_else(CNCRAGE >= 15,"15-24","0-14")
                                                             )))))),"years",sep=" "))
table2 <- make.table.1(cancer.survivor.df,"PRIORCNCR",c("CNCRAGE_G","CNCRTYPE"))
table2.factor.value.label.list <- list(CNCRAGE_G.values.df = data.frame(value = paste(c("0-14","15-24",
                                                                                  "25-34","35-44",
                                                                                  "45-54","55-64",
                                                                                  "65+","Don't know / Refused"),
                                                                                  "years",sep=" "),
                                                                        stringsAsFactors = F) %>%
                                         mutate(Factor = "CNCRAGE_G",Factor.desc = "Age Told Had Cancer",
                                                label = value),
                                       CNCRTYPE.values.df = data.frame(value = as.character(c(1:30,77,99)),
                                                                       label = c("Breast cancer",
                                                                                 "Cervical cancer (cancer of the cervix)",
                                                                                 "Endometrial cancer (cancer of the uterus)",
                                                                                 "Ovarian cancer (cancer of the ovary)",
                                                                                 "Head and neck cancer",
                                                                                 "Oral cancer",
                                                                                 "Pharyngeal (throat) cancer",
                                                                                 "Thyroid","Larynx",
                                                                                 "Colon (intestine) cancer",
                                                                                 "Esophageal (esophagus)",
                                                                                 "Liver cancer",
                                                                                 "Pancreatic (pancreas) cancer",
                                                                                 "Rectal (rectum) cancer",
                                                                                 "Stomach",
                                                                                 "Hodgkin´s Lymphoma (Hodgkin’s disease)",
                                                                                 "Leukemia (blood) cancer",
                                                                                 "Non-Hodgkin´s Lymphoma",
                                                                                 "Prostate cancer",
                                                                                 "Testicular cancer",
                                                                                 "Melanoma","Other skin cancer",
                                                                                 NA,
                                                                                 "Lung","Bladder cancer",
                                                                                 "Renal (kidney) cancer",
                                                                                 "Bone","Brain",
                                                                                 "Neuroblastoma","Other",
                                                                                 "Don't know / not sure",
                                                                                 "Refused"),
                                                                       stringsAsFactors = F) %>%
                                         mutate(Factor = "CNCRTYPE",Factor.desc = "Type of Cancer"))
##  combine table 2 factor levels
cat('\nbinding list dfs into single table2.factor.value.label.df')
table2.factor.value.label.df <- bind_rows(table2.factor.value.label.list[[1]],table2.factor.value.label.list[[2]])
if(length(table2.factor.value.label.list)>=3){
  for(i in 3:length(table2.factor.value.label.list)){
    table2.factor.value.label.df <- bind_rows(table2.factor.value.label.df,table2.factor.value.label.list[[i]])
  }
}

##  join print formatting to table2
table2$print.df.formatted <- left_join(table2$print.df,
                                       table2.factor.value.label.df %>%
                                         mutate(value = coalesce(as.character(value),"Unknown")),
                                       by=c("Factor"="Factor","value"="value")) %>%
  mutate(value = coalesce(value,"Unknown"),
         label = coalesce(label,value)) %>%
  select(Factor,Factor.desc,label,`Prior Cancer: Yes n (%)` = Yes) %>% rowwise() %>%
  mutate(Factor.desc = if_else(Factor=="CNCRTYPE",as.character("Type of Cancer"),as.character(Factor.desc)))

##### table 3 #####
##  relationship between cancer status and influenza vaccine
##  2x2 ratio
##  odds ratio
# #### You will need to decide which associations you want to present 
#       (i.e.; association between cancer survivorship and influenza vaccination, 
#       pneumococcal vaccination, both, etc.; and absolute or relative measures of the associations), 
#       and construct your table in such a way to allow the reader to understand what you are presenting.
#       My expectation is that all students will be able to present unadjusted (crude) associations; 
#       I am not expecting anyone to be able to adjust for confounders in their analysis at this time. 
#       However, if someone is interested in controlling for confounders (e.g.; via logistic regression), 
#       please check in with me to make sure we are on the same page.
#  Please pay attention to things like titles, headings, labels, footnotes, significant digits, abbreviations, etc.
##  Prior Cancer (exposure) vs. Flu shot Outcome (exposure)
cat('\nwriting 2x2 tables for FLUSHOT3 and PNEUVAC3')
prior.cancer.2x2s <- list(flu.vacc = make.2x2.table(cancer.df,"PRIORCNCR","FLUSHOT3"),
                          pneu.vacc = make.2x2.table(cancer.df,"PRIORCNCR","PNEUVAC3"),
                          shingles.vacc = make.2x2.table(cancer.df,'PRIORCNCR',"SHINGLES"),
                          tetanus.vacc = make.2x2.table(cancer.df,'PRIORCNCR',"TNSARCV"))
## totaling ##    
prior.cancer.2x2s <- lapply(prior.cancer.2x2s,function(x) x %>%
                              rowwise() %>% mutate(total = No_Prior_Cancer + Yes_Prior_Cancer) %>%
                              bind_rows(data.frame(vacc_outcome = "total",stringsAsFactors = F,
                                                   No_Prior_Cancer = colSums(.[,2:4])[["No_Prior_Cancer"]],
                                                   Yes_Prior_Cancer = colSums(.[,2:4])[["Yes_Prior_Cancer"]],
                                                   total = colSums(.[,2:4])[["total"]])))
##### risk ratios #####
##  (exposure positive & outcome positive / outcome total) / (exposure negative & outcome positive / outcome total)
risk.ratios <- lapply(names(prior.cancer.2x2s),function(x) {
  round((prior.cancer.2x2s[[x]]$Yes_Prior_Cancer[2] / prior.cancer.2x2s[[x]]$Yes_Prior_Cancer[3]) /
          (prior.cancer.2x2s[[x]]$No_Prior_Cancer[2] / prior.cancer.2x2s[[x]]$No_Prior_Cancer[3]),digits = 2)
}
)
names(risk.ratios) = names(prior.cancer.2x2s)

##### write readme  #####
###   write snippet for each table output to excel
##  table 1
table1.readme <- 
  "Table 1 compares counts and proportions of factors across Prior Cancer groups, either Yes, No, or DoN.
I'm taking DoN to mean Don't Know.
'label' is level of factor.
factors without labels will be defined upon getting a better data dictionary.
"
##  table 2
table2.readme <- 
  "Table 2 compares counts and proportions of factors within the Prior Cancer: Yes group ONLY.
Cancer Age and Cancer Type are compared. Cancer Age is categorized into the same categories as Age.
Type of cancer has no definition in data dictionary
"
##  2x2 tables
prior.cancer.2x2s.readme <- 
  "two by two tables take prior cancer status as exposure and vaccine history as outcome
Only patients who had knowledge and chose to report both their cancer history and vaccine history are included
(i.e. no Don't Know / Refused)
"
##  risk ratios
risk.ratios.readme <- 
  "Risk ratios take cancer history as exposure and vaccine history as outcome.
Only patients who had knowledge and choose to report both their cancer history and vaccine history are included
(i.e. no Don't Know / Refused; same as 2x2 tables)
"

##### write tables  #####
cat('\nwriting tables')
ofp <- file.path(gsub(paste0("/",basename(fp)),"",fp),paste0("VaccineCancer_tables_",Sys.Date(),"_ET.xlsx"))
cat('\nbuilding output.list')
output.list <- list(README = as.data.frame(t(as.data.frame(list(table1.readme = table1.readme,
                                                                table2.readme = table2.readme,
                                                                prior.cancer.2x2s.readme = prior.cancer.2x2s.readme,
                                                                risk.ratios.readme = risk.ratios.readme)))) %>% 
                      mutate(output = rownames(.)) %>% select(output,everything()),
                    table1 = as.data.frame(table1$print.df.formatted %>% select(-Factor)),
                    table2 = as.data.frame(table2$print.df.formatted %>% select(-Factor)),
                    risk.ratios = as.data.frame(risk.ratios))
cat('\nadding prior cancer 2x2 tables')
for(i in 1:length(prior.cancer.2x2s)){
  output.list[[names(prior.cancer.2x2s)[i]]] <- as.data.frame(prior.cancer.2x2s[[i]])
}
write_xlsx(x = output.list,path = ofp,col_names = T)
