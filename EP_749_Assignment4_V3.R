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
# a.	These shells can be rough drafts you are not required to have settled 
#   on the final content or format of your tables for this assignment. 
#   I want you to turn something in, however, so I can see who needs 
#   some help to think through their analysis. 
# b.	We will spend time in class discussing your table shells from the perspective 
#   of how to plan an analysis (we will not be discussing formatting).

##  V2  ##
##  2018 10 13 :: ET
##  change excel writing library to writexl

##  TODO  ##
##  2018 11 01
##    include subjects missing data for education and marital status

##### load libraries  #####
library(dplyr)
library(tidyr)
library(epitools)
library(writexl)
library(readxl)

##### table 1 function  #####
##  define groups by cancer status
##  summarize, at minimum, demographics
##    sex, age, race/ethnicity, marital status, education, employment, annual income, 
##    health coverage, shingles vacc, tetanus vacc
##  include overall-population (w/out grouping) in output
make.table.1 <- function(df,group.var,demog.var){
  ##  group.var is variable to group
  ##  demog.var is character vector of variables to summarize across groups
  ##  calculate n in each group
  group.count <- df %>% select_at(vars(one_of(group.var))) %>% group_by_all() %>% count() %>% spread(group.var,n) %>%
    mutate(`Total Population` = rowSums(.,na.rm = F))
  ##  make count table
  # make first row counts
  count.df <- data.frame(Factor = "Total",
                         value = "Total",
                         n = as.numeric(df %>% count()),stringsAsFactors = F) %>%
    rename(`Total Population` = n) %>%
    bind_cols(df %>%
                select_at(vars(one_of(group.var))) %>%
                group_by_all() %>% count()%>%
                group_by_at(group.var) %>% spread(group.var,n))
  ##  loop through demographic variables to make table of patient counts
  for(i in 1:length(demog.var)){
    cat('\nbinding',demog.var[i])
    # create row with count across groups
    total.pop.row <-
      df %>% ungroup() %>% select_at(demog.var[i]) %>% group_by_all() %>% count() %>%
      mutate(Factor = colnames(.)[1]) %>% select(Factor,everything()) %>%
      ungroup() %>%
      rename_at(vars(one_of(demog.var[i])),funs(sub(demog.var[i],"value",.))) %>%
      rename(`Total Population` = n) %>%
      mutate(value = as.character(value))
    #colnames(row.to.bind) <- 'All Participants'
    #cat('\nworking on total.pop.row adding cross-group counts. first glimpse:\n')  
    #print(total.pop.row)
    # create row with count within each groups
    group.pop.row <- df %>%
      select_at(vars(one_of(group.var,demog.var[i]))) %>%
      group_by_all() %>% count() %>%
      group_by_at(group.var) %>% spread(group.var,n) %>%
      mutate(Factor = colnames(.)[1]) %>% select(Factor,everything()) %>%
      rename_at(vars(one_of(demog.var[i])),funs(sub(demog.var[i],"value",.))) %>%
      mutate(value = as.character(value))
    #cat('\nworking on group.pop.row:\n')
    #print(group.pop.row)
    # bind cols into row; bind row into dataframe
    row.to.bind <- left_join(total.pop.row,group.pop.row,by=c("Factor"="Factor","value"="value"))
    count.df <- bind_rows(count.df,row.to.bind)
  }
  # change NA to 0
  count.df <- count.df %>%
    mutate_if(is.numeric,funs(if_else(is.na(.),as.numeric(0),as.numeric(.))))
  ##  make percent table
  cat('\ncreating percent df\n\n')
  perc.df <- count.df
  #print(perc.df)
  for(perc.df.col in colnames(perc.df)[3:length(colnames(perc.df))]){
    #print(group.count)
    #print(perc.df.col)
    #cat('\ngroup.count[[perc.df.col]]:',group.count[[perc.df.col]])
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
#TODO#  #   Add Risk ratios to Table 3s
make.2x2.table <- function(df,exposure,outcome){
  rt.df <- df %>% select_at(vars(one_of(exposure,outcome))) %>% 
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
    group_by_at(vars(one_of(exposure))) %>% spread(exposure,n) %>% as.data.frame()
  # transpose so exposures are rows and outcomes are columns
  rt.df2 <- rt.df %>% select(-vacc_outcome)
  rownames(rt.df2) = rt.df$vacc_outcome
  rt.df2 <- t(rt.df2)
  # add totals
  rt.df2 <- cbind(rt.df2,"total" = rowSums(rt.df2))
  rt.df2 <- rbind(rt.df2,"total" = colSums(rt.df2))
  # add risk ratio
  rt.df2 <- cbind(rt.df2,"risk ratio" = rt.df2[,2]/rt.df2[,3])
  # change back to data frame
  # return
  return(as.data.frame(cbind(rt.df2,"exposure" = rownames(rt.df2))) %>% select(exposure,everything()))
}

##### read in file  #####
cat('\nchoose file')
fp <- file.choose()
cat('\nreading excel file')
cancer.df <- read_excel(path = fp,sheet = "Raw Data")

##### process df  #####
##    since age included non-age numbers, DON'T USE AGE
cat('\nprocessing cancer df from excel
only including subjects who know if they have or have not had a cancer diagnosis')
CNCRTYPE_G.values.df =data.frame(value = as.character(c(1:29,77,99)),
                                 label = c("breast",
                                           rep("cervical_enrometrial_ovarian",3),
                                           rep("other",2),
                                           "gastrointestinal","other",
                                           rep("gastrointestinal",2),rep("other",2),
                                           rep("gastrointestinal",2),"other",
                                           "leukemia","other",rep("prostate_testicular",2),
                                           rep("skin",2),"other","lung",rep("bladder_kidney",2),
                                           "other",rep("brain",2),rep("other",3)),
                                 stringsAsFactors = F)
##  process cancer.df
cancer.df <- cancer.df %>% mutate(IDATE = as.Date(IDATE,format = "%m%d%Y"),
                                  # group don't know, not sure, refused, and no data
                                  CNCRAGE_G = if_else(is.na(CNCRAGE) | CNCRAGE %in% c(98,99),"Don't Know / Not Sure / Refused",
                                                      if_else(CNCRAGE >=65,"65+",
                                                              if_else(CNCRAGE >= 55,"55-64",
                                                                      if_else(CNCRAGE >= 45,"45-54",
                                                                              if_else(CNCRAGE >= 35,"35-44",
                                                                                      if_else(CNCRAGE >= 25,"25-34",
                                                                                              if_else(CNCRAGE >= 15,"15-24","0-14")
                                                                                      ))))))) %>%
  # group cancer types
  left_join(CNCRTYPE_G.values.df %>% mutate(value = as.numeric(value)),by=c("CNCRTYPE"="value")) %>% 
  rename(CNCRTYPE_G=label) %>%
  # add 'X' to beginning of variables that start with '_'
  rename_at(vars(starts_with("_")),funs(paste0("X",.))) %>%
  # calculate CNCRHAVE_text
  left_join(data.frame(level = c(1,2,7,9),
                       CNCRHAVE_TEXT = c("Yes","No","Don't Know","Refused")),
            by=c("CNCRHAVE"="level")) %>%
  # calculate FLUVACC_ANY
  rowwise() %>% mutate(FLUVACC_ANY = min(FLUSHOT3,FLUSPRY2)) %>% ungroup() %>%
  # filtering subjects 
  #   reported prior cancer diagnoses
  #   reported FLUVACC_ANY
  #   reported PNEUVAC3
  #   Any income group (including unknown)
  #   Known health plan
  #   Known MEDCOST
  filter(CNCRHAVE %in% c(1,2) &
           FLUVACC_ANY %in% c(1,2) &
           PNEUVAC3 %in% c(1,2) &
           #MARITAL != 9 &
           #X_EDUCAG != 9 & ##  any marital or education status
           HLTHPLAN %in% c(1,2) &
           MEDCOST %in% c(1,2))

##### create table 1 #####
##  2018 10 21 :: ET 
##    Don't use PRIORCNCR!! It's a fucked-up variable, I don't know why, but it is. Use CNCRHAVE_TEXT
demog.var <- c("FLUVACC_ANY","SEX","PNEUVAC3",
               "X_AGE_G","X_RACE_G",
               "MARITAL","X_EDUCAG",
               "X_INCOMG","HLTHPLAN",
               "SHINGLES","TNSARCV","MEDCOST")
group.var <- "CNCRHAVE_TEXT"
table1 <- make.table.1(cancer.df %>% mutate_at(vars(one_of(group.var)),funs(paste("Prior Cancer:",.,sep=" "))),
                       group.var,demog.var)
######     make list of data frames for each factor containing values and descriptions  #####
##  replace factor values with meaningful info
#     then join factor values df to print table
cat('\ncreating table1.factor.value.label.list')
table1.factor.value.label.list = list(
  FLUSHOT3.values.df = data.frame(value = c(1,2,7,9),
                                  label = c("Yes","No","Don't Know","Refused"),
                                  stringsAsFactors = F) %>%
    mutate(Factor = "FLUVACC_ANY",Factor.desc = "Any Flu vacc in past 12 mo."),
  FLUSHOT3.values.df = data.frame(value = c(1,2,7,9),
                                  label = c("Yes","No","Don't Know","Refused"),
                                  stringsAsFactors = F) %>%
    mutate(Factor = "FLUSHOT3",Factor.desc = "Flu shot in past 12 mo."),
  PNEUVAC3.values.df = data.frame(value = c(1,2,7,9),
                                  label = c("Yes","No","Don't Know","Refused"),
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
                                            "Multi-racial, non-Hispanic",
                                            "Don't Know"),
                                  stringsAsFactors = F) %>%
    mutate(Factor = "X_RACE_G",Factor.desc = "Race Group"),
  MARITAL.values.df = data.frame(value = c(1,2,3,4,5,6,9),
                                 label = c("Married","Divorced","Widowed",
                                           "Separated","Never Married",
                                           "A member of an unmarried couple",
                                           "Refused"),
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
                                            "Don't know/Not sure/Missing"),
                                  stringsAsFactors = F) %>%
    mutate(Factor = "X_INCOMG",Factor.desc = "Income Group"),
  HLTHPLAN.values.df = data.frame(value = c(1,2,7,9),
                                  label=c("Yes","No","Don't Know","Refused"),
                                  stringsAsFactors = F) %>%
    mutate(Factor = "HLTHPLAN",Factor.desc = "Health plan"),
  SHINGLES.values.df = data.frame(value = c(1,2,7,NA),
                                  label=c("Yes","No","Don't Know","no data"),
                                  stringsAsFactors = F) %>%
    mutate(Factor = "SHINGLES",Factor.desc = "Shingles vaccine"),
  TNSARCV.values.df = data.frame(value = c(1,2,7,9,NA),
                                 label=c("Yes","No","Don't Know",
                                         'Refused',"no data"),
                                 stringsAsFactors = F) %>%
    mutate(Factor = "TNSARCV",Factor.desc = "Tetanus vacc. within 10 y."),
  MEDCOST.values.df = data.frame(value  = c(1,2,7,9),
                                 label=c("Yes","No","Don't Know",
                                         'Refused'),stringsAsFactors = F) %>%
    mutate(Factor = "MEDCOST",Factor.desc = "Could not see Dr. b/c of cost")
)
##  combine values with labels into a factor values table
cat('\nbinding list dfs into single table1.factor.value.label.df')
table1.factor.value.label.df <- bind_rows(table1.factor.value.label.list)

##### print-formatting for table 1  #####
# join factor & value labels to table 1 print.df
table1$print.df.formatted <- left_join(table1$print.df,
                                       table1.factor.value.label.df %>% 
                                         mutate(value = as.character(value)),
                                       by=c("Factor"="Factor","value"="value")) %>% 
  mutate(Factor.desc = coalesce(Factor.desc,Factor),
         label = coalesce(label,value)) %>%
  select(Factor,Factor.desc,label,everything(),-value)
##### create table 2  #####
##  info about cancer survivors ONLY
#     e.g. type of cancer
##  features: CNCRAGE,CNCRTYPE,
##  create CNCRAGE_G
#cancer.survivor.df <- cancer.df %>% filter(CNCRHAVE==1)
table2 <- make.table.1(cancer.df,"CNCRHAVE_TEXT",c("CNCRAGE_G","CNCRTYPE","CNCRTYPE_G"))
table2.factor.value.label.list <- list(CNCRAGE_G.values.df = data.frame(value = c("0-14","15-24",
                                                                                  "25-34","35-44",
                                                                                  "45-54","55-64",
                                                                                  "65+",
                                                                                  "Don't Know / Not Sure",
                                                                                  "no data",
                                                                                  "Refused",
                                                                                  "Don't Know / Not Sure / Refused"),
                                                                        stringsAsFactors = F) %>%
                                         mutate(Factor = "CNCRAGE_G",Factor.desc = "Age Told Had Cancer",
                                                label = value),
                                       #  2018 10 21 :: change cancer types as of 2009 BRFSS
                                       CNCRTYPE.values.df = data.frame(value = as.character(c(1:29,77,99)),
                                                                       label = c("Breast cancer",
                                                                                 "Cervical cancer (cancer of the cervix)",
                                                                                 "Endometrial cancer (cancer of the uterus)",
                                                                                 "Ovarian cancer (cancer of the ovary)",
                                                                                 "Head and neck cancer",
                                                                                 "Oral cancer",
                                                                                 "Pharyngeal (throat) cancer",
                                                                                 "Thyroid",
                                                                                 "Colon (intestine) cancer",
                                                                                 "Esophageal (esophagus)",
                                                                                 "Liver cancer",
                                                                                 "Pancreatic (pancreas) cancer",
                                                                                 "Rectal (rectum) cancer",
                                                                                 "Stomach",
                                                                                 "Hodgkin's Lymphoma (Hodgkin's disease)",
                                                                                 "Leukemia (blood) cancer",
                                                                                 "Non-Hodgkin´s Lymphoma",
                                                                                 "Prostate cancer",
                                                                                 "Testicular cancer",
                                                                                 "Melanoma","Other skin cancer",
                                                                                 "Heart",
                                                                                 "Lung","Bladder cancer",
                                                                                 "Renal (kidney) cancer",
                                                                                 "Bone","Brain",
                                                                                 "Neuroblastoma","Other",
                                                                                 "Don't know / not sure",
                                                                                 "Refused"),
                                                                       stringsAsFactors = F) %>%
                                         mutate(Factor = "CNCRTYPE",Factor.desc = "Type of Cancer"),
                                       CNCRTYPE_G.values.df = CNCRTYPE_G.values.df %>%
                                         mutate(Factor = "CNCRTYPE_G",Factor.desc = "Cancer group"))

##  combine table 2 factor levels
cat('\nbinding list dfs into single table2.factor.value.label.df')
table2.factor.value.label.df <- bind_rows(table2.factor.value.label.list)

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
cat('\nwriting 2x2 tables for FLUVAC_ANY and PNEUVAC3')
prior.cancer.2x2s <- list(flu.vacc = make.2x2.table(cancer.df,"CNCRHAVE_TEXT","FLUVACC_ANY") %>%
                            mutate_if(is.factor,funs(levels(.)[.])) %>% 
                            mutate_at(vars(-matches("exposure")),funs(as.numeric(.))),
                          pneu.vacc = make.2x2.table(cancer.df,"CNCRHAVE_TEXT","PNEUVAC3") %>%
                            mutate_if(is.factor,funs(levels(.)[.])) %>% 
                            mutate_at(vars(-matches("exposure")),funs(as.numeric(.)))
                          )

##### epitools  #####
cat('\nwriting epitabs')
prior.cancer.epitabs <- list(flu.vacc.epi = epitab(x=as.matrix(prior.cancer.2x2s$flu.vacc[c(1,2),c(2,3)]),method = "riskratio"),
                             pneu.vacc.epi = epitab(x=as.matrix(prior.cancer.2x2s$pneu.vacc[c(1,2),c(2,3)]),method = "riskratio"))
##### write readme  #####
###   write snippet for each table output to excel
##  table 1
table1.readme <- 
  "Table 1 compares counts and proportions of factors across CNCRHAVE groups.
FLUSHOT_ANY is combined flu vacc and flu spray
Only subjects who know if they have had a prior cancer diagnosis ('Yes' or 'No') are included
'label' is level of factor.
filtering subjects
  reported prior cancer diagnoses
  reported FLUVACC_ANY
  any answer to PNEUVAC3
  Known Marital Status
  known education level
  Any income group (including unknown)
  Known health plan
  Known MEDCOST
"
##  table 2
table2.readme <- 
  "Table 2 compares counts and proportions of factors within the Prior Cancer: Yes group ONLY.
Cancer Age and Cancer Type are compared. Cancer Age is categorized into the same categories as Age.
Type of cancer has been updated with most accurate known defs
"
##  2x2 tables
prior.cancer.2x2s.readme <- 
  "two by two tables take prior cancer status as exposure and vaccine history as outcome
Only patients who had knowledge and chose to report both their cancer history and vaccine history are included
(i.e. no Don't Know / Refused)
risk ratios are calculated where yes vac is outcome
"

##### write tables  #####
cat('\nwriting tables')
ofp <- file.path(gsub(paste0("/",basename(fp)),"",fp),paste0("VaccineCancer_tables_",Sys.Date(),"_ET.xlsx"))
cat('\nbuilding output.list')
output.list <- list(README = as.data.frame(t(as.data.frame(list(table1.readme = table1.readme,
                                                                table2.readme = table2.readme,
                                                                prior.cancer.2x2s.readme = prior.cancer.2x2s.readme)))) %>% 
                      mutate(output = rownames(.)) %>% select(output,everything()),
                    table1 = as.data.frame(table1$print.df.formatted %>% select(-Factor)),
                    table2 = as.data.frame(table2$print.df.formatted %>% select(-Factor)),
                    processed.df = as.data.frame(cancer.df))
cat('\nadding prior cancer 2x2 tables')
for(i in 1:length(prior.cancer.2x2s)){
  output.list[[names(prior.cancer.2x2s)[i]]] <- as.data.frame(prior.cancer.2x2s[[i]])
}
for(i in 1:length(prior.cancer.epitabs)){
  output.list[[names(prior.cancer.epitabs)[i]]] <- as.data.frame(prior.cancer.epitabs[[i]])
}
write_xlsx(x = output.list,path = ofp,col_names = T)
