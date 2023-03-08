# install.packages("haven")
# install.packages("admiral")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("metacore")
# install.packages("metatools")
# install.packages("xportr")
# install.packages("readxl")
# install.packages("stringr")
# install.packages("lubridate")
# install.packages("Hmisc")
# install.packages("janitor")


library(haven)
library(admiral)
library(dplyr)
library(tidyr)
library(metacore)
library(metatools)
library(xportr)
library(stringr)
library(lubridate)
#library(Hmisc)
#library(janitor)

##import required datasets
vs <- read_xpt("./sdtm/vs.xpt")
adsl <- read_xpt("./key/adsl.xpt")

##import the specs
metacore <- metacore::spec_to_metacore("metadata/specs.xlsx",
                                       where_sep_sheet = F, quiet = T)

advs_metacore <- metacore %>%  select_dataset ('ADVS')
specs_part1 <- advs_metacore$ds_vars
specs_part2 <- advs_metacore$derivations %>% mutate(variable = str_split_i(derivation_id,"\\.",2))
specs <- left_join(specs_part1,specs_part2, by= "variable")

#correct an error in the specs
specs[specs$variable == "PARAM","derivation"] <- "VS.VSTEST plus units"
specs[specs$variable == "ATPTN","derivation"] <- "VS.VSTPTNUM"
specs[specs$variable == "TRTA","derivation"] <- "ADSL.TRT01A"

#advs1 <- build_from_derived(advs_metacore, list("vs" = vs, "adsl" = adsl), predecessor_only = FALSE, keep = TRUE)

#specs[,c("vs_needed","adsl_needed","simple_fl")] <- c()

for (i in 1:dim(specs)[1]){
  desc <- as.character(specs[i,"derivation"])
  afterpoint <- str_split_1(desc,"\\.")[2]

  if (is.na(afterpoint)){
    specs[i,"simple_fl"] <- "N"
    specs[i,"to_do"] <- "Y"
  }

  else{
    if (str_detect(desc, "VS.")){
      specs[i,"vs_needed"] <- str_split_1(afterpoint," ")[1]
      specs[i,"input"] <- str_split_1(afterpoint," ")[1]

    }
    else if (str_detect(desc, "ADSL.")){
      specs[i,"adsl_needed"] <- str_split_1(afterpoint," ")[1]
      specs[i,"input"] <- str_split_1(afterpoint," ")[1]

    }

    if (length(str_split_1(afterpoint," ")) == 1 & afterpoint %in%  c(colnames(vs),colnames(adsl))) {
      specs[i,"simple_fl"] <- "Y"

      if (afterpoint == specs[i,"variable"]){
        specs[i,"to_do"] <- "N"
      }
      else{
        specs[i,"to_do"] <- "Y"
      }

    }

    else {
      specs[i,"simple_fl"] <- "N"
      specs[i,"to_do"] <- "Y"
    }
  }



}

rm(desc,i,afterpoint)

vs_needed <- c(na.exclude(specs$vs_needed), "VSSTRESU")
adsl_needed <- na.exclude(specs$adsl_needed)
simple_variables <- specs$variable[specs$simple_fl=="Y"]
simple_variables_todo <- specs$variable[specs$simple_fl=="Y"&specs$to_do =="Y"]
complex_variables_todo <- specs$variable[specs$simple_fl=="N"&specs$to_do =="Y"]
#simple_variables_todo_vs <- specs$Variable[specs$simple_fl=="Y"&specs$to_do =="Y"&!is.na(specs$vs_needed)]
#simple_variables_todo_adsl <- specs$Variable[specs$simple_fl=="Y"&specs$to_do =="Y"&!is.na(specs$adsl_needed)]

##keep only the records in vs that are subjects present in adsl and variables in both that are really needed
subjects <- adsl$USUBJID
vs_filtered <- filter(vs,USUBJID %in% subjects)
vs_selected <- select(vs_filtered, c("USUBJID",any_of(vs_needed)))
adsl_selected <- select(adsl, any_of(adsl_needed))

##merging them together
advs1 <- left_join(adsl_selected,vs_selected, by = "USUBJID")

##simple variables

for (variable in simple_variables_todo){
  value <- as.character(specs[specs$variable == variable,"input"])
  advs1[,variable] <- advs1[,value]
  #print(variable)
  #print(value)
}

rm(value,variable)


##coded variables
codelist <-  advs_metacore$codelist %>% mutate(name = str_split_i(name,"_",1))
coded_variables <- complex_variables_todo[complex_variables_todo%in%codelist$name]
complex_variables_todo <- complex_variables_todo[!complex_variables_todo%in%coded_variables]

#Param is not a true coded variable, it is VSTEST concatenated with VSSTRESU
advs2 <- advs1 %>% mutate(
  VSSTRESU_ = case_when(VSSTRESU == "BEATS/MIN"~ tolower(VSSTRESU),
                        VSSTRESU == "" & PARAMCD == "DIABP" ~ "mmHg",
                        VSSTRESU == "" & PARAMCD == "SYSBP" ~ "mmHg",
                        VSSTRESU == "" & PARAMCD == "PULSE" ~ "beats/min",
                        TRUE ~ VSSTRESU),
  PARAM = paste0(VSTEST," (",VSSTRESU_,")"),
  VISIT_ = if_else(VISIT == "END OF TREATMENT", "End of Treatment",str_to_sentence(VISIT)))

advs3 <- advs2 %>%
  create_cat_var(advs_metacore, ref_var = AGE,
                 grp_var = AGEGR1, num_grp_var = AGEGR1N) %>%
  create_var_from_codelist(metacore = advs_metacore,
                           input_var = RACE,
                           out_var = RACEN) %>%
  create_var_from_codelist(metacore = advs_metacore,
                           input_var = PARAM,
                           out_var = PARAMN) %>%
  create_var_from_codelist(metacore = advs_metacore,
                           input_var = VISIT_,
                           out_var = AVISIT)  %>%
  create_var_from_codelist(metacore = advs_metacore,
                           input_var = AVISIT,
                           out_var = AVISITN)





packageVersion("dplyr")
##complex variables

#now the complex variables remaining are
complex_variables_todo



#ADT, ANL01FL
advs4<- mutate(advs3,
               #ADT
               ADT = ymd(VSDTC),
               #ANL01FL
               ANL01FL = if_else(is.na(AVISIT),"","Y")
)


#advs3_5 <- mutate(advs3,
#                  AVISIT = if_else(is.na(AVISIT), "", AVISIT))

#BASE
bases <- filter(advs4, ABLFL == "Y") %>% rename("BASE" = "AVAL") %>% select(c("USUBJID","PARAM","BASETYPE","BASE"))
advs5 <- left_join(advs4,bases, by = c("USUBJID","PARAM","BASETYPE"))


#CHG and PCHG
advs6 <- mutate(advs5,
                #CHG
                CHG = AVAL - BASE,
                #PCHG
                PCHG = 100*(CHG/BASE)
)

advs_raw <- select(advs6, specs$variable) %>% mutate(TRTAN = if_else(TRTAN == 2,0,TRTAN),
                                                     TRTPN = if_else(TRTPN == 2,0,TRTPN)
)


advs_raw %>%
  check_variables(advs_metacore) %>% # Check all variables specified are present and no more
  check_ct_data(advs_metacore, na_acceptable = TRUE) %>% # Checks all variables with CT only contain values within the CT
  order_cols(advs_metacore) %>% # Orders the columns according to the spec
  sort_by_key(advs_metacore) %>% # Sorts the rows by the sort keys
  xportr_type(advs_metacore) %>% # Coerce variable type to match spec
  xportr_length(advs_metacore) %>% # Assigns SAS length from a variable level metadata
  xportr_label(advs_metacore) %>% # Assigns variable label from advs_metacore specifications
  xportr_df_label(advs_metacore) %>% # Assigns dataset label from metacore specifications
  xportr::xportr_write("./adam/advs.xpt")

#read_xpt("./adam/advs.xpt")

