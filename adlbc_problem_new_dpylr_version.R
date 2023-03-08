library(admiral)
library(dplyr)
library(tidyr)
library(metacore)
library(metatools)
library(xportr)
library(stringr)
library(haven)
packageVersion("xportr")
packageVersion("dplyr")

adsl <- read_xpt("key/adsl.xpt")
lb <- read_xpt("sdtm/lb.xpt")

lb <- convert_blanks_to_na(lb)

# Metadata ----------------------------------------------------------------


# Read in the metadata ----
metacore <- spec_to_metacore("metadata/specs.xlsx", quiet = TRUE, where_sep_sheet = FALSE)

# Get the specifications for the dataset we are currently building
adlbc_spec <- metacore %>%
  select_dataset("ADLBC")

var_spec <- adlbc_spec$var_spec %>%
  mutate(
    format = ifelse(is.na(format), "", format)
  )


# Derivation --------------------------------------------------------------

# ADSL variables
adsl_vars <- vars(AGE,AGEGR1,AGEGR1N,RACE,RACEN,SEX,SAFFL,TRTSDT,TRTEDT,TRT01P,TRT01PN,
                  TRT01A,TRT01AN,COMP24FL,DISCONFL,DSRAEFL,SUBJID,VISNUMEN)

# filter CHEM, derive ADT
lbc <- lb %>%
  filter(LBCAT=='CHEMISTRY') %>%
  subset(!str_detect(VISIT, "AMBUL|RETRIEVAL")) %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  ## Calculate ADT
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = LBDTC
  ) %>%
  ## Calculate DY
  derive_vars_dy(reference_date = TRTSDT, source_vars = vars(ADT)) %>%
  rename (TRTA = TRT01A, TRTAN = TRT01AN, TRTP = TRT01P, TRTPN = TRT01PN )


# add param from codelist
lbc <- lbc %>%
  create_var_from_codelist(adlbc_spec,LBTESTCD, PARAMCD, decode_to_code = FALSE) %>%
  create_var_from_codelist(adlbc_spec,PARAMCD, PARAM)%>%
  create_var_from_codelist(adlbc_spec,PARAM, PARAMN, decode_to_code = TRUE) %>%
  ## renamae variables
  rename(A1HI = LBSTNRHI, A1LO = LBSTNRLO ) %>%
  # reset PARAMCD, derive PARCAT1, A2LO, A2HI
  mutate(PARAMCD = case_when(PARAMN<100 ~ LBTESTCD,
                             PARAMN>100 ~ paste0("_", LBTESTCD)),
         AVAL = LBSTRESN,
         PARCAT1 = "CHEM",
         R2A1HI = AVAL/A1HI,
         R2A1LO = AVAL/A1LO,
         ABLFL = ifelse(VISITNUM==1,"Y","")
  )

# time
lbc <- lbc %>%
  # Derive Timing
  mutate(AVISIT = case_when (VISIT=="SCREENING 1" ~ "BASELINE",
                             str_detect(VISIT, "UNSCHED") ~ "",
                             TRUE ~ VISIT),
         AVISIT = case_when(!is.na(AVISIT) ~ str_to_title(AVISIT),
                            TRUE ~ NA_character_)
  ) %>%
  create_var_from_codelist(adlbc_spec, AVISIT ,AVISITN,decode_to_code = TRUE) %>%
  # derive base variables
  mutate(AVAL_ = case_when(is.na(AVAL) ~ as.numeric(str_remove(LBSTRESC,"[<>=]")),
                           TRUE ~ AVAL),
         ANRIND = case_when(AVAL_ > 1.5*A1HI ~ "H",
                            AVAL_ < 0.5*A1LO ~ "L",
                            TRUE ~ "N"),
         ANRIND = case_when(is.na(AVAL_) ~ NA_character_,
                            TRUE ~ ANRIND)
  ) %>%
  call_derivation(
    derivation = derive_var_base,
    variable_params = list(
      params(source_var = AVAL,   new_var = BASE),
      params(source_var = ANRIND, new_var = BNRIND),
      params(source_var = R2A1LO, new_var = BR2A1LO),
      params(source_var = R2A1HI, new_var = BR2A1HI)
    ),
    by_vars = vars(USUBJID,PARAMCD))%>%
  # CHG
  derive_var_chg() %>%
  mutate(CHG = case_when(is.na(AVISITN) ~ AVAL-BASE,
                         AVISITN == 0 ~ -999,
                         TRUE ~ CHG),
         CHG = na_if(CHG,-999)
  ) %>%
  # ALBTRVAL
  mutate(uln = abs(AVAL - (1.5*A1HI)),
         lln = abs(0.5*A1LO - AVAL),
         ALBTRVAL = pmax(uln,lln)
  )%>%
  select(-c("uln","lln"))

# value @ AVISITN=24 (VISITNUM=12) or less if discontinued
v12 <- lbc %>%
  select(USUBJID,AVISIT,AVISITN,ADT,LBSEQ,PARAMCD,AVAL,DISCONFL,COMP24FL) %>%
  filter(AVISITN <= 24 & AVISITN > 0) %>%
  group_by(USUBJID,PARAMCD) %>%
  arrange(USUBJID,PARAMCD,ADT,LBSEQ) %>%
  slice_tail(n=1) %>%
  ungroup() %>%
  filter((AVISITN==24 & AVISITN > 0) | (DISCONFL=="Y" | COMP24FL=="Y") ) %>%
  mutate(AENTMTFL='Y')


lbc <- lbc %>%
  derive_vars_merged(
    dataset_add = v12,
    new_vars = vars(AENTMTFL),
    by_vars = vars(USUBJID,AVISIT,AVISITN,ADT,LBSEQ,PARAMCD,AVAL)
  )


# ANL01FL

anfl <- lbc %>%
  filter(!is.na(AVISITN) & AVISITN != 0) %>%
  group_by(USUBJID,PARAMCD) %>%
  arrange(USUBJID,PARAMCD,desc(ALBTRVAL), ADT) %>%
  slice_head(n=1) %>%
  ungroup()%>%
  mutate(ANL01FL = 'Y')

lbc <- lbc %>%
  left_join(anfl %>% select(USUBJID,PARAMCD,AVISITN,ADT,ALBTRVAL,ANL01FL))


eot <- lbc %>%
  filter(AENTMTFL=="Y") %>%
  mutate(AVISIT = "End of Treatment",
         AVISITN = 99)

adlbc <- rbind(lbc,eot)


# fix dec place
adlbc <- adlbc %>%
  mutate_at(c("A1LO","A1HI","LBSTRESN"),round,5)

adlbc <- drop_unspec_vars(adlbc, adlbc_spec)


# Apply Metadata ----
adlbc <- adlbc %>%
  order_cols(adlbc_spec) %>%
  set_variable_labels(adlbc_spec)%>%
  check_ct_data(adlbc_spec) %>%
  check_variables(adlbc_spec)

#remove.packages("dplyr")
#remotes::install_version("dplyr")

# export format and xpt


adlbc <- adlbc %>%
  xportr_format(var_spec) %>%
  xportr_df_label(adlbc_spec) %>%
  xportr_write(path = "adam/adlbc.xpt")




