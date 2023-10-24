census21 <- function(geog.code){
  
  # Pop density
  tib.popdens.nomis <-  nomis_get_data(id = "NM_2026_1", geography = geog.code)
  tib.popdens <- tib.popdens.nomis %>% select(GEOGRAPHY_CODE, OBS_VALUE) %>% rename(popdens = OBS_VALUE)
  
  # Deprivation measures
  # There are 6 levels (number and %). Gives a deprivation indication but not which domain the deprivation
  # is caused by. We're interested in "% households not deprived in any dimension"
  tib.depriv.nomis <-  nomis_get_data(id = "NM_2031_1", geography = geog.code)
  tib.depriv <- tib.depriv.nomis %>% 
    filter(C2021_DEP_6==1 & MEASURES_NAME=="Percent") %>% select(GEOGRAPHY_CODE, OBS_VALUE) %>% rename(pnodep = OBS_VALUE)
  
  # General health (observed and age standardised). NB adjusted values aren't available at the LSOA level.
  tib.health.nomis <-  nomis_get_data(id = "NM_2055_1", geography = geog.code)
  tib.healthasp.nomis <-  nomis_get_data(id = "NM_2092_1", geography = geog.code)
  
  # Poor health = Bad health (code 4) + Very bad health (code 5)
  tib.health.long <- tib.health.nomis %>% filter(C2021_HEALTH_6 %in% c(4,5) & MEASURES_NAME=="Percent") %>%select(GEOGRAPHY_CODE, C2021_HEALTH_6_CODE, OBS_VALUE)
  tib.health <- tib.health.long %>% pivot_wider(names_from=C2021_HEALTH_6_CODE, values_from=OBS_VALUE) %>% mutate(poorhealth =`_4` +`_5`) %>% select(-`_4`, -`_5`)
  
  # Adjusted Poor health = Bad health (code 3) + Very bad health (code 4)
  tib.healthasp.long <- tib.healthasp.nomis %>% filter(C2021_HEALTH_5 %in% c(3,4) & MEASURES_NAME=="Value") %>%select(GEOGRAPHY_CODE, C2021_HEALTH_5_CODE, OBS_VALUE)
  tib.healthasp <- tib.healthasp.long %>% pivot_wider(names_from=C2021_HEALTH_5_CODE, values_from=OBS_VALUE) %>% mutate(adjpoorhealth =`_4` +`_5`) %>% select(-`_4`, -`_5`)
  
  # Disability (observed and age-standardised)
  tib.disable.nomis <-  nomis_get_data(id = "NM_2056_1", geography = geog.code)
  tib.disableasp.nomis <-  nomis_get_data(id = "NM_2093_1", geography = geog.code)  
  
  tib.disable <- tib.disable.nomis %>% 
    filter(C2021_DISABILITY_5==1001 & MEASURES_NAME=="Percent") %>% select(GEOGRAPHY_CODE, OBS_VALUE) %>% rename(disable = OBS_VALUE)
  
  tib.disableasp <- tib.disableasp.nomis %>% 
    filter(C2021_DISABILITY_3==2) %>% mutate(adjdisable =100 - OBS_VALUE) %>% select(GEOGRAPHY_CODE, adjdisable) 
  
  # Overcrowding
  tib.crowd.nomis <-  nomis_get_data(id = "NM_2070_1", geography = geog.code)
  # Occupancy numbers of -1 (code 4) and -2 or less (code 5) implies overcrowded
  tib.crowd.long <- tib.crowd.nomis %>% filter(C2021_OCCRAT_BEDROOMS_6 %in% c(4,5) & MEASURES_NAME=="Percent") %>%select(GEOGRAPHY_CODE, C2021_OCCRAT_BEDROOMS_6_CODE, OBS_VALUE)
  tib.crowd <- tib.crowd.long %>% pivot_wider(names_from=C2021_OCCRAT_BEDROOMS_6_CODE, values_from=OBS_VALUE) %>% mutate(overcrowd =`_4` +`_5`) %>% select(-`_4`, -`_5`)
  
  # Tenure - socially rented
  tib.tenure.nomis <- nomis_get_data(id = "NM_2072_1", geography = geog.code)
  tib.tenure <- tib.tenure.nomis %>%
    filter(C2021_TENURE_9==1003 & MEASURES_NAME=="Percent") %>% select(GEOGRAPHY_CODE, OBS_VALUE) %>% rename(socialrent = OBS_VALUE)
  
  # Socio-economic class, never worked or long-term unemployed, or full-time student
  tib.employ.nomis <- nomis_get_data(id = "NM_2079_1", geography = geog.code)
  tib.employ.long <- tib.employ.nomis %>%
    filter(C2021_NSSEC_10 %in% c(8,9) & MEASURES_NAME=="Percent") %>% select(GEOGRAPHY_CODE, C2021_NSSEC_10_CODE, OBS_VALUE) 
  tib.employ <- tib.employ.long %>% pivot_wider(names_from=C2021_NSSEC_10_CODE, values_from=OBS_VALUE) %>% mutate(nowork =`_8`,student=`_9`) %>% select(-`_8`, -`_9`)
  
  # Communal establishment (TS001)
  tib.communal.nomis <- nomis_get_data(id = "NM_2021_1", geography = geog.code)
  tib.communal <- tib.communal.nomis %>%
    filter(C2021_RESTYPE_3==2 & MEASURES_NAME=="Percent") %>% select(GEOGRAPHY_CODE, OBS_VALUE) %>% rename(communal = OBS_VALUE)
  
  # No car or van (TS045)
  tib.car.nomis <- nomis_get_data(id = "NM_2063_1", geography = geog.code)
  tib.car <- tib.car.nomis %>%
    filter(C2021_CARS_5==1 & MEASURES_NAME=="Percent") %>% select(GEOGRAPHY_CODE, OBS_VALUE) %>% rename(nocar = OBS_VALUE)
  
  
  # Central heating (TS046)
  tib.heat.nomis <- nomis_get_data(id = "NM_2064_1", geography = geog.code)
  tib.heat <- tib.heat.nomis %>%
    filter(C2021_HEATING_13==1 & MEASURES_NAME=="Percent") %>% select(GEOGRAPHY_CODE, OBS_VALUE) %>% rename(noheat = OBS_VALUE)
  
  # Education -highest qualification (TS067)
  tib.educate.nomis <- nomis_get_data(id = "NM_2084_1", geography = geog.code)
  tib.educate <- tib.educate.nomis %>%
    filter(C2021_HIQUAL_8==1 & MEASURES_NAME=="Percent") %>% select(GEOGRAPHY_CODE, OBS_VALUE) %>% rename(noqual = OBS_VALUE)
  
  # Sex - % female (TS008)
  tib.sex.nomis <- nomis_get_data(id = "NM_2028_1", geography = geog.code)
  tib.sex <- tib.sex.nomis %>%
    filter(C_SEX==1 & MEASURES_NAME=="Percent") %>% select(GEOGRAPHY_CODE, OBS_VALUE) %>% rename(female = OBS_VALUE)
  
  
  # Age by 5 year band  - % > 65 (TS007A)
  tib.age.nomis <- nomis_get_data(id = "NM_2020_1", geography = geog.code)
  tib.age.long <- tib.age.nomis %>%
    filter(C2021_AGE_19 %in% 14:18 & MEASURES_NAME=="Percent") %>% select(GEOGRAPHY_CODE, C2021_AGE_19_CODE, OBS_VALUE)
  tib.age <- tib.age.long %>% pivot_wider(names_from=C2021_AGE_19_CODE, values_from=OBS_VALUE) %>% mutate(over65 =`_14` +`_15`+`_16`+`_17`+`_18`) %>% select(-`_14`, -`_15`,-`_16`,-`_17`,-`_18`)
  
  # Migrant indicator - outside UK (TS019)
  tib.migrant.nomis <- nomis_get_data(id = "NM_2039_1", geography = geog.code)
  tib.migrant <- tib.migrant.nomis %>%
    filter(C2021_MIGIND_4==4 & MEASURES_NAME=="Percent") %>% select(GEOGRAPHY_CODE, OBS_VALUE) %>% rename(nonukmigrant = OBS_VALUE)
  
  # Ethnicity - % identified as white (all categories) (TS021)
  tib.ethnicity.nomis <- nomis_get_data(id = "NM_2041_1", geography = geog.code)
  tib.ethnicity <- tib.ethnicity.nomis %>%
    filter(C2021_ETH_20==1004 & MEASURES_NAME=="Percent") %>% select(GEOGRAPHY_CODE, OBS_VALUE) %>% rename(white = OBS_VALUE)
  
  
  # Join all census data together
  dat.out <- purrr::reduce(list(tib.popdens,tib.depriv, tib.health, tib.healthasp,
                                tib.disable, tib.disableasp, tib.crowd, tib.tenure, tib.employ, 
                                tib.communal, tib.car, tib.heat, tib.educate, tib.age, tib.sex, tib.migrant, tib.ethnicity), dplyr::left_join, by = 'GEOGRAPHY_CODE')
  
  return(dat.out)
}