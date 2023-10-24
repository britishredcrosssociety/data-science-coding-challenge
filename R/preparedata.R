# Uk 2021 Census analysis for British Red Cross job application


devtools::install_github("humaniverse/geographr")
pacman::p_load(tidyverse, nomisr, geographr, sf)


######################################
#  Get Census variables for analysis #
######################################

# Used https://www.nomisweb.co.uk/api/v01/dataset/def.htm to find the Id
# of the relevant datasets

# Read in function to download Census 2021 data variables at a specified geography

source("R/census21.R")


# Get Census variables at county/UA-level for all counties
dat.all.counties <- census21("TYPE155")


# Get Census variables at LSOA-level for all counties in Merseyside
merseyside.lsoa <- c("650117151TYPE151","650117152TYPE151","650117153TYPE151","650117154TYPE151","650117155TYPE151")
dat.merseyside.lsoa <- census21(merseyside.lsoa)

######################################
#       Get boundary data            #
######################################

# UTLA (counties and UAs) from geographr package
utla21 <- boundaries_utla21

# Create merseyside subset
utla21_merseyside <- utla21 %>% filter(utla21_name %in% c("Liverpool", "Sefton", "Knowsley", "Wirral", "St. Helens"))

# Couldn't locate the 2021 LSOA boundaries in the geographr package, so instead have downloaded the boundaries
# from https://geoportal.statistics.gov.uk/ and transform to lat/long
lsoa <- st_read("data/LSOA_Dec_2021_Boundaries.gpkg")
lsoa.latlong = st_transform(lsoa, st_crs(utla21))


######################################
# Join census mapping variables to   #
#          boundary file             #
######################################

counties.dat <- right_join(utla21, dat.all.counties, by=join_by("utla21_code"=="GEOGRAPHY_CODE"))
lsoa.merseyside.dat <- right_join(lsoa.latlong, dat.merseyside.lsoa, by=join_by("LSOA21CD"=="GEOGRAPHY_CODE"))



######################################
#    Get tibbles of breakdowns of    #
#   adjusted health and disability   #
#       score of all counties        #
######################################

# Breakdown of scores - Adjusted Health scores 1-5
adjhealth.nomis <-  nomis_get_data(id = "NM_2092_1", geography = "TYPE155")

# Breakdown of scores - Adjusted Health scores 1-3
adjdisable.nomis <-  nomis_get_data(id = "NM_2093_1", geography = "TYPE155")


#################################
#    Define covariate choices   #
#   for maps, plots and models  #
#################################

# Covariate choices for plots and models
covariate.choices <- c("Overcrowded", 
                       "Social rented","No car or van","Never worked/Long-term unemployed",
                       "Full-time student",
                        "No qualifications", "Over 65", "Female", 
                       "Identifies as white")

covariate.vars <- c("overcrowd", "socialrent", "nocar","nowork", "student","noqual", 
                    "over65", "female", "white")

DF.covars <- data.frame(var = covariate.vars, 
                        label=covariate.choices)

# Get min and max values of covariates to generate colour ramps for covariate maps
DF.covars$minval <- c(floor(min(lsoa.merseyside.dat$overcrowd)), floor(min(lsoa.merseyside.dat$socialrent)),
                      floor(min(lsoa.merseyside.dat$nocar)),
                      floor(min(lsoa.merseyside.dat$nowork)),floor(min(lsoa.merseyside.dat$student)),
                      floor(min(lsoa.merseyside.dat$noqual)),
                      floor(min(lsoa.merseyside.dat$over65)),floor(min(lsoa.merseyside.dat$female)),
                      floor(min(lsoa.merseyside.dat$white)))

DF.covars$maxval <- c(ceiling(max(lsoa.merseyside.dat$overcrowd)), ceiling(max(lsoa.merseyside.dat$socialrent)),
                      ceiling(max(lsoa.merseyside.dat$nocar)),
                      ceiling(max(lsoa.merseyside.dat$nowork)),ceiling(max(lsoa.merseyside.dat$student)),
                      ceiling(max(lsoa.merseyside.dat$noqual)),
                      ceiling(max(lsoa.merseyside.dat$over65)),ceiling(max(lsoa.merseyside.dat$female)),
                      ceiling(max(lsoa.merseyside.dat$white)))

# Fix the default covariate for initial maps, plots and models
default.covar <- "Overcrowded"



###############################
#  Create static map labels   #
###############################

# Labels for counties
counties.dat$label <- paste0("Name:", counties.dat$utla21_name,
                             "<br/> % poor health:", counties.dat$adjpoorhealth, 
                             "<br/> % disabled:  ", counties.dat$adjdisable)


# LSOA label for just the health measures
lsoa.merseyside.dat$label <- paste0("Name:", lsoa.merseyside.dat$LSOA21NM,
                                    "<br/> % poor health:", lsoa.merseyside.dat$poorhealth, 
                                    "<br/> % disabled:  ", lsoa.merseyside.dat$disable)


# LSOA label showing info for all covariates
lsoa.merseyside.dat$model.label <- paste0("Name:", lsoa.merseyside.dat$LSOA21NM,
                                          "<br/> % Poor health:", lsoa.merseyside.dat$poorhealth, 
                                          "<br/> % Overcrowded:  ", lsoa.merseyside.dat$overcrowd,
                                          "<br/> % Social rented: ",lsoa.merseyside.dat$socialrent,
                                          "<br/> % No car or van: ",lsoa.merseyside.dat$nocar,
                                          "<br/> % Long-term unemployed: ",lsoa.merseyside.dat$nowork,
                                          "<br/> % Full-time student: ",lsoa.merseyside.dat$student,
                                          "<br/> % No qualifications: ",lsoa.merseyside.dat$noqual,
                                          "<br/> % Over 65: ",lsoa.merseyside.dat$over65,
                                          "<br/> % Female: ",lsoa.merseyside.dat$female,
                                          "<br/> % Identify as white: ",lsoa.merseyside.dat$white)


################################
# Create colour ramps for maps #
################################

# Adjusted poor health - County level
adjhealth.bins <- seq(2,10,by=1)
adjhealth.pal <- colorBin("YlOrRd", bins = adjhealth.bins)

# Adjusted disabled - County level
adjdisable.bins <- seq(10,26,by=2)
adjdisable.pal <- colorBin("BuPu", bins = adjdisable.bins)

# Poor health - LSOAs in Merseyside
mersey.bins <- seq(0,18,by=2)
mersey.pal <- colorBin("YlGnBu", bins = mersey.bins)

#############################
# Create text for info page #
#############################

box1.text <-  "The <i>Health Maps</i> tab displays maps and bar plots relating to two health-related 
              measures captured during the 2021 Census: 
              <ul><li>% of people reporting poor health</li> 
              <li>% of people classified as disabled under the Equality Act.</li></ul>
              
              <h5><i>Poor health</i></h5>
                Individuals were asked to provide an assessment of their general health, 
                selecting from 5 options (very good, good, fair, bad, very bad). 
                In this analysis I looked at the percentage of people who rated themselves 
                as having bad or very bad health within each small area.
              <h5><i>Disabled under the Equality Act</i></h5>
                Individuals were asked whether their day-to-day activities were limited by 
                long-term physical or mental illness. They were considered disabled under the
                 Equality act if they indicated that their activities were limited either a little 
                 or a lot.
                <h5>Age-standardisation</h5>
                In these maps and plots I display the age-standardised scores for each county or 
                unitary authority within England and Wales. This means that any differences we see 
                between areas are not due to any differences in the age distribution of each
                small area population."

box2.text <- "The <i>Explore Merseyside</i> tab focuses on poor health in Merseyside at the LSOA-level.
              Merseyside is comprised of five counties (Liverpool, Sefton, Wirral, Knowsley, St. Helens).
              Here I explore the associations between poor health and other information collected
              during the 2021 Census. I refer to this other information as <i>covariates</i> and I'm
              interested in seeing whether we can use these measures to 
              predict poor health. Scatterplots of % poor health at the LSOA-level are plotted against the 
              following covariates:
              <ul><li>Household-level data
                    <ul>
                      <li>% households classed as overcrowded, defined as having an occupancy rating of -1 or less</li>
                      <li>% households that are socially rented either through a local council or housing association</li>
                      <li>% households with no car or van</li>
                    </ul>
                </li>
                <li>Individual-level socio-economic data
                    <ul>
                      <li>% people > 16 years who have never worked or are long-term unemployed</li>
                      <li>% people > 16 years who are full-time students</li>
                      <li>% people with no qualifications</li>
                    </ul>
                </li>
                <li>Individual-level demographic data
                    <ul>
                      <li>% aged over 65</li>
                      <li>% female</li>
                      <li>% people identifying their ethic group as white (all subclasses)</li>
                    </ul>
                </li>
              </ul>
             <p>Note that the occupancy rating is based on number of rooms per household. This information is taken from the 
             Valuation Office Agency rather than the census itself. </p>

             <p>Maps of % poor health and each of the covariates are also presented.</p>"

box3.text <- "The <i>Guessing Health</i> tab explores whether we can use household-level and individual-level socio-economic 
              and demographic data from the census to predict the self-assessed health status of a small area. This is achieved by 
              fitting a simple linear model to the LSOA-level % poor health data for Merseyside. The model can contain any combination
              of the available covariates, and the page displays information on how well the estimated/fitted health value matches the
              observed value. A scatterplot of observed vs estimated/fitted values is displayed alongside a map of residuals which is simply
              the difference between the observed and the fitted values. Additional summary statistics are included in a table. These are:
              <ul>
                <li>R-squared, a measure of how much variation in % poor health is explained by the covariates in the model. A value of 1 
                indicates the fitted values of the model perfectly match the observed data, whereas 0 indicates there's no relationship
                between the observed and fitted values. </li>
                <li>Root mean squared error (RMSE) is another measure of how closely the fitted values match the observed. Unlike R-squared, 
                its not on a set scale, but the closer it is to 0 the better. It can be used to compare two different models. It's calculated
                by taking the square root of mean of the squared differences between the observed and the fitted i.e. 
                sqrt(mean(Observed-Fitted)<sup>2</sup>) </li>
                <li>The Variance Inflation Factor (VIF) gives an indication of correlation between two or more variables in the model. In the table
                I indicate which variables in the model have a VIF greater than 5, which is a common threshold to use to decide whether the 
                resulting correlation is problem in the model or not. If you see a VIF > 5 it tells us that we're getting the same information
                about the variation in poor health from two different covariates, and one should be removed from the model. </li>
              </ul>"