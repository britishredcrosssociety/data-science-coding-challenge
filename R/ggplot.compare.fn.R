##################################################################
# Function to create bar plots comparing the health and          #
# disability status of an inputted county with Liverpool         #
##################################################################

ggplot.compare.fn <- function(input.county){
  
  county.selected <- input.county

  
  # Filter the dataset
  county.compare.health <- adjhealth.nomis %>% filter(GEOGRAPHY_CODE %in% c("E08000012", county.selected))
  county.compare.disability <- adjdisable.nomis %>% filter(GEOGRAPHY_CODE %in% c("E08000012", county.selected))
  
  county.name <- as.character(unique(county.compare.health %>% filter(GEOGRAPHY_CODE==county.selected) %>% select(GEOGRAPHY_NAME)))
  
  # Fix the order of the bars
  county.compare.health$GEOGRAPHY_NAME <- factor(county.compare.health$GEOGRAPHY_NAME, levels=c("Liverpool", county.name))
  county.compare.disability$GEOGRAPHY_NAME <- factor(county.compare.disability$GEOGRAPHY_NAME, levels=c("Liverpool", county.name))
  
  # Create the ggplots
  ggplot.compare.health <- ggplot(county.compare.health, aes(x = GEOGRAPHY_NAME, y = OBS_VALUE, fill=C2021_HEALTH_5_CODE)) +
    geom_bar(stat = "identity")+coord_flip()+ylab("Percentage (%)")+xlab("County/UA")+
    scale_fill_brewer(palette="Accent", name="Health",labels=c("Very good", "Good", "Fair", "Bad", "Very Bad"))+
    theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=1))
  
  # Reversed the disability score so the order matches the health rating (good to bad)
  ggplot.compare.disable <- ggplot(county.compare.disability, aes(x = GEOGRAPHY_NAME, y = OBS_VALUE, fill=factor(2-C2021_DISABILITY_3))) +
    geom_bar(stat = "identity")+coord_flip()+ylab("Percentage (%)")+xlab("County/UA")+
    scale_fill_brewer(palette="Dark2", name="Disability",labels=c("Not disabled", "Limited a little", "Limited a lot"))+
    theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=1))
  return(list(health.plot=ggplot.compare.health, disable.plot=ggplot.compare.disable))
  
}