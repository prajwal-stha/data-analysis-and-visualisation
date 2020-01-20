setwd('/Users/prajwalshrestha/Desktop/Sandbox/data-analysis-and-visualisation/CODE-PLAYGROUND')


getwd()

# Maintain the reproducibility of the results
set.seed(786)


# Loading required libraries
if(!require(pacman))install.packages("pacman")


devtools::install_github('bbc/bbplot')


pacman::p_load('tidyverse', 
               'skimr', 
               'Hmisc', 
               'DataExplorer', 
               'ggcorrplot', 
               'funModeling', 
               'lubridate', 
               'scales', 
               'grid', 
               'RColorBrewer', 
               'bbplot', 
               'caret', 
               'caretEnsemble', 
               'ggvis', 
               'mice', 
               'VIM', 
               'highcharter', 
               'rgdal', 
               'aod', 
               'cluster', 
               'readr', 
               'Rtsne'
               )


# Remove objects
remove(list = ls())

victimData_df <- read.csv('Nepal_Individual_Level_Replication_Data.csv', stringsAsFactors = FALSE)


head(victimData_df)

View(victimData_df)


glimpse(victimData_df)

summary(victimData_df)

# Checking the number of empty values data
md.pattern(victimData_df)

# Histogram of Missing data
aggr_plot <- aggr(victimData_df, col=c('navyblue', 'red'), 
                  numbers=TRUE, sortVars=TRUE, 
                  labels=names(victimData_df), 
                  cex.axis=.7, 
                  gap=3, 
                  ylab=c("Graph for missing data", "Pattern"))


# Getting the length of the unique entries
victimData_df_unique_entries <- victimData_df %>% 
  map_df(~ n_distinct(.)) %>% 
  gather(FEATURE, DISTINCT_ROWS) %>% 
  arrange(desc(DISTINCT_ROWS))
view(victimData_df_unique_entries)


df_status(victimData_df)



# Removing age_2 and Informer feature from the dataset
victimData_df = subset(victimData_df, select = -c(age_2, Informer))




df_status(victimData_df)


# Checking the number of NA record
victimData_df %>% 
  map_df(~ sum(is.na(.)))



# Changing empty string to NA
victimData_df %>% 
  na_if("")

# Summary matrix for numerical features
victimData_df %>% 
  select(Age) %>% 
  skim()


df_status(victimData_df)

# Data Imputation


# Counting the number of empty records in the Event_Date variable
count(filter(victimData_df, Event_Date == '' | is.na(Event_Date)))

# Getting unique entries for the Event Date variable
event_date_unique_value <- unique(victimData_df$Event_Date[!is.na(victimData_df$Event_Date) & victimData_df$Event_Date != ''])

# Calculating most frequent value of Event Date variable
event_date_mode_value <- event_date_unique_value[which.max(tabulate(match(victimData_df$Event_Date, event_date_unique_value)))]

# Imputing the empty data with the mode value from the Event Date
victimData_df$Event_Date[victimData_df$Event_Date == '' | is.na(victimData_df$Event_Date)] <- event_date_mode_value


# Counting the number of empty records after imputation
count(filter(victimData_df, Event_Date == '' | is.na(Event_Date)))

# Transforming the type of variable
victimData_df$Event_Date <- dmy(victimData_df$Event_Date)


# Counting the number of NA values in Age variable
count(filter(victimData_df, is.na(Age)))



# Changing the NA values in Age variable with Mean of the Age
victimData_df <- victimData_df %>%
  mutate(Age = as.numeric(Age)) %>% 
  transform(victimData_df, Age = ifelse(is.na(Age), round(mean(victimData_df$Age, na.rm = TRUE)), Age))


# Counting the number of NA values after imputation
count(filter(victimData_df, is.na(Age)))


# Checking the distribution of Age variable
with(victimData_df, table(Age))


# Counting the number of NA values in IncidentOutcome variable
count(filter(victimData_df, is.na(IncidentOutcome)))

# Changing NA values to mode values in IncidentOutcome variable
IncidentOutcome_unique_value <- unique(victimData_df$IncidentOutcome[!is.na(victimData_df$IncidentOutcome)])

# Calculating most frequent value of IncidentOutcome variable
IncidentOutcome_mode_value <- IncidentOutcome_unique_value[which.max(tabulate(match(victimData_df$IncidentOutcome, IncidentOutcome_unique_value)))]

# Replacing the NA value with the mode value
victimData_df$IncidentOutcome[is.na(victimData_df$IncidentOutcome)] <- IncidentOutcome_mode_value

# Counting the number of NA variables after Imputation
count(filter(victimData_df, is.na(IncidentOutcome)))




# Counting the number of NA values in Killed_Type variable
count(filter(victimData_df, is.na(Killed_Type)))


# Changing NA values to mode values in Killed_Type variable
Killed_Type_unique_value <- unique(victimData_df$Killed_Type[!is.na(victimData_df$Killed_Type)])


# Calculating most frequent value of Killed_Type variable
Killed_Type_mode_value <- Killed_Type_unique_value[which.max(tabulate(match(victimData_df$Killed_Type, Killed_Type_unique_value)))]


# Replacing the NA value with the mode value
victimData_df$Killed_Type[is.na(victimData_df$Killed_Type)] <- Killed_Type_mode_value


# Counting the number of NA variables after Imputation
count(filter(victimData_df, is.na(Killed_Type)))



# Counting the number of NA values in Gender variable
count(filter(victimData_df, is.na(Gender)))


# Changing NA values to mode values in Gender variable
Gender_unique_value <- unique(victimData_df$Gender[!is.na(victimData_df$Gender)])

# Calculating most frequent value of Gender variable
Gender_mode_value <- Gender_unique_value[which.max(tabulate(match(victimData_df$Gender, Gender_unique_value)))]

# Replacing the NA value with the mode value
victimData_df$Gender[is.na(victimData_df$Gender)] <- Gender_mode_value



# Counting the number of NA variables after Imputation
count(filter(victimData_df, is.na(Gender)))



# Counting the number of NA values in MaritalStatus variable
count(filter(victimData_df, is.na(MaritalStatus)))


# Changing NA values to mode values in MaritalStatus variable
MaritalStatus_unique_value <- unique(victimData_df$MaritalStatus[!is.na(victimData_df$MaritalStatus)])

# Calculating most frequent value of MaritalStatus variable
MaritalStatus_mode_value <- MaritalStatus_unique_value[which.max(tabulate(match(victimData_df$MaritalStatus, MaritalStatus_unique_value)))]

# Replacing the NA value with the mode value
victimData_df$MaritalStatus[is.na(victimData_df$MaritalStatus)] <- MaritalStatus_mode_value

# Replacing the 0 value that denotes nothing
victimData_df$MaritalStatus[victimData_df$MaritalStatus == 0] <- MaritalStatus_mode_value


# Counting the number of NA variables after Imputation
count(filter(victimData_df, is.na(MaritalStatus) | MaritalStatus == 0))



# Counting the number of NA values in LanguageGroup variable
count(filter(victimData_df, is.na(LanguageGroup)))


# Changing NA values to mode values in LanguageGroup variable
LanguageGroup_unique_value <- unique(victimData_df$LanguageGroup[!is.na(victimData_df$LanguageGroup)])

# Calculating most frequent value of LanguageGroup variable
LanguageGroup_mode_value <- LanguageGroup_unique_value[which.max(tabulate(match(victimData_df$LanguageGroup, LanguageGroup_unique_value)))]


# Replacing the NA value with the mode value
victimData_df$LanguageGroup[is.na(victimData_df$LanguageGroup)] <- LanguageGroup_mode_value


# Counting the number of NA variables after Imputation
count(filter(victimData_df, is.na(LanguageGroup)))


# Counting the number of NA values in CasteGroup variable
count(filter(victimData_df, is.na(CasteGroup)))


# Changing NA values to mode values in CasteGroup variable
CasteGroup_unique_value <- unique(victimData_df$CasteGroup[!is.na(victimData_df$CasteGroup)])

# Calculating most frequent value of CasteGroup variable
CasteGroup_mode_value <- CasteGroup_unique_value[which.max(tabulate(match(victimData_df$CasteGroup, CasteGroup_unique_value)))]

# Replacing the NA value with the mode value
victimData_df$CasteGroup[is.na(victimData_df$CasteGroup)] <- CasteGroup_mode_value


# Counting the number of NA variables after Imputation
count(filter(victimData_df, is.na(CasteGroup)))



# Counting the number of NA values in Partyaffiliation variable
count(filter(victimData_df, is.na(Partyaffiliation)))


# Changing NA values to mode values in Partyaffiliation variable
Partyaffiliation_unique_value <- unique(victimData_df$Partyaffiliation[!is.na(victimData_df$Partyaffiliation)])

# Calculating most frequent value of Partyaffiliation variable
Partyaffiliation_mode_value <- Partyaffiliation_unique_value[which.max(tabulate(match(victimData_df$Partyaffiliation, Partyaffiliation_unique_value)))]

# Replacing the NA value with the mode value
victimData_df$Partyaffiliation[is.na(victimData_df$Partyaffiliation)] <- Partyaffiliation_mode_value


# Counting the number of NA variables after Imputation
count(filter(victimData_df, is.na(Partyaffiliation)))



# Counting the number of NA values in EconomicStatus variable
count(filter(victimData_df, is.na(EconomicStatus)))


# Changing NA values to mode values in EconomicStatus variable
EconomicStatus_unique_value <- unique(victimData_df$EconomicStatus[!is.na(victimData_df$EconomicStatus)])


# Calculating most frequent value of EconomicStatus variable
EconomicStatus_mode_value <- EconomicStatus_unique_value[which.max(tabulate(match(victimData_df$EconomicStatus, EconomicStatus_unique_value)))]


# Replacing the NA value with the mode value
victimData_df$EconomicStatus[is.na(victimData_df$EconomicStatus)] <- EconomicStatus_mode_value


# Counting the number of NA variables after Imputation
count(filter(victimData_df, is.na(EconomicStatus)))



# Counting the number of NA values in Education variable
count(filter(victimData_df, is.na(Education)))


# Changing NA values to mode values in Education variable
Education_unique_value <- unique(victimData_df$Education[!is.na(victimData_df$Education)])


# Calculating most frequent value of Education variable
Education_mode_value <- Education_unique_value[which.max(tabulate(match(victimData_df$Education, Education_unique_value)))]

# Replacing the NA value with the mode value
victimData_df$Education[is.na(victimData_df$Education)] <- Education_mode_value


# Counting the number of NA variables after Imputation
count(filter(victimData_df, is.na(Education)))


# Changing NA values to mode values in OccupationCategory variable
OccupationCategory_unique_value <- unique(victimData_df$OccupationCategory[!is.na(victimData_df$OccupationCategory)])

# Calculating most frequent value of OccupationCategory variable
OccupationCategory_mode_value <- OccupationCategory_unique_value[which.max(tabulate(match(victimData_df$OccupationCategory, OccupationCategory_unique_value)))]

# Replacing the NA value with the mode value
victimData_df$OccupationCategory[is.na(victimData_df$OccupationCategory)] <- OccupationCategory_mode_value


# Counting the number of NA values in LivelihoodDependence variable
count(filter(victimData_df, is.na(LivelihoodDependence)))


# Changing NA values to mode values in LivelihoodDependence variable
LivelihoodDependence_unique_value <- unique(victimData_df$LivelihoodDependence[!is.na(victimData_df$LivelihoodDependence)])

# Calculating most frequent value of LivelihoodDependence variable
LivelihoodDependence_mode_value <- LivelihoodDependence_unique_value[which.max(tabulate(match(victimData_df$LivelihoodDependence, LivelihoodDependence_unique_value)))]

# Replacing the NA value with the mode value
victimData_df$LivelihoodDependence[is.na(victimData_df$LivelihoodDependence)] <- LivelihoodDependence_mode_value


# Counting the number of NA variables after Imputation
count(filter(victimData_df, is.na(LivelihoodDependence)))


# Counting the number of NA values in Compensation variable
count(filter(victimData_df, is.na(Compensation)))


# Changing NA values to mode values in Compensation variable
Compensation_unique_value <- unique(victimData_df$Compensation[!is.na(victimData_df$Compensation)])

# Calculating most frequent value of Gender variable
Compensation_mode_value <- Compensation_unique_value[which.max(tabulate(match(victimData_df$Compensation, Compensation_unique_value)))]

# Replacing the NA value with the mode value
victimData_df$Compensation[is.na(victimData_df$Compensation)] <- Compensation_mode_value



# Counting the number of NA variables after Imputation
count(filter(victimData_df, is.na(Compensation)))



# Counting the number of NA values in Displacement variable
count(filter(victimData_df, is.na(Displacement)))


# Changing NA values to mode values in Displacement variable
Displacement_unique_value <- unique(victimData_df$Displacement[!is.na(victimData_df$Displacement)])

# Calculating most frequent value of Displacement variable
Displacement_mode_value <- Displacement_unique_value[which.max(tabulate(match(victimData_df$Displacement, Displacement_unique_value)))]

# Replacing the NA value with the mode value
victimData_df$Displacement[is.na(victimData_df$Displacement)] <- Displacement_mode_value


# Counting the number of NA variables after Imputation
count(filter(victimData_df, is.na(Displacement)))



# Counting the number of NA values in DisplacementResettlement variable
count(filter(victimData_df, is.na(DisplacementResettlement)))


# Changing NA values to mode values in DisplacementResettlement variable
DisplacementResettlement_unique_value <- unique(victimData_df$DisplacementResettlement[!is.na(victimData_df$DisplacementResettlement)])

# Calculating most frequent value of DisplacementResettlement variable
DisplacementResettlement_mode_value <- DisplacementResettlement_unique_value[which.max(tabulate(match(victimData_df$DisplacementResettlement, DisplacementResettlement_unique_value)))]

# Replacing the NA value with the mode value
victimData_df$DisplacementResettlement[is.na(victimData_df$DisplacementResettlement)] <- DisplacementResettlement_mode_value


# Counting the number of NA variables after Imputation
count(filter(victimData_df, is.na(DisplacementResettlement)))




# Counting the number of observation with NA value in Year Variable
count(filter(victimData_df, is.na(Year)))


# Getting the values from Event_Date where Year variable is NA
victimData_df$Year <- ifelse(is.na(victimData_df$Year) & !is.na(victimData_df$Event_Date), year(victimData_df$Event_Date), victimData_df$Year)

# Counting the number of NA value after the imputation
count(filter(victimData_df, is.na(Year)))


# Creating new feature perpetrator to check if it is state, maoist or other
victimData_df = victimData_df %>% 
  mutate(perpetrator = case_when(
           By_State == 1 & By_Maoist == 0 & Other_Perpetrators == 0 ~ "State" , 
           By_Maoist == 1 & By_State == 0 & Other_Perpetrators == 0 ~ "Maoist" ,
           Other_Perpetrators == 1 & By_State == 0 & By_Maoist == 0 ~ "Other"
         )) 
# Checking the first 20 records
head(victimData_df$perpetrator, 20)


# Checking the number of NA record
victimData_df %>% 
  map_df(~ sum(is.na(.)))


## Data Profiling 


# EDA

mapdata <- get_data_from_map(download_map_data("countries/np/np-all"))
view(mapdata)


# read shapefile
nepal_shp <- readOGR(dsn=path.expand("NepalMaps/baselayers/NPL_adm"), layer="NPL_adm3", stringsAsFactors = FALSE)

# fortify shapefile data to data frame
nepal.adm3.shp.df <- fortify(nepal_shp, region = "NAME_3")


# Create new feature IncidentDistrict_alt that includes the district name of the map shape data
victimData_df$IncidentDistrict_alt <- str_to_title(str_to_lower(as.character(victimData_df$IncidentDistrict)))


# identify the mismatched districts by uncommenting 2 lines below
# unique(victimData_df$IncidentDistrict_alt[!victimData_df$IncidentDistrict_alt %in% unique(nepal.adm3.shp.df$id)])
# unique(unique(nepal.adm3.shp.df$id)[!unique(nepal.adm3.shp.df$id) %in% victimData_df$IncidentDistrict_alt])

# fix the mismatched districts
victimData_df$IncidentDistrict_alt[victimData_df$IncidentDistrict_alt == "Dhanusha"] <- "Dhanusa"
victimData_df$IncidentDistrict_alt[victimData_df$IncidentDistrict_alt == "Gorakha"] <- "Gorkha"
victimData_df$IncidentDistrict_alt[victimData_df$IncidentDistrict_alt == "Kavre"] <- "Kavrepalanchok"
victimData_df$IncidentDistrict_alt[victimData_df$IncidentDistrict_alt == "Kapilbastu"] <- "Kapilvastu"
victimData_df$IncidentDistrict_alt[victimData_df$IncidentDistrict_alt == "Makawanpur"] <- "Makwanpur"
victimData_df$IncidentDistrict_alt[victimData_df$IncidentDistrict_alt == "Parvat"] <- "Parbat"
victimData_df$IncidentDistrict_alt[victimData_df$IncidentDistrict_alt == "Sindhupalchowk"] <- "Sindhupalchok"
victimData_df$IncidentDistrict_alt[victimData_df$IncidentDistrict_alt == "Udaypur"] <- "Udayapur"
victimData_df$IncidentDistrict_alt[victimData_df$IncidentDistrict_alt == "Panchathar"] <- "Panchthar"
# unique(victimData_df$IncidentDistrict_alt[!victimData_df$IncidentDistrict_alt %in% unique(nepal.adm3.shp.df$id)])

# Preparing data for Choropleth
victimData_df_count_by_district <- victimData_df %>% 
  group_by(IncidentDistrict_alt) %>%
  summarise(count=n())

# Adding record for Mananag as there are no any victms for the district
victimData_df_count_by_district %>% add_row(IncidentDistrict_alt = 'Manang', count = 0)


nepal.adm3.shp.df <- merge(nepal.adm3.shp.df, victimData_df_count_by_district, by.x="id", by.y="IncidentDistrict_alt")

# Map 
map <- ggplot(data = nepal.adm3.shp.df, aes(x = long, y = lat, group = group))

# setting up bare theme
theme_bare <- theme(
  axis.line=element_blank(), 
  axis.text.x=element_blank(), 
  axis.text.y=element_blank(),
  axis.ticks=element_blank(), 
  axis.title.x=element_blank(), 
  axis.title.y=element_blank(),
  legend.text=element_text(size=7),
  legend.title=element_text(size=8),
  panel.background=element_blank(),
  panel.border=element_rect(colour = "gray", fill=NA, size=0.5)
)


# finding the centers of all the district polygons. 
centroids <- rgeos::gCentroid(nepal_shp, byid=TRUE, id=unique(nepal_shp$NAME_3))

# Creating data frame for the centroids
centroids_df <- centroids %>% 
  as.data.frame() %>% 
  mutate(label=row.names(.))

# plot map
map + 
  geom_polygon(aes(fill=count), color='gray', size=0.1) +
  guides(fill=guide_colorbar(title="Victims Count")) + 
  ggtitle("Victim Count per district") + 
  scale_fill_gradient(high="#e34a33", low="#fee8c8", guide="colorbar") +
  coord_fixed(1.3) +
  theme(legend.justification=c(0,-0.1), legend.position=c(0.05,0)) +
  with(centroids_df, annotate(geom="text", x=x, y=y, label=label, size=2)) + 
  theme_bare

# Histogram Plot for Age Variable
ggplot(victimData_df, aes(x=Age)) + 
  geom_histogram(aes(y = ..density..), fill="#1380A1", color="#e9ecef", alpha=0.9, bins = 20) + 
  geom_density() + 
  labs(title = "Age") + 
  bbc_style() +
  theme(plot.title = element_text(color = "#063376"))


# Grouping the total number of victims by Year
victimData_df_count_by_year <- victimData_df %>% 
  group_by(Year) %>%
  summarise(count=n())

# Bar Plot for Number of victims per year
ggplot(victimData_df, aes(x = factor(Year))) +
  geom_bar(stat="count", position="dodge") +
  labs(subtitle="Number of Victims by Per Year") +
  ylab('Number of Victims per Year') +
  xlab('Year') +
  bbc_style() + 
  theme(legend.position="bottom")



# Donut chart for Region2
victimData_df_region_count = victimData_df %>% 
  group_by(Region2) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(Region2)) %>%
  mutate(percentage = round(n/sum(n), 4) * 100,
         lab.pos = cumsum(percentage) -.5 * percentage,
         DevelopmentRegion = case_when(
           Region2 == 1 ~ "Eastern",
           Region2 == 2 ~ "Central",
           Region2 == 3 ~ "Western",
           Region2 == 4 ~ "Mid-Western",
           Region2 == 5 ~ "Far-Western"
         )) 

ggplot(victimData_df_region_count, aes(x = 2, y = percentage, fill = DevelopmentRegion)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 200) +
  geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "white") +
  theme_void() +
  labs(fill = "Political Regions")
  xlim(.2,2.5)

# Donut chart for Region1
victimData_df_geography_count = victimData_df %>% 
  group_by(Region1) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(Region1)) %>%
  mutate(percentage = round(n/sum(n), 4) * 100,
         lab.pos = cumsum(percentage) -.5 * percentage,
         GeographicalRegion = case_when(
           Region1 == 1 ~ "Terai (Plain)",
           Region1 == 2 ~ "Hill",
           Region1 == 3 ~ "Mountainous"
         )) 

ggplot(victimData_df_geography_count, aes(x = 2, y = percentage, fill = GeographicalRegion)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 200) +
  geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "white") +
  theme_void() +
  labs(fill = "Geographical Regions")
  xlim(.2,2.5)

# Bar Chart for Pattern of Incident Outcome
ggplot(victimData_df, aes(x = Year, fill = as.factor(IncidentOutcome))) +
  geom_bar(stat="count", position="dodge") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_fill_manual(values = c("#1380A1", "#FAAB18", "#990000"),
                    name="Incident Outcome",
                    breaks=c(1, 2, 3),
                    labels=c("Killed", "Disappeared", "Injury")) +
  labs(title="Pattern of Incident Outcome", subtitle = "1996-2006") +
  scale_x_continuous(breaks = seq(from = 1996, to = 2006, by = 1)) +
  bbc_style() + 
  ylab('Number of Victims')

# Bar Chart for Killed Type
KilledTypeLabel = c('Other', 'Combat Fighting', 'Extra Judicial Killing', 'Serious Nature', 'Electrocution', 'Self-bomb explosion', 'Bomb explosion', 'Killed')

ggplot(victimData_df, aes(x = factor(Killed_Type), fill = factor(Killed_Type))) +
  geom_bar(stat="count", position="dodge") +
  scale_fill_manual(values = c("#1380A1", "#FAAB18", "#990000", "#cbcbcb","#333333", "#DC143C", "#FF1493", "#FF6347"),
                    name="Killed Type",
                    breaks=c(0, 1, 2, 3, 4, 5, 6, 9),
                    labels=KilledTypeLabel) +
  labs(subtitle="Victims by Killed Type") +
  scale_x_discrete(labels = KilledTypeLabel) +
  ylab('Number of Victims') +
  xlab('Victims Type') +
  bbc_style() + 
  theme(axis.text.x = element_text(angle = 90), legend.position="bottom")


# Bar Chart for perpetrator
ggplot(victimData_df, aes(x = factor(perpetrator), fill = factor(perpetrator))) +
  geom_bar(stat="count", position="dodge") +
  labs(subtitle="Victims by Perpetrator") +
  ylab('Number of Victims') +
  xlab('Perpetrator') +
  bbc_style() + 
  theme(axis.text.x = element_text(angle = 90), legend.position="bottom")


# Donut chart for Gender
victimData_df_gender_count = victimData_df %>% 
  group_by(Gender) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(Gender)) %>%
  mutate(percentage = round(n/sum(n), 4) * 100,
         lab.pos = cumsum(percentage) -.5 * percentage,
         GenderLabel = case_when(
           Gender == 0 ~ "Female",
           Gender == 1 ~ "Male"
         )) 

ggplot(victimData_df_gender_count, aes(x = 2, y = percentage, fill = GenderLabel)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 200) +
  geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "white") +
  theme_void() +
  labs(fill = 'Gender')
xlim(.2,2.5)

# Bar chart for MaritalStatus
MaritalStatusLabel = c('Married', 'Unmarried', 'Divorced', 'Widow', 'Widower', 'Not clear')

ggplot(victimData_df, aes(x = factor(MaritalStatus), fill = factor(MaritalStatus))) +
  geom_bar(stat="count", position="dodge") +
  scale_fill_manual(values = c("#1380A1", "#FAAB18", "#990000", "#cbcbcb","#333333", "#DC143C"),
                    name="Marital Status",
                    breaks=c(1, 2, 3, 4, 5, 6),
                    labels=MaritalStatusLabel) +
  labs(subtitle="Victims by Marital Status") +
  scale_x_discrete(labels = MaritalStatusLabel) +
  ylab('Number of Victims') +
  xlab('Marital Status') +
  bbc_style() + 
  theme(axis.text.x = element_text(angle = 90), legend.position="bottom")

# Bar Chart for plotting the number of victims as of Language Group
LanguageGroupLabels <- c("Nepali", "Newari", "Tamang", "Gurung", "Tharu", "Maithili", "Hindi", "Awadhi", "Bhojpuri", "Magar", "Chepang", "Rai", "Thakali", "Dhimal", "Dotali", "Muslim", "Thami", "Jirel", "Other", "Not clear")


ggplot(victimData_df, aes(x = factor(LanguageGroup), fill = factor(LanguageGroup))) +
  geom_bar(stat="count", position="dodge") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_fill_manual(values = c("#1380A1", "#FAAB18", "#990000", "#cbcbcb","#333333", "#DC143C", "#FF1493", "#FF6347", "#FFA500", "#FFD700", "#FFDAB9", "#BDB76B", "#DDA0DD", "#FF00FF", "#663399", "#8B008B", "#483D8B", "#32CD32", "#2E8B57", "#556B2F"),
                    name="Language Group",
                    breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
                    labels=LanguageGroupLabels) +
  labs(subtitle="Victims by language group") +
  scale_x_discrete(labels = LanguageGroupLabels) +
  ylab('Number of Victims') +
  xlab('Language Group') +
  bbc_style() +
  theme(axis.text.x = element_text(angle = 90)
  )


# Bar Chart for plotting the number of victims as of caste Group
CasteGroupLabels <- c("Bramin/Chettri", "Janajati", "Aadibashi", "Madeshi", "Dalit", "Muslim", "Not clear", "Other")

ggplot(victimData_df, aes(x = factor(CasteGroup), fill = factor(CasteGroup))) +
  geom_bar(stat="count", position="dodge") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_fill_manual(values = c("#1380A1", "#FAAB18", "#990000", "#cbcbcb","#333333", "#DC143C", "#FF1493", "#FF6347"),
                    name="Caste Group",
                    breaks=c(1, 2, 3, 4, 5, 6, 7, 8),
                    labels=CasteGroupLabels) +
  labs(subtitle="Victims by Caste group") +
  scale_x_discrete(labels = CasteGroupLabels) +
  ylab('Number of Victims') +
  xlab('Caste Group') +
  bbc_style() + 
  theme(axis.text.x = element_text(angle = 90) 
  )


# Bar Chart for plotting the number of victims as of party affiliation
PartyAffiliationLabels <- c("Nepali Congress (Democratic)", "Nepali Congress", "CPN-UML", "CPN-ML", "United Maoist", "People’s Front- Nepal", "RPP", "Nepal Sadbhawana Party", "United People’s Front", "Nepal Workers and Peasants Party", "Nepal Communist Party-United", "National People’s Liberation Party", "Limbuwan National Liberation Front", "Not identified")


ggplot(victimData_df, aes(x = as.factor(Partyaffiliation), fill = as.factor(Partyaffiliation))) +
  geom_bar(stat="count", position="dodge", width = 0.3) +
  scale_fill_manual(values = c("#1380A1", "#FAAB18", "#990000", "#cbcbcb","#333333", "#DC143C", "#FF1493", "#FF6347", "#FFE4E1", "#FFD700", "#FFDAB9", "#BDB76B", "#DDA0DD", "#DAA520"),
                    name="Party Affiliation",
                    breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 99),
                    labels=PartyAffiliationLabels) +
  labs(subtitle="Victims by Political Party Affiliation") +
  scale_x_discrete(labels = PartyAffiliationLabels) +
  ylab('Number of Victims') +
  xlab('Party Affiliation') +
  bbc_style() + 
  theme(axis.text.x = element_text(angle = 90), legend.position="bottom")


# Bar Chart for plotting the number of victims as of Economic Status
EconomicStatusLabels <- c("Higher", "Middle", "Lower-Middle", "Lower", "Not clear")


ggplot(victimData_df, aes(x = factor(EconomicStatus), fill = factor(EconomicStatus))) +
  geom_bar(stat="count", position="dodge") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() +
  scale_fill_manual(values = c("#1380A1", "#FAAB18", "#990000", "#cbcbcb","#333333"),
                    name="Economic Status",
                    breaks=c(1, 2, 3, 4, 5),
                    labels=EconomicStatusLabels) +
  labs(subtitle="Victims by Economic status") +
  scale_x_discrete(labels = EconomicStatusLabels)


# Bar Chart for plotting the number of victims as of party affiliation
EducationLabels <- c("Master", "Bachelor", "Intermediate", "High School", "Lower High School", "Primary School", "Literate", "Illiterate", "Not clear")


ggplot(victimData_df, aes(x = factor(Education), fill = factor(Education))) +
  geom_bar(stat="count", position="dodge") +
  scale_fill_manual(values = c("#1380A1", "#FAAB18", "#990000", "#cbcbcb","#333333", "#DC143C", "#FF1493", "#FF6347", "#FFA500"),
                    name="Education",
                    breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                    labels=EducationLabels) +
  labs(subtitle="Victims by Education") +
  scale_x_discrete(labels = EducationLabels) +
  ylab('Number of Victims') +
  xlab('Education') +
  bbc_style() + 
  theme(axis.text.x = element_text(angle = 90), legend.position="bottom")


# Bar Chart for plotting the number of victims as of Occupation category
OccupationCategoryLabels <- c("Agriculture", "Wage Laborer", "Employed", "Teacher", "Police", "Army", "Lawyer", "Doctor", "Politician", "Social worker", "Rights activists", "Musician", "Player", "Driver", "Student", "Journalist", "Businessman", "Ex-security personnel", "other")


ggplot(victimData_df, aes(x = factor(OccupationCategory), fill = factor(OccupationCategory))) +
  geom_bar(stat="count", position="dodge") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_fill_manual(values = c("#1380A1", "#FAAB18", "#990000", "#cbcbcb","#333333", "#DC143C", "#FF1493", "#FF6347", "#FFA500", "#FFD700", "#FFDAB9", "#BDB76B", "#DDA0DD", "#FF00FF", "#663399", "#8B008B", "#483D8B", "#32CD32", "#2E8B57"),
                    name="Occupation Category",
                    breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19),
                    labels=OccupationCategoryLabels) +
  labs(subtitle="Victims by Occupation category") +
  scale_x_discrete(labels = OccupationCategoryLabels) +
  ylab('Number of Victims') +
  xlab('Occupation Categories') +
  bbc_style() +
  theme(axis.text.x = element_text(angle = 90)
  )


# Bar Chart for Livelihood Dependence
LivelihoodDependenceLabel = c('Agriculture', 'Other', 'Unknown')

ggplot(victimData_df, aes(x = factor(LivelihoodDependence), fill = factor(LivelihoodDependence))) +
  geom_bar(stat="count", position="dodge") +
  scale_fill_manual(values = c("#1380A1", "#FAAB18", "#990000"),
                    name="Livelihood bases of victim’s dependent",
                    breaks=c(1, 2, 3),
                    labels=LivelihoodDependenceLabel) +
  labs(subtitle="Livelihood bases of victim’s dependent") +
  scale_x_discrete(labels = LivelihoodDependenceLabel) +
  ylab('Livelihood bases of victim’s dependent') +
  xlab('Livelihood Dependence') +
  bbc_style() + 
  theme(legend.position="bottom")


# Bar Chart for Displacement
DisplacementLabel = c('No', 'Yes')

ggplot(victimData_df, aes(x = factor(Displacement), fill = factor(Displacement))) +
  geom_bar(stat="count", position="dodge") +
  scale_fill_manual(values = c("#1380A1", "#FAAB18"),
                    name="Displacement Impact on Victim",
                    breaks=c(0, 1),
                    labels=DisplacementLabel) +
  labs(subtitle="Displacement Impact on Victim") +
  scale_x_discrete(labels = DisplacementLabel) +
  ylab('Displacement Impact on Victim') +
  xlab('Displacement Impact on Victim') +
  bbc_style() + 
  theme(legend.position="bottom")

# Bar Chart for Displacement Resettlement
DisplacementResettlementLabel = c('Not resettled', 'Resettled')

ggplot(victimData_df, aes(x = factor(DisplacementResettlement), fill = factor(DisplacementResettlement))) +
  geom_bar(stat="count", position="dodge") +
  scale_fill_manual(values = c("#1380A1", "#FAAB18"),
                    name="Displacement Resettlement",
                    breaks=c(0, 1),
                    labels=DisplacementResettlementLabel) +
  labs(subtitle="Displacement Resettlement") +
  scale_x_discrete(labels = DisplacementResettlementLabel) +
  ylab('Displacement Resettlement') +
  xlab('Displacement Resettlement') +
  bbc_style() + 
  theme(legend.position="bottom")

# Print Session Info
devtools::session_info()


# Modelling

# Clustering
# Since the data set contains more categorical variables we can't perform any kind of prediction
# We will apply clustering on the basis of the numbers of victims count and age

# Compute Gower distance
view(victimData_df)
victimData_alt_df = subset(victimData_df, select = c(IncidentDistrict, PermanentDistrict, Age, Year, Region2, Region1, Mobility, IncidentOutcome, Killed_Type, perpetrator, Gender, MaritalStatus, LanguageGroup, CasteGroup, Partyaffiliation, EconomicStatus, Education, OccupationCategory, LivelihoodDependence, Compensation, Displacement, DisplacementResettlement))
# Converting the date type and character type feature to factor for maintaining the com
victimData_alt_df$perpetrator <- as.factor(victimData_alt_df$perpetrator)
victimData_alt_df$IncidentDistrict <- as.factor(victimData_alt_df$IncidentDistrict)
#victimData_alt_df$Event_Date <- as.factor(victimData_alt_df$Event_Date)


gower_dist <- daisy(victimData_alt_df, metric = "gower")
gower_mat <- as.matrix(gower_dist)

victimData_alt_df[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]


sil_width <- c(NA)
for(i in 2:4){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
sil_width
plot(1:4, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:4, sil_width)


k <- 2

pam_fit <- pam(gower_dist, diss = TRUE, k)

pam_results <- victimData_alt_df %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

# Plotting the cluster
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))



