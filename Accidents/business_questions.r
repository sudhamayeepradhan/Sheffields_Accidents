
# 1) What is the proportion of fatal and serious accidents in Sheffield by month between 2014-2017?

accidentssheff %>% select(Accident_Severity) %>% group_by(Accident_Severity) %>% summarise(Number = n())

accidentssheff %>% 
  mutate(Year = year(Timestamp_a),
         Month = month(Timestamp_a),
         Severity = Accident_Severity) %>% 
  group_by(Year, Severity, Month) %>% summarise(Number=n()) %>% arrange(desc(Year), Severity, Month) %>%
  # Bar plot
  ggplot(aes(x = factor(Month), y = Number, fill=Severity)) +
  facet_wrap(~ Year, ncol = 2) +
 # geom_text(aes(label = Number))+
  geom_bar(stat = "identity", position = "fill") + 
  labs(title = "The total number of accidents by month",
       y = "Number of accidents",
       x = "Month") +  theme_minimal()


# 2) Provide a break down of accidents by location where fatal/serious accident happened in 2014-2017?

accidentsloc <- accidentssheff %>% select(Accident_Index, Latitude, Longitude, Accident_Severity) %>% filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  filter (Accident_Severity == "Fatal" | Accident_Severity == "Serious") %>% mutate(latlon = paste(Latitude, Longitude, sep=":")) %>% collect()

accidentslocfat <- accidentssheff %>% select(Accident_Index, Latitude, Longitude, Accident_Severity) %>% filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  filter (Accident_Severity == "Fatal") %>% mutate(latlon = paste(Latitude, Longitude, sep=":")) %>% collect()

map <- gvisMap(accidentsloc, locationvar = 'latlon', tipvar = 'Accident_Index')
plot(map)

map <- gvisMap(accidentslocfat, locationvar = 'latlon', tipvar = 'Accident_Index')
plot(map)

accidentstime %>% filter(Local_Authority_District == 'Sheffield') %>% 
  left_join(vehiclestbl, by=c("Accident_Index")) %>% select(Junction_Location) %>% group_by(Junction_Location) %>% summarise(n())


# 3): How does the number of serious and fatal traffic accidents vary by hour, and by season between 2014-2017?

accidentssheff <- accidentssheff %>% mutate(Season = ifelse(month(Timestamp_a) == 12 | month(Timestamp_a) == 1 | month(Timestamp_a) == 2, "Winter",
                                                            ifelse(month(Timestamp_a) == 3 | month(Timestamp_a) == 4 | month(Timestamp_a) == 5, "Spring",
                                                                   ifelse(month(Timestamp_a) == 6 | month(Timestamp_a) == 7 | month(Timestamp_a) == 8, "Summer",
                                                                          ifelse(month(Timestamp_a) == 9 | month(Timestamp_a) == 10 | month(Timestamp_a) == 11, "Autumn", NA_real_)))))
# Season
accidentssheff %>% 
  mutate(Year = year(Timestamp_a),Severity = Accident_Severity) %>% filter(Accident_Severity == "Fatal" | Accident_Severity == "Serious") %>% group_by(Severity, Year, Season) %>%
  summarise(Number=n()) %>% arrange(Severity, Season, desc(Year), Number) %>%
  ggplot(aes(x = factor(Season), y = Number, fill=Severity)) + 
  facet_wrap(~ Year, ncol = 2) +
  geom_bar(stat = "identity") +
  labs(title = "The number of fatal and serious accidents by season",
       y = "Number of accidents",
       x = "Season") + theme_bw() + theme_minimal()
  scale_fill_discrete(h =c(0,300)) + 
    
# Hour
    accidentssheff %>% 
    mutate(Year = year(Timestamp_a),Severity = Accident_Severity, Hour = hour(Timestamp_a)) %>% filter(Accident_Severity == "Fatal" | Accident_Severity == "Serious") %>% group_by(Severity, Year, Hour) %>%
    summarise(Number=n()) %>% arrange(Severity, Hour, desc(Year), Number) %>%
    ggplot(aes(x = factor(Hour), y = Number, fill=Severity)) + 
    facet_wrap(~ Year, ncol = 2) +
    geom_bar(stat = "identity") +
    labs(title = "The number of fatal and serious accidents by hour",
         y = "Number of accidents",
         x = "Hour") + theme_bw() + theme_minimal()
  scale_fill_discrete(h =c(0,300)) + 

# 4) How does the number of people with serious/fatal injuries vary by day of the week, by season, where serious
#    and fatal road accidents took place in Sheffield in 2017? 

# Accidents
accidentssheff %>% 
  mutate(Year = year(Timestamp_a),
         Day = Day_of_Week,
         Severity = Accident_Severity
         ) %>% filter ((Severity == "Fatal" | Severity == "Serious") & Year == 2017) %>%
  group_by(Season, Day, Severity) %>% summarise(Number=n()) %>% arrange(Severity, Day, Season) %>%
  ggplot(aes(x = factor(Day, level = level_order), y = Number, fill=Severity)) + 
  facet_wrap(~ Season, ncol = 2) +
  geom_bar(stat = "identity") +
  labs(title = "The number of fatal and serious accidents by day of the week in 2017",
       y = "Number of accidents",
       x = "Day") +  theme_bw()

# People

accidentstbl %>% filter(Local_Authority_District == 'Sheffield') %>% left_join(casualtiestbl, by=c("Accident_Index")) %>%
  mutate(Year = year(Timestamp_a),
         Day = Day_of_Week,
         Severity = Casualty_Severity
  ) %>% filter ((Severity == "Fatal" | Severity == "Serious") & Year == 2017) %>%
  group_by(Season, Day, Severity) %>% summarise(Number=n()) %>% arrange(Severity, Day, Season) %>%
  ggplot(aes(x = factor(Day, level = level_order), y = Number, fill=Severity)) + 
  facet_wrap(~ Season, ncol = 2) +
  geom_bar(stat = "identity") +
  labs(title = "The number of people with fatal and serious injuries by day of the week in 2017",
       y = "Number of accidents",
       x = "Day") +  theme_bw()

# 5) Where did the fatal and serious accidents take place by environmental factors in 2017? (i.e. light condition, road surface)

accidentssheff %>% 
  mutate( Month = month(Timestamp_a),
          Year = year(Timestamp_a),
          Weather = Road_Surface,
          Severity = Accident_Severity) %>% filter((Severity == "Fatal" | Severity == "Serious") & Year == 2017) %>% 
  #filter(Weather != "Fine no high winds" & Weather != "Unknown" & Weather != "Fine + high winds") %>%
  group_by(Weather, Year, Month) %>% summarise(Number=n()) %>% arrange(Weather,Year, desc(Month)) %>%
  ggplot(aes(x = factor(Month), y = Number, fill=Weather)) + 
  geom_bar(stat = "identity") + 
#  geom_bar(stat = "identity",position = "dodge") + 
#  facet_wrap(~ Year, ncol = 2) +
  labs(title = "Proportion of accidents by weather monthly",
       y = "Number of accidents",
       x = "Month") + theme_minimal() 

# Lights
accidentsloclight <- accidentssheff %>% select(Timestamp_a, Accident_Index, Latitude, Longitude, Accident_Severity, Light_Conditions) %>% filter(!is.na(Latitude) & !is.na(Longitude)) %>% mutate(Year = year(Timestamp_a) ,latlon = paste(Latitude, Longitude, sep=":")) %>% 
  filter ((Accident_Severity == "Fatal" | Accident_Severity == "Serious") & (Light_Conditions == "Darkness - no lighting" | Light_Conditions == "Darkness - lights unlit") & Year == 2017) %>% collect()

map <- gvisMap(accidentsloclight, locationvar = 'latlon', tipvar = 'Accident_Severity')
plot(map)

# Surface
accidentslocweather <- accidentssheff %>% select(Timestamp_a, Accident_Index, Latitude, Longitude, Accident_Severity, Road_Surface, Weather) %>% filter(!is.na(Latitude) & !is.na(Longitude)) %>% mutate(Year = year(Timestamp_a) ,latlon = paste(Latitude, Longitude, sep=":")) %>% 
  filter ((Accident_Severity == "Fatal")& (Road_Surface == "Wet or damp" | Road_Surface == "Frost or ice") & Year == 2017) %>% collect()

map <- gvisMap(accidentslocweather, locationvar = 'latlon', tipvar = 'Weather')
plot(map)
