ne_crops<-read_csv("nebraska_crop_history.csv")
ks_crops<-read_csv("kansas_crop_history.csv")

both_crops<-rbind(ne_crops, ks_crops)

tidy_crops<-both_crops%>%
  select(State, Year, `Data Item`, Value)%>%
  separate(`Data Item`, sep = " - ", into = c("crop", "unit"))%>%
  distinct()%>%
  filter(crop %in% c("WHEAT", "SOYBEANS", "RYE", "OATS", "HAY", "FLAXSEED", "CORN, GRAIN", "BARLEY"))%>%
  filter(unit %in% c("ACRES HARVESTED", "EAR COUNT, MEASURED IN EARS / ACRE",
                     "PLANT POPULATION, MEASURED IN PLANTS / ACRE", "PRODUCTION, MEASURED IN $",
                     "YIELD, MEASURED IN BU / ACRE"))%>%
  group_by(State, Year, crop, unit) %>% 
  distinct(as.numeric(Value))%>%
  na.omit()%>%
  rename(Value = `as.numeric(Value)`)

  rid<-tidy_crops[c(-916, -917, -918, -919, -920, -921, -922, -923, -924, -925, -926, -927, -928, -929, -930, -931, -932, -933, -934, -935, -936, -937, -938, -939, -940, -941, -942, -943, -944, -945, -946, -947, -948, -949, -950, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28, -29, -30, -31, -32, -33, -34, -35, -36, -37, -38, -39, -40, -41, -42, -43, -44, -45, -46, -47, -48, -49, -50, -51),]
  
  tidiest_crops<-spread(rid,key = unit, value =Value)%>%
    ungroup()%>%
  select(State, Year, crop, `YIELD, MEASURED IN BU / ACRE`)%>%
  rename(state = State, year = Year, bu_per_acre = `YIELD, MEASURED IN BU / ACRE`)%>%
   mutate(crop = ifelse((crop == "CORN, GRAIN"), "CORN_GRAIN", crop))
  
  #separate(crop, sep = ", ", into = c("crop", "rid"))%>%
  #select(-rid)

 iowa<-read.csv("iowa_crop_history.csv") 
 
 all_states<-iowa%>%
   mutate(state = "IOWA")%>%
   bind_rows(tidiest_crops)
 
 ia<-read_csv("iowa_avg_weather.csv")
 ks<-read_csv("kansas_avg_weather.csv")
 ne<-read_csv("nebraska_avg_weather.csv")
 
 weather<-bind_rows(ia, ks, ne)%>%
   mutate(state = ifelse((state == "IA"), "IOWA",
                         ifelse((state == "NE"), "NEBRASKA",
                                ifelse((state == "KS"), "KANSAS", "argh"))))

all<-left_join(all_states, weather, by = c("year", "state"))%>%
  select(-ears_per_acre, -plant_pop, -production_dollars)

write_csv(all, "tristate_yield_weather.csv")                     
