crops<-read_csv("data/NASS-Iowa.csv")

tidy_crops<-crops%>%
  select(Year, `Data Item`, Value)%>%
  separate(`Data Item`, sep = " - ", into = c("crop", "unit"))%>%
  spread(key = unit, value = Value)%>%
  select(Year, crop, `ACRES HARVESTED`, `EAR COUNT, MEASURED IN EARS / ACRE`,
         `PLANT POPULATION, MEASURED IN PLANTS / ACRE`, `PRODUCTION, MEASURED IN $`,
         `YIELD, MEASURED IN BU / ACRE`)%>%
  rename(year = Year, acres_harvested = `ACRES HARVESTED`, ears_per_acre = `EAR COUNT, MEASURED IN EARS / ACRE`,
         plant_pop = `PLANT POPULATION, MEASURED IN PLANTS / ACRE`, production_dollars = 
           `PRODUCTION, MEASURED IN $`, bu_per_acre = `YIELD, MEASURED IN BU / ACRE`)%>%
  filter(crop %in% c("WHEAT", "SOYBEANS", "RYE", "OATS", "HAY", "FLAXSEED", "CORN, GRAIN", "BARLEY"))%>%
  mutate(crop = ifelse((crop == "CORN, GRAIN"), "CORN_GRAIN", crop))
  
  separate(crop, sep = ", ", into = c("crop", "rid"))%>%
  select(-rid)

  

                        ifelse ((new == "CORN, SILAGE"), "corn_silage", crop)))
  
  
mutate(df, thiscol = ifelse(new == "oldname"), "newname", thiscol)

  mutate(spring_year = ifelse((date >"2003-11-01" & date < "2004-04-30"), 2004,
                              ifelse((date >"2004-11-01" & date < "2005-04-30"), 2005,
                                     ifelse((date >"2005-11-01" & date < "2006-04-30"), 2006,
                                            ifelse((date >"2006-11-01" & date < "2007-04-30"), 2007,
                                                   ifelse((date >"2007-11-01" & date < "2008-04-30"), 2008,
                                                          ifelse((date >"2008-11-01" & date < "2009-04-30"), 2009,
                                                                 ifelse((date >"2009-11-01" & date < "2010-04-30"), 2010,
                                                                        ifelse((date >"2010-11-01" & date < "2011-04-30"), 2011, 0)))))))))
  
  ggplot(tidy_crops, aes(y=bu_per_acre, x = acres_harvested, color = crop ))+
    geom_point()
  
