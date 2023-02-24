#import useful libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
#read in the data
data <- read.table('./covid19_vaccine.csv',header= T,sep=',')

head(data,5)
#check the names and data types
tibble(names(data), sapply(data,class))

#remove information on provinces and check for missing values

data = subset(data,select=-province_state)

cat("Number of missing values are:",sum(is.na(data)))
cat("The number of countries:", length(unique(data$country_region)))

#We then calculate the ratio of the partially and the fully vaccinated people 
#and add for every single day and country and 
#add those two columns in the dataframe

ratio_partially <- data[,"people_partially_vaccinated"]/data[,"population"]*100
ratio_fully <- data[,"people_fully_vaccinated"]/data[,"population"]*100

data$partially_vaccinated_ratio <- as.numeric(format(round(ratio_partially, 2), nsmall = 2, format = "f"))
data$fully_vaccinated_ratio <- as.numeric(format(round(ratio_fully, 2), nsmall = 2, format = "f"))

data$date<- as.Date(data$date)

#let’s observe our country’s (South Africa) fully vaccinated ratio

data %>%
  filter(country_region %in% c('South Africa')) %>%
  ggplot() +
  geom_smooth(mapping = aes(x = date, y = partially_vaccinated_ratio, color = 'partially_vaccinated_ratio')) +
  geom_smooth(mapping = aes(x = date, y = fully_vaccinated_ratio, color = 'fully_vaccinated_ratio')) +
  labs(x = "Date",
       y = "Fully & Partially vaccinated ratio",
       title = "People Fully & Partially Vaccinated Ratio in South Africa")

#We calculate the total population of South Africa
gr_popul = mean(data[ grep("South Africa", data$country_region) , ]$population)

likegr_eu = data[data$population >= gr_popul*0.9 & data$population <= gr_popul*1.1 & data$continent_name =="Africa",]

likegr_eu %>%
  filter(country_region %in% (unique(likegr_eu$country_region))) %>%
  ggplot() +
  geom_smooth(mapping = aes(x = date, y = fully_vaccinated_ratio, color = country_region)) +
  labs(x = "Date",
       y = "Fully vaccinated ratio",
       title = "People Fully Vaccinated Ratio in African countries that have approximately the same population with South Africa")+ theme(plot.title = element_text(size=10))

eu = data[data$fully_vaccinated_ratio>50 & data$fully_vaccinated_ratio<100 &data$continent_name == "Africa", ]
eu = eu[!is.na(eu$country_region), ]

mycolors = c(brewer.pal(name="Dark2", n = length(unique(eu$country_region))%/%6), brewer.pal(name="Paired", n = length(unique(eu$country_region)) - length(unique(eu$country_region))%/%6))

ggplot(eu, aes(x=date, y=fully_vaccinated_ratio, group=1, color = country_region ))+
  geom_line() +
  scale_color_manual(values = mycolors)+
  labs(x = "Date", y = "Fully vaccinated ratio", title = "Countries with fully vaccinated more than 50% ratio per date in Africa") + theme(plot.title = element_text(size=10)) + theme_minimal()
asia = 0
asia_pop = 0
africa = 0
africa_pop = 0
europe = 0
europe_pop = 0
north_a = 0
north_a_pop = 0
south_a = 0 
south_a_pop = 0
oceania = 0
oceania_pop = 0
for (country in unique(data$country_region)) {
  if(country!='World'){
    eachcountry = data[data$country_region == country, ]
    
    if(!is.na(eachcountry$continent_name)){
      vac = eachcountry$people_fully_vaccinated[length(eachcountry$people_fully_vaccinated)]
      for (vac in rev(eachcountry$people_fully_vaccinated)) {
        if(!is.na(vac))
          break
      }
      if(!is.na(vac)){
        if(eachcountry$continent_name == 'Asia'){
          asia = asia + vac
          asia_pop = asia_pop + max(eachcountry$population,na.rm = TRUE)
        }
        if(eachcountry$continent_name == 'Europe'){
          europe = europe + vac
          europe_pop = europe_pop + max(eachcountry$population,na.rm = TRUE)
        }
        if(eachcountry$continent_name == 'Africa'){
          africa = africa + vac
          africa_pop = africa_pop + max(eachcountry$population,na.rm = TRUE)
        }
        if(eachcountry$continent_name == 'North America'){
          north_a = north_a + vac
          north_a_pop = north_a_pop + max(eachcountry$population,na.rm = TRUE)
        }  
        if(eachcountry$continent_name == 'South America'){
          south_a = south_a + vac
          south_a_pop = south_a_pop + max(eachcountry$population,na.rm = TRUE)
        }
        if(eachcountry$continent_name == 'Oceania'){
          oceania = oceania + vac
          oceania_pop = oceania_pop + max(eachcountry$population,na.rm = TRUE)
        } 
      }
    }
  }
}
for (max_date_asia in as.character(rev(data[data$continent_name == "Asia", ]$date))) {
  if(!is.na(max_date_asia))
    break
}
for (max_date_europe in as.character(rev(data[data$continent_name == "Europe", ]$date))) {
  if(!is.na(max_date_europe))
    break
}
for (max_date_africa in as.character(rev(data[data$continent_name == "Africa", ]$date))) {
  if(!is.na(max_date_africa))
    break
}

for (max_date_north_a in as.character(rev(data[data$continent_name == "North America", ]$date))) {
  if(!is.na(max_date_north_a))
    break
}
for (max_date_south_a in as.character(rev(data[data$continent_name == "South America", ]$date))) {
  if(!is.na(max_date_south_a))
    break
}
for (max_date_oceania in as.character(rev(data[data$continent_name == "Oceania", ]$date))) {
  if(!is.na(max_date_oceania))
    break
}
asia_ratio_fully_vaccinated = asia/asia_pop
africa_ratio_fully_vaccinated = africa/africa_pop
europe_ratio_fully_vaccinated = europe/europe_pop
north_a_ratio_fully_vaccinated = north_a/north_a_pop
south_a_ratio_fully_vaccinated = south_a/south_a_pop
oceania_ratio_fully_vaccinated = oceania/oceania_pop
continent_ratios = c(asia_ratio_fully_vaccinated, africa_ratio_fully_vaccinated, europe_ratio_fully_vaccinated, north_a_ratio_fully_vaccinated, south_a_ratio_fully_vaccinated,oceania_ratio_fully_vaccinated)
continents = c("Asia", "Africa", "Europe", "North America", "South America", "Oceania")
continent.data = data.frame(continent_ratios, continents)

ggplot(continent.data, aes(x=continents, y=continent_ratios, fill=continents)) +
  geom_bar(stat="identity", color="black", position=position_dodge(0.9), size=1) +
  geom_text(aes(label=sprintf("%0.4f", round(continent_ratios, digits = 4))), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5) +
  scale_fill_brewer(palette="Paired") +
  labs(x = "Continets", y = "Fully vaccinated ratio", title = "People Fully Vaccinated Ratio per continent") + theme(plot.title = element_text(size=15)) + theme_minimal()+ scale_x_discrete(guide = guide_axis(angle = 90))

sa = data[data$continent_name=="Africa", ][!is.na(data[data$continent_name=="South America", ]$country_region), ]
sa = sa[sa$people_fully_vaccinated>0 & !is.na(sa$people_fully_vaccinated), ]

ggplot(sa, aes(x=country_region, y=(people_fully_vaccinated), fill=country_region)) +
  geom_bar(stat="identity", color="black", position=position_dodge(1), size=.5) +
  scale_fill_brewer(palette="Paired") +
  labs(x = "African Countries", y = "People Fully Vaccinated", title = "People Fully Vaccinated in Africa") + theme(plot.title = element_text(size=20)) + theme_minimal() +
  scale_x_discrete(guide = guide_axis(angle = 90))

ggplot(sa, aes(x=country_region, y=(fully_vaccinated_ratio), fill=country_region)) +
  geom_bar(stat="identity", color="black", position=position_dodge(1), size=.5) +
  scale_fill_brewer(palette="Paired") +
  labs(x = "African Countries", y = "Fully vaccinated ratio", title = "People Fully Vaccinated Ratio in Africa") + theme(plot.title = element_text(size=10)) + theme_minimal() +
  scale_x_discrete(guide = guide_axis(angle = 90))


sa = data[data$continent_name=="South America", ][!is.na(data[data$continent_name=="South America", ]$country_region), ]
sa = sa[sa$people_fully_vaccinated>0 & !is.na(sa$people_fully_vaccinated), ]


most_pop = data[data$population>max(data$population,na.rm = TRUE)*0.09 & data$population<max(data$population, na.rm = TRUE), ]
most_pop = most_pop[!is.na(most_pop$country_region), ]

ggplot(most_pop, aes(x=country_region, y=(fully_vaccinated_ratio), fill=country_region)) +
  geom_bar(stat="identity", color="black", position=position_dodge(1), size=.5) +
  scale_fill_brewer(palette="Paired") +
  labs(x = "Most popululated countries worldwide", y = "Fully vaccinated ratio", title = "People Fully Vaccinated Ratio in the countries with the largest population") + theme(plot.title = element_text(size=10)) + theme_minimal() +
  scale_x_discrete(guide = guide_axis(angle = 90))


most_ratio = data[data$fully_vaccinated_ratio>82 & data$fully_vaccinated_ratio<100, ]
most_ratio = most_ratio[!is.na(most_ratio$country_region), ]

ggplot(most_ratio, aes(x=country_region, y=(fully_vaccinated_ratio), fill=country_region)) +
  geom_bar(stat="identity", color="black", position=position_dodge(1), size=.5) +
  scale_fill_brewer(palette="Paired") +
  labs(x = "Countries", y = "Fully vaccinated ratio", title = "People Fully Vaccinated Ratio in countries with the highest ratios globally") + theme(plot.title = element_text(size=10)) + theme_minimal() +
  scale_x_discrete(guide = guide_axis(angle = 90))


most_ratio = data[data$partially_vaccinated_ratio>82 & data$partially_vaccinated_ratio<100 & data$population>10000000, ]
most_ratio = most_ratio[!is.na(most_ratio$country_region), ]

ggplot(most_ratio, aes(x=country_region, y=(partially_vaccinated_ratio), fill=country_region)) +
  geom_bar(stat="identity", color="black", position=position_dodge(1), size=.5) +
  scale_fill_brewer(palette="Paired") +
  labs(x = "Countries", y = "Fully vaccinated ratio", title = "People Partially Vaccinated Ratio in countries with the highest ratios globally") + theme(plot.title = element_text(size=10)) + theme_minimal() +
  scale_x_discrete(guide = guide_axis(angle = 90))


most_ratio = data[data$partially_vaccinated_ratio>82 & data$partially_vaccinated_ratio<100 & data$population>10000000, ]
most_ratio = most_ratio[!is.na(most_ratio$country_region), ]

mycolors = c(brewer.pal(name="Dark2", n = length(unique(most_ratio$country_region))%/%3), brewer.pal(name="Paired", n = length(unique(most_ratio$country_region)) - length(unique(most_ratio$country_region))%/%3))

ggplot(most_ratio, aes(x=date, y=partially_vaccinated_ratio, group=1, color = country_region ))+
  geom_line() +
  scale_color_manual(values = mycolors)+
  labs(x = "Countries with the highest partial vaccinated ratio globally per date", y = "Partially vaccinated ratio", title = "People Partially Vaccinated Ratio in countries with the highest ratios globally") + theme(plot.title = element_text(size=10)) + theme_minimal()


