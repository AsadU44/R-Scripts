
#Calling libraries
library(tidyverse)
library(nycflights13)
library(hflights)

#Making data frame
flight<- data.frame(flights)

#Check name of the columns
names(flights)

#Calling hflights and making tibbles

fl<- hflights
fl #(Very inconvenient with long columns, so lets make tibbles)
fl<-as.tibble(fl)
fl

#Selecting columns

fl1<- select(fl, ArrTime, DayOfWeek)

Another way using pipe

fl1<- fl %>% select(ArrTime, DayOfWeek) #(ctrl+shit+M = pipe shortcut)


#Selecting and omitting columns by groups

fl1<-  fl %>% select(Month:DepTime, 
              AirTime:Origin)

Omitting,

fl2<-  fl %>% select(!c(ArrTime:CancellationCode))


#Bringing columns to the left

fl3<- fl %>% select(DayofMonth, ArrTime, FlightNum, everything())


#Relocating columns

fl4<- fl %>% relocate(ArrTime:AirTime, .before = Year)

fl5<- fl %>% relocate(Year:AirTime, .after = Origin)

fl6<- fl %>% relocate(Year:AirTime, .after = last_col())


#Filtering based on values in rows

fl7<- fl %>% filter(Month==7, Distance>2000)

fl8<- fl %>% filter(Distance>3000 | Distance< 2000) 



#Renaming values inside a column

new.names<- c("AA"="America", "AS"="Alaska", "CO"="Columbia",
              "B6"="JetBlue")

names(new.names)

fl$Carrier_Names<- new.names[fl$UniqueCarrier]



#Grouping 
diamonds<- diamonds

diamonds1<- diamonds %>% group_by(cut) %>% summarise(mean(price))

#Grouping by multiple variable
diamonds2<- diamonds %>% group_by(Big=carat>1, cut) %>%
            summarise(mean(price))

Lets use a ggplot

diamonds2<- diamonds %>% group_by(Big=carat>1, cut) %>%
               summarise(avg_price=mean(price)) %>% 
              ggplot(aes(x=avg_price,y=cut, fill=Big
                          ))+geom_col(position = 'dodge')

#We can also use mutate function

diamonds3<- diamonds %>% group_by(cut) %>% 
  mutate(avg_price = mean(price))




