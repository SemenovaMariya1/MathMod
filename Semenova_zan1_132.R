#Семенова Мария Григорьевна, группа ДА-132, Задание №1. 23 регион - Краснодарский край

install.packages("tidyverse")
install.packages("rnoaa")
library(tidyverse)
library(rnoaa)
library(dplyr)
library(lubridate)


#скачиваем данные со всех метеостанций
station_data = ghcnd_stations()
write.csv(station_data, file = "station_data.csv")
station_data = read_csv("station_data.csv")
#загружаем станции по региону 23 - Краснодасркий край
krasnodar = data.frame(id="KRASNODAR", latitude = 44.32255,
                       longitude = 38.63176 )
krasnodar_around = meteo_nearby_stations(lat_lon_df = krasnodar, station_data = station_data,
                                         limit = 14, var = c("PRCP","TAVG"),
                                         year_min = 2003, year_max = 2004)
#загрузим данные с идентификаторами метеостанций для региона
krasnodar_id = krasnodar_around[["KRASNODAR"]][["id"]][1]
all_krasnodar_data = meteo_tidy_ghcnd(stationid = krasnodar_id)
summary(all_krasnodar_data)

#преобразуем класс данных
all_krasnodar_data$date
class(all_krasnodar_data$date)
all_krasnodar_data$date+1
as.numeric(all_krasnodar_data$date)

#работа с табличными даннми, преобразования для дальнейшей работы
all_krasnodar_data1 = all_krasnodar_data %>% mutate(
  year = year(all_krasnodar_data$date),
  month = month(all_krasnodar_data$date),
  day = yday(all_krasnodar_data$date)
) %>% select(year, month, day, tavg)
all_krasnodar_data2 = all_krasnodar_data1 %>% mutate(tavg = case_when(TRUE ~ tavg/10)
)
all_krasnodar_data2 = filter(all_krasnodar_data2,year > 2002 & year < 2004)

#перевод значений NA и температур ниже 5 градусов в 0
all_krasnodar_data2[is.na(all_krasnodar_data2$tavg),"tavg"] = 0
all_krasnodar_data2[all_krasnodar_data2$tavg<5, "tavg"] = 0
summary(all_krasnodar_data2)

#сгруппироуем значения по году и месяцу и просуммируем температуры
group_meteodata =all_krasnodar_data2 %>% group_by(year,month)
sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)
sumT_month=groups_month%>%summarise(St=mean(tsum))

# подготовка данных (значения взяты из задания)
y = 1.0 
afi = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
di = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
Kf = 300 
Qj = 1600 
Lj = 2.2 
Ej = 25 

#Расчитаем показатель Fi по месяцам 
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)

#Расчитаем показатель Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))

#Расчитаем урожайность пшеницы
Yield = (sum(sumT_month$Yi)) 
Yield 
#Урожайность пшеницы 21,93 ц/га.