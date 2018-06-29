library(readr) # for guess_encoding()

guess_encoding("car.csv")
rent <- read_csv("car.csv")

str(rent)

rent$일자 <- ymd(rent$일자)
colnames(rent) <- c("date", "wday", "gender", "age", "lo_si", "lo_gu", "lo_dong","y")

rent %>% 
  dplyr::group_by(date, gender) %>% 
  dplyr::summarise(sum_tel = sum(y))

rent %>% 
  dplyr::group_by(date,age) %>% 
  dplyr::summarise(sum_tel = sum(y))

rent %>% 
  dplyr::group_by(age) %>% 
  dplyr::summarise(sum_tel = sum(y)) %>% 
  dplyr::arrange(desc(sum_tel))

rent %>% 
  dplyr::group_by(gender) %>% 
  dplyr::summarise(sum_tel = sum(y)) %>% 
  dplyr::arrange(desc(sum_tel))

rent %>% 
  dplyr::group_by(lo_si) %>% 
  dplyr::summarise(sum_tel = sum(y)) %>% 
  dplyr::arrange(desc(sum_tel))

seoul_rent <- rent %>% 
  dplyr:: filter(lo_si == "서울특별시") %>% 
  dplyr:: group_by(date) %>% 
  dplyr:: summarise(sum_tel = sum(y))



example_weather <- fread("example_weather.csv")

example_weather_2 <- example_weather %>% 
  dplyr:: select(2,3,9,10)

colnames(example_weather_2) <- c("date", "temp", "wind", "humi")

example_weather_2 <- as.data.table(example_weather_2)
setkey(example_weather_2, date)

example_weather_2$date <- ymd(example_weather_2$date)


seoul_rent <- as.data.table(seoul_rent)
setkey(seoul_rent, date)
str(seoul_rent)

# why error? -> date 연도 다름
merge(seoul_rent, example_weather_2)


example_weather_2$usage <- seoul_rent$sum_tel

ggplot(example_weather_2, aes(x = temp, y = usage)) +
  geom_point()


list_rent <- list()
data_number <- c(1:12)
paste0("car_", data_number, ".csv")

for(i in 1:length(data_number)){

 list_rent[[i]] <- read_csv(paste0("car_", data_number[i], ".csv"))
 rent_example <- rbindlist(list_rent)
 rent_example

}


colnames(rent_example) <- c("date", "wday", "gender", "age", "lo_si", "lo_gu", "lo_dong","y")
rent_example$date <- ymd(rent_example$date)
rent_example <- as.data.table(rent_example)
setkey(rent_example, date)


weather_example <- fread("weather_example_year.csv")

weather_example<- weather_example %>% 
  dplyr:: select(2,3,9,10)

colnames(weather_example) <- c("date", "temp", "wind", "humi")

weather_example <- as.data.table(weather_example)
setkey(weather_example, date)
weather_example$date <- ymd(weather_example$date)



seoul_rent <- rent_example %>% 
  dplyr:: filter(lo_si == "경상남도") %>% 
  dplyr:: group_by(date) %>% 
  dplyr:: summarise(sum_tel = sum(y))

seoul_rent$date <- ymd(seoul_rent$date)
seoul_rent <- as.data.table(seoul_rent)
setkey(seoul_rent, date)

seoul_merge <- merge(seoul_rent, weather_example)

ggplot(seoul_merge, aes(x = humi, y = sum_tel)) +
  geom_point() +
  geom_smooth()

cor(seoul_merge$sum_tel, seoul_merge$temp)



rent_example %>% 
  dplyr::group_by(date, gender) %>% 
  dplyr::summarise(sum_tel = sum(y))

rent_example %>% 
  dplyr::group_by(date,age) %>% 
  dplyr::summarise(sum_tel = sum(y))

rent_example %>% 
  dplyr::group_by(age) %>% 
  dplyr::summarise(sum_tel = sum(y)) %>% 
  dplyr::arrange(desc(sum_tel))

rent_example %>% 
  dplyr::group_by(gender) %>% 
  dplyr::summarise(sum_tel = sum(y)) %>% 
  dplyr::arrange(desc(sum_tel))

rent_example %>% 
  dplyr::group_by(lo_si) %>% 
  dplyr::summarise(sum_tel = sum(y)) %>% 
  dplyr::arrange(desc(sum_tel))

rent_example %>% 
  dplyr::group_by(wday) %>% 
  dplyr::summarise(sum_tel = sum(y)) 

rent_example %>% 
  dplyr:: mutate(usage_month = month(date)) %>% 
  dplyr:: group_by(usage_month) %>% 
  dplyr:: summarise(sum_tel = sum(y))


# install.packages("devtools")
devtools::install_github("robjhyndman/anomalous")
library(anomalous)

z <- ts(matrix(rnorm(3000),ncol=100),freq=4)
y <- tsmeasures(z)
biplot.features(y)
anomaly(y)
