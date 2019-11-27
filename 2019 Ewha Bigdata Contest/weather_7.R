library(tidyverse)

# Load Data
setwd("../Desktop/ESAA/2019 Ewha Data Analysis Contest/data")
sanbul <- readxl::read_xls("area_sanbul.xls", skip = 2)

# 한글 변수명을 영어로 변환 
names(sanbul) <- c('start_year', 'start_month', 'start_date', 'start_hour', 'start_day', 
                   'end_year', 'end_month', 'end_date', 'end_hour',
                   'area_gwanseo', 'area_city', 'area_sigungu', 'area_eupmeon', 'area_dongri', 'area_jibun',
                   'cause_detail', 'damage_area') 

# 날씨 데이터 
weather <- read.csv("weather.csv") 

# 결측치 확인 
colSums(is.na(weather[,3:6])) 
## temp(1개), rainfall(47495개), wind(12개), humidity(8개)

## rainfall은 0으로 대체하고, 나머지 변수는 결측치 제외하고 일주일 평균 기상 데이터값을 계산
weather$rainfall[is.na(weather$rainfall)] <- 0
weather %>% 
  filter(rainfall == '0')


# 기온, 강수량, 풍속, 습도 7일치 평균값 계산 
weather$temp_mean <- list(NA)
weather$rainfall_mean <- list(NA)
weather$wind_mean <- list(NA)
weather$humidity_mean <- list(NA)

for (i in 7:nrow(weather)){
  a <- weather[(i-6):i,]
  weather$temp_mean[i] <- mean(a[,3], na.rm=T)
  weather$rainfall_mean[i] <- mean(a[,4], na.rm=T)
  weather$wind_mean[i] <- mean(a[,5], na.rm=T)
  weather$humidity_mean[i] <- mean(a[,6], na.rm=T)
}


# 첫 6일은 일주일의 평균값을 사용할 수 없으므로 1일, 2일, ..의 평균값 
for (i in 1:6){
  b <- weather[1:i,]
  weather$temp_mean[i] <- lapply(b[,3:6], mean)[[1]]
  weather$rainfall_mean[i] <- lapply(b[,3:6], mean)[[2]]
  weather$wind_mean[i] <- lapply(b[,3:6], mean)[[3]]
  weather$humidity_mean[i] <- lapply(b[,3:6], mean)[[4]]
} 

weather$temp_mean <- as.vector(unlist(weather$temp_mean))
weather$rainfall_mean <- as.vector(unlist(weather$rainfall_mean))
weather$wind_mean <- as.vector(unlist(weather$wind_mean))
weather$humidity_mean <- as.vector(unlist(weather$humidity_mean))

# Write CSV 
write.csv(weather, "weather_week.csv")

