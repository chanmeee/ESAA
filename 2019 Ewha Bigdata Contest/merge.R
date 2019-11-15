# Load Library
library(tidyverse)
library(dplyr)
library(cellranger)

# Set Directory
setwd("../2019 Ewha Data Analysis Contest/data")

# Load Data

# 1) 발화기기 
## 경로 지정 및 데이터 목록 
dir <- ("C:\\Users\\Chanmi Yoo\\Desktop\\ESAA\\2019 Ewha Data Analysis Contest\\data\\fire machine")
file_list <- list.files(dir)

## 데이터 불러오기 
fire_machine <- data.frame()
year <- data.frame()

for (file in file_list) {
  print(file)
  temp1 <- readxl::read_excel(paste(dir, file, sep = "\\"), range = cell_cols("B:R"))
  names(temp1)[1] <- c('구분')
  temp1$구분[1] <- '합계'
  
  ## 들불, 산불 구분 
  i = 3
  while (temp1$구분[i] != '소계'){
    i = i+1
    if(temp1$구분[i] == '소계') break
  }
  
  n = 2
  for (n in 2:nrow(temp1)){
    ifelse (n < i, 
            temp1$구분[n] <- paste('들불_', temp1$구분[n]),
            temp1$구분[n] <- paste('산불_', temp1$구분[n]))
  }
  
  ## 엑셀 파일 붙이기 
  fire_machine <- rbind(fire_machine, temp1)
  
  ## year 변수 생성 
  temp2 <- data.frame(rep(substr(file, 1, 4), nrow(temp1)))
  names(temp2) <- c('연도')
  year <- rbind(year, temp2)
}

fire_machine <- cbind(fire_machine, year) ## year 행 추가 

# Save Data
write.csv(fire_machine, "fire_machine.csv")

# 2) 발화요인 

## 경로 지정 및 데이터 목록 
dir <- ("C:\\Users\\Chanmi Yoo\\Desktop\\ESAA\\2019 Ewha Data Analysis Contest\\data\\fire cause")
file_list <- list.files(dir)

## 데이터 불러오기 
fire_cause <- data.frame()
year <- data.frame()

for (file in file_list) {
  print(file)
  temp1 <- readxl::read_excel(paste(dir, file, sep = "\\"), range = cell_cols("B:N"))
  names(temp1)[1] <- c('구분')
  temp1$구분[1] <- '합계'
  
  ## 들불, 산불 구분 
  i = 3
  while (temp1$구분[i] != '소계'){
    i = i+1
    if(temp1$구분[i] == '소계') break
  }
  
  n = 2
  for (n in 2:nrow(temp1)){
    ifelse (n < i, 
            temp1$구분[n] <- paste('들불_', temp1$구분[n]),
            temp1$구분[n] <- paste('산불_', temp1$구분[n]))
  }
  
  ## 엑셀 파일 붙이기 
  fire_cause <- rbind(fire_cause, temp1)
  
  ## year 변수 생성 
  temp2 <- data.frame(rep(substr(file, 1, 4), nrow(temp1)))
  names(temp2) <- c('연도')
  year <- rbind(year, temp2)
}

fire_cause <- cbind(fire_cause, year) ## year 행 추가 

# Save Data
write.csv(fire_cause, "fire_cause.csv")


# 3) 연소확대사유 

## 경로 지정 및 데이터 목록 
dir <- ("C:\\Users\\Chanmi Yoo\\Desktop\\ESAA\\2019 Ewha Data Analysis Contest\\data\\fire extendcause")
file_list <- list.files(dir)

## 데이터 불러오기 
fire_extendcause <- data.frame()
year <- data.frame()

for (file in file_list) {
  print(file)
  temp1 <- readxl::read_excel(paste(dir, file, sep = "\\"), range = cell_cols("B:N"))
  names(temp1)[1] <- c('구분')
  temp1$구분[1] <- '합계'
  
  ## 들불, 산불 구분 
  i = 3
  while (temp1$구분[i] != '소계'){
    i = i+1
    if(temp1$구분[i] == '소계') break
  }
  
  n = 2
  for (n in 2:nrow(temp1)){
    ifelse (n < i, 
            temp1$구분[n] <- paste('들불_', temp1$구분[n]),
            temp1$구분[n] <- paste('산불_', temp1$구분[n]))
  }
  
  ## 엑셀 파일 붙이기 
  fire_extendcause <- rbind(fire_extendcause, temp1)
  
  ## year 변수 생성 
  temp2 <- data.frame(rep(substr(file, 1, 4), nrow(temp1)))
  names(temp2) <- c('연도')
  year <- rbind(year, temp2)
}

fire_extendcause <- cbind(fire_extendcause, year) ## year 행 추가 

# Save Data
write.csv(fire_extendcause, "fire_extendcause.csv")



# 4) 최초착화물  

## 경로 지정 및 데이터 목록 
dir <- ("C:\\Users\\Chanmi Yoo\\Desktop\\ESAA\\2019 Ewha Data Analysis Contest\\data\\fire firstobject")
file_list <- list.files(dir)

## 데이터 불러오기 
fire_firstobject <- data.frame()
year <- data.frame()

for (file in file_list) {
  print(file)
  temp1 <- readxl::read_excel(paste(dir, file, sep = "\\"), range = cell_cols("B:N"))
  names(temp1)[1] <- c('구분')
  temp1$구분[1] <- '합계'
  
  ## 들불, 산불 구분 
  i = 3
  while (temp1$구분[i] != '소계'){
    i = i+1
    if(temp1$구분[i] == '소계') break
  }
  
  n = 2
  for (n in 2:nrow(temp1)){
    ifelse (n < i, 
            temp1$구분[n] <- paste('들불_', temp1$구분[n]),
            temp1$구분[n] <- paste('산불_', temp1$구분[n]))
  }
  
  ## 엑셀 파일 붙이기 
  fire_firstobject <- rbind(fire_firstobject, temp1)
  
  ## year 변수 생성 
  temp2 <- data.frame(rep(substr(file, 1, 4), nrow(temp1)))
  names(temp2) <- c('연도')
  year <- rbind(year, temp2)
}

fire_firstobject <- cbind(fire_firstobject, year) ## year 행 추가 

# Save Data
write.csv(fire_firstobject, "fire_firstobject.csv")


