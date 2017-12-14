# energy

#시간 변환 패키지
install.packages("lubridate")
library(lubridate)
str(energy)
energy$date<-ymd_hms(energy$date)
