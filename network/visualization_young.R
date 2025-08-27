
# 시각화.
# install.packages('sf')
# library(sf)
library(ggplot2)
library(readxl)
library(dplyr)
library(purrr)
library(gridExtra)
library(patchwork)
# 지도 데이터
# shp_data <- 'gadm/gadm41_KOR_2.shp'
# seoul_shp <- 'tl_emd._seoul_4326.shp'
# map_sf <- st_read(shp_data, quiet = TRUE)
# seoul_sf <-  st_read(seoul_shp, quiet = TRUE)

# 행정구역별 위경도
file_path <- '행정구역별_위경도_좌표.xlsx'
seoul_coord <- read_excel(file_path, sheet = '서울특별시') 
seoul_coord

seoul_coord <- seoul_coord %>% mutate(node = paste(시도, 시군구)) %>% distinct(node, .keep_all = TRUE)
seoul_coord

location <- seoul_coord %>% select('위도', '경도', '시군구')
location


#join membership and location
membership_df_list <- lapply(membership_list, function(v) data.frame(v))
membership_df_list

membership_df <- Reduce(function(x, y) left_join(x, y, by = "X1"), membership_df_list)
colnames(membership_df) <- c('구' ,'group_2014', 'group_2019', 'group_2020', 'group_2021', 'group_2022', 'group_2023', 'group_2024')
membership_location <- left_join(membership_df, location, by = c('구' = '시군구'))

savegraph <- paste0('plots/membership', format(Sys.time(), "%d-%H-%M-%S"),'.png')
CairoPNG(savegraph, width=1200, height=300 * dim(draws)[2])
p1 <- ggplot(membership_location) + geom_point( mapping = aes(x = 경도, y = 위도, color = group_2014, size= 1))+geom_text(mapping = aes(x = 경도, y = 위도, label = 구), size = 2.5, vjust = -0.5)   # vjust는 텍스트 위치 조정
p2 <- ggplot(membership_location) + geom_point( mapping = aes(x = 경도, y = 위도, color = group_2019, size= 1))+geom_text(mapping = aes(x = 경도, y = 위도, label = 구), size = 2.5, vjust = -0.5)  # vjust는 텍스트 위치 조정
p3 <- ggplot(membership_location) + geom_point( mapping = aes(x = 경도, y = 위도, color = group_2020, size= 1))+geom_text(mapping = aes(x = 경도, y = 위도, label = 구), size = 2.5, vjust = -0.5)   # vjust는 텍스트 위치 조정
p4 <- ggplot(membership_location) + geom_point( mapping = aes(x = 경도, y = 위도, color = group_2021, size= 1))+geom_text(mapping = aes(x = 경도, y = 위도, label = 구), size = 2.5, vjust = -0.5)   # vjust는 텍스트 위치 조정
p5 <- ggplot(membership_location) + geom_point( mapping = aes(x = 경도, y = 위도, color = group_2022, size= 1))+geom_text(mapping = aes(x = 경도, y = 위도, label = 구), size = 2.5, vjust = -0.5)   # vjust는 텍스트 위치 조정
p6 <- ggplot(membership_location) + geom_point( mapping = aes(x = 경도, y = 위도, color = group_2023, size= 1))+geom_text(mapping = aes(x = 경도, y = 위도, label = 구), size = 2.5, vjust = -0.5)   # vjust는 텍스트 위치 조정
p7 <- ggplot(membership_location) + geom_point( mapping = aes(x = 경도, y = 위도, color = group_2024, size= 1))+geom_text(mapping = aes(x = 경도, y = 위도, label = 구), size = 2.5, vjust = -0.5)  # vjust는 텍스트 위치 조정
grid.arrange(p1, p2, p3, p4, p5, p6, p7, nrow = 3, ncol = 3)
theme_minimal()
dev.off()


membership_location
