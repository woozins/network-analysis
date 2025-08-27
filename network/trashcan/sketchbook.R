library(igraph)
library(ggraph)
library(dplyr)

city_coords <- data.frame(
  name = c("서울", "부산", "대구", "인천", "광주", "대전", "울산", "세종"),
  lon = c(126.9780, 129.0756, 128.6014, 126.7052, 126.8530, 127.3845, 129.3114, 127.2892),
  lat = c(37.5665, 35.1796, 35.8714, 37.4563, 35.1595, 36.3504, 35.5384, 36.4800)
)

# 예시 edges
edges <- data.frame(
  from = c("서울", "서울", "부산", "대구"),
  to   = c("부산", "대구", "서울", "서울"),
  weight = c(10, 5, 3, 2)
)

# 노드 추출
nodes <- unique(c(edges$from, edges$to))

# 좌표 연결
node_coords <- city_coords %>%
  filter(name %in% nodes) %>% rename(x=lon, y= lat)

# 그래프 생성
g <- graph_from_data_frame(edges, vertices = node_coords)

# 시각화
ggraph(g, layout = "manual", x = node_coords$x, y = node_coords$y) +
  geom_edge_link(aes(width = weight), arrow = arrow(length = unit(3, 'mm')), end_cap = circle(2, 'mm')) +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), color = "black", size = 4, vjust = -1) +
  theme_void()







