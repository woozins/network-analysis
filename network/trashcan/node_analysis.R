library(igraph)

data <- read.csv('migration_flow_summary.csv')
data_seoul <- read.csv('서울시_구간_이동_전입자수_서로다른구.csv')
data_seoul_2019 <- read.csv('서울시_구간_이동_전입자수_서로다른구_2019.csv')

#node strength

edges <- data_seoul
edges_2019 <- data_seoul_2019
colnames(edges) <- c('from','to','weight')
colnames(edges_2019) <- c('from','to','weight')

g_weighted <- graph_from_data_frame(edges, directed = TRUE)
g_weighted_2019 <- graph_from_data_frame(edges_2019, directed = TRUE)

node.strength <-strength(g_weighted, mode = "all", weights = E(g_weighted)$weight)


#HITS algorithm
#weighted HITS
weighted_hits <- function(adj_matrix, max_iter = 100, tol = 1e-6) {
  n <- nrow(adj_matrix)
  
  # 초기화
  h <- rep(1, n)
  a <- rep(1, n)
  
  for (iter in 1:max_iter) {
    a_new <- t(adj_matrix) %*% h
    h_new <- adj_matrix %*% a_new
    
    # 정규화
    a_new <- a_new / sqrt(sum(a_new^2))
    h_new <- h_new / sqrt(sum(h_new^2))
    
    # 수렴 조건
    if (max(abs(h_new - h)) < tol && max(abs(a_new - a)) < tol) {
      break
    }
    
    a <- a_new
    h <- h_new
  }
  
  list(authority = as.vector(a), hub = as.vector(h))
}


adj <- as_adjacency_matrix(g_weighted, attr = "weight", sparse = FALSE)

result <- weighted_hits(adj)
result.frame <- data.frame(
  Node = V(g_weighted)$name,
  Authority = result$authority,
  Hub = result$hub
)

result.frame
result.frame[order(-result.frame$Authority),]
result.frame[order(-result.frame$Hub),]


adj_2019 <- as_adjacency_matrix(g_weighted_2019, attr = "weight", sparse = FALSE)

