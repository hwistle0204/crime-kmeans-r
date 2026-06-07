# Crime Data K-means Clustering Analysis


# Packages ----------------------------------------------------
library(dplyr)
library(ggplot2)
library(sp)
library(sf)
library(rgeos)
library(extrafont)
library(showtext)
library(curl)
library(readxl)
library(rgdal)

options(scipen = 10000)
showtext_auto()

# Data and scaling -------------------------------------------

crime_data <- readxl::read_xlsx("finaldata.xlsx")
scale_data <- scale(crime_data[, -c(1:2)])

# EDA ---------------------------------------------------------
# 1. Crime occurrence map: homicide

region_center <- read.csv("시군구.csv", fileEncoding = "EUC-KR")
map <- rgdal::readOGR("LARD_ADM_SECT_SGG_11.shp", encoding = "EUC-KR")
cent <- rgeos::gCentroid(map, byid = TRUE)
new_map <- ggplot2::fortify(map)

id <- seq(0, 24, 1)

homicide_df <- data.frame(
  시군구 = crime_data$sigun,
  살인 = crime_data$살인.발생
)

robbery_df <- data.frame(
  시군구 = crime_data$sigun,
  강도 = crime_data$강도.발생
)

sexual_violence_df <- data.frame(
  시군구 = crime_data$sigun,
  성폭력 = crime_data$성폭력.발생
)

theft_df <- data.frame(
  시군구 = crime_data$sigun,
  절도 = crime_data$절.도발생
)

assault_df <- data.frame(
  시군구 = crime_data$sigun,
  폭행 = crime_data$폭행.발생
)

final_data <- region_center %>%
  left_join(homicide_df, by = "시군구") %>%
  left_join(robbery_df, by = "시군구") %>%
  left_join(sexual_violence_df, by = "시군구") %>%
  left_join(theft_df, by = "시군구") %>%
  left_join(assault_df, by = "시군구")

final_data$sum <- apply(final_data[, 6:10], 1, sum)
final_data$mean <- apply(final_data[, 6:10], 1, mean)
final_data$id <- id

map_data <- merge(new_map, final_data, by = "id")

gra_homicide <- ggplot() +
  geom_polygon(
    data = map_data,
    aes(x = long, y = lat, group = group, fill = 살인),
    col = "black"
  ) +
  scale_fill_gradient(low = "white", high = "red", limits = c(1, 13)) +
  geom_text(
    aes(
      x = as.numeric(cent@coords[, 1]),
      y = as.numeric(cent@coords[, 2]),
      label = paste(region_center$시군구)
    ),
    fontface = "bold"
  ) +
  labs(title = "살인 발생 건수") +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20),
    legend.text = element_text(hjust = 0.5, size = 13),
    axis.line = element_blank()
  )

ggsave("gra_살인.png", plot = gra_homicide)

# 2. Correlation check ---------------------------------------
cor(crime_data[, -c(1:2)])
plot(crime_data[, -c(1:2)])

# K-means -----------------------------------------------------

kmeans_function <- function(data, scale_data, k, delete_col, seed_num) {
  set.seed(seed_num)

  df_kmeans <- kmeans(scale_data, centers = k)
  data$result_kmeans <- df_kmeans$cluster

  summary_by_cluster <- data[, -delete_col] %>%
    group_by(result_kmeans) %>%
    summarise_all(mean)

  data$result_kmeans <- as.factor(data$result_kmeans)

  result <- list(
    summary_by_cluster = summary_by_cluster,
    data = data
  )

  return(result)
}

# K-means with k = 4 -----------------------------------------
# c(1:2, 10:13): columns to remove when summarizing variables.

kmeans_results_2000 <- lapply(
  1:2000,
  FUN = function(x) {
    kmeans_function(
      data = crime_data,
      scale_data = scale_data,
      k = 4,
      delete_col = c(1:2, 10:13),
      seed_num = x
    )[[2]][, "result_kmeans"]
  }
)

result_2000 <- do.call(cbind, kmeans_results_2000) %>%
  `rownames<-`(crime_data$sigun) %>%
  `colnames<-`(paste0("seed", 1:2000))

result_2000 <- as.data.frame(sapply(result_2000, as.numeric))

# Calculate final cluster label using mode.
result_2000$final <- apply(result_2000, 1, function(x) {
  as.numeric(names(which.max(table(x))))
})

# Final cluster labels
result_2000$final

# Result check ------------------------------------------------
# 1. Visualize clusters on map

region_center <- read.csv("시군구.csv", fileEncoding = "EUC-KR")
map <- rgdal::readOGR("LARD_ADM_SECT_SGG_11.shp", encoding = "EUC-KR")
cent <- rgeos::gCentroid(map, byid = TRUE)
new_map <- ggplot2::fortify(map)

id <- seq(0, 24, 1)

cluster_df <- data.frame(
  시군구 = crime_data$sigun,
  cluster = result_2000$final
)

final_data <- left_join(region_center, cluster_df, by = "시군구")
final_data$id <- id

cluster_map_data <- merge(new_map, final_data, by = "id")

cluster_colors <- c("#CB181D", "#FCAE91", "#FB6A4A", "#FEE5D9")

gra_4kmeans <- ggplot() +
  geom_polygon(
    data = cluster_map_data,
    aes(x = long, y = lat, group = group, fill = factor(cluster)),
    col = "black"
  ) +
  scale_fill_manual(values = cluster_colors) +
  geom_text(
    aes(
      x = as.numeric(cent@coords[, 1]),
      y = as.numeric(cent@coords[, 2]),
      label = paste(region_center$시군구)
    ),
    fontface = "bold"
  ) +
  labs(
    title = "K-means clustering (k = 4)",
    fill = "Clustered group"
  ) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.7, size = 20),
    legend.text = element_text(hjust = 0.5, size = 13),
    axis.line = element_blank()
  )

ggsave("gra_4kmeans.png", plot = gra_4kmeans)

# 2. Explanatory variable boxplot ----------------------------

boxplot_data <- cbind(crime_data[, -1], result_kmeans = as.factor(result_2000$final))

ggplot(boxplot_data, aes(x = result_kmeans, y = divorce)) +
  geom_boxplot() +
  labs(title = "군집별 이혼 건수 비교") +
  xlab("Cluster") +
  ylab("이혼건수") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    title = element_text(size = 15, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 20)
  )
