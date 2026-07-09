colnames(data)
scaled_data <- scale(data[, c("Flight_Duration","Day_of_Week")])
set.seed(123)
wcss <- numeric(10)

for (i in 1:10) {
  kmeans_model <- kmeans(scaled_data, centers = i, nstart = 20)
  wcss[i] <- kmeans_model$tot.withinss
}

plot(1:10, wcss, type="b",pch = 19, frame = FALSE,
     xlab="Number of Clusters",
     ylab="WCSS",
     main="Elbow Method")
kmeans_model <- kmeans(scaled_data, centers = 3, nstart = 25)
data$Cluster <- as.factor(kmeans_model$cluster)
centers<- kmeans_model$centers
centers_order <- order(centers[,2])
label_map <- c("Begining of the week", "Mid week", "Weekend")
data$Segment<- label_map[centers_order] [match(kmeans_model$cluster,centers_order)]
distances<- sqrt (rowSums((scaled_data - kmeans_model$centers[kmeans_model$cluster,])^2))
threshold<- quantile(distances, 0.95)
outliers <- data[distances >threshold,]
library(ggplot2)
ggplot(data, aes(x = Flight_Duration,y= Day_of_Week, color = Cluster))+
  geom_point (size = 3, alpha =0.7)+
  geom_point(data = outliers, aes(x = Flight_Duration,y= Day_of_Week), color ="red", size =4, shape =8)+
  labs(title = "Flight Segments by Duration and Day of week",
         x= "Flight_Duration",
         y= "Day_of_Week")+
   theme_minimal()
