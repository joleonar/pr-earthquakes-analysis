require(bbplot)
require(ggplot2)
require(lubridate)
require(ggmap)
require(cluster)
require(dplyr)
require(prophet)

df <- read.csv("https://earthquake.usgs.gov/fdsnws/event/1/query?format=csv&starttime=2019-12-20&endtime=2020-01-20&latitude=17.949&longitude=-66.851&maxradiuskm=50")

df$dateTime <- as.POSIXct(df$time, format="%Y-%m-%dT%H:%M:%OS", tz="GMT")

# Change the time zone
df$dateTime <- with_tz(df$dateTime, "America/Puerto_Rico")

# Remove events with magnitude 0
df <- df[df$mag > 0,]

# Basics
summary(df$mag)

ggplot(data=df ,aes(x=dateTime,y=mag)) + 
  geom_point(size = df$mag, alpha = 0.7) +
  geom_smooth() +
  bbc_style() +
  theme(axis.title = element_text(size = 18), 
        plot.margin = unit(c(1.0,1.5,1.0,1.0), "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  xlab('Date') + ylab('Magnitude') +
  scale_x_datetime(date_breaks = "3 day") +
  ggtitle("The magnitude value of Puerto Rico's earthquakes", subtitle = "From 01-01-2020 to 20-01-2020")


ggplot(data=df ,aes(x=dateTime,y=depth)) + 
  geom_point(size = df$mag, alpha = 0.7) +
  geom_smooth() +
  bbc_style() +
  theme(axis.title = element_text(size = 18), 
        plot.margin = unit(c(1.0,1.5,1.0,1.0), "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  xlab('Date') + ylab('Depth') +
  scale_x_datetime(date_breaks = "3 day") +
  ggtitle("The depth value of Puerto Rico's earthquakes", subtitle = "From 01-01-2020 to 20-01-2020")


map <- get_googlemap(center=c(-66.7707549, 17.9472521), zoom = 10, maptype = 'roadmap', size = c(640, 640), scale = 2)

map %>% ggmap() +
  geom_point(data = df, 
             aes(x = longitude, y = latitude)) +
  stat_density2d(data=df, aes(x=longitude, y=latitude, fill = stat(level), alpha=..level..),
                 geom='polygon') +
  scale_color_brewer(palette='Set1')+
  theme(legend.position = 'none',
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 18),
        axis.text.x  = element_text(size = 14),
        axis.text.y  = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.margin = unit(c(1.0,1.5,1.0,0.5), 'cm')) +
  xlab('Longitude') + ylab('Latitude') +
  ggtitle("Puerto Rico's Earthquakes", subtitle = "From 01-01-2020 to 20-01-2020")


ggplot(data=df ,aes(x=df$mag,y=depth)) + 
  geom_point() +
  bbc_style() +
  theme(axis.title = element_text(size = 18), 
        plot.margin = unit(c(1.0,1.5,1.0,1.0), "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  xlab('Magnitude') + ylab('Depth') +
  ggtitle("Scatterplot of Puerto Rico's Earthquakes", subtitle = "From 01-01-2020 to 20-01-2020")


# Clustering
features <- df %>%
  select(latitude, longitude, mag, depth)
p <- pam(features, 4, trace.lev = 1)
print(p$clusinfo)
p_silhouette <- silhouette(p)
clusplot(p, shade=TRUE, color=TRUE)
plot(p_silhouette, col='black', border='gray',
     main='Silhouette plot of the cluster')

features$cluster <- p$clustering

# Time Series
values.by.day <- df %>%
  select(dateTime, mag) 

colnames(values.by.day) <- c('ds', 'y')
m <- prophet(values.by.day)
future <- make_future_dataframe(m, periods = 10)
forecast <- predict(m, future)
plot(m, forecast) +
  bbc_style()

prophet_plot_components(m, forecast) +
  bbc_style()

write.csv(values.by.day, "values_by_day.csv", row.names = FALSE)

## Correlation with Google Trends
trend <- read.csv("~/Development/pr-earthquake/trend.csv")
df$date <- date(df$dateTime)
max.per.day <- df %>%
  select(mag, date) %>%
  group_by(date) %>%
  summarise(max = max(mag))



  
  



