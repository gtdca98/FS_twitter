
## rutina en R para proceso de datos

library("twitteR")
library("wordcloud")
library("tm")
require(sm)
library(stringr)
library(lubridate)

#Revise la documentacion de twitteR documentation y la api de Twitter para generar las llaves
consumer_key <- 'XXXXXXXXXXXXxxXXXXXXXXXXX'
consumer_secret <- 'XXXXXXXXXXXXxxXXXXXXXXXXXXXXXXXXXXXXXxxXXXXXXXXXXX'
access_token <- '3XXXXXXXXXXXXxxXXXXXXXXXXX'
access_secret <- 'XXXXXXXXXXXXxxXXXXXXXXXXX'
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)



## obtenemos y guardamos informafción de Twitter
r_sof1 <- searchTwitter('rebsamen sofia frida', since = '2017-09-20', until = '2017-09-21', resultType="popular",   n=14)
saveRDS(r_sof1,"Frida_Sofia_Rebsamen_20sepI.rds")

r_sof2 <- searchTwitter('rebsamen sofia frida', since = '2017-09-21', until = '2017-09-22', resultType="popular",   n=40)
saveRDS(r_sof2,"Frida_Sofia_Rebsamen_21sepI.rds")

r_sof3 <- searchTwitter('rebsamen sofia frida', since = '2017-09-22', until = '2017-09-23', resultType="popular",   n=14)
saveRDS(r_sof3,"Frida_Sofia_Rebsamen_22sepI.rds")

r_sof4 <- searchTwitter('rebsamen sofia frida', since = '2017-09-23', until = '2017-09-24', resultType="popular",   n=100)
saveRDS(r_sof4,"Frida_Sofia_Rebsamen_23sepI.rds")

## lectura de archivos

r_soft1 = readRDS("Frida_Sofia_Rebsamen_20sepI.rds")
r_soft2 = readRDS("Frida_Sofia_Rebsamen_21sepI.rds")
r_soft3 = readRDS("Frida_Sofia_Rebsamen_22sepI.rds")
r_soft4 = readRDS("Frida_Sofia_Rebsamen_23sepI.rds")
r_soft5 = readRDS("Frida_Sofia_Rebsamen_24sepI.rds")

## funcion para generar woerd cloud
genera_word_cloud<-function(tweets,mf){
  r_stats=tweets
  print(length(r_stats))
  #Recupera texto
  r_stats_text <- sapply(r_stats, function(x) x$getText())
  r_stats_text2=str_replace_all(r_stats_text,"[^[:graph:]]", " ") 
  
  #crea corpus
  r_stats_text_corpus <- Corpus(VectorSource(r_stats_text2))


  ## limpieza  
  removeURL <- function(x) gsub("https://[[:alnum:]].*$", "", x)
  r_stats_text_corpusa <- tm_map(r_stats_text_corpus,
                                 content_transformer(removeURL)
                                 )
                                 
  
  r_stats_text_corpusb <- tm_map(r_stats_text_corpusa,
                                content_transformer(function(x) iconv(x, to='UTF-8', sub='byte'))
  )

### generacion de wordclouds
genera_word_cloud(r_soft1,1)
genera_word_cloud(r_soft2,1)
genera_word_cloud(r_soft3,1)
genera_word_cloud(r_soft4,1)

### obtencion de cifras retweetCount 
 sum(unlist(sapply(r_soft1, function(x) x$retweetCount)))
 sum(unlist(sapply(r_soft2, function(x) x$retweetCount)))
 sum(unlist(sapply(r_soft3, function(x) x$retweetCount)))
 sum(unlist(sapply(r_soft4, function(x) x$retweetCount)))

### obtencion de cifras favoriteCount 
 sum(unlist(sapply(r_soft1, function(x) x$favoriteCount)))
 sum(unlist(sapply(r_soft2, function(x) x$favoriteCount)))
 sum(unlist(sapply(r_soft3, function(x) x$favoriteCount)))
 sum(unlist(sapply(r_soft4, function(x) x$favoriteCount)))

### User name fuente
unlist(sapply(r_soft1, function(x) x$screenName))
unlist(sapply(r_soft2, function(x) x$screenName))
unlist(sapply(r_soft3, function(x) x$screenName))
unlist(sapply(r_soft4, function(x) x$screenName))

### Creacion del mapa de difusion
### Fuente en https://gist.github.com/dsparks/4329876


r_sofll <- searchTwitter('rebsamen sofia frida',since = '2017-09-20', until = '2017-09-21',  n=4500)
saveRDS(r_sofll,"Frida_Sofia_Rebsamen_20sep_latlon.rds")

tweetFrame <- twListToDF(r_sofll)  # Convert to a nice dF

userInfo <- lookupUsers(tweetFrame$screenName)  # Batch lookup of user info
userFrame <- twListToDF(userInfo)  # Convert to a nice dF

locatedUsers <- !is.na(userFrame$location)  # Keep only users with location info
sum(locatedUsers)

locations <- geocode(userFrame$location[locatedUsers])  # Use amazing API to guess
saveRDS(locations,"ubicaciones20sep.rds")
# approximate lat/lon from textual location data.
with(locations, plot(longitude, latitude))

worldMap <- map_data("world")  # Easiest way to grab a world map shapefile

zp1 <- ggplot(worldMap)
zp1 <- zp1 + geom_path(aes(x = long, y = lat, group = group),  # Draw map
                       colour = gray(2/3), lwd = 1/3)
zp1 <- zp1 + geom_point(data = locations,  # Add points indicating users
                        aes(x = longitude, y = latitude),
                        colour = "RED", alpha = 1/2, size = 1)
zp1 <- zp1 + coord_equal()  # Better projections are left for a future post
zp1 <- zp1 + theme_minimal()  # Drop background annotations
print(zp1)







