# R script for analysing movie data
library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)

# set working directory at top level
setwd("~/Documents/Programming for Data Scientists/Assignment")

# readi n files from movie tweets
tweetRatings <- read.csv("ratings_mod.csv", header = F, stringsAsFactors = F)
lensRatings <- read.csv("u_mod.csv", header = F, stringsAsFactors = F)
tweetMovies <- read.csv("movies_mod.csv", header = F, stringsAsFactors = F)
lensMovies <- read.csv("movieLens_movie_mod.csv", header = F, stringsAsFactors = F)

# -------------------------------------------------------------------------#
# create new dataframe to analyse movies and their ratings -- twitter
# -------------------------------------------------------------------------#
movieRatings <- select(tweetRatings, V2, V3)
names(movieRatings)[1] <- "MovieID"
names(movieRatings)[2] <- "Rating"
movieRatings_group <- group_by(movieRatings, MovieID)
movieRatings_summary <- summarise(movieRatings_group,
                                  Rating = sum(Rating))

# -------------------------------------------------------------------------#
# create plot -- dist of rating per movie -- twitter
# -------------------------------------------------------------------------#
palette <- brewer.pal(8, name = "Dark2")
movieRate <- ggplot(movieRatings_summary) + geom_line(aes(x = MovieID, y = Rating, colour = Rating)) + ylab("Number of Ratings") + xlab("Movie ID") +
  ggtitle("Number of Ratings per Movie") + scale_colour_gradient(name = "# of Ratings", low = palette[5], high = palette[6]) +
  theme(legend.background = element_rect(fill = "white")) + theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_blank())
ggsave(movieRate, file = "movieRatingTwitter.png")

# -------------------------------------------------------------------------#
# analyse users and their ratings -- twitter
# -------------------------------------------------------------------------#
userRatings <- select(tweetRatings, V1)
names(userRatings) <- c("UserID")
userRatings_group <- count(userRatings, UserID)

# -------------------------------------------------------------------------#
# create plot to analyse users and their ratings -- twitter
# -------------------------------------------------------------------------#
palette <- brewer.pal(8, name = "Accent")
userRate <- ggplot(userRatings_group) + geom_line(aes(x = UserID, y = n, colour = n)) + scale_colour_gradient(low = palette[5], high = palette[6], name = "# of Ratings") +
  xlab("User") + ylab("Number of Ratings") + ggtitle("Number of Ratings per User") + theme(legend.position = "top") +
  theme(legend.background = element_rect(fill = "white")) + theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_blank())
ggsave(userRate, file = "userRatingsTwitter.png")

# -------------------------------------------------------------------------#
# analyse ratings over time -- twitter
# -------------------------------------------------------------------------#
timeRatings <- select(tweetRatings, V3,V4)
names(timeRatings) <- c("Rating", "Date")
origin <- ymd("1970-01-01")
timeRatings$Date <- origin + timeRatings$Date
timeRatings$year_day <- yday(timeRatings$Date)
timeRatings$month <- month(timeRatings$Date)
timeRatings_group <- group_by(timeRatings, year_day)
timeRatings_summary <- summarise(timeRatings_group,
                                 Rating = sum(Rating),
                                 month = min(month))

# -------------------------------------------------------------------------#
# create plot to analyse ratings over time for twitter
# -------------------------------------------------------------------------#
palette <- brewer.pal(11, name="PiYG")
tweetTime <- ggplot(timeRatings_summary) + geom_line(aes(x = year_day, y = Rating, colour = month)) +
  scale_colour_gradient(low = palette[1], high = palette[11], breaks = c(2,4,6,8), labels = c("Feb", "Apr", "Jun", "Aug")) +
  xlab("Day of Year") + ylab("Number of Ratings") + ggtitle("Number of Ratings over Time") + theme(legend.position = "top") +
  theme(legend.background = element_rect(fill = "white")) + theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_blank())
ggsave(tweetTime, file = "timeRatingTwitter.png")

# -------------------------------------------------------------------------#
# anakyse average rating over time -- twitter
# -------------------------------------------------------------------------#
avgTimeRatings <- select(tweetRatings, V3, V4)
names(avgTimeRatings) <- c("Rating", "Date")
avgTimeRatings$Date <- origin + avgTimeRatings$Date
avgTimeRatings$year_day <- yday(avgTimeRatings$Date)
avgTimeRatings$month <- month(avgTimeRatings$Date)
avgTimeRatings_group <- group_by(avgTimeRatings, year_day)
avgTimeRatings_summary <- summarise(avgTimeRatings_group,
                      Rating = mean(Rating),
                      Month = mean(month))

# -------------------------------------------------------------------------#
# plot avg rating over time -- twitter
# -------------------------------------------------------------------------#
palette <- brewer.pal(11, name = "RdYlGn")
avgRate <- ggplot(avgTimeRatings_summary) + geom_line(aes(x = year_day, y = Rating, colour = Month)) +
  scale_colour_gradient(low = palette[1], high = palette[11], breaks = c(2,4,6,8), labels = c("Feb", "Apr", "Jun", "Aug")) +
  xlab("Day of Year") + ylab("Average Rating") + ggtitle("Average Ratings over each Day") + theme(legend.position = "top") +
  theme(legend.background = element_rect(fill = "white")) + theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_blank())
ggsave(avgRate, file = "avgRatingsTwitter.png")

# -------------------------------------------------------------------------#
# anakyse movies and their rating -- movielens
# -------------------------------------------------------------------------#
lensMovieRatings <- select(lensRatings, V2,V3)
names(lensMovieRatings) <- c("MovieID", "Rating")
lensmovie_group <- group_by(lensMovieRatings, MovieID)
lensmovie_summary <- summarise(lensmovie_group,
                           Rating = sum(Rating))
# -------------------------------------------------------------------------#
# create plot dist of ratings per movie -- movielens
# -------------------------------------------------------------------------#
palette <- brewer.pal(3, name = "PRGn")
lensMoviesPlot <- ggplot(lensmovie_summary) + geom_point(aes(x = MovieID, y = Rating, colour = Rating)) +
  scale_colour_gradient(low = palette[1], high = palette[3]) +
  xlab("MovieID") + ylab("Number of Ratings") + ggtitle("Distribution of Ratings per Movie -- MovieLens") + theme(legend.position = "right") +
  theme(legend.background = element_rect(fill = "white")) + theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_blank()) + theme(panel.grid.major = element_blank())
ggsave(lensMoviesPlot, file = "lensMovieRatings.png")

# -------------------------------------------------------------------------#
# analyse ratings per user -- movielens
# -------------------------------------------------------------------------#
lensUserRatings <- select(lensRatings, V1, V3)
names(lensUserRatings) = c("UserID", "Rating")
lensuser_group <- group_by(lensUserRatings, UserID)
lensuser_summary <- summarise(lensuser_group,
                          Rating = sum(Rating))

# -------------------------------------------------------------------------#
# create plot dist of ratings per user -- movielens
# -------------------------------------------------------------------------#
palette <- brewer.pal(3, name = "Spectral")
lensUserPlot <- ggplot(lensuser_summary) + geom_line(aes(x = UserID, y = Rating, colour = Rating)) +
  scale_colour_gradient(low = palette[1], high = palette[3]) +
  xlab("UserID") + ylab("Number of Ratings") + ggtitle("Distribution of Ratings per User -- MovieLens") + theme(legend.position = "right") +
  theme(legend.background = element_rect(fill = "white")) + theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor.x = element_blank()) + theme(panel.grid.major = element_blank())
ggsave(lensUserPlot, file = "lensUserRatings.png")

# -------------------------------------------------------------------------#
# analyse ratings over time -- movielens
# -------------------------------------------------------------------------#
lensTimeRatings <- select(lensRatings, V3,V4)
names(lensTimeRatings) <- c("Rating", "Date")
lensTimeRatings$Date <- origin + lensTimeRatings$Date
lensTimeRatings$Date <- round_date(lensTimeRatings$Date, "day")
lensTimeRatings$month <- month(lensTimeRatings$Date)
lensTime_group <- group_by(lensTimeRatings, Date)
lensTime_summary <- summarise(lensTime_group,
                              Rating = sum(Rating),
                              month = min(month))

# -------------------------------------------------------------------------#
# create plot to show ratings over time -- movielens
# -------------------------------------------------------------------------#
palette = brewer.pal(9, name = "Blues")
lensTime_plot <- ggplot(lensTime_summary) + geom_line(aes(x = Date, y = Rating, colour = month)) +
  scale_colour_gradient(low = palette[3], high = palette[9], breaks = c(3, 6, 9, 12), labels = c("Apr", "Sept", "Nov", "Jan")) +
  xlab("Month") + ylab("Number of Ratings") + ggtitle("Distribution of Ratings over Time -- MovieLens") + theme(legend.position = "top") +
  theme(legend.background = element_rect(fill = "white")) + theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_blank()) + theme(panel.grid.major = element_blank())
ggsave(lensTime_plot, file = "lensTimeRatings.png")

# -------------------------------------------------------------------------#
# analyse avg ratings over time -- movielens
# -------------------------------------------------------------------------#
lensTimeAve_summary <- summarise(lensTime_group,
                                 Rating = mean(Rating),
                                 Month = min(month))

# -------------------------------------------------------------------------#
# create plot to show avg ratings over time -- movielens
# -------------------------------------------------------------------------#
palette <- brewer.pal(9, name = "YlOrRd")
lensAvgTime <- ggplot(lensTimeAve_summary) + geom_line(aes(x = Date, y = Rating, colour = Month)) +
  scale_colour_gradient(low = palette[1], high = palette[9], breaks = c(3, 6, 9, 12), labels = c("Apr", "Sept", "Nov", "Jan")) +
  xlab("Month") + ylab("Average Rating") + ggtitle("Average Ratings over Time -- MovieLens") + theme(legend.position = "top") +
  theme(legend.background = element_rect(fill = "white")) + theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_blank()) + theme(panel.grid.major = element_blank())
ggsave(lensAvgTime, file = "lensAvgTimeRatings.png")

# -------------------------------------------------------------------------#
# analyse top 5 and bottom 5 -- movietweets
# -------------------------------------------------------------------------#
temp <- select(tweetRatings, V2, V3)
names(temp) <- c("MovieID", "Rating")
temp2 <- select(tweetMovies, V1, V2)
names(temp2) <- c("MovieID", "Title")
temp2$MovieID <- as.numeric(temp2$MovieID)
temp3 <- merge(temp, temp2, by.x = "MovieID", by.y = "MovieID")
temp4 <- group_by(temp3, MovieID)
temp5 <- summarise(temp4,
                   count = n(),
                   Rating = sum(Rating))
tweetMov <- filter(temp5, count > 25)
tweetMov <- arrange(tweetMov, Rating)
bot5 <- filter(temp4, MovieID == 1935896 | MovieID == 1480656 | MovieID == 1132449 | MovieID == 1905040 | MovieID == 1413495)
top5 <- filter(temp4, MovieID == 1300854 | MovieID == 770828 | MovieID == 1408101 | MovieID == 816711 | MovieID == 1670345)
top5 <- distinct(top5, Title)
bot5 <- distinct(bot5, Title)

# -------------------------------------------------------------------------#
# analyse top 5 and bottom 5 -- movielens
# -------------------------------------------------------------------------#
temp <- select(lensMovies, V1, V2)
names(temp) <- c("MovieID", "Title")
temp2 <- select(lensRatings, V2, V3)
names(temp2) <- c("MovieID", "Rating")
temp3 <- merge(temp,temp2, by.x = "MovieID", by.y = "MovieID")
temp4 <- group_by(temp3, MovieID)
temp5 <- summarise(temp4,
                   count = n(),
                   Rating = sum(Rating))
lensMov <- filter(temp5, count > 25)
lensMov <- arrange(lensMov, Rating)
lens_bot5 <- filter(temp4, MovieID == 457 | MovieID == 398 | MovieID == 352 | MovieID == 564 | MovieID == 1215)
lens_bot5 <- distinct(lens_bot5, Title)
lens_top5 <- filter(temp4, MovieID == 50 | MovieID == 100 | MovieID == 181 | MovieID == 258 | MovieID == 174)
lens_top5 <- distinct(lens_top5, Title)

# -------------------------------------------------------------------------#
# analyse genre ratings -- twitter
# -------------------------------------------------------------------------#
genreList <- select(tweetMovies, V1, V3:V6)
ratings <- select(tweetRatings, V2:V4)
names(genreList) <- c("MovieID", "Genre1", "Genre2", "Genre3")
names(ratings) <- c("MovieID", "Rating", "Date")
ratingsGenre <- merge(ratings, genreList, by.x = "MovieID", by.y = "MovieID")
ratingsGenre$Date <- origin + ratingsGenre$Date
ratingsGenre$year_day <- yday(ratingsGenre$Date)
ratingsGenre$month <- month(ratingsGenre$Date)
genre_group <- group_by(ratingsGenre, year_day, Genre1, Genre2, Genre3)
genre_summary <- summarise(genre_group,
                           Rating = mean(Rating))
keep <- c("Crime", "Comedy", "Drama", "Short", "Biography", "Action", "Documentary", "Adventure", "Romance", "Musical", "Horror", "Mystery", "Animation", "Sci-Fi", "Western", "Thriller", "Fantasy", "War", "Film-Noir","Family", "History", "Adult", "Music", "Sport")
genre_summary <- genre_summary[genre_summary$Genre1 %in% keep, ]
# -------------------------------------------------------------------------#
# plot genre ratings -- twitter
# -------------------------------------------------------------------------#
twitterGenre <- qplot(x = year_day, y = Rating, data = genre_summary, geom = "line") +
  facet_wrap(~Genre1, ncol = 5)
ggsave(twitterGenre, file = "twitterGenreRatings.png")

# -------------------------------------------------------------------------#
# analyse genre ratings -- movielens
# -------------------------------------------------------------------------#
lensList <- select(lensMovies, V1, V6:V24)
names(lensList) <- c("UserID", "unknown", "Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
lensGenreRatings <- select(lensRatings, V1, V3, V4)
names(lensGenreRatings) <- c("UserID", "Rating", "Date")
lensGenreRatings$Date <- origin + lensGenreRatings$Date
lensGenreRatings$Date <- round_date(lensGenreRatings$Date, "day")
lensGenreRatings$month <- month(lensGenreRatings$Date)
lensRatingGenres <- merge(lensList, lensGenreRatings, by.x = "UserID", by.y = "UserID")
lensGenre_unknown <- group_by(lensRatingGenres, Date, unknown)
lensGenre_unknown <- summarise(lensGenre_unknown,
                               Rating = mean(Rating),
                               month = first(month))
lensGenre_action <- group_by(lensRatingGenres, Date, Action)
lensGenre_action <- summarise(lensGenre_action,
                               Rating = mean(Rating),
                               month = first(month))
lensGenre_adventure <- group_by(lensRatingGenres, Date, Adventure)
lensGenre_adventure <- summarise(lensGenre_adventure,
                              Rating = mean(Rating),
                              month = first(month))
lensGenre_animation <- group_by(lensRatingGenres, Date, Animation)
lensGenre_animation <- summarise(lensGenre_animation,
                                 Rating = mean(Rating),
                                 month = first(month))
lensGenre_children <- group_by(lensRatingGenres, Date, Children)
lensGenre_children <- summarise(lensGenre_children,
                                 Rating = mean(Rating),
                                 month = first(month))
lensGenre_comedy <- group_by(lensRatingGenres, Date, Comedy)
lensGenre_comedy <- summarise(lensGenre_comedy,
                                Rating = mean(Rating),
                                month = first(month))
lensGenre_crime <- group_by(lensRatingGenres, Date, Crime)
lensGenre_crime <- summarise(lensGenre_crime,
                              Rating = mean(Rating),
                              month = first(month))
lensGenre_documentary <- group_by(lensRatingGenres, Date, Documentary)
lensGenre_documentary <- summarise(lensGenre_documentary,
                             Rating = mean(Rating),
                             month = first(month))
lensGenre_drama <- group_by(lensRatingGenres, Date, Drama)
lensGenre_drama <- summarise(lensGenre_drama,
                             Rating = mean(Rating),
                             month = first(month))
lensGenre_fantasy <- group_by(lensRatingGenres, Date, Fantasy)
lensGenre_fantasy <- summarise(lensGenre_fantasy,
                             Rating = mean(Rating),
                             month = first(month))
lensGenre_filmnoir <- group_by(lensRatingGenres, Date, FilmNoir)
lensGenre_filmnoir <- summarise(lensGenre_filmnoir,
                             Rating = mean(Rating),
                             month = first(month))
lensGenre_horror <- group_by(lensRatingGenres, Date, Horror)
lensGenre_horror <- summarise(lensGenre_horror,
                             Rating = mean(Rating),
                             month = first(month))
lensGenre_musical <- group_by(lensRatingGenres, Date, Musical)
lensGenre_musical <- summarise(lensGenre_musical,
                             Rating = mean(Rating),
                             month = first(month))
lensGenre_mystery <- group_by(lensRatingGenres, Date, Mystery)
lensGenre_mystery <- summarise(lensGenre_mystery,
                             Rating = mean(Rating),
                             month = first(month))
lensGenre_romance <- group_by(lensRatingGenres, Date, Romance)
lensGenre_romance <- summarise(lensGenre_romance,
                             Rating = mean(Rating),
                             month = first(month))
lensGenre_thriller <- group_by(lensRatingGenres, Date, Thriller)
lensGenre_thriller <- summarise(lensGenre_thriller,
                             Rating = mean(Rating),
                             month = first(month))
lensGenre_scifi <- group_by(lensRatingGenres, Date, SciFi)
lensGenre_scifi <- summarise(lensGenre_scifi,
                             Rating = mean(Rating),
                             month = first(month))
lensGenre_war <- group_by(lensRatingGenres, Date, War)
lensGenre_war <- summarise(lensGenre_war,
                             Rating = mean(Rating),
                             month = first(month))
lensGenre_western <- group_by(lensRatingGenres, Date, Western)
lensGenre_western <- summarise(lensGenre_western,
                             Rating = mean(Rating),
                             month = first(month))
lenGenreName <- c("unknown", "Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
# -------------------------------------------------------------------------#
# plot genre ratings -- movielens
# -------------------------------------------------------------------------#
# create  afunction to plot each genre using ggplot2 library and save to file
plotRatings <- function(data, plot, title) {
  require(ggplot2)
  palette <- brewer.pal(9, name = "Set1")
  plot <- ggplot(data, aes(Date, Rating)) + geom_line(aes(colour = month)) +
    scale_colour_gradient(low = palette[1], high = palette[9],  breaks = c(3, 6, 9, 12), labels = c("Apr", "Sept", "Nov", "Jan")) +
    xlab("Date") + ylab("Average Rating") + ggtitle(paste("Average Ratings for Genre --", title, sep = " ")) + theme(legend.position = "top") +
    theme(legend.background = element_rect(fill = "white")) + theme(panel.background = element_rect(fill = "grey")) +
    theme(panel.grid.minor.x = element_blank()) + theme(panel.grid.major = element_blank())
  ggsave(file = paste("lensRating", title, ".png", sep = ""))
}

# plot each genre
plotRatings(lensGenre_unknown, lenGenreName[1], lenGenreName[1])
plotRatings(lensGenre_action, lenGenreName[2], lenGenreName[2])
plotRatings(lensGenre_adventure, lenGenreName[3], lenGenreName[3])
plotRatings(lensGenre_animation, lenGenreName[4], lenGenreName[4])
plotRatings(lensGenre_children, lenGenreName[5], lenGenreName[5])
plotRatings(lensGenre_comedy, lenGenreName[6], lenGenreName[6])
plotRatings(lensGenre_crime, lenGenreName[7], lenGenreName[7])
plotRatings(lensGenre_documentary, lenGenreName[8], lenGenreName[8])
plotRatings(lensGenre_drama, lenGenreName[9], lenGenreName[9])
plotRatings(lensGenre_fantasy, lenGenreName[10], lenGenreName[10])
plotRatings(lensGenre_filmnoir, lenGenreName[11], lenGenreName[11])
plotRatings(lensGenre_horror, lenGenreName[12], lenGenreName[12])
plotRatings(lensGenre_musical, lenGenreName[13], lenGenreName[13])
plotRatings(lensGenre_mystery, lenGenreName[14], lenGenreName[14])
plotRatings(lensGenre_romance, lenGenreName[15], lenGenreName[15])
plotRatings(lensGenre_scifi, lenGenreName[16], lenGenreName[16])
plotRatings(lensGenre_thriller, lenGenreName[17], lenGenreName[17])
plotRatings(lensGenre_war, lenGenreName[18], lenGenreName[18])
plotRatings(lensGenre_western, lenGenreName[19], lenGenreName[19])
