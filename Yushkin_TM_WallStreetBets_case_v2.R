# Title: Case II - WallStreetBets Gamestop Analysis
# NAME: Alexey Yushkin
# Date: Mar 9 2021
# Note: Some chunks of the code are commented out because they take too much time to run.
#       Instead, the script uses pre-processed data stored in csv files. 


# Setting working directory
setwd("~/Downloads/TEXT_ANALYTICS_AND_NLP/Hult_NLP/personal/session II/WallStreetBets")

# Turn off scientific notation
options(scipen = 999)


#################### Importing libraries and data #################### 

# Uploading libraries
library(tm)
library(tidyverse)
library(ggplot2)
library(rtweet)
library(mgsub)
library(lubridate)
library(reshape2)
library(ggthemes)
library(imputeTS)


## Loading data
# Database of posts
txt <- read.csv("CASE_gme.csv", header = TRUE)

# Database of prices
price <- read.csv("gme_HLOC.csv", header = TRUE)
# Selecting the colum we are interested in
price <- select(price, date, GME.Volume, GME.Close)
# Transforming type of date column 
price$date <- as.Date(price$date)


#################### Creating custom functions #################### 

# Function for changing case to lower
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

# Function for text cleaning
clean_text <- function(text, stopwords){
  # Replacing emojis which are missing in emojis module with text
  text <- gsub("=\xa8", " police cars revolving light ", text)
  text <- gsub("=\x80", " rocket ", text)
  text <- gsub(">\xfb>2<\xfb", " paper hands ", text)
  text <- gsub("=%", "fire", text)
  text <- gsub("=\xb8", " money with wings ", text)
  text <- gsub("<\xf3\017\n<\b=;", " gay bear ", text)
  text <- gsub(">\x84", " magic wand ", text)
  text <- gsub(">2<\xfb=\x8e", " diamond hands ", text)
  text <- gsub(">u", " hot face ", text)
  text <- gsub("\v<\xfb=\x8e>\032<\xfb", " diamond hands ", text)
  text <- gsub("<\b=;", " gay bear ", text)
  text <- gsub("=L=\x8e", " diamond hands ", text)
  text <- gsub(">\032<\xfc=\x8e", " diamond hands ", text)
  text <- gsub(">\x8d", " gorilla ", text)
  text <- gsub("=\xa6", " sweat droplets ", text)
  text <- gsub("=\x8e=L<\xfc", " diamond hands ", text)
  text <- gsub("=\x8e =P", " diamond hands ", text)
  # Encoding fix
  text <- stringi::stri_encode(text, "", "UTF-8")
  # Replacing emojis with text
  text <- mgsub(text, emojis$code, paste0(" ", emojis$description," "))
  # Replacing some punctuation codes
  text <- gsub("\023", "'", text)
  text <- gsub("\024", "-", text)
  text <- gsub("\030", "'", text)
  text <- gsub("\031", "'", text)
  text <- gsub("\034", '"', text)
  text <- gsub("\035", '"', text)
  # Removing URLs
  text <- qdapRegex::rm_url(text)
  # Making lower case
  text <- tryTolower(text)
  # Removing punctuation
  text <- removePunctuation(text)
  # Removing numbers
  text <- removeNumbers(text)
  # Removing stop words
  text <- removeWords(text, stopwords)
  # Removing unknown emojis
  text <- mgsub(text, "�", "")
  # Removing extra whitespace
  text <- stripWhitespace(text)
  # Removing leading and trailing whitespace
  text <- trimws(text, which = "both")
  return(text)
}

# Function for creating word frequency matrix 
wfm <- function(txt_subset, control = list()){
  # Creating corpus
  corp <- VCorpus(VectorSource(txt_subset))
  # Creating DTM
  dtm <- as.matrix(DocumentTermMatrix(corp, 
                                      control = control))
  # Creating WFM
  wfm <- colSums(dtm)
  return(wfm)
}

# Function for transforming WFM into dataframe 
wfm_df <- function(wfm){
  # Creating dataframe
  wfm_df <- data.frame(word = names(wfm), freq = wfm)
  # Removing indexes
  rownames(wfm_df) <- NULL
  # Sorting words by frequency
  wfm_df <- wfm_df[order(wfm_df$freq, decreasing = TRUE),]
  # Returning dataframe with WFM
  return(wfm_df)
}

# Bigram token maker
bigram_tokens <- function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# Trigram token maker
trigram_tokens <- function(x){
  unlist(lapply(NLP::ngrams(words(x), 3), paste, collapse = " "), 
         use.names = FALSE)
}

# Function for plotting word frequency vs price
plot_word_freq_price <- function(df_plot, plot_title = "", 
                                 vis_start_date = "2019-06-02", vis_end_date = "2021-02-02",
                                 date_line_1 = "", date_line_2 = "",
                                 date_line_3 = "", date_line_4 = "",
                                 date_line_5 = "", date_line_6 = ""){
  # Preparing data for visualization
  # plot_data <- melt(subset(df_plot, select = c(Date, Price, Volume, Term_Freq)), id.var = "Date")
  plot_data <- melt(subset(df_plot, select = c(Date, Price, Term_Freq)), id.var = "Date")
  # Imputing missing data using linear interpolation
  plot_data_imputed <- data.frame(Date = plot_data$Date, 
                                  variable = plot_data$variable, 
                                  value = na_interpolation(plot_data$value, option = "linear"))
  # Creating visualization
  plot_ <- ggplot(plot_data, aes(x = Date, y = value, color = variable)) + 
    geom_point() +
    geom_line() +
    geom_line(data = plot_data_imputed, linetype = "dashed") +
    scale_color_brewer(palette = "Dark2") +
    facet_grid(variable ~ ., scales = "free_y") + 
    scale_y_continuous(trans = "log10") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 16),
          panel.grid.major.x = element_line(size = 0.05, color = "black"),
          panel.grid.major.y = element_line(size = 0.05, color = "black"),
          panel.grid.minor.x = element_line(size = 0.05, color = "black"),
          panel.grid.minor.y = element_blank(),
          text = element_text(size = 14),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text.y = element_text(size = 15, angle = 90)) +
    ylab("Values") +
    scale_x_continuous(minor_breaks = seq(as.Date(vis_start_date), as.Date(vis_end_date), 1),
                       breaks = seq(as.Date(vis_start_date), as.Date(vis_end_date), 
                                    round(dim(df_plot)[1] / 6, 0))) +
    ggtitle(plot_title) + 
    geom_vline(aes(xintercept = as.Date(date_line_1)), color = "green", alpha = 0.5) +
    geom_vline(aes(xintercept = as.Date(date_line_2)), color = "red", alpha = 0.5) +
    geom_vline(aes(xintercept = as.Date(date_line_3)), color = "green", alpha = 0.5) +
    geom_vline(aes(xintercept = as.Date(date_line_4)), color = "red", alpha = 0.5) +
    geom_vline(aes(xintercept = as.Date(date_line_5)), color = "green", alpha = 0.5) +
    geom_vline(aes(xintercept = as.Date(date_line_6)), color = "red", alpha = 0.5)
  # Saving the plot 
  ggsave(filename = paste(plot_title, ".png", sep = ""), 
         plot = plot_, 
         width = 17, height = 8)
  # Printing the plot
  plot(plot_)
}

# Function for plotting polarity vs price
plot_polarity_price <- function(plot_data, plot_title = "",
                                vis_start_date = "2019-06-02", vis_end_date = "2021-02-02",
                                date_line_1 = "", date_line_2 = "",
                                date_line_3 = "", date_line_4 = "",
                                date_line_5 = "", date_line_6 = ""){
  # Imputing missing data using linear interpolation
  plot_data_imputed <- data.frame(Date = plot_data$Date, 
                                  variable = plot_data$variable, 
                                  value = na_interpolation(plot_data$value, option = "linear"))
  # Creating visualization
  plot_ <- ggplot(plot_data, aes(x = Date, y = value, color = variable)) + 
    geom_point() + 
    geom_line() +
    geom_line(data = plot_data_imputed, linetype = "dashed") +
    scale_color_brewer(palette = "Dark2") +
    facet_grid(variable ~ ., scales = "free_y") + 
    scale_y_continuous(trans = "log10") +
    theme_minimal() +
    theme(legend.position = "none", 
          plot.title = element_text(hjust = 0.5, size = 16),
          panel.grid.major.x = element_line(size = 0.05, color = "black"),
          panel.grid.major.y = element_line(size = 0.05, color = "black"),
          panel.grid.minor.x = element_line(size = 0.05, color = "black"),
          panel.grid.minor.y = element_blank(),
          text = element_text(size = 14),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text.y = element_text(size = 15, angle = 90)) +
    ylab("Values") +
    scale_x_continuous(minor_breaks = seq(as.Date(vis_start_date), as.Date(vis_end_date), 1), 
                       breaks = seq(as.Date(vis_start_date), as.Date(vis_end_date), 
                                    round(dim(plot_data)[1] / 24, 0))) +
    ggtitle(plot_title) + 
    geom_vline(aes(xintercept = as.Date(date_line_1)), color = "green", alpha = 0.5) +
    geom_vline(aes(xintercept = as.Date(date_line_2)), color = "red", alpha = 0.5) +
    geom_vline(aes(xintercept = as.Date(date_line_3)), color = "green", alpha = 0.5) +
    geom_vline(aes(xintercept = as.Date(date_line_4)), color = "red", alpha = 0.5) +
    geom_vline(aes(xintercept = as.Date(date_line_5)), color = "green", alpha = 0.5) +
    geom_vline(aes(xintercept = as.Date(date_line_6)), color = "red", alpha = 0.5)
  # Saving the plot 
  ggsave(filename = paste(plot_title, ".png", sep = ""), 
         plot = plot_, 
         width = 17, height = 8)
  # Printing the plot
  plot(plot_)
}


#################### EDA and Cleaning of non-text data #################### 

# Checking the basic statistics
summary(txt)

# Checking dimensions, column names, types of data and the first records
glimpse(txt)

# Time range
min(txt$post_date)
max(txt$comm_date)

# id column has wrong values - maximum id is 498 despite the fact that the dataset has 18,940 rows
# so we can remove this column...
txt$id <- NULL

# ... and create a new one
txt <- txt %>% 
  mutate(doc_id = row_number())


# After checking unknown symbols (emojis) which process is left out of scope of this script, 
# but the result has been taken into account in the clean_text function, column link 
# is not needed any more
txt$link <- NULL


# Columns post_date_weekday and comm_date_weekday contain wrong values because 
# despite the fact that, for instance, in 2021 posts and comments have been made during 13 and 14 days
# accordingly, post_date_weekday and comm_date_weekday contain only ones
# All post weekdays
table(txt$post_date_weekday)

# All comment weekdays
table(txt$comm_date_weekday)

txt %>% 
  filter(post_date >= "2021-01-01") %>% 
  select(post_date_weekday) %>% 
  unique()

txt %>% 
  filter(comm_date >= "2021-01-01") %>% 
  select(comm_date_weekday) %>% 
  unique()

# Transforming data type for dates columns
txt$post_date <- as.Date(txt$post_date)
txt$comm_date <- as.Date(txt$comm_date)

# Defining the correct weekdays
txt$post_date_weekday <- wday(txt$post_date)
txt$comm_date_weekday <- wday(txt$comm_date)

# Checking the result
txt %>% 
  filter(comm_date >= "2021-01-01") %>% 
  select(comm_date_weekday) %>% 
  unique()


# Checking for duplicates
duplicates <- duplicated(txt)
sum(duplicates) 
# no duplicates


# Checking the structure column
unique(txt$structure)
# All the posts numbers
post_numbers <- str_extract(txt$structure, "[0-9]+")
# Posts without comments
(posts_wo_comments_ids <- names(which(table(post_numbers) == 1)))
# Posts "365" "366" "367" "368" "369" "370" "371" "372" "373" "374" "375" "376" "377" "378" "379" 
# don't have any comments, and actually they are not posts, but the comments, so they are numerated 
# in the wrong way

# The dataset also contains observations with the same values in the structure column, 
# but rest of the variables are different, for example
txt %>% 
  filter(structure == "247") %>% 
  select(structure, comm_date, title)

# So the structure column is irrelevant and can be deleted
txt$structure <- NULL


#################### Cleaning and preparing text data #################### 

# Checking the number of unique titles and posts
length(unique(txt$title))
length(unique(txt$post_text))


# Creating a variable with stopwords
stops <- stopwords("SMART")


# Creating a dataframe with unique titles
title_unique <- txt %>%
  group_by(title) %>%
  select(title) %>%
  filter(row_number() == 1)

# Renaming titles columns
names(title_unique)[names(title_unique) == "title"] <- "title_old"
names(txt)[names(txt) == "title"] <- "title_old"

# Cleaning up titles
title_unique$title <- clean_text(title_unique$title_old, stops)

# Joining cleaned titles to the main dataframe
txt <- left_join(txt, title_unique, by = c("title_old" = "title_old"))

# Deleting the old column of titles
txt$title_old <- NULL


# # Creating dataframe with unique posts
# post_text_unique <- txt %>%
#   group_by(title) %>%
#   select(title, post_text) %>%
#   filter(row_number() == 1)
# 
# # Cleaning up posts
# post_text_unique$post_text <- clean_text(post_text_unique$post_text, stops)
# 
# # Joining cleaned posts to the main dataframe
# txt <- left_join(txt, post_text_unique, by = c("title" = "title"))
# 
# # Deleting the old column of posts
# txt$post_text.x <- NULL
# 
# # Renaming the column with cleaned posts
# names(txt)[names(txt) == "post_text.y"] <- "post_text"
# 
# 
# # Cleaning up comments
# txt$comment <- clean_text(txt$comment, stops)
# 
# 
# # Creating a column with all text data
# txt$text <- paste(txt$comment, txt$title, txt$post_text)
# 
# # Changing the columns order
# txt <- txt %>%
#   select(doc_id, text, comment, title, post_text, upvote_prop, post_score, everything())
# 
# 
# # Writing the result into the file
# write.csv(txt, "clean_dataset_new.csv", row.names = FALSE)


# Reading the dataframe from the file
txt <- read.csv("clean_dataset_new.csv", header = TRUE)

# Transforming data type for dates columns
txt$post_date <- as.Date(txt$post_date)
txt$comm_date <- as.Date(txt$comm_date)

# Counting quantities of terms "gamestop" and "gme"
txt$gamestop_gme <- as.numeric(sapply(c("gamestop|gme"), function(i) str_count(txt$text, i)))

# Selecting only texts that mention "gamestop" and/or "gme"
txt <- filter(txt, gamestop_gme > 0)

# Resetting documents ids
txt <- txt %>%
  mutate(doc_id = row_number())


#################### Word frequencies #################### 

# Unigrams for all text
control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))
# Creating WFM 
wfm_1 <- wfm(txt$text, control = control)
# Creating WFM dataframe
wfm_df_1 <- wfm_df(wfm_1)
# Top words
(wfm_df_1 <- wfm_df_1[1:20,])


# Unigrams for comments only
control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))
# Creating WFM 
wfm_1 <- wfm(txt$comment, control = control)
# Creating WFM dataframe
wfm_df_1 <- wfm_df(wfm_1)
# Top words
(wfm_df_1 <- wfm_df_1[1:20,])


# Extracting posts and comments that have been made before the first spike
txt <- txt %>% 
  filter(comm_date < "2021-01-20")


# Bigrams
control = list(tokenize = bigram_tokens,
               weighting = function(x) weightTfIdf(x, normalize = FALSE))
# Creating WFM
wfm_2 <- wfm(txt$text, control = control)
# Creating WFM dataframe
wfm_df_2 <- wfm_df(wfm_2)
# Top words
(wfm_df_2 <- wfm_df_2[1:20,])


# Trigrams
control = list(tokenize = trigram_tokens,
               weighting = function(x) weightTfIdf(x, normalize = FALSE))
# Creating WFM
wfm_3 <- wfm(txt$comment, control = control)
# Creating WFM dataframe
wfm_df_3 <- wfm_df(wfm_3)
# Top words
(wfm_df_3 <- wfm_df_3[1:15,])


#################### Associations #################### 

# Reading the dataframe from the file
txt <- read.csv("clean_dataset_new.csv", header = TRUE)

# Transforming data type for dates columns
txt$post_date <- as.Date(txt$post_date)
txt$comm_date <- as.Date(txt$comm_date)

# Counting quantities of terms "gamestop" and "gme"
txt$gamestop <- as.numeric(sapply("gamestop", function(i) str_count(txt$text, i)))
txt$gme <- as.numeric(sapply(c("gme"), function(i) str_count(txt$text, i)))


# Creating corpus and TDM
txt_corpus <- VCorpus(VectorSource(txt$text))
tdm  <- TermDocumentMatrix(txt_corpus)

# Reducing TDM
(reduced_tdm <- removeSparseTerms(tdm, sparse = 0.8))

# Finding word associations
associations <- findAssocs(reduced_tdm, "gamestop", 0.2) 

# Organizing the word associations
assoc_df <- data.frame(terms = names(associations[[1]]),
                       value = unlist(associations))
assoc_df$terms <- factor(assoc_df$terms, levels = rev(assoc_df$terms))
rownames(assoc_df) <- NULL

# Making a dot plot
plot_ <- ggplot(assoc_df, aes(y = terms)) +
  geom_point(aes(x = value), color = "red") +
  geom_text(aes(x = value, label = value), color = "red", hjust = 0.5, vjust = 2, size = 5) +
  scale_x_continuous(breaks = seq(0.18, 0.5, 0.02)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        text = element_text(size = 14),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  ggtitle("Associations") +
  xlab("Value") +
  ylab("Terms") +
  theme_hc()

# Saving the plot 
ggsave(filename = "Associations.png", 
       plot = plot_, 
       width = 17, height = 8)

# Printing the plot
plot(plot_)


#################### Checking relationships between word frequencies and price #################### 

# Defining constants for visualization
first_wave_start_date = "2021-01-20"
first_wave_end_date = "2021-01-22" 
second_wave_start_date = "2021-01-25"
second_wave_end_date = "2021-01-27" 
third_wave_start_date = "2021-01-28"
third_wave_end_date = "2021-01-29" 
vis_start_date = "2021-01-03"
vis_end_date = "2021-02-02"


# Counting quantities of the most frequent terms
txt$shorts <- as.numeric(sapply("shorts", function(i) str_count(txt$comment, i)))
txt$share <- as.numeric(sapply("share", function(i) str_count(txt$comment, i)))

# Creating dataframe with dates
data <- data.frame(comm_date = (seq(as.Date(min(txt$post_date)), as.Date(max(txt$comm_date)), by = "day")))

# Joining text data
data <- data %>% 
  left_join(txt, by = c("comm_date" = "comm_date")) %>% 
  select(comm_date, text, comment, upvote_prop, post_score, gamestop, gme, shorts, share)

# Replacing missing frequencies with zeros and missing text information with empty strings
data$gamestop[is.na(data$gamestop)] <- 0
data$gme[is.na(data$gme)] <- 0
data$shorts[is.na(data$shorts)] <- 0
data$share[is.na(data$share)] <- 0
data$text[is.na(data$text)] <- ""
data$comment[is.na(data$comment)] <- ""

# Creating s column for joint frequencies
data$Terms_Freq = data$shorts + data$share + data$gamestop

# Joining market data
data <- data %>% 
  left_join(price, by = c("comm_date" = "date"))


## Term "gamestop"
# Creating dataframe for visualization
df_plot <- data %>%
  select(comm_date, gamestop, GME.Close, GME.Volume) %>%
  filter(comm_date >= vis_start_date & comm_date <= vis_end_date) %>% 
  group_by(comm_date) %>%
  rename(Date = comm_date) %>% 
  transmute(Price = GME.Close, Volume = GME.Volume, Term_Freq = sum(gamestop)) %>% 
  filter(row_number() == 1)

# Calling the plotting function
plot_word_freq_price(df_plot = df_plot,
                     plot_title = 'Price and Term "gamestop" Frequency',
                     vis_start_date = vis_start_date, vis_end_date = vis_end_date,
                     date_line_1 = first_wave_start_date, date_line_2 = first_wave_end_date,
                     date_line_3 = second_wave_start_date, date_line_4 = second_wave_end_date,
                     date_line_5 = third_wave_start_date, date_line_6 = third_wave_end_date
                     )

## Term "gme"
# Creating dataframe for visualization
df_plot <- data %>%
  select(comm_date, gme, GME.Close, GME.Volume) %>%
  filter(comm_date >= vis_start_date & comm_date <= vis_end_date) %>% 
  group_by(comm_date) %>%
  rename(Date = comm_date) %>% 
  transmute(Term_Freq = sum(gme), Price = GME.Close, Volume = GME.Volume) %>% 
  filter(row_number() == 1)

# Calling the plotting function
plot_word_freq_price(df_plot = df_plot,
                     plot_title = 'Price and Term "gme" Frequency',
                     vis_start_date = vis_start_date, vis_end_date = vis_end_date,
                     date_line_1 = first_wave_start_date, date_line_2 = first_wave_end_date,
                     date_line_3 = second_wave_start_date, date_line_4 = second_wave_end_date
                     )

## Term "share"
# Creating dataframe for visualization
df_plot <- data %>%
  select(comm_date, share, GME.Close, GME.Volume) %>%
  filter(comm_date >= vis_start_date & comm_date <= vis_end_date) %>% 
  group_by(comm_date) %>%
  rename(Date = comm_date) %>% 
  transmute(Price = GME.Close, Volume = GME.Volume, Term_Freq = sum(share)) %>% 
  filter(row_number() == 1)

# Calling the plotting function
plot_word_freq_price(df_plot = df_plot,
                     plot_title = 'Price and Term "share" Frequency',
                     vis_start_date = vis_start_date, vis_end_date = vis_end_date,
                     date_line_1 = first_wave_start_date, date_line_2 = first_wave_end_date,
                     date_line_3 = second_wave_start_date, date_line_4 = second_wave_end_date,
                     date_line_5 = third_wave_start_date, date_line_6 = third_wave_end_date
                     )

## Term "shorts"
# Creating dataframe for visualization
df_plot <- data %>%
  select(comm_date, shorts, GME.Close, GME.Volume) %>%
  filter(comm_date >= vis_start_date & comm_date <= vis_end_date) %>% 
  group_by(comm_date) %>%
  rename(Date = comm_date) %>% 
  transmute(Term_Freq = sum(shorts), Price = GME.Close, Volume = GME.Volume) %>% 
  filter(row_number() == 1) 

# Calling the plotting function
plot_word_freq_price(df_plot = df_plot,
                     plot_title = 'Price and Term "shorts" Frequency',
                     vis_start_date = vis_start_date, vis_end_date = vis_end_date,
                     date_line_1 = first_wave_start_date, date_line_2 = first_wave_end_date,
                     date_line_3 = second_wave_start_date, date_line_4 = second_wave_end_date,
                     date_line_5 = third_wave_start_date, date_line_6 = third_wave_end_date
                     )

## Terms "shorts", "share", and "gamestop" jointly
# Creating dataframe for visualization
df_plot <- data %>%
  select(comm_date, Terms_Freq, GME.Close, GME.Volume) %>%
  filter(comm_date >= vis_start_date & comm_date <= vis_end_date) %>% 
  group_by(comm_date) %>%
  rename(Date = comm_date) %>% 
  transmute(Term_Freq = sum(Terms_Freq), Price = GME.Close, Volume = GME.Volume) %>% 
  filter(row_number() == 1)

# Calling the plotting function
plot_word_freq_price(df_plot = df_plot,
                     plot_title = 'Price and Terms "shorts", "share", and "gamestop" Joint Frequency',
                     vis_start_date = vis_start_date, vis_end_date = vis_end_date,
                     date_line_1 = first_wave_start_date, date_line_2 = first_wave_end_date,
                     date_line_3 = second_wave_start_date, date_line_4 = second_wave_end_date,
                     date_line_5 = third_wave_start_date, date_line_6 = third_wave_end_date
                     )


#################### Polarity #################### 

# # Transforming data type for dates columns
# data$comm_date <- as.Date(data$comm_date)
# 
# 
# # Getting the polarity for each comment
# pol_doc <- qdap::polarity(as.character(data$comment))
# 
# # Organizing the temporal and polarity info
# time_pol <- data.frame(pol = pol_doc$all$polarity,
#                        day = data$comm_date)
# 
# # Writing the result into the file
# write.csv(time_pol, "polarity_comm_total_new.csv", row.names = FALSE)
# 
# 
# # Getting the polarity for each text
# pol_doc <- qdap::polarity(as.character(data$text))
# 
# # Organizing the temporal and polarity info
# time_pol <- data.frame(pol = pol_doc$all$polarity,
#                        day = data$comm_date)
# 
# # Writing the result into the file
# write.csv(time_pol, "polarity_text_total_new.csv", row.names = FALSE)


# Reading the dataframe from the file
time_pol <- read.csv("polarity_comm_total_new.csv", header = TRUE)

# Combining txt dataframe and polarity column
data <- cbind(data, time_pol$pol)

# Renaming the polarity column
names(data)[names(data) == "time_pol$pol"] <- "Comments_Polarity"


# Reading the dataframe from the file
time_pol <- read.csv("polarity_text_total_new.csv", header = TRUE)

# Combining txt dataframe and polarity column
data <- cbind(data, time_pol$pol)

# Renaming the polarity column
names(data)[names(data) == "time_pol$pol"] <- "All_Text_Polarity"


# Taking into account upvote_prop 
# (works the same way as upvote_prop and post_score jointly do and better than 
# only post_score does)
data$Adjusted_Comments_Polarity <- data$Comments_Polarity * data$upvote_prop
data$Adjusted_All_Text_Polarity <- data$All_Text_Polarity * data$upvote_prop


# Day by day averages
day_pol_comm <- aggregate(Comments_Polarity ~ comm_date, data, mean)
day_pol_text <- aggregate(All_Text_Polarity ~ comm_date, data, mean)
adj_day_pol_comm <- aggregate(Adjusted_Comments_Polarity ~ comm_date, data, mean)
adj_day_pol_text <- aggregate(Adjusted_All_Text_Polarity ~ comm_date, data, mean)

# Day by day sum
day_terms_sum <- aggregate(Terms_Freq ~ comm_date, data, sum)


# Creating dataframe with dates
day_pol <- data.frame(Date = seq(as.Date(min(data$comm_date)), as.Date(max(data$comm_date)), by = "day"))

# Adding text polarity into the dataframe
day_pol <- day_pol %>% 
  left_join(day_pol_text, by = c("Date" = "comm_date"))

# Adding comment polarity into the dataframe
day_pol <- day_pol %>% 
  left_join(day_pol_comm, by = c("Date" = "comm_date"))

# Adding adjusted text polarity into the dataframe
day_pol <- day_pol %>% 
  left_join(adj_day_pol_text, by = c("Date" = "comm_date"))

# Adding adjusted comment polarity into the dataframe
day_pol <- day_pol %>% 
  left_join(adj_day_pol_comm, by = c("Date" = "comm_date"))

# Adding terms frequency into the dataframe
day_pol <- day_pol %>% 
  left_join(day_terms_sum, by = c("Date" = "comm_date"))

# Adding prices to the dataframe
day_pol <- day_pol %>%
  left_join(price, by = c("Date" = "date"))


# Renaming columns for visualization
names(day_pol)[names(day_pol) == "GME.Volume"] <- "Volume"
names(day_pol)[names(day_pol) == "GME.Close"] <- "Price"


# Adding 10 to the polarity scores to plot on log10 scale
day_pol$All_Text_Polarity = day_pol$All_Text_Polarity + 10
day_pol$Comments_Polarity = day_pol$Comments_Polarity + 10
day_pol$Adjusted_All_Text_Polarity = day_pol$Adjusted_All_Text_Polarity + 10
day_pol$Adjusted_Comments_Polarity = day_pol$Adjusted_Comments_Polarity + 10


# Subsetting the dataset for visualization by the date range
start_date <- "2021-01-11"
end_date <- "2021-02-02"
pol_vis <- day_pol %>% 
  filter(Date >= start_date & Date <= end_date)

# Preparing data for visualization
plot_data <- melt(subset(pol_vis, select = c(Date, Price, Volume, All_Text_Polarity, Comments_Polarity)), 
                  id.var = "Date")

# Calling the visualization function
plot_polarity_price(plot_data = plot_data, 
                    plot_title = "Price, Volume, All Text Polarity, and Comment Polarity",
                    date_line_1 = "2021-01-20", date_line_2 = "2021-01-22",
                    date_line_3 = "2021-01-25", date_line_4 = "2021-01-27",
                    date_line_5 = "2021-01-28", date_line_6 = "2021-01-29",
                    vis_start_date = start_date, vis_end_date = end_date)


# Subsetting the dataset for visualization by the date range
start_date <- "2021-01-11"
end_date <- "2021-02-02"
pol_vis <- day_pol %>% 
  filter(Date >= start_date & Date <= end_date)

# Preparing data for visualization
plot_data <- melt(subset(pol_vis, select = c(Date, Price, Volume, Adjusted_All_Text_Polarity, 
                                             Adjusted_Comments_Polarity)), 
                  id.var = "Date")

# Calling the visualization function (doesn't work better than before adjustment)
plot_polarity_price(plot_data = plot_data, 
                    plot_title = "Price, Volume, Adjusted All Text Polarity, and Adjusted Comment Polarity",
                    date_line_1 = "2021-01-20", date_line_2 = "2021-01-22",
                    date_line_3 = "2021-01-25", date_line_4 = "2021-01-27",
                    date_line_5 = "2021-01-28", date_line_6 = "2021-01-29",
                    vis_start_date = start_date, vis_end_date = end_date)


# Subsetting the dataset for visualization by the date range
start_date <- "2021-01-14"
end_date <- "2021-01-23"
pol_vis <- day_pol %>% 
  filter(Date >= start_date & Date <= end_date)

# Preparing data for visualization
plot_data <- melt(subset(pol_vis, select = c(Date, Price, Comments_Polarity)), id.var = "Date")

# Calling the visualization function
plot_polarity_price(plot_data = plot_data, plot_title = "Price and Comments Polarity I",
                    date_line_1 = "2021-01-20", date_line_2 = "2021-01-22",
                    vis_start_date = start_date, vis_end_date = end_date)


# Subsetting the dataset for visualization by the date range
start_date <- "2021-01-22"
end_date <- "2021-01-29"
pol_vis <- day_pol %>% 
  filter(Date >= start_date & Date <= end_date)

# Preparing data for visualization
plot_data <- melt(subset(pol_vis, select = c(Date, Price, Comments_Polarity)), id.var = "Date")

# Calling the visualization function
plot_polarity_price(plot_data = plot_data, plot_title = "Price and Comments Polarity II",
                    date_line_3 = "2021-01-25", date_line_4 = "2021-01-27",
                    vis_start_date = start_date, vis_end_date = end_date)


#################### Summary Chart #################### 

# Subsetting the dataset for visualization by the date range
start_date <- "2021-01-11"
end_date <- "2021-02-02"
pol_vis <- day_pol %>% 
  filter(Date >= start_date & Date <= end_date)

# Preparing data for visualization
plot_data <- melt(subset(pol_vis, select = c(Date, Price, Volume, Terms_Freq, Comments_Polarity)), 
                  id.var = "Date")

# Calling the visualization function
plot_polarity_price(plot_data = plot_data, 
                    plot_title = "Price, Volume, Terms Frequency, and Comment Polarity I",
                    date_line_1 = "2021-01-20", date_line_2 = "2021-01-22",
                    date_line_3 = "2021-01-25", date_line_4 = "2021-01-27",
                    date_line_5 = "2021-01-28", date_line_6 = "2021-01-29",
                    vis_start_date = start_date, vis_end_date = end_date)


# Subsetting the dataset for visualization by the date range
start_date <- "2021-01-11"
end_date <- "2021-01-25"
pol_vis <- day_pol %>% 
  filter(Date >= start_date & Date <= end_date)

# Preparing data for visualization
plot_data <- melt(subset(pol_vis, select = c(Date, Price, Volume, Terms_Freq, Comments_Polarity)), 
                  id.var = "Date")

# Calling the visualization function
plot_polarity_price(plot_data = plot_data, 
                    plot_title = "Price, Volume, Terms Frequency, and Comment Polarity II",
                    date_line_1 = "2021-01-20", date_line_2 = "2021-01-22",
                    vis_start_date = start_date, vis_end_date = end_date)


# Subsetting the dataset for visualization by the date range
start_date <- "2021-01-22"
end_date <- "2021-02-02"
pol_vis <- day_pol %>% 
  filter(Date >= start_date & Date <= end_date)

# Preparing data for visualization
plot_data <- melt(subset(pol_vis, select = c(Date, Price, Volume, Terms_Freq, Comments_Polarity)), 
                  id.var = "Date")

# Calling the visualization function
plot_polarity_price(plot_data = plot_data, 
                    plot_title = "Price, Volume, Terms Frequency, and Comment Polarity III",
                    date_line_3 = "2021-01-25", date_line_4 = "2021-01-27",
                    date_line_5 = "2021-01-28", date_line_6 = "2021-01-29",
                    vis_start_date = start_date, vis_end_date = end_date)

# End