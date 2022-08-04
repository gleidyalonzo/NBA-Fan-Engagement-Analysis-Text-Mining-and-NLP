#NBA Fan Engagement
#Gleidy Rodriguez
#Course: Text Analytics

#Calling Libraries necesari or text mining

#Setting option
options(stringsAsFactors=F)
Sys.setlocale('LC_ALL','C')

#setting working direcotry
setwd("/cloud/project/Case/Case I/Data")

library(dplyr)
library(readr)
library(qdap)
library(tm)
library(stringi)
library(stringr)
library(spelling)
library("ggplot2")
#Combining all csv files into one corpus. This code source
#Stackoverflow:https://stackoverflow.com/questions/30242065/trying-to-merge-multiple-csv-files-in-r#:~:text=To%20combine%20many%20CSV%20files%3A%20Set%20your%20file,ldply%20%28list.files%20%28%29%2C%20read.csv%2C%20header%3DTRUE%2C%20sep%3D%27t%27%29%20View%20%28veri%29

tweets <- read.csv("A_Oct2019.csv")


#UDF wrapper for basic cleaning operations
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Write a function accepting a text column
# use gsub subsituting 'http\\S+\\s*' for '' which removes URLS
# use gsub substituting '(RT|via)((?:\\b\\W*@\\w+)+)' for '' which removes "RT" exactly
# use tolower in the function on the text
# return the changed text
basicSubs <- function(x){
  x <- mgsub('http\\S+\\s*', '',x,ignore.case = TRUE)
  y <- mgsub('\\bRT\\b','',x)
  z <- tolower(x)
  return(x)
}

#eliminating urls and rt
tweets$text <- basicSubs(tweets$text)
tweets$text <-  tryTolower(tweets$text)

#spellcheck
tweets$text <- spell_check_text(tweets$text, lang = "en_US")

#checking how many twees contain most popular players full name
grep('lebron james',tweets$text,ignore.case = T)
tweets$text[1179]

grep('steph curry',tweets$text,ignore.case = T)

grep('damian lillard',tweets$text,ignore.case = T)

grep('james harden',tweets$text,ignore.case = T)

grep('giannis antetokounmpo',tweets$text,ignore.case = T)

grep('kawhi leonard',tweets$text,ignore.case = T)

#changing full name patters to last name/first name only 
patterns <- c('lebron james','steph curry','damian lillard','james harden','giannis antetokounmpo','kawhi leonard')
replacements <- c('lebron','curry','lillard','harden','giannis','kawhi')
tweets$text <-mgsub(patterns,replacements,tweets$text )

#checking tweet that had steph curry and see if it change
tweets$text[6812]

#creating an index where the name patter of each paler exists
lebron_idx <- grep('lebron',tweets$text,ignore.case = T)
curry_idx <-grep('curry',tweets$text,ignore.case = T)
lillard_idx <- grep('lillard',tweets$text,ignore.case = T)
harden_idx <- grep('harden',tweets$text,ignore.case = T)
giannis_idx <-grep('giannis',tweets$text,ignore.case = T)
kawhi_idx <-grep('kawhi',tweets$text,ignore.case = T)

#####Individual Players Frequency Analysis

# Use sum with stri_count on the newt txt object
# with lebron, lillard, harden, giannis, curry, kawhi

lebronJ  <- sum(stri_count(tweets$text, fixed ='lebron'))
damianL  <- sum(stri_count(tweets$text, fixed ='lillard'))
stephC   <- sum(stri_count(tweets$text, regex = "harden"))
jamesH   <- sum(stri_count(tweets$text, regex = "curry"))
giannisA <- sum(stri_count(tweets$text, regex = "giannis"))
kawhiL <- sum(stri_count(tweets$text, regex = "kawhi"))

# Organize term objects into a data frame
players_Freq <- data.frame(players = c('lebron','damian','steph','james','gianni','kawhi'),
                           freq  = c(lebronJ,damianL,stephC,jamesH,giannisA,kawhiL))

# Examine
player_Freq

# vizualizing results

ggplot(players_Freq, aes(x = reorder(players, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() +  theme(legend.position = "none")


#####Nike and Adidas Players Frequency Analysis

# creating a vector with the nike and adidas players
nike_players <- sum(lebronJ,giannisA,kawhiL)
adida_players<- sum(stephC,damianL,jamesH)
#creating a dataframe that containst the sum of patterns
nike_vs_adidas_freq <-data.frame(brand = c('nike','adidas'),
                                 freq  = c(nike_players,adida_players))

# Examine
nike_vs_adidas_freq

#vizualizing results
ggplot(nike_vs_adidas_freq, aes(x = reorder(brand, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() +  theme(legend.position = "none")

######Frequnecy Analysis Laker and Miami Heat 

#creating an index where the name patter of each paler exists
laker_idx <- grep('lakers',tweets$text,ignore.case = T)
miamiH_idx <-grep('miami heat',tweets$text,ignore.case = T)

# Use sum with stri_count on the newt txt object
# with lakers and miami heat
lakers <- sum(stri_count(tweets$text, fixed ='lakers'))
miami_heat <- sum(stri_count(tweets$text, fixed ='miami heat'))
#creating a dataframe that containst the sum of patterns
laker_vs_miamiHeat_freq <-data.frame(team = c('Lakers','Miami Heat'),
                                     freq  = c(lakers,miami_heat))
#vizualizing resuls
ggplot(laker_vs_miamiHeat_freq , aes(x = reorder(team, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() +  theme(legend.position = "none")

####Polarity Analysis

##Nike and Adidas Player

##created a vector list that contain all text where players name patter exist per brand sposored
nike_text <- c(tweets$text[lebron_idx],tweets$text[giannis_idx],tweets$text[kawhi_idx])
adidas_text <- c(tweets$text[lillard_idx],tweets$text[curry_idx],tweets$text[harden_idx])

## polarity on adidas and nikes players
nike_po <- polarity(nike_text)
adidas_po <- polarity(adidas_text)

##Most Popular Players
#created a vector that contain all text related to lebron james and kawhi leonard
lebron_text <- c(tweets$text[lebron_idx])
kawhi_text <- c(tweets$text[kawhi_idx])

#polarity analysis on lebron and kawhi
lebron_po <- polarity(lebron_text)
kawhi_po <- polarity(kawhi_text)

##Laker and Miami Heat
#created a vector that contain all text related to lakers and miami heat
lakers_text <-c(tweets$text[laker_idx])
miamiH_text <-c(tweets$text[miamiH_idx])

laker_po <- polarity(lakers_text)
miamiH_po <- polarity(miamiH_text)

#end

