library(tidytext)
library(dplyr)
library(gutenbergr)
library(tidyr)
library(stringr)
library(ggplot2)
library(textdata)
#NRC:https://github.com/juliasilge/tidytext/issues/146
#library(remotes)
#install_github("EmilHvitfeldt/textdata")
#install_github("juliasilge/tidytext")
get_sentiments("nrc")
get_sentiments("bing")
get_sentiments("afinn")
MD<-gutenberg_download(2701,mirror=NULL)
#word frequecies
tidy_MD <- MD %>%
  mutate(
    linenumber=row_number(),
    chapter=cumsum(str_detect(text,regex("^chapter[\\divxlc]",ignore_case = TRUE)))
  )%>%
  ungroup()%>%
  unnest_tokens(word, text) %>% #convert it to tidy format
  anti_join(stop_words)

tidy_MD %>%
  count(word,sort = TRUE) #the most common words

#sentiment analysis with inner join
nrc_fear<-get_sentiments("nrc")%>% #for the fear words
  filter(sentiment=="fear")
tidy_MD %>% 
  inner_join(nrc_fear) %>% #perform the sentiment analysis
  count(word,sort = TRUE) #the most common fear words in MD

##count up how many positive and negative words there are in defined section
bing <- tidy_MD %>% 
  inner_join(get_sentiments("bing")) %>%
  count(chapter,index = linenumber %/% 100, sentiment) %>% #define index(integer division)to keep track of where we are in the narrative to 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)
ggplot(bing,aes(index,sentiment,fill=chapter))+geom_col(show.legend = FALSE)+facet_wrap(~chapter,ncol=2,scales="free_x")

afinn<-tidy_MD%>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index=linenumber%/%80)%>%
  summarise(sentiment=sum(value))%>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  tidy_MD %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  tidy_MD %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)
##plot the sentiment scores 
bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")
## look at how many positive and negative words are in these lexicons
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)


