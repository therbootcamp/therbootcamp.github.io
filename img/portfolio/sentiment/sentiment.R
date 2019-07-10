DIR = "~/Dropbox (2.0)/Work/Software/newpage/img/portfolio/sentiment/"

# 0. load packages
require(tidyverse)
require(xml2)
require(rvest)
require(tidytext)

# 1. get website urls
companies <- c(
  "Glencore", "Vitol", "Trafigura",
  "Mercuria_Energy_Group", "Nestlé",
  "Gunvor_(company)", "Hoffmann-La_Roche",
  "Novartis", "ABB_(company)", 
  "Coop_(Switzerland)", "Migros", 
  "LafargeHolcim")
urls <- paste0("https://en.wikipedia.org/wiki/", 
               companies)

# 2. read texts
texts <- map(urls, function(x){
  x %>% read_html() %>% 
  html_node("body") %>%
  html_text()})
texts<- tibble(company = companies, 
               text = unlist(texts)) 

# 3. get sentiments
sentiment <- texts %>% 
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(company) %>%
  summarize(sentiment = mean(score))

# 4. plot results
sentiment %>% 
  arrange(sentiment) %>%
  mutate(company_fact = factor(company, 
                               levels = company,
                               ordered = TRUE)) %>%
  ggplot(aes(x = company_fact, 
             y = sentiment,
             fill = sentiment)) +
  geom_bar(stat = "identity") + 
  theme_light() +
  theme(axis.text.x = 
          element_text(angle = 90, 
                       hjust = 1)) +
  theme(axis.title.x=element_blank(),
        legend.position = "none") +
  scale_fill_gradient(low = "red",
                      high = "green3")+
  labs(y = "Sentiment",
       title = "Sentiment of the 12 largest Swiss companies",
       subtitle = "accordingt to their English Wikipedia article")
  

a =   sentiment %>% 
  arrange(sentiment) %>%
  mutate(company_fact = factor(company, 
                               levels = company,
                               ordered = TRUE)) %>%
  ggplot(aes(x = company_fact, 
                 y = sentiment,
                 fill = sentiment)) +
  geom_bar(stat = "identity") + 
  theme_light() +
  theme(axis.text.x = 
          element_text(angle = 90, 
                       hjust = 1)) +
  theme(axis.title.x=element_blank(),
        legend.position = "none") +
  scale_fill_gradient(low = "red",
                      high = "green3") +
  labs(y = "Sentiment",
       title = "Sentiment of the 12 largest Swiss companies",
       subtitle = "accordingt to their English Wikipedia article")

ggsave(filename = "sentiment_plot.png",plot=a,device = 'png',width =6, height = 4,path = DIR,units = "in")

