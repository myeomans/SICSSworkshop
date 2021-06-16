
############### Topic Models

# First we use stm to estimate the topic model

# Note - you can save models as RDS files, too!
# 
# dfm<-as.dfm(ngramTokens(rev_small$text,ngrams=1,stop.words = FALSE))
# # #
#  topicMod20<-stm(dfm,K=20)
# #
#  topicMod30<-stm(dfm,K=30)
# 
#  saveRDS(topicMod20,file="data/topicMod20.RDS")
#  saveRDS(topicMod30,file="data/topicMod30.RDS")

# Two models - 20 and 30 topics (K is *very* hard to choose)

topicMod20<-readRDS("data/topicMod20.RDS")
#topicMod30<-readRDS("data/topicMod30.RDS")

######## Let's focus on the 20 topic model for now...

# Most common topics, and mst common words from each topic
plot(topicMod20,type="summary",n = 7,xlim = c(0,.3)) 

# We can also grab more words per topic
labelTopics(topicMod20)

# Estimate effects of topics and star rating
ee<-estimateEffect(1:20~stars,topicMod20,
                   meta= rev_small[,c("stars","user_male")])

# The default plotting function is bad... Here's another version
bind_rows(lapply(summary(ee)$tables,function(x) x[2,1:2])) %>%
  mutate(topic=factor(paste("Topic",1:20),ordered=T,
                      levels=paste("Topic",1:20)),
         se_u=Estimate+`Std. Error`,
         se_l=Estimate-`Std. Error`) %>%
  ggplot(aes(x=topic,y=Estimate,ymin=se_l,ymax=se_u)) +
  geom_point() + 
  geom_errorbar() +
  coord_flip() +
  geom_hline(yintercept = 0)+
  theme_bw()

# Which topics correlate with one another?
plot(topicCorr(topicMod20))


############### Word Vectors

# The real word vector files are ~ 6GB - too big! This is a smaller version,
# containing only the 50,000 most common words
vecSmall<-readRDS("data/vecSmall.RDS")

# one column with words, and 300 with vector projections (uninterpretable!)
head(vecSmall[,1:20])

# Some of my own home-brewed functions for vector calculation
source("vectorFunctions.R")

# Calculating similarity using bag of words doesn't know the difference between sad and happy!
bowSimCalc(x=c("I am very sad","I am very happy"),
           y="I am thrilled")

# However, processing the text as dense vectors allows the meaning to emerge. 
vecSimCalc(x=c("I am very sad","I am very happy"),
           y="I am thrilled",
           vecfile=vecSmall)


############### Politeness

# Politeness requires SpaCyR, which requires SpaCy

# run only once, on a new machine, to install
#spacyr::spacy_install()

# run every session to initialize
spacyr::spacy_initialize()


# Calculate politeness counts
# rev_polite<-politeness(rev_small$text,parser="spacy")
# saveRDS(rev_polite,file="data/rev_polite.RDS")

rev_polite<-readRDS("data/rev_polite.RDS")

obviousgender=(!is.na(rev_small$user_male))&(abs(rev_small$user_male-.5)>.4)

# Looks like a big difference in overall word count... 
politenessPlot(rev_polite %>%
                 filter(obviousgender),
               rev_small$user_male[obviousgender],
               middle_out=.05,
               drop_blank = 0,
               split_levels = c("Female","Male"))

# Confirmed... Men write shorter texts
rev_small %>%
  with(summary(lm(word_count~user_male)))

# A quick way to fix - divide every column by a user's word count!
rev_polite_av=as.data.frame(apply(rev_polite,2,function(x) mean(rev_small$word_count)*x/rev_small$word_count))

# Re-plot with averages
politenessPlot(rev_polite_av %>%
                 filter(obviousgender),
               rev_small$user_male[obviousgender],
               middle_out=.05,
               split_levels = c("Female","Male")) +
  # Note that politenessPlot can be customized like any normal ggplot with the + sign
  scale_y_continuous(name = "Feature Count per Average-length text",
                     breaks= c(.1,.5,1,2,5,10),
                     trans = "sqrt")

ggsave("genderreview.png")

# Men also give lower reviews... interesting!
rev_small %>%
  with(summary(lm(stars~user_male)))
