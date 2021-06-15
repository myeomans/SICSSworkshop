
############### Topic Models


# dfm<-as.dfm(ngramTokens(rev_small$text,ngrams=1,stop.words = FALSE))
# 
# topicMod<-stm(dfm,K=30)

# Note - you can save models as RDS files, too!
# saveRDS(topicMod,file="data/topicMod.RDS")


topicMod<-readRDS("data/topicMod.RDS")





############### Word Vectors








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

# Men also give lower reviews... interesting!
rev_small %>%
  with(summary(lm(stars~user_male)))
