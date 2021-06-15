
############### Topic Models


dfm<-as.dfm(ngramTokens(rev_small$text,ngrams=1,stop.words = FALSE))

topicMod<-stm(dfm,K=30)

# Note - you can save models as RDS files, too!
# saveRDS(topicMod,file="data/topicMod.RDS")
# topicMod<-readRDS("data/topicMod.RDS")





############### Word Vectors








############### Politeness

# Calculate politeness counts
rev_polite<-politeness(rev_small$text,parser="spacy")

# Looks like a big difference in overall word count... 
politenessPlot(rev_polite %>%
                 filter(!is.na(rev_small$user_male)),
               rev_small$user_male[!is.na(rev_small$user_male)],
               middle_out=.05,
               drop_blank = 0,
               split_levels = c("Female","Male"))

# Confirmed... Men write shorter texts
rev_small %>%
  with(summary(lm(word_count~user_male)))

# A quick way to fix - divide every column by a user's word count!
rev_polite_av=as.data.frame(apply(rev_polite,2,function(x) x/rev_small$word_count))

# Re-plot with averages
politenessPlot(rev_polite_av %>%
                 filter(!is.na(rev_small$user_male)),
               rev_small$user_male[!is.na(rev_small$user_male)],
               middle_out=.05,
               drop_blank = 0,
               split_levels = c("Female","Male")) +
  # Note that politenessPlot can be customized like any normal ggplot with the + sign
  scale_y_continuous(name = "Feature Count/ Total Word Count",
                     breaks= c(.001,.004,.01,.04,.1),
                     trans = "sqrt")

# Men also give lower reviews... interesting!
rev_small %>%
  with(summary(lm(stars~user_male)))
