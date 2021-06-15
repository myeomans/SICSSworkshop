
############### Topic Models

# There are way too many examples for us... Let's trim it down
# This is the only city I've lived in from the data (no London, alas!)
# Let's make it even smaller this time, as these algorithms are more complex (i.e. slower)
bus_tiny<-busses %>%
  filter(city=="Cambridge" & RestaurantsPriceRange2==2)
rev_tiny <- reviews %>%
  filter(business_id%in%bus_tiny$business_id)

# Save data as we go to save time (always)
saveRDS(rev_tiny,file="data/rev_tiny.RDS")
saveRDS(bus_tiny,file="data/bus_tiny.RDS")

# Get rid of the bid dataset for now, to save resources
rm(busses,reviews)


dfm<-as.dfm(ngramTokens(rev_tiny$text,ngrams=1,stop.words = FALSE))

topicMod<-stm(dfm,K=30)

# Note - you can save models as RDS files, too!
# saveRDS(topicMod,file="data/topicMod.RDS")
# topicMod<-readRDS("data/topicMod.RDS")





############### Word Vectors








############### Politeness

# Calculate politeness counts
rev_polite<-politeness(rev_tiny$text,parser="spacy")

# Looks like a big difference in overall word count... 
politenessPlot(rev_polite %>%
                 filter(!is.na(rev_tiny$user_male)),
               rev_tiny$user_male[!is.na(rev_tiny$user_male)],
               middle_out=.05,
               drop_blank = 0,
               split_levels = c("Female","Male"))

# Confirmed... Men write shorter texts
rev_tiny %>%
  with(summary(lm(word_count~user_male)))

# A quick way to fix - divide every column by a user's word count!
rev_polite_av=as.data.frame(apply(rev_polite,2,function(x) x/rev_tiny$word_count))

# Re-plot with averages
politenessPlot(rev_polite_av %>%
                 filter(!is.na(rev_tiny$user_male)),
               rev_tiny$user_male[!is.na(rev_tiny$user_male)],
               middle_out=.05,
               drop_blank = 0,
               split_levels = c("Female","Male")) +
  # Note that politenessPlot can be customized like any normal ggplot with the + sign
  scale_y_continuous(name = "Feature Count/ Total Word Count",
                     breaks= c(.001,.004,.01,.04,.1),
                     trans = "sqrt")

# Men also give lower reviews... interesting!
rev_tiny %>%
  with(summary(lm(stars~user_male)))
