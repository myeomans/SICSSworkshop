#################################################################################################
#
# Data Analysis Code for Conversational Receptiveness
#             Study 1
#           Mike Yeomans
#
##################################################################################################

# Load data
CRstudy1A<-read.csv("data/CRstudy1A.csv")
CRstudy1B<-read.csv("data/CRstudy1B.csv")


# Drop non-opposers
CRstudy1A<-CRstudy1A %>%
  filter(text_agree==0 & issue_pos!=4)

# calculate average ratings from ratersand merge into writer data

CRstudy1A <- CRstudy1A %>%
  left_join(CRstudy1B %>%
              group_by(targetID) %>%
              summarize(receptiveAll=mean(as.numeric(ownRating),na.rm=TRUE)),
            by=c("ResponseId"="targetID"))


# Effect of (weak) treatment effect on average rating of their response

CRstudy1A %>%
  with(summary(lm(scale(receptiveAll)~receptive)))

CRstudy1A %>% 
  group_by(receptive) %>%
  summarize(mean=mean(receptiveAll),
            sd=sd(receptiveAll))


# median of treatment group is in the XX percentile of control group
.median<-CRstudy1A %>%
  filter(receptive==1) %>%
  with(median(receptiveAll))

CRstudy1A %>% 
  filter(receptive==0) %>%
  with(mean(receptiveAll<=.median))


###############################################
# Other Linguistic Baselines
###############################################

simple.polite<-politeness(CRstudy1A$text,parser="none")

CRstudy1A <- CRstudy1A %>%
  mutate(wordcount=str_count(text,"[[:alpha:]]+"),
         sentiment=(simple.polite$Positive.Emotion-simple.polite$Negative.Emotion)/wordcount,
         emotion=(simple.polite$Positive.Emotion+simple.polite$Negative.Emotion)/wordcount)

CRstudy1A %>%
  with(list(sentiment=cor.test(receptiveAll,sentiment),
            wordcount=cor.test(receptiveAll,wordcount)))

### Moral Foundations Theory Tests
morFoDict<-quanteda::dictionary(file="data/MFD2.0.dic")

CRstudy1A <- CRstudy1A %>%
  left_join(quanteda::dfm(CRstudy1A$text, dictionary=morFoDict)%>%
              as_tibble() %>%
              mutate(ResponseId=CRstudy1A$ResponseId),
            by="ResponseId")%>%
  mutate(writer_cons=factor(ifelse(issue=="blm",issue_agree,1-issue_agree),
                            levels=c(0,1),labels=c("Liberal","Conservative")),
         consLibMFT=scale(care.virtue+care.vice+fairness.virtue+fairness.vice
                          -loyalty.virtue-loyalty.vice-authority.virtue-authority.vice
                          -sanctity.virtue-sanctity.vice), 
         targetMFT=ifelse(writer_cons=="Liberal",(-1)*consLibMFT,consLibMFT))

summary(lm(scale(receptiveAll)~scale(targetMFT),
           data=CRstudy1A))
confint(lm(scale(receptiveAll)~scale(targetMFT),
           data=CRstudy1A))

###############################################
# Cross-validation Results
###############################################

polite.data<-politeness(CRstudy1A$text,parser="spacy")

# Cross-Validation Prep

ngram.data<-CRstudy1A$text %>%
  doc2concrete::ngramTokens(ngrams = 1:3,sparse=.99)

DV<-(CRstudy1A$receptiveAll)

# This takes a while... the model is saved
# 
# CV.models<-list(ngrams=list(exes=ngram.data),
#                 politeness=list(exes=polite.data))
# 
# 
# cycles<-2 # repeats of the whole procedure to smooth out CV error
# 
# for(x in 1:length(CV.models)){
#   CV.models[[x]][["guesses"]]<-array(NA,c(length(DV),cycles))
#   CV.models[[x]][["coefs"]]<-NA
#   for(cycle in 1:cycles){
#     cycleModel<-politenessProjection(df_polite_train = CV.models[[x]]$exes,
#                                      covar=DV,
#                                      cv_folds=10)
#     CV.models[[x]][["guesses"]][,cycle]<-cycleModel$train_proj
#   }
#   CV.models[[x]][["guess"]]<-rowMeans(CV.models[[x]][["guesses"]],na.rm=TRUE)
#   CV.models[[x]][["coefs"]]<-cycleModel$train_coefs
# }
# saveRDS(CV.models,file="data/CVmodels.RDS")

# Load from memory
CV.models<-readRDS("data/CVmodels.RDS")

CRstudy1A$receptiveNLP<-CV.models[["politeness"]][["guess"]]
CRstudy1A$receptiveNLPngrams<-CV.models[["ngrams"]][["guess"]]


# Not a big difference in accuracy
CRstudy1A %>%
  with(cor.test(receptiveAll,receptiveNLP))

CRstudy1A %>%
  with(cor.test(receptiveAll,receptiveNLPngrams))

# Very different models

sort(CV.models[["politeness"]][["coefs"]])

# lots of topic-specific words here
sort(CV.models[["ngrams"]][["coefs"]])


######################################################################################################
# Does receptiveness model transfer across topics?
######################################################################################################

# store data as we go in this list
transfer_tests<-data.frame(train=c("blm","sa","blm","sa"),
                           test=c("sa","blm","sa","blm"),
                           features=c("polite","polite","ngrams","ngrams"),
                           r=NA,ci_l=NA,ci_u=NA)

blm.polite<-CRstudy1A %>%
  filter(issue=="blm") %>% 
  select(text) %>%
  politeness::politeness(parser="spacy")

blm.DV.rated<-CRstudy1A %>% 
  filter(issue=="blm") %>% 
  select(receptiveAll) %>%
  unlist() %>%
  as.numeric()

sa.polite<-CRstudy1A %>%
  filter(issue=="sa") %>% 
  select(text) %>%
  unlist() %>%
  politeness::politeness(parser="spacy")

sa.DV.rated<-CRstudy1A %>% 
  filter(issue=="sa") %>% 
  select(receptiveAll) %>%
  unlist() %>%
  as.numeric()

###############################################
#Estimate the transfer learning rate from BLM to SA

cordat<-cor.test(politenessProjection(blm.polite,
                                             blm.DV.rated,
                                             sa.polite)$test_proj,
                        sa.DV.rated)

transfer_tests[transfer_tests$train=="blm" & transfer_tests$test=="sa" & transfer_tests$features=="polite",
               c("r","ci_l","ci_u")]<-c(cordat$estimate,cordat$conf.int)

#Estimate the transfer learning rate from SA to BLM

cordat<-cor.test(politenessProjection(sa.polite,
                                             sa.DV.rated,
                                             blm.polite)$test_proj,
                        blm.DV.rated)

transfer_tests[transfer_tests$train=="sa" & transfer_tests$test=="blm" & transfer_tests$features=="polite",
               c("r","ci_l","ci_u")]<-c(cordat$estimate,cordat$conf.int)

##########d################################
# bag-of-words transfer learning
##########################################

blm.DV.rated<-CRstudy1A %>% 
  filter(issue=="blm") %>% 
  select(receptiveAll) %>%
  unlist() %>%
  as.numeric()

sa.DV.rated<-CRstudy1A %>% 
  filter(issue=="sa") %>% 
  select(receptiveAll) %>%
  unlist() %>%
  as.numeric()

# Train on BLM
blm.ng<-CRstudy1A %>%
  filter(issue=="blm") %>% 
  select(text) %>%
  unlist() %>%
  ngramTokens(ngrams = 1:3) %>%
  as_tibble()

# Test on SA (using ngrams from BLM training)

sa.ng<-CRstudy1A %>%
  filter(issue=="sa") %>% 
  select(text) %>%
  unlist() %>%
  ngramTokens(ngrams = 1:3, vocabmatch = blm.ng) %>%
  as_tibble()

# Estimate transfer learning rate
cordat<-cor.test(politenessProjection(blm.ng,
                                    blm.DV.rated,
                                    sa.ng)$test_proj,
               sa.DV.rated)


transfer_tests[transfer_tests$train=="blm" & transfer_tests$test=="sa" & transfer_tests$features=="ngrams",
               c("r","ci_l","ci_u")]<-c(cordat$estimate,cordat$conf.int)


# Train on SA

sa.ng<-CRstudy1A %>%
  filter(issue=="sa") %>% 
  select(text) %>%
  unlist() %>%
  ngramTokens(ngrams = 1:3) %>%
  as_tibble()

# Test on BLM (using ngrams from SA training)

blm.ng<-CRstudy1A %>%
  filter(issue=="blm") %>% 
  select(text) %>%
  unlist() %>%
  ngramTokens(ngrams = 1:3, vocabmatch = sa.ng) %>%
  as_tibble()

# Estimate transfer learning rate
cordat<-cor.test(politenessProjection(sa.ng,
                                    sa.DV.rated,
                                    blm.ng)$test_proj,
               blm.DV.rated)


transfer_tests[transfer_tests$train=="sa" & transfer_tests$test=="blm" & transfer_tests$features=="ngrams",
               c("r","ci_l","ci_u")]<-c(cordat$estimate,cordat$conf.int)

#### Compare results.....  add in overall model estimates
plotDat<-transfer_tests %>%
  bind_rows(CRstudy1A %>%
              group_by(issue) %>%
              summarize(r=cor.test(receptiveAll,receptiveNLP)$estimate,
                        ci_l=cor.test(receptiveAll,receptiveNLP)$conf.int[1],
                        ci_u=cor.test(receptiveAll,receptiveNLP)$conf.int[2]) %>%
              mutate(train=issue,test=issue,features="polite") %>%
              select(-issue) %>%
              ungroup()) %>%
  bind_rows(CRstudy1A %>%
              group_by(issue) %>%
              summarize(r=cor.test(receptiveAll,receptiveNLPngrams)$estimate,
                        ci_l=cor.test(receptiveAll,receptiveNLPngrams)$conf.int[1],
                        ci_u=cor.test(receptiveAll,receptiveNLPngrams)$conf.int[2]) %>%
              mutate(train=issue,test=issue,features="ngrams") %>%
              select(-issue) %>%
              ungroup())

#big drop off for ngrams, politeness is doing just as well!

plotDat %>%
  mutate(matched=ifelse(train==test,"All Issues","Held-Out Issue")) %>%
  mutate(fnames=ifelse(features=="polite","Politeness","N-Grams")) %>%
  ggplot(aes(x=test,group=matched,fill=matched,y=r,ymin=ci_l,ymax=ci_u)) +
  theme_bw() +
  geom_bar(stat="identity",position = "dodge") +
  geom_errorbar(width=.4,position=position_dodge(width=1))+
  facet_wrap(~fnames) +
  labs(x="Test Set Issue",y="Correlation with Human Raters",
       fill="Training Set Data") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20),
        strip.text = element_text(size=20),
        legend.text = element_text(size=16),
        legend.title = element_text(size=20),
        legend.position=c(0.3,0.3),
        legend.background = element_rect(colour ="black"),
        strip.background = element_rect(fill="white"))

ggsave("transferPlot.png",units="cm",width=30,height=20)

##############################################################################################
# Get a plot of the feature counts
##############################################################################################

train.polite<-CRstudy1A %>%
  select(text) %>%
  unlist() %>%
  politeness::politeness(parser="spacy")

train.DV<-CRstudy1A %>% 
  select(receptiveAll) %>%
  unlist() %>%
  as.numeric()

politenessPlot(train.polite,train.DV,
               split_levels=c("Unreceptive","Receptive"),
               split_cols = c("firebrick","skyblue"),
               split_name="Rater Consensus",
               middle_out=.05) +
  guides(fill = guide_legend(title.position = "left",nrow = 2,
                             title.hjust = .5)) +
  theme(text=element_text(family=""),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("figure1.png",units="cm",width=20,height=14)



##############################################################################################
# Grab some examples for the slides
##############################################################################################

CRstudy1A %>%
  filter(grepl("The public reaction has not been overblown",seedtext)) %>%
  with(findPoliteTexts(text, politeness(text,parser="spacy"),receptiveNLP,type="least"))

CRstudy1A %>%
  filter(grepl("The public reaction has not been overblown",seedtext)) %>%
  with(findPoliteTexts(text, politeness(text,parser="spacy"),receptiveNLP,type = "most"))


#################################################################################
# Compare to human accuracy using pairwise comparisons
#################################################################################

CRstudy1B$otherRating<-NA
tpb<-txtProgressBar(0,nrow(CRstudy1B))
for(.s in 1:nrow(CRstudy1B)){
  CRstudy1B[.s,]$otherRating<-CRstudy1B %>%
    filter((CRstudy1B$targetID==CRstudy1B[.s,]$targetID)
           &(ResponseId!=CRstudy1B[.s,]$ResponseId)) %>%
    select(ownRating) %>%
    unlist() %>%
    as.numeric() %>%
    mean()
  setTxtProgressBar(tpb,.s)
}

# move from one row per judgment to one row per judge
judgePairs<-left_join(CRstudy1B %>%
                        select(target, ownRating,ResponseId) %>%
                        spread(target, ownRating) %>%
                        rename(ownA="1", ownB="2"),
                      CRstudy1B %>%
                        select(target, otherRating,ResponseId) %>%
                        spread(target, otherRating) %>%
                        rename(otherA="1", otherB="2")) 

head(judgePairs)


judgePairs%>%
  mutate(ownChoose=1*(ownA>ownB), # which does the judge think is more receptive?
         otherChoose=1*(otherA>otherB),  # which do the other judges think is more receptive?
         acc=1*(ownChoose==otherChoose)) %>%
  summarize(m=mean(acc),se=sd(acc)/sqrt(n()),
            l=m-1.96*se,u=m+1.96*se)

#################################################################################
# Now join results from all the other models to the same data structure
#################################################################################

accData <- CRstudy1B %>%
  left_join(CRstudy1A %>%
              select(ResponseId,receptiveNLP,sentiment,wordcount,targetMFT),
            by=c("targetID"="ResponseId"))

left_join(accData %>%
            select(target, receptiveNLP,ResponseId) %>%
            spread(target, receptiveNLP) %>%
            rename(algoA="1", algoB="2"),
          accData %>%
            select(target, otherRating,ResponseId) %>%
            spread(target, otherRating) %>%
            rename(crowdA="1", crowdB="2"))%>%
  left_join(accData %>%
              select(target, ownRating,ResponseId) %>%
              spread(target, ownRating) %>%
              rename(humanA="1", humanB="2")) %>%
  left_join(accData %>%
              select(target, sentiment,ResponseId) %>%
              spread(target, sentiment) %>%
              rename(sentimentA="1", sentimentB="2")) %>%
  left_join(accData %>%
              select(target, wordcount,ResponseId) %>%
              spread(target, wordcount) %>%
              rename(wordcountA="1", wordcountB="2")) %>%
  left_join(accData %>%
              select(target, targetMFT,ResponseId) %>%
              spread(target, targetMFT) %>%
              rename(mftA="1", mftB="2"))%>%
  mutate(crowdChoose=(crowdA>crowdB),
         humanAcc=1*((humanA>humanB)==crowdChoose),
         sentimentAcc=1*((sentimentA>sentimentB)==crowdChoose),
         wordcountAcc=1*((wordcountA>wordcountB)==crowdChoose),
         mftAcc=1*((mftA>mftB)==crowdChoose),
         algoAcc=1*((algoA>algoB)==crowdChoose)
  ) %>%
  select(sentimentAcc,wordcountAcc,humanAcc,mftAcc,algoAcc) %>%
  # restructure data, calculate confidence intervals
  gather(predictor,acc) %>%
  group_by(predictor) %>%
  summarize(m=mean(acc),l=m-1.96*sd(acc)/sqrt(n()),u=m+1.96*sd(acc)/sqrt(n())) %>%
  
  # make a giant plot of it all
  mutate(m=as.numeric(m),u=as.numeric(u),l=as.numeric(l),
         predictor=factor(predictor,ordered=TRUE,
                          levels=c("mftAcc","sentimentAcc","wordcountAcc",
                                   "transferAcc","algoAcc","humanAcc"),
                          labels=c("Moral Foundations","Sentiment","Word Counts",
                                   "Out-of-Topic Algorithm",
                                   "Cross-Validated Algorithm","Human Judges"))) %>%
  ggplot(aes(x=predictor,y=m,ymin=l,ymax=u,fill=predictor)) +
  geom_bar(stat="identity") +
  coord_flip(ylim=c(.49,.7))+
  geom_hline(yintercept=.5)+
  geom_errorbar(width=0.1) +
  scale_y_continuous(breaks = seq(0,1,.1),position = "right",
                     labels=paste0(seq(0,100,10),"%"))+
  scale_fill_manual(values=c("cornflowerblue","dodgerblue1","skyblue",
                             "forestgreen","sienna2")) +
  theme_bw() +
  geom_hline(yintercept=0)+
  labs(x="",y="Pairwise Match with Average Judges")+
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size=24),
        axis.text.x = element_text(size=16,face="bold"))


#ggsave("accPlot.png",units="cm",width=30,height=20)

