#########
# This file does all the quanteda/wordfish analysis.
# We ran this in RStudio to easily save graphs. 

library(quanteda)
require(readtext)
require(topicmodels)

submissions_data <- readtext("delta_submissions.csv", text_field='selftext')
corpus_sub <- corpus(submissions_data)
dfmat_sub <-  dfm(corpus_sub,  remove=stopwords("english"), 
                  stem = FALSE, remove_punct = TRUE)

topfeatures(dfmat_sub, 20)
# create a dank wordcloud
set.seed(100)
textplot_wordcloud(dfmat_sub, min_count = 6, random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"))
# run a topicmodel
my_lda_fit20 <- LDA(convert(dfm2, to = "topicmodels"), k = 15)
get_terms(my_lda_fit20, 10)


# filter out data for categories
keyness_corpus <- corpus_subset(corpus_sub, 
                             category %in% c("Supports Trump", "Supports Clinton",
                                             "Other Candidate", "Other"))

dfm2 <-  dfm(keyness_corpus, groups="category", remove=stopwords("english"), 
                  stem = FALSE, remove_punct = TRUE)

# Check what order the documents appear in
dfm2[1:4, 1:5]

# draw some graphs from wordfish
# Note: in this case, "Supports Clinton" was document 3 and "Supports Trump" was document 4, so we know
# 3 is to the left of 4.
wf <- textmodel_wordfish(dfm2, dir=c(3, 4))
textplot_scale1d(wf, margin = "features",
                 highlighted = c("trump", "hillary", "vote", "fascist",
                                "emails", "sanders", "turnout", "bias",
                                "republican", "iraq", "america", "media", 'sexist'), 
                 highlighted_color = "red")


# We modified the below two lines to create all the charts with keywords.
result_keyness <- textstat_keyness(dfm2, target="Other")
textplot_keyness(result_keyness, n=15) 

textplot_scale1d(wf)
