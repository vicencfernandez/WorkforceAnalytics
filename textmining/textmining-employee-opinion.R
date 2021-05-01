#' ---
#' title: "Workforce Analytics: Text Mining on Employees' Opinion"
#' author: "Vicenc Fernandez"
#' date: "4/12/2021"
#' output: 
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     number_sections: true
#' ---
#' 
## ----setup, include=FALSE----------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' # The context and the data set
#' 
#' We have a data set with 149 positive and negative employees' opinions on how they feel about the organization where they are working. The data set contains three variables:  
#' 
#' * commentID: An integer variable with the opinion ID
#' * comment: A character variable with the employee opinion about their organization
#' * assessment: A factor variable indicating if the opinion is positive or negative 
#' 
#' The activity's objective is to predict the kind of opinion (positive or negative) based on the employee comment. In this report, the analysis strategy focuses on two criteria: 
#' 
#' 1. The term frequency (tf), and 
#' 2. The term frequency & the inverse document frequency (tf-idf)
#' 
#' # Packages for the analysis and reporting
#' 
#' We need to install and load several packages to analyze our data set with text mining.
#' 
## ---- message=FALSE----------------------------------------------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(tidytext)
library(textrecipes)

#' 
#' The `tidyverse` is an opinionated collection of R packages designed for data science. The `tidymodels` framework is a collection of packages for modeling and machine learning using tidyverse principles. The `tidytext` is a package that make many text mining tasks easier, more effective, and consistent with tidy data principles. The `textrecipes` is an extension package for Text Processing of the `recipes` package (in `tidymodels` package). 
#' 
#' We also load some extra packages for visualization of some figures and tables in this document.
#' 
## ---- message=FALSE----------------------------------------------------------------------------------------------------------
library(gridExtra)
library(knitr)
library(kableExtra)

#' 
#' The `gridExtra`package provides a number of user-level functions to work with "grid" graphics, notably to arrange multiple grid-based plots on a page, and draw tables. The `knitr` is a package for dynamic report generation in R. Finally, the `kableExtra` is a package to build complex table with `kable` and Pipe Syntax.
#' 
#' # Loading and cleaning the data set
#' 
#' The first step is to load the data set and check that everything is right. Instead of using a standard R data.frame, we have decided to use a tibble because this makes it much easier to work with large data.
#' 
## ----------------------------------------------------------------------------------------------------------------------------
opinions <- read.csv("employee_opinions.csv", sep = ";") %>% as_tibble()
opinions

#' 
#' We can see that the variable $assessment$ has been defined as a character, but we prefer to define it as a factor. So, let's change it.
#' 
## ----------------------------------------------------------------------------------------------------------------------------
opinions$assessment <- as.factor(opinions$assessment)
opinions

#' 
#' Now, our data set is ready for the analysis.
#' 
#' 
#' # Explorative data analysis of the data set
#' 
#' We are going to follow tehe following steps to carry out the analysis of the opinions
#' 
#' 1. Tokenize the data set
#' 2. Remove the stops words
#' 3. Evaluate the number of occurrences (n)
#' 4. Evaluate the term frequency (tf)
#' 5. Evaluate the term frequency & the inverse document frequency (tf-idf)
#' 
#' ## Tokenize the data set
#' 
#' As we have said, the first step is to convert our data set to a Tidy Data Structure, which is characterized by three aspects: 
#' 
#' * Each variable is a column
#' * Each observation (token) is a row
#' * Each type of observational unit is a table
#' 
#' The name of this process is tokenization, and there are different approaches. Let's see some of the most common ones. To tokenize our text data, we can use the function `unnest_tokens()` where we need to indicate the following parameters:
#' 
#' * The first one ($tbl$) is the data.frame or tibble where we have our data
#' * The second one  ($output$) is the name of the new variable where we are going to add the tokens
#' * The third one ($input$) is the name of the variable or column of our tibble where we have the text data
#' * The last one ($token$) is the unit for tokenizing. The most common are 'words', 'ngrams', and 'sentences'
#' 
#' Now, let's see how to tokenize by words, which means one word by row.  
#' 
## ----------------------------------------------------------------------------------------------------------------------------
opinion_tok1 <- opinions %>%
  unnest_tokens(output = word, input = comment, token="words")
opinion_tok1

#' 
#' We can also tokenize by groups of two words (called bi-grams) or three words (called tri-grams). Let's see these cases.
#' 
## ----------------------------------------------------------------------------------------------------------------------------
opinion_tok2 <- opinions %>%
  unnest_tokens(bigram, comment, "ngrams", n = 2)
opinion_tok2

#' 
## ----------------------------------------------------------------------------------------------------------------------------
opinion_tok3 <- opinions %>%
  unnest_tokens(trigram, comment, "ngrams", n = 3)
opinion_tok3

#' 
#' Finally, we can tokenize our text data by sentences (i.e., separated by points). You can see that the first and second opinion have just one sentence. But the third and sixth opinions have several sentences. 
#' 
## ----------------------------------------------------------------------------------------------------------------------------
opinion_toksentence <- opinions %>%
  unnest_tokens(sentence, comment, "sentences")
opinion_toksentence

#' 
#' 
#' ## The stop words
#' 
#' The stop words are those words that don't give essential information in a text analysis because they are simple connectors, or they are too common. Some examples of stop words are articles, prepositions, and adverbs. We can see the complete list in the variable $stop_words$.
#' 
## ----------------------------------------------------------------------------------------------------------------------------
head (stop_words, 20)

#' 
#' Let's check if the most common words in our text data are tge stops words.
#' 
## ----------------------------------------------------------------------------------------------------------------------------
opinion_tok1 %>% count(assessment, word, sort = TRUE)

#' 
#' As  we can see, the most common words are the stop words. For this reason, it's widespread to remove these words from the analysis. In the following table, you can see the differences between the opinions with stop words and without stop words.
#' 
## ---- message=FALSE----------------------------------------------------------------------------------------------------------
opinion_tok1_stop <- opinion_tok1 %>% anti_join(stop_words)

kable (cbind (head (opinion_tok1[,3], 10), head (opinion_tok1_stop[,3], 10)), 
       col.names = c("Tokens with stops words", "Tokens without stops words"),
       table.attr = "style='width:40%;'", 
       caption = "Tokenization with and without stops words") %>%
  kable_styling(position = "center")


#' 
#' 
#' ## The number of occurrences  
#' 
#' The first analysis we can carry out is to count the most common words for the positive and the negative opinions and compare them. Let's see the results graphically.
#' 
## ----------------------------------------------------------------------------------------------------------------------------
count_pos <- opinion_tok1_stop %>%
  filter(assessment == "positive") %>%
  count(word, sort = TRUE)

count_neg <- opinion_tok1_stop %>%
  filter(assessment == "negative") %>%
  count(word, sort = TRUE)

plot_pos <- count_pos %>%
  mutate(word = reorder(word, n)) %>%
  head(20) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "tomato", alpha = 0.7, show.legend = FALSE) +
  labs(y = NULL, title = "Positive Opinions")

plot_neg <- count_neg %>%
  mutate(word = reorder(word, n)) %>%
  head(20) %>%
  ggplot(aes(n, word, fill = 2)) +
  geom_col(fill = "steelblue", alpha = 0.7, show.legend = FALSE) +
  labs(y = NULL, title = "Negative Opinios")

grid.arrange(plot_pos, plot_neg, ncol = 2)

#' 
#' We can see that some words appear in both kinds of opinions, such as company and people. These words don't help us to differentiate between positive and negative opinions.
#' 
#' ## The term frequency 
#' 
#' Instead of comparing the number of occurrences, we can use the term frequency to avoid the effect by the difference between the number of positive and negative opinions in the data set or the difference between the opinions' length. 
#' 
#' 
## ----------------------------------------------------------------------------------------------------------------------------
plot_pos <- count_pos %>%
  mutate(tf = n / sum(n)) %>%
  select(-n) %>%
  mutate(word = reorder (word, tf)) %>%
  head(20) %>%
  ggplot(aes(tf, word)) +
  geom_col(fill = "tomato", alpha = 0.7, show.legend = FALSE) +
  labs(y = NULL, title = "Positive Opinions")

plot_neg <- count_neg %>%
  mutate(tf = n / sum(n)) %>%
  select(-n) %>%
  mutate(word = reorder (word, tf)) %>%
  head(20) %>%
  ggplot(aes(tf, word)) +
  geom_col(fill = "steelblue", alpha = 0.7, show.legend = FALSE) +
  labs(y = NULL, title = "Negative Opinios")

grid.arrange(plot_pos, plot_neg, ncol = 2)

#' 
#' In this case, we get very similar results because the number of positive and negative opinions are the same, and the length of most comments is also the same.
#' 
#' 
#' ## The term frequency and the inverse document frequency
#' 
#' In both previous analysis, the main problem was that some of the most common words appear in both kind of opinions. To resolve this situation, we can use the statistic $tf-idf$. Let's define some terms to understand how it works:
#' 
#' * term frequency ($tf$), how frequently a word occurs in a document
#' * inverse document frequency ($idf$), which decreases the weight for commonly used words and increases the weight for words that are not used very much in a collection of documents. 
#' 
#' The statistic $tf-idf$ is intended to measure how important a word is to a document in a collection  of documents. In other words, the tf-idf tells us if one word is common in one specific document and, at the same time, uncommon in the rest of documents. 
#' 
#' The `bind_tf_idf()` function calculates the $tf$, $idf$, and $tf-idf$ from the tidy text. We need to indicate the following parameters:
#' 
#' * The first one ($tbl$) is the data.frame or tibble where we have our data
#' * The second one  ($term$) is the name of the new variable where we have the tokens
#' * The third one ($document$) is the name of the variable or column of our tibble where we have the name of the documents (in pour case, the type of opinion: positive and negative)
#' * The last one ($n$) is the name of the variable where we have the number of ocurrences of each token
#' 
#' 
## ----------------------------------------------------------------------------------------------------------------------------
count_total <- opinion_tok1_stop %>%
  count(assessment, word, sort = TRUE) %>%
  bind_tf_idf(word, assessment, n)

count_total

#' 
#' When the variable $idf=0$ means that the token (the word) appears in all documents (in positive and negative opinions), so it's not a good token to differentiate among them. Now, let's see the results graphically. 
#' 
## ----------------------------------------------------------------------------------------------------------------------------
plot_pos <- count_total %>%
  filter (assessment == "positive" & tf_idf > 0) %>%
  mutate(word = reorder (word, tf_idf)) %>%
  head(20) %>%
  ggplot(aes(tf_idf, word)) +
  geom_col(fill = "tomato", alpha = 0.7, show.legend = FALSE) +
  labs(y = NULL, title = "Positive Opinions")

plot_neg <- count_total %>%
  filter (assessment == "negative" & tf_idf > 0) %>%
  mutate(word = reorder (word, tf_idf)) %>%
  head(20) %>%
  ggplot(aes(tf_idf, word)) +
  geom_col(fill = "steelblue", alpha = 0.7, show.legend = FALSE) +
  labs(y = NULL, title = "Negative Opinios")

grid.arrange(plot_pos, plot_neg, ncol = 2)

#' 
#' Now, the most frequent and important words are others. These words are better predictors for positive and negative opinions than what we have seen until now. 
#' 
#' # Predicting model based on the term frequency
#' 
#' After introducing some concepts and explorative data analysis, let's try to build a model to predict the kind of opinion based on the employee comments. As we want to predict the value of a categorical variable (positive and negative), we will use a simple logistic regression model.
#' 
#' We will follow three different approaches to build, train and test our logistic regression model:
#' 
#' * A model where the tokens will be one word.
#' * A model where the tokens will be bi-grams. 
#' 
#' ## Predicting model with tokens 1 word
#' 
#' To compare all our analyses' results, we have decided to set the seed to 10210 (this number is random). 
#' 
## ---- message=FALSE----------------------------------------------------------------------------------------------------------
set.seed(10210)

#' 
#' Before building our model, we need to split up our data set into a training data set and a testing data set. In this case, we have decided that the training data set contains the 75% of the original data set and to stratify the samples based on whether the opinion is positive or negative. 
#' 
#' 
## ----------------------------------------------------------------------------------------------------------------------------
opinion_split <- initial_split(opinions, strata = "assessment", p = 0.75)
train_data <- opinion_split %>% training()
test_data <- opinion_split %>% testing()

#' 
#' There are several ways to preprocess our data set (e.g., missing values imputation, removing predictors, centering, and scaling) before the analysis. In this case, we will create a recipe that allows us to handle all the data preprocessing.
#' 
#' In the recipe, we need to indicate the following parameters:
#' 
#' * The $formula$, where $assessment$ is the dependent variable and $comment$ is the independent variable
#' * The $data$, where we have decided to use the training data set to build the model
#' 
## ----------------------------------------------------------------------------------------------------------------------------
data_rec <- recipe(formula = assessment ~ comment, data = train_data) %>%
  step_filter(comment != "") %>% 
  step_tokenize(comment) %>%
  step_stopwords(comment, keep = FALSE) %>%
  step_tokenfilter(comment, min_times = 1) %>%
  step_tf(comment) %>%
  prep(training = train_data)
data_rec

#' 
#' After the initial definition of the recipe, we can add new processes:
#' 
#' * To remove any empty row that we could have it in the data set. 
#' * To tokenize the data set as we have seen before. As we haven't indicated anything, the function will tokenize the data set by words (1-gram). 
#' * To remove the stop words. 
#' * To filter the tokens that appear at least once in the data set 
#' * To calculate the term frequency oe each word
#' * To prepare our data set based on the previous steps. 
#' 
#' Now, we need to carry out (bake) our recipe with our data sets.
#' 
## ----------------------------------------------------------------------------------------------------------------------------
train_baked <- data_rec %>% bake(new_data = train_data)
test_baked  <- data_rec %>% bake(new_data = test_data)

#' 
#' With the baked recipe, we can start working in the predictive model. As we have seen before, we have decided to use a logistics regression model. To create the predictive model structure, we need to define two elements:
#' 
#' * The type of model and the mode - As we explained before, we will use a logistic regression for classification
#' * The computational engine - There are several options, but here we have decided to use `glmnet`
#' 
## ----------------------------------------------------------------------------------------------------------------------------
glm_model <- logistic_reg(mode = "classification", mixture = 0, penalty = 0.1) %>%
  set_engine("glmnet")
glm_model

#' 
#' Now, it's time to fit the predictive model structure to our training data set, so we need two define two elements:
#' 
#' * The independent and dependent variables of the model - The independent variable is $assessment$, and the dependent variable is $comment$
#' * The data set to fit - As we are building the model, we will use the training data set
#' 
## ----------------------------------------------------------------------------------------------------------------------------
final_model <- glm_model %>%
  fit(assessment ~ ., data = train_baked)

#' 
#' The final step is to assess the performance of the model. The most straightforward way is to show the actual and predictive values of the testing data set.
#' 
## ----------------------------------------------------------------------------------------------------------------------------
predictions_glm <- final_model %>%
  predict(new_data = test_baked) %>%
  bind_cols(test_baked %>% select(assessment))

kable (head(predictions_glm), 
       col.names = c("Predictive Values", "Actual Values"),
       table.attr = "style='width:40%;'", 
       caption = "Comparison between actual and predicted values") %>%
  kable_styling(position = "center")

#' 
#' We can see that not all predictive values fit the actual values. Another better way to show the results is the Confusion Matrix, where we can see the number of false positives, false negatives, true positives, and true negatives. 
#' 
## ----------------------------------------------------------------------------------------------------------------------------
predictions_glm %>%
  conf_mat(assessment, .pred_class) %>%
  pluck(1) %>%
  as_tibble() %>%
  ggplot(aes(Prediction, Truth, alpha = n, fill = c("1","2","3","4")))  +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = n), colour = "white", alpha = 1, size = 8)

#' 
#' Another way to evaluate the predictive model is to assess the accuracy. In other words, the fraction of predictions the model got right.
#' 
## ----------------------------------------------------------------------------------------------------------------------------
predictions_glm %>%
  metrics(assessment, .pred_class) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy") 

#' 
#' As we can see, the results are not vey good (only 66.7%). Maybe, other predictive models can help us to predict better the type of opinion.
#' 
#' We can also assess the predictive model with the ROC curve:
#' 
## ----------------------------------------------------------------------------------------------------------------------------
test_baked %>% 
  select(assessment) %>%
  mutate(
    my_class = parsnip:::predict_class(final_model, test_baked),
    my_prop = parsnip:::predict_classprob(final_model, test_baked) %>% pull(`negative`)
  ) %>%
  roc_curve(assessment, my_prop) %>%
  autoplot()

#' 
#' 
#' ## Predicting model with tokens bi-grams (two words)
#' 
#' Now, we are going to evaluate what happens when we use bi-grams instead of words. In cases where we have short sentences and small data sets, bi-grams are not recommended. So, we expect worse results than in the previous section. To start, we define the same seed again. 
#' 
## ---- message=FALSE----------------------------------------------------------------------------------------------------------
set.seed(10210)

#' 
#' We split up the data set again.
#' 
## ----------------------------------------------------------------------------------------------------------------------------
opinion_split <- initial_split(opinions, strata = "assessment", p = 0.75)
train_data <- opinion_split %>% training()
test_data <- opinion_split %>% testing()

#' 
#' Now, our recipe will be very similar. The only difference is the way how we are tokenizing our text data. In this case, we are using bi-grams (set of two words).
#' 
## ----------------------------------------------------------------------------------------------------------------------------
data_rec <- recipe(formula = assessment ~ comment, data = train_data) %>%
  step_filter(comment != "") %>% 
  step_tokenize(comment, token = "ngrams", options = list(n = 2)) %>%
  step_stopwords(comment, keep = FALSE) %>%
  step_tokenfilter(comment, min_times = 1) %>%
  step_tf(comment) %>%
  prep(training = train_data)
data_rec

#' 
#' Now, we need to carry out (bake) our recipe with our data sets.
#' 
## ----------------------------------------------------------------------------------------------------------------------------
train_baked <- data_rec %>% bake(new_data = train_data)
test_baked  <- data_rec %>% bake(new_data = test_data)

#' 
#' Now, it's time to define the model structure, to fit the predictive model to our training data, and predict the values of the testing data set. 
#' 
## ----------------------------------------------------------------------------------------------------------------------------
glm_model <- logistic_reg(mode = "classification", mixture = 0, penalty = 0.1) %>%
  set_engine("glmnet")
glm_model

final_model <- glm_model %>%
  fit(assessment ~ ., data = train_baked)

predictions_glm <- final_model %>%
  predict(new_data = test_baked) %>%
  bind_cols(test_baked %>% select(assessment))

#' 
#' The final step is to assess the performance of the model. Let's see the results is a Confusion Matrix. 
#' 
## ----------------------------------------------------------------------------------------------------------------------------
predictions_glm %>%
  conf_mat(assessment, .pred_class) %>%
  pluck(1) %>%
  as_tibble() %>%
  ggplot(aes(Prediction, Truth, alpha = n, fill = c("1","2","3","4")))  +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = n), colour = "white", alpha = 1, size = 8)

#' 
#' If we prefer to assess the model by the accuracy, the result is 55.6%. As we have predicted at the beginning, the result is worse than when we use words as tokens.  
#' 
## ----------------------------------------------------------------------------------------------------------------------------
predictions_glm %>%
  metrics(assessment, .pred_class) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy") 

#' 
#' Finally, we can assess the predictive model with the ROC curve:
#' 
## ----------------------------------------------------------------------------------------------------------------------------
test_baked %>% 
  select(assessment) %>%
  mutate(
    my_class = parsnip:::predict_class(final_model, test_baked),
    my_prop = parsnip:::predict_classprob(final_model, test_baked) %>% pull(`negative`)
  ) %>%
  roc_curve(assessment, my_prop) %>%
  autoplot()

#' 
#' 
#' # Predicting model based on the term frequency & inverse document frequency 
#' 
#' Now, we will evaluate what happens when we use the statistic term frequency & inverse document frequency with one word as a token. As we have been discussing before, we expect better results for the nature of the statistic. To start, we define the same seed again. 
#' 
## ---- message=FALSE----------------------------------------------------------------------------------------------------------
set.seed(10210)

#' 
#' We split up the data set again.
#' 
## ----------------------------------------------------------------------------------------------------------------------------
opinion_split <- initial_split(opinions, strata = "assessment", p = 0.75)
train_data <- opinion_split %>% training()
test_data <- opinion_split %>% testing()

#' 
#' Now, our recipe will be very similar. The only difference is the way how we will convert a tokenlist into multiple variables containing the term frequency-inverse document frequency of tokens.
#' 
## ----------------------------------------------------------------------------------------------------------------------------
data_rec <- recipe(formula = assessment ~ comment, data = train_data) %>%
  step_filter(comment != "") %>% 
  step_tokenize(comment) %>%
  step_stopwords(comment, keep = FALSE) %>%
  step_tokenfilter(comment, min_times = 1) %>%
  step_tfidf(comment) %>%
  prep(training = train_data)
data_rec

#' 
#' Now, we need to carry out (bake) our recipe with our data sets.
#' 
## ----------------------------------------------------------------------------------------------------------------------------
train_baked <- data_rec %>% bake(new_data = train_data)
test_baked  <- data_rec %>% bake(new_data = test_data)

#' 
#' The next step is to define the model structure, to fit the predictive model to our training data, and to predict the values from the testing data set. 
#' 
## ----------------------------------------------------------------------------------------------------------------------------
glm_model <- logistic_reg(mode = "classification", mixture = 0, penalty = 0.1) %>%
  set_engine("glmnet")
glm_model

final_model <- glm_model %>%
  fit(assessment ~ ., data = train_baked)

predictions_glm <- final_model %>%
  predict(new_data = test_baked) %>%
  bind_cols(test_baked %>% select(assessment))

#' 
#' The final step is to assess the performance of the model. Let's see the results is a Confusion Matrix. 
#' 
## ----------------------------------------------------------------------------------------------------------------------------
predictions_glm %>%
  conf_mat(assessment, .pred_class) %>%
  pluck(1) %>%
  as_tibble() %>%
  ggplot(aes(Prediction, Truth, alpha = n, fill = c("1","2","3","4")))  +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = n), colour = "white", alpha = 1, size = 8)

#' 
#' If we prefer to assess the model by the accuracy, the result is 75%. This result is better than the previous ones. As the value is greater than 70%, we can start considering using the model to predict the employee opinions' kind (positive or negative).
#' 
## ----------------------------------------------------------------------------------------------------------------------------
predictions_glm %>%
  metrics(assessment, .pred_class) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy") 

#' 
#' Finally, we can assess the predictive model with the ROC curve:
#' 
## ----------------------------------------------------------------------------------------------------------------------------
test_baked %>% 
  select(assessment) %>%
  mutate(
    my_class = parsnip:::predict_class(final_model, test_baked),
    my_prop = parsnip:::predict_classprob(final_model, test_baked) %>% pull(`negative`)
  ) %>%
  roc_curve(assessment, my_prop) %>%
  autoplot()

#' 
#' 
#' # Conclusions
#' 
#' This document has built several models to predict how employees feel about their organizations from their comments. We decided to use a logistic regression model and two approaches. The first one takes term frequency (tf) as the core variable, and the second uses the statistic term frequency and inverse document frequency (tf-idf). 
#' 
#' Two main conclusions:
#' 
#' 1. For small samples, the best token is the word
#' 2. The `tf-idf` works better than `tf` for classification
#' 
