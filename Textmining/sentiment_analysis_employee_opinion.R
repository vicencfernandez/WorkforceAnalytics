#' ---
#' title: "Workforce Analytics: Sentiment Analysis on Employees' Opinion"
#' author: "Vicenc Fernandez, Ph.D."
#' date: "4/20/2021"
#' output: 
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     number_sections: true
#' editor_options: 
#'   chunk_output_type: console
#' ---
#' 
## ----setup, include=FALSE--------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' # The context and the data set
#' 
#' This is the second tutorial for text mining on employees' opinion. See the first one at [https://rpubs.com/vicenc/textmining-employees](https://rpubs.com/vicenc/textmining-employees).
#' 
#' We have a data set with 149 positive and negative employees' opinions on how they feel about the organization where they are working. You can download the data set from [https://github.com/vicencfernandez/WorkforceAnalytics](https://github.com/vicencfernandez/WorkforceAnalytics). The data set contains three variables:  
#' 
#' * commentID: An integer variable with the opinion ID
#' * comment: A character variable with the employee opinion about their organization
#' * assessment: A factor variable indicating if the opinion is positive or negative 
#' 
#' The activity's objective is to predict the kind of opinion (positive or negative) based on the employee comment. In this report, the analysis strategy focuses on two sentiment lexicons.
#' 
#' # Packages for the analysis and reporting
#' 
#' We need to install and load several packages to analyze our data set with text mining.
#' 
## ---- message=FALSE--------------------------------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(tidytext)
library(textrecipes)
library(textdata)

#' 
#' The `tidyverse` is an opinionated collection of R packages designed for data science. The `tidymodels` framework is a collection of packages for modeling and machine learning using tidyverse principles. The `tidytext` is a package that make many text mining tasks easier, more effective, and consistent with tidy data principles. The `textrecipes` is an extension package for Text Processing of the `recipes` package (in `tidymodels` package). The `textdata` is a package that includes various sentiment lexicons and labeled text data sets for classification and analysis. 
#' 
#' We also load some extra packages for visualization of some figures and tables in this document.
#' 
## ---- message=FALSE--------------------------------------------------------------------------------------------
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
## --------------------------------------------------------------------------------------------------------------
opinions <- read.csv("employee_opinions.csv", sep = ";") %>% as_tibble()
opinions

#' 
#' We can see that the variable $assessment$ has been defined as a character, but we prefer to define it as a factor. So, let's change it.
#' 
## --------------------------------------------------------------------------------------------------------------
opinions$assessment <- as.factor(opinions$assessment)
opinions

#' 
#' Now, our data set is ready for the analysis.
#' 
#' # Explorative data analysis of the data set
#' 
#' We are going to follow the following steps to carry out the analysis of the opinions
#' 
#' 1. Tokenize the data set
#' 2. Remove the stops words
#' 3. Apply different sentiment lexicons
#' 
#' ## Tokenize the data set
#' 
#' As we have said, the first step is to convert our data set to a Tidy Data Structure, which is characterized by three aspects: 
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
## --------------------------------------------------------------------------------------------------------------
opinion_tok1 <- opinions %>%
  unnest_tokens(output = word, input = comment, token="words")
opinion_tok1

#' 
#' ## The stop words
#' 
#' The stop words are those words that don't give essential information in a text analysis because they are simple connectors, or they are too common. Some examples of stop words are articles, prepositions, and adverbs. We can see the complete list in the variable $stop_words$.
#' 
## --------------------------------------------------------------------------------------------------------------
head (stop_words, 20)

#' 
#' Let's check if the most common words in our text data are tge stops words.
#' 
## --------------------------------------------------------------------------------------------------------------
opinion_tok1 %>% count(assessment, word, sort = TRUE)

#' 
#' As  we can see, the most common words are the stop words. For this reason, it's widespread to remove these words from the analysis. In the following table, you can see the differences between the opinions with stop words and without stop words.
#' 
## ---- message=FALSE--------------------------------------------------------------------------------------------
opinion_tok1_stop <- opinion_tok1 %>% anti_join(stop_words)

kable (cbind (head (opinion_tok1[,3], 10), head (opinion_tok1_stop[,3], 10)), 
       col.names = c("Tokens with stops words", "Tokens without stops words"),
       table.attr = "style='width:40%;'", 
       caption = "Tokenization with and without stops words") %>%
  kable_styling(position = "center")


#' 
#' ## Sentiment lexicons
#' 
#' A lexicon is a dictionary where words are classified in different sentiments. So, a new approach for analyzing our text is to substitute the words by the sentiments related to these words (according to the lexicon), and then, to carry out our statistical techniques.  
#' 
#' There are several sentiment lexicons, but the most common ones are:
#' 
#' * **bing** (by Bing Liu and collaborators), where the lexicon classifies words with a binary variable. $-1$ means a negative emotion, and $+1$ means a positive emotion.
#' * **afinn** (by Finn Årup Nielsen), where the lexicon classifies words in a range between $-5$ and $+5$, where $-5$ means a very negative emotion and $+5$ means a very positive emotion.  
#' * **nrc** (by Saif Mohammad and Peter Turney), where the lexicon classifies words in positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust. In this lexicon, words can take one or more emotions with a binary variable (yes/no).
#' * **loughran** (by Tim Loughran and Bill McDonald), where the lexicon classifies words in negative, positive, litigious, uncertainty, constraining, and superfluous. In this lexicon, words can take one or more emotions with a binary variable (yes/no). The lexicon was created for use with financial documents.
#' 
#' Let's see some examples of these lexicon with more details.
#' 
## --------------------------------------------------------------------------------------------------------------
## Lexicon bing
lexicon_bing()

# Lexicon afinn
lexicon_afinn()

# Lexicon nrc 
lexicon_nrc()

#Lexicon loughram
lexicon_loughran()

#' 
#' You can also get these lexicons with the function `get_sentiments()`. 
#' 
## ---- eval=FALSE-----------------------------------------------------------------------------------------------
## get_sentiments(lexicon = "bing")
## get_sentiments(lexicon = "afinn")
## get_sentiments(lexicon = "nrc")
## get_sentiments(lexicon = "loughran")

#' 
#' Let's see an example of the lexicon loughran
#' 
## --------------------------------------------------------------------------------------------------------------
lexicon_loughran() %>%
  filter(word == "encumber")

#' 
#' 
#' Based on this lexicons, we can identify which are the most common emotions. Let's see two examples. The first one based on `bing`. Let's see which are the most common positive and negative words in the employee's comments. 
#' 
## ---- message=FALSE--------------------------------------------------------------------------------------------
opinion_tok1_stop %>%
  inner_join(lexicon_bing()) %>%
  filter(sentiment == "positive") %>%
  count(word, sort = TRUE)

opinion_tok1_stop %>%
  inner_join(lexicon_bing()) %>%
  filter(sentiment == "negative") %>%
  count(word, sort = TRUE)

#' 
#' As we can see, the most common positive word is 'love' and the most negative one is 'hard'. Remember that the final emotion of the word depends on the context, but as a independent term, most words tend to a positive or negative emotion.  
#' 
#' Let's see a another example with the lexicon `nrc`. We can identify which are the most common words with an 'anger' feeling in the positive employees' opinion. As we can see, the feeling only appear three times in this comments.  
#' 
## ---- message=FALSE--------------------------------------------------------------------------------------------
nrc_anger <- lexicon_nrc() %>% 
  filter(sentiment == "anger")

# The most common words with the sentiment 'anger' in the positive comments
opinion_tok1_stop %>%
  filter(assessment == "positive") %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)

# Number of words with the sentiment 'anger' in the positive comments
opinion_tok1_stop %>%
  filter(assessment == "positive") %>%
  inner_join(nrc_anger) %>%
  count(sentiment, sort = TRUE)

#' 
#' Now, we can identify which are the most common words with an 'anger' feeling in the negative employees' opinion. As we can see, there are 19 different words (23 occurrences) related to this feeling in this comments.
#' 
## ---- message=FALSE--------------------------------------------------------------------------------------------
# The most common words with the sentiment 'anger' in the negative comments
opinion_tok1_stop %>%
  filter(assessment == "negative") %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)

# Number of words with the sentiment 'anger' in the negative comments
opinion_tok1_stop %>%
  filter(assessment == "negative") %>%
  inner_join(nrc_anger) %>%
  count(sentiment, sort = TRUE)

#' 
#' We could do similar analysis with each sentiment lexicon. 
#' 
#' To end, let's see what happens when we analyze the employees' opinions using the lexicon `bing` graphically. In the following plot, we can see the positive opinion in green, and the negative opinion in orange. We can see that most of the differences between the positive and negative words in the positive opinions is greater than 0, and most of the differences between the positive and negative words in the negative opinions is less than 0. It's not perfect, but it could be a good indicator.
#' 
## ---- message=FALSE--------------------------------------------------------------------------------------------
opinion_tok1_stop_sentiment <- opinion_tok1_stop %>%
  inner_join(get_sentiments("bing")) %>%
  count(commentID, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative) %>%
  inner_join(data.frame(commentID = opinion_tok1_stop$commentID, assessment = opinion_tok1_stop$assessment)) 

ggplot(opinion_tok1_stop_sentiment, aes(commentID, sentiment, fill = assessment)) +
  geom_col(show.legend = FALSE)

#' 
#' # Predicting model based on sentiment lexicons
#' 
#' Let's see see how to carry of them. The first one is the lexicon afinn
#' 
#' After introducing some concepts and explorative data analysis, let's try to build a model to predict the kind of opinion based on the employee comments. As we want to predict the value of a categorical variable (positive and negative), we will use a simple logistic regression model.
#' 
#' We will follow different approaches to build, train and test our logistic regression model:
#' 
#' * A model based on the lexicon affin
#' * A model based on the lexicon loughran 
#' 
#' 
#' 
#' ## Predicting model based on the lexicon affin
#' 
#' To compare all our analyses' results, we have decided to set the seed to 10210 (this number is random). 
#' 
## ---- message=FALSE--------------------------------------------------------------------------------------------
set.seed(10210)

#' 
#' Before building our model, we need to split up our data set into a training data set and a testing data set. In this case, we have decided that the training data set contains the 75% of the original data set and to stratify the samples based on whether the opinion is positive or negative. 
#' 
#' 
## --------------------------------------------------------------------------------------------------------------
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
## --------------------------------------------------------------------------------------------------------------
data_rec <- recipe(formula = assessment ~ comment, data = train_data) %>%
  step_tokenize(comment) %>%
  step_stopwords(comment, keep = FALSE) %>%
  step_word_embeddings(comment, embeddings = lexicon_afinn()) %>%
  prep(training = train_data)
data_rec

#' 
#' After the initial definition of the recipe, we can add new processes:
#' 
#' * To tokenize the data set as we have seen before. As we haven't indicated anything, the function will tokenize the data set by words (1-gram). 
#' * To remove the stop words. 
#' * To evaluate/substitute the words (tokens) by sentiments based on the lexicon affin.
#' * To prepare our data set based on the previous steps. 
#' 
#' Now, we need to carry out (bake) our recipe with our data sets.
#' 
## --------------------------------------------------------------------------------------------------------------
train_baked <- data_rec %>% bake(new_data = train_data)
test_baked  <- data_rec %>% bake(new_data = test_data)

#' 
#' With the baked recipe, we can start working in the predictive model. As we have seen before, we have decided to use a logistics regression model. To create the predictive model structure, we need to define two elements:
#' 
#' * The type of model and the mode - As we explained before, we will use a logistic regression for classification
#' * The computational engine - There are several options, but here we have decided to use `glm`
#' 
## --------------------------------------------------------------------------------------------------------------
glm_model <- logistic_reg(mode = "classification") %>%
  set_engine("glm")
glm_model

#' 
#' Now, it's time to fit the predictive model structure to our training data set, so we need two define two elements:
#' 
#' * The independent and dependent variables of the model - The independent variable is $assessment$, and the dependent variable is $comment$
#' * The data set to fit - As we are building the model, we will use the training data set
#' 
## --------------------------------------------------------------------------------------------------------------
final_model <- glm_model %>%
  fit(assessment ~ ., data = train_baked)

#' 
#' The final step is to assess the performance of the model. The most straightforward way is to show the actual and predictive values of the testing data set.
#' 
## --------------------------------------------------------------------------------------------------------------
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
## --------------------------------------------------------------------------------------------------------------
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
## --------------------------------------------------------------------------------------------------------------
predictions_glm %>%
  metrics(assessment, .pred_class) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy") 

#' 
#' As we can see, the results is very good (83.3%).
#' 
#' We can also assess the predictive model with the ROC curve:
#' 
## --------------------------------------------------------------------------------------------------------------
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
#' ## Predicting model based on the lexicon loughran
#' 
#' Now, we are going to evaluate what happens when we use the lexicon loughran. To start, we define the same seed again. 
#' 
## ---- message=FALSE--------------------------------------------------------------------------------------------
set.seed(10210)

#' 
#' We split up the data set again.
#' 
## --------------------------------------------------------------------------------------------------------------
opinion_split <- initial_split(opinions, strata = "assessment", p = 0.75)
train_data <- opinion_split %>% training()
test_data <- opinion_split %>% testing()

#' 
#' The pre-process of our data set is the same as we have seen before, but now we are going to use the function set_word_embedding to evaluate the words of our data set and to mask with our lexicon loughran. However, previously we need to trasfor the lexicon a format that our recipe can understand.
#' 
## --------------------------------------------------------------------------------------------------------------
lexicon_loughran_wide <- lexicon_loughran() %>%
  mutate(var = 1) %>% 
  tidyr::pivot_wider(names_from = sentiment, 
                     values_from = var, 
                     values_fill = list(var = 0))

#' 
#' Now, our recipe will be very similar. The only difference is the lexicon that we are using.
#' 
## --------------------------------------------------------------------------------------------------------------
data_rec <- recipe(formula = assessment ~ comment, data = train_data) %>%
  step_tokenize(comment) %>%
  step_stopwords(comment, keep = FALSE) %>%
  step_word_embeddings(comment, embeddings = lexicon_loughran_wide, prefix = "loughran") %>%
  prep(training = train_data)
data_rec

#' 
#' Now, we need to carry out (bake) our recipe with our data sets.
#' 
## --------------------------------------------------------------------------------------------------------------
train_baked <- data_rec %>% bake(new_data = train_data)
test_baked  <- data_rec %>% bake(new_data = test_data)

#' 
#' Now, it's time to define the model structure, to fit the predictive model to our training data, and predict the values of the testing data set. 
#' 
## --------------------------------------------------------------------------------------------------------------
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
## --------------------------------------------------------------------------------------------------------------
predictions_glm %>%
  conf_mat(assessment, .pred_class) %>%
  pluck(1) %>%
  as_tibble() %>%
  ggplot(aes(Prediction, Truth, alpha = n, fill = c("1","2","3","4")))  +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = n), colour = "white", alpha = 1, size = 8)

#' 
#' If we prefer to assess the model by the accuracy, the result is 80.6%. It's also a very good predictor.  
#' 
## --------------------------------------------------------------------------------------------------------------
predictions_glm %>%
  metrics(assessment, .pred_class) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy") 

#' 
#' Finally, we can assess the predictive model with the ROC curve:
#' 
## --------------------------------------------------------------------------------------------------------------
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
#' 
#' ## Predicting model based on the lexicon nrc
#' 
#' Now, we are going to evaluate what happens when we use the lexicon nrc To start, we define the same seed again. 
#' 
## ---- message=FALSE--------------------------------------------------------------------------------------------
set.seed(10210)

#' 
#' We split up the data set again.
#' 
## --------------------------------------------------------------------------------------------------------------
opinion_split <- initial_split(opinions, strata = "assessment", p = 0.75)
train_data <- opinion_split %>% training()
test_data <- opinion_split %>% testing()

#' 
#' The pre-process of our data set is the same as we have seen before, but now we are going to use the function set_word_embedding to evaluate the words of our data set and to mask with our lexicon nrc However, previously we need to transform the lexicon a format that our recipe can understand.
#' 
## --------------------------------------------------------------------------------------------------------------
lexicon_nrc_wide <- lexicon_nrc() %>%
  mutate(var = 1) %>% 
  tidyr::pivot_wider(names_from = sentiment, 
                     values_from = var, 
                     values_fill = list(var = 0))

#' 
#' Now, our recipe will be very similar. The only difference is the lexicon that we are using.
#' 
## --------------------------------------------------------------------------------------------------------------
data_rec <- recipe(formula = assessment ~ comment, data = train_data) %>%
  step_tokenize(comment) %>%
  step_stopwords(comment, keep = FALSE) %>%
  step_word_embeddings(comment, embeddings = lexicon_nrc_wide, prefix = "nrc") %>%
  prep(training = train_data)
data_rec

#' 
#' Now, we need to carry out (bake) our recipe with our data sets.
#' 
## --------------------------------------------------------------------------------------------------------------
train_baked <- data_rec %>% bake(new_data = train_data)
test_baked  <- data_rec %>% bake(new_data = test_data)

#' 
#' Now, it's time to define the model structure, to fit the predictive model to our training data, and predict the values of the testing data set. 
#' 
## --------------------------------------------------------------------------------------------------------------
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
## --------------------------------------------------------------------------------------------------------------
predictions_glm %>%
  conf_mat(assessment, .pred_class) %>%
  pluck(1) %>%
  as_tibble() %>%
  ggplot(aes(Prediction, Truth, alpha = n, fill = c("1","2","3","4")))  +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = n), colour = "white", alpha = 1, size = 8)

#' 
#' If we prefer to assess the model by the accuracy, the result is 69.4%. It's the worst predictor that we have seen today.  
#' 
## --------------------------------------------------------------------------------------------------------------
predictions_glm %>%
  metrics(assessment, .pred_class) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy") 

#' 
#' Finally, we can assess the predictive model with the ROC curve:
#' 
## --------------------------------------------------------------------------------------------------------------
test_baked %>% 
  select(assessment) %>%
  mutate(
    my_class = parsnip:::predict_class(final_model, test_baked),
    my_prop = parsnip:::predict_classprob(final_model, test_baked) %>% pull(`negative`)
  ) %>%
  roc_curve(assessment, my_prop) %>%
  autoplot()

#' 
#' # Conclusions
#' 
#' This document has built several models to predict how employees feel about their organizations from their comments. We decided to use a logistic regression model and three different sentiment lexicons: affin, loughran, and nrc.
