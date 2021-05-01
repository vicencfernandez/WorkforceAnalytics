#' ---
#' title: "Workforce Analytics: TextMinimg with Word Embedding on Employees' Opinion"
#' author: "Vicenc Fernandez, Ph.D."
#' date: "1/5/2021"
#' output: 
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     number_sections: true
#' editor_options: 
#'   chunk_output_type: console
#' ---
#' 
## ----setup, include=FALSE--------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' # The context and the data set
#' 
#' This is the third tutorial for text mining on employees' opinion. See the first one at [https://rpubs.com/vicenc/textmining-employees](https://rpubs.com/vicenc/textmining-employees), and the second one at [https://rpubs.com/vicenc/sentiment-analysis](https://rpubs.com/vicenc/sentiment-analysis).
#' 
#' We have a data set with 149 positive and negative employees' opinions on how they feel about the organization where they are working. You can download the data set from [https://github.com/vicencfernandez/WorkforceAnalytics](https://github.com/vicencfernandez/WorkforceAnalytics). The data set contains three variables:  
#' 
#' * commentID: An integer variable with the opinion ID
#' * comment: A character variable with the employee opinion about their organization
#' * assessment: A factor variable indicating if the opinion is positive or negative 
#' 
#' The activity's objective is to predict the kind of opinion (positive or negative) based on the employee comment. In this report, the analysis strategy focuses on a word embbeding approach.
#' 
#' # Packages for the analysis and reporting
#' 
#' We need to install and load several packages to analyze our data set with text mining.
#' 
## ---- message=FALSE--------------------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(tidytext)
library(textrecipes)
library(textdata)
library(irlba)
library(widyr)

#' 
#' The `tidyverse` is an opinionated collection of R packages designed for data science. The `tidymodels` framework is a collection of packages for modeling and machine learning using tidyverse principles. The `tidytext` is a package that make many text mining tasks easier, more effective, and consistent with tidy data principles. The `textrecipes` is an extension package for Text Processing of the `recipes` package (in `tidymodels` package). The `textdata` is a package that includes various sentiment lexicons and labeled text data sets for classification and analysis. The `irlba` is a set of methods for truncated singular value decomposition and principal components analysis of large sparse and dense matrices (SVD). The `widyr` is useful package for several operations such as co-occurrence counts, correlations, or clustering that are mathematically convenient on wide matrices.
#' 
#' We also load some extra packages for visualization of some figures and tables in this document.
#' 
## ---- message=FALSE--------------------------------------------------------------------------------
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
## --------------------------------------------------------------------------------------------------
opinions <- read.csv("employee_opinions.csv", sep = ";") %>% as_tibble()
opinions

#' 
#' We can see that the variable $assessment$ has been defined as a character, but we prefer to define it as a factor. So, let's change it.
#' 
## --------------------------------------------------------------------------------------------------
opinions$assessment <- as.factor(opinions$assessment)
opinions

#' 
#' Now, our data set is ready for analysis. The first step will be to remove the stop words. Please, see the previous tutorials to know more about tokenization and stop words. 
#' 
## ---- message = FALSE------------------------------------------------------------------------------
opinion_non_stop <- opinions %>%
  unnest_tokens(output = word, input = comment, token = "words") %>%
  anti_join(stop_words) %>%
  group_by(commentID) %>%
  summarise(comment = str_c(word, collapse = " "))

#' 
#' # Word Embedding Approach
#' 
#' The basic idea behind word embedding is to find similarities between words in one or several documents by using some model to predict the co-occurrence of words within a small set of words. In other words, how often two words appear close to each other. But, what do we mean by close? It's something that we will have to decide (and test). We will use the term 'context window' as the distance in words that we are going to consider between the word that we are analyzing and the maximum distance of our consideration. Lets' see an example. 
#' 
#' Consider the sentence 'this company is the best place where I have worked in my life'. If the center word is 'place' and the context window (our definition of proximity) is three words, we will consider that the close words are: 'is', 'the', 'best', 'where', 'I', and 'have' are near to 'place'. 
#' 
#' ## Word Embedding Approaches
#' 
#' There are two approaches for using Word Embedding techniques: 
#' 
#' * Continuous Bag of Words - CBOW
#' * Skip-Gram Model
#' 
#' The CBOW techniques tries to predict a specific word based on a set of words. This approach is very common when we are implementing a predictive web search. 
#' 
#' The Skip-Gram Model techniques tries to predict a set of words based on a specific word. This approach is very common when we want to identify the context of the word. In this document, we focus on this approach.   
#' 
#' ## Building an Word Embedding Model 
#' 
#' Most of Word Embedding Model are built by neural networks. In this case, we are going to create a simple model with `tidytext`. As we have said before, we need to define our 'context window'. For presenting the process, we have decided to use a 'context window' with a length of six words.
#' 
#' The first step is to tokenize our data by words, but previously we need to create a variable (called `skipgramID`) that allow us to identify the opinion and the ngram.
#' 
## --------------------------------------------------------------------------------------------------
tidy_skipgrams <- opinion_non_stop %>%
  unnest_tokens(ngram, comment, token = "ngrams", n = 6) %>%
  filter(ngram != "") %>%
  mutate(ngramID = row_number()) %>%
  tidyr::unite(skipgramID, commentID, ngramID) %>%
  unnest_tokens(word, ngram)
tidy_skipgrams

#' 
#' Now, we need to calculate the probabilities for the unigrams (how often we find each word in the original text) and for the skipgrams (how often we find each word next to every other word within the context window). 
#' 
## ---- warning=FALSE--------------------------------------------------------------------------------
unigram_probs <- opinion_non_stop %>%
  unnest_tokens(word, comment) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))
unigram_probs

skipgram_probs <- tidy_skipgrams %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))
skipgram_probs

#' 
#' Finally, we normalize the skipgram probabilities based on the unigram probabilities.
#' 
## --------------------------------------------------------------------------------------------------
normalized_prob <- skipgram_probs %>%
  filter(n > 3) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
    select(word1 = word, p1 = p),
  by = "word1"
  ) %>%
  left_join(unigram_probs %>%
    select(word2 = word, p2 = p),
  by = "word2"
  ) %>%
  mutate(p_together = p / p1 / p2)
normalized_prob

#' 
#' We can identify the words that appear more frequently together or the words that appear more regularly with one specific word (in this example, company).
#' 
## --------------------------------------------------------------------------------------------------
normalized_prob %>%
  filter(word1 != word2) %>%
  arrange(-p_together)

normalized_prob %>%
  filter(word1 == "company") %>%
  arrange(-p_together)

#' 
#' The best option to work with this information is to transform the `data.frame` to a sparse matrix and reduce its dimensionality. For reducing the number of dimensions, we have decided to carry out a singular value decomposition (SVD), which provides a way to factorize a matrix into singular vectors and singular values. We set 50 as the number of dimensions to reduce.
#' 
## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------
pmi_matrix <- normalized_prob %>%
  mutate(pmi = log10(p_together)) %>%
  cast_sparse(word1, word2, pmi)

pmi_matrix@x[is.na(pmi_matrix@x)] <- 0

pmi_svd <- irlba(pmi_matrix, 50, maxit = 500)
word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)

#' 
#' Now, we can build a function (called `search_proximity`) that allow us to find close words based on the `word_vectors` that we have just created.
#' 
## ---- message = FALSE, warning = FALSE-------------------------------------------------------------
search_proximity <- function(word_vectors, selected_vector) {
  product <- word_vectors %*% selected_vector

  similarities <- product %>%
    tidy() %>%
    as_tibble() %>%
    mutate(token = rownames(product)) %>%
    rename(similarity = x) %>%
    arrange(-similarity)
}

#' 
#' Let's see two examples. For instance, we can see which are the most common words that appear together with company and employees.
#' 
## ---- message = FALSE, warning = FALSE-------------------------------------------------------------
company_together <- search_proximity(word_vectors, word_vectors["company", ])
company_together

employee_together <- search_proximity(word_vectors, word_vectors["employees", ])
employee_together

#' 
#' Finally, we can visualize these results. 
#' 
## --------------------------------------------------------------------------------------------------
company_together %>%
  mutate(selected = "company") %>%
  bind_rows(employee_together %>%
    mutate(selected = "employee")) %>%
  group_by(selected) %>%
  top_n(15, similarity) %>%
  ungroup() %>%
  mutate(token = reorder(token, similarity)) %>%
  ggplot(aes(token, similarity, fill = selected)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~selected, scales = "free") +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = NULL, title = "What word vectors are most similar to Company and Employees?",
    subtitle = "Calculated using counts and matrix factorization"
  )

#' 
#' Using the `word_vectors`, we can apply some mathematical functions, such as $+$ and $-$. The typical example is: $King - Man + Woman = Queen$. Let's see an example with our data set.
#' 
## ---- warning=FALSE--------------------------------------------------------------------------------
mystery_product <- word_vectors["company", ] - word_vectors["motivates", ]
new_words <- search_proximity(word_vectors, mystery_product)
new_words

#' 
#' 
#' # Predicting model based on Word Embedding Models
#' 
#' After introducing some concepts, let's try to build a model to predict the kind of opinion based on the employee comments. As we want to predict the value of a categorical variable (positive and negative), we will use a simple logistic regression model.
#' 
#' We will follow different approaches to build, train and test our logistic regression model:
#' 
#' * A model based on a 'context window' with a length of 4 words
#' * A model based on a 'context window' with a length of 6 words
#' 
#' 
#' ## Predicting model with a 'context window' with a length of 4 words
#' 
#' The first step is to create our word vector with a 'context window' of four words, as we have seen before.
#' 
## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------
set.seed(10)

tidy_skipgrams <- opinion_non_stop %>%
  unnest_tokens(ngram, comment, token = "ngrams", n = 4) %>%
  filter(ngram != "") %>%
  mutate(ngramID = row_number()) %>%
  tidyr::unite(skipgramID, commentID, ngramID) %>%
  unnest_tokens(word, ngram)

unigram_probs <- opinion_non_stop %>%
  unnest_tokens(word, comment) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

skipgram_probs <- tidy_skipgrams %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob <- skipgram_probs %>%
  filter(n > 3) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
    select(word1 = word, p1 = p),
  by = "word1"
  ) %>%
  left_join(unigram_probs %>%
    select(word2 = word, p2 = p),
  by = "word2"
  ) %>%
  mutate(p_together = p / p1 / p2)

pmi_matrix <- normalized_prob %>%
  mutate(pmi = log10(p_together)) %>%
  cast_sparse(word1, word2, pmi)

pmi_matrix@x[is.na(pmi_matrix@x)] <- 0

pmi_svd <- irlba(pmi_matrix, 50, maxit = 500)
word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)

my_embeddings_vector <- tibble(
  tokens = row.names(word_vectors),
  word_vectors %>% as_tibble()
)

#' 
#' To compare all our analyses' results, we have decided to set the seed to 10210 (this number is random). 
#' 
## ---- message=FALSE--------------------------------------------------------------------------------
set.seed(10210)

#' 
#' Before building our model, we need to split up our data set into a training data set and a testing data set. In this case, we have decided that the training data set contains the 75% of the original data set and to stratify the samples based on whether the opinion is positive or negative. 
#' 
## --------------------------------------------------------------------------------------------------
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
## --------------------------------------------------------------------------------------------------
data_rec <- recipe(formula = assessment ~ comment, data = train_data) %>%
  step_tokenize(comment) %>%
  step_stopwords(comment, keep = FALSE) %>%
  step_word_embeddings(comment, embeddings = my_embeddings_vector) %>%
  prep(training = train_data)
data_rec

#' 
#' After the initial definition of the recipe, we can add new processes:
#' 
#' * To tokenize the data set as we have seen before. As we haven't indicated anything, the function will tokenize the data set by words (1-gram). 
#' * To remove the stop words. 
#' * To evaluate the words (tokens) by our world embedded model.
#' * To prepare our data set based on the previous steps. 
#' 
#' Now, we need to carry out (bake) our recipe with our data sets.
#' 
## --------------------------------------------------------------------------------------------------
train_baked <- data_rec %>% bake(new_data = train_data)
test_baked <- data_rec %>% bake(new_data = test_data)

#' 
#' With the baked recipe, we can start working in the predictive model. As we have seen before, we have decided to use a logistics regression model. To create the predictive model structure, we need to define two elements:
#' 
#' * The type of model and the mode - As we explained before, we will use a logistic regression for classification
#' * The computational engine - There are several options, but here we have decided to use `glmnet`
#' 
## --------------------------------------------------------------------------------------------------
glm_model <- logistic_reg(mode = "classification", mixture = 0, penalty = 0.1) %>%
  set_engine("glmnet")
glm_model

#' 
#' Now, it's time to fit the predictive model structure to our training data set, so we need two define two elements:
#' 
#' * The independent and dependent variables of the model - The independent variable is $assessment$, and the dependent variable is $comment$
#' * The data set to fit - As we are building the model, we will use the training data set
#' 
## --------------------------------------------------------------------------------------------------
final_model <- glm_model %>%
  fit(assessment ~ ., data = train_baked)

#' 
#' The final step is to assess the performance of the model. The most straightforward way is to show the actual and predictive values of the testing data set.
#' 
## --------------------------------------------------------------------------------------------------
predictions_glm <- final_model %>%
  predict(new_data = test_baked) %>%
  bind_cols(test_baked %>% select(assessment))

kable(head(predictions_glm),
  col.names = c("Predictive Values", "Actual Values"),
  table.attr = "style='width:40%;'",
  caption = "Comparison between actual and predicted values"
) %>%
  kable_styling(position = "center")

#' 
#' We can see that not all predictive values fit the actual values. Another better way to show the results is the Confusion Matrix, where we can see the number of false positives, false negatives, true positives, and true negatives. 
#' 
## --------------------------------------------------------------------------------------------------
predictions_glm %>%
  conf_mat(assessment, .pred_class) %>%
  pluck(1) %>%
  as_tibble() %>%
  ggplot(aes(Prediction, Truth, alpha = n, fill = c("1", "2", "3", "4"))) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = n), colour = "white", alpha = 1, size = 8)

#' 
#' Another way to evaluate the predictive model is to assess the accuracy. In other words, the fraction of predictions the model got right.
#' 
## --------------------------------------------------------------------------------------------------
predictions_glm %>%
  metrics(assessment, .pred_class) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy")

#' 
#' As we can see, the results is not very good (66.7%).
#' 
#' We can also assess the predictive model with the ROC curve:
#' 
## --------------------------------------------------------------------------------------------------
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
#' ## Predicting model with a 'context window' with a length of 6 words
#' 
#' Now, we are going to evaluate what happens when we use a 'context window' of six words. The first step is to create a new word vector.
#' 
## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------
set.seed(10)

tidy_skipgrams <- opinion_non_stop %>%
  unnest_tokens(ngram, comment, token = "ngrams", n = 6) %>%
  filter(ngram != "") %>%
  mutate(ngramID = row_number()) %>%
  tidyr::unite(skipgramID, commentID, ngramID) %>%
  unnest_tokens(word, ngram)

unigram_probs <- opinion_non_stop %>%
  unnest_tokens(word, comment) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

skipgram_probs <- tidy_skipgrams %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob <- skipgram_probs %>%
  filter(n > 3) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
    select(word1 = word, p1 = p),
  by = "word1"
  ) %>%
  left_join(unigram_probs %>%
    select(word2 = word, p2 = p),
  by = "word2"
  ) %>%
  mutate(p_together = p / p1 / p2)

pmi_matrix <- normalized_prob %>%
  mutate(pmi = log10(p_together)) %>%
  cast_sparse(word1, word2, pmi)

pmi_matrix@x[is.na(pmi_matrix@x)] <- 0

pmi_svd <- irlba(pmi_matrix, 25, maxit = 500)
word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)

my_embeddings_vector <- tibble(
  tokens = row.names(word_vectors),
  word_vectors %>% as_tibble()
)

#' 
#' To start, we define the same seed again. 
#' 
## --------------------------------------------------------------------------------------------------
set.seed(10210)

#' 
#' We split up the data set again.
#' 
## --------------------------------------------------------------------------------------------------
opinion_split <- initial_split(opinions, strata = "assessment", p = 0.75)
train_data <- opinion_split %>% training()
test_data <- opinion_split %>% testing()

#' 
#' The pre-process of our data set is the same as we have seen before, but now we are going to use the new word vector.
#' 
## --------------------------------------------------------------------------------------------------
data_rec <- recipe(formula = assessment ~ comment, data = train_data) %>%
  step_tokenize(comment) %>%
  step_stopwords(comment, keep = FALSE) %>%
  step_word_embeddings(comment, embeddings = my_embeddings_vector) %>%
  prep(training = train_data)
data_rec

#' 
#' Now, we need to carry out (bake) our recipe with our data sets.
#' 
## --------------------------------------------------------------------------------------------------
train_baked <- data_rec %>% bake(new_data = train_data)
test_baked <- data_rec %>% bake(new_data = test_data)

#' 
#' Now, it's time to define the model structure, to fit the predictive model to our training data, and predict the values of the testing data set. 
#' 
## --------------------------------------------------------------------------------------------------
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
## --------------------------------------------------------------------------------------------------
predictions_glm %>%
  conf_mat(assessment, .pred_class) %>%
  pluck(1) %>%
  as_tibble() %>%
  ggplot(aes(Prediction, Truth, alpha = n, fill = c("1", "2", "3", "4"))) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = n), colour = "white", alpha = 1, size = 8)

#' 
#' If we prefer to assess the model by the accuracy, the result is 58.3%. It's not a good predictor.  
#' 
## --------------------------------------------------------------------------------------------------
predictions_glm %>%
  metrics(assessment, .pred_class) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy")

#' 
#' Finally, we can assess the predictive model with the ROC curve:
#' 
## --------------------------------------------------------------------------------------------------
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
#' # Conclusions
#' 
#' This document has built two simple models to predict how employees feel about their organizations from their comments. We decided to use a logistic regression model and word embedding vector with two different 'context windows'. None of them has given us an excellent result. The first one could be interesting, but not the second one. We need to remember that word embedding models are fine to predict concurrence words, not for predicting other issues.
#' 
#' # References
#' 
#' 
#' Mikolov, T; Chen, K.; Corrado, G.; Deam. J. (2013). Efficient Estimation of Word Representations in Vector Space. Retrieved from [https://arxiv.org/pdf/1301.3781.pdf](https://arxiv.org/pdf/1301.3781.pdf)
#' 
#' Ball, C. (2018) Word Embeddings. Retrieved from [https://cbail.github.io/textasdata/word2vec/rmarkdown/word2vec.html](https://cbail.github.io/textasdata/word2vec/rmarkdown/word2vec.html)
#' 
#' Silge, J. (2017). Word Vectors with tidy data principles. Retrieved from [https://juliasilge.com/blog/tidy-word-vectors/](https://juliasilge.com/blog/tidy-word-vectors/)
#' 
