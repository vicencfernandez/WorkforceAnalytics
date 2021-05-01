#' ---
#' title: "Student's-t test for workforce data"
#' author: "Vicenc Fernandez"
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
## ----setup, include=FALSE-------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' # The context 
#' 
#' One of the basic statistical techniques, when we are analyzing populations, is the Student's t-test. We can mainly use the Student's t-test to achieve one of the following two objectives:
#' 
#' * To evaluate a characteristic's value in a population (e.g., the average salary of my employees is 1,500 euros)
#' * To evaluate if the characteristic's value in two populations is different (e.g., the average salary of my tech employees is greater than my business employees)
#' 
#' Suppose we have information on the entire population. In that case, we don't need to carry out any test because we only need to compare the value with the reference (when we have one population) or compare the value of the two populations. But, in many cases, we don't have the complete sample. We only have a sample of the populations. In these cases, the Student's t-test allows us to evaluate the population based on the samples.
#' 
#' 
#' ## Defining hypothesws for testing
#' 
#' To carry out the Student's t-test, we need to define the null and the alternative hypotheses. On the one hand, the null hypothesis suggests that nothing is going on and everything is the same. On the other hand, the alternative hypothesis is the opposite of the null hypothesis. Let's see one example. 
#' 
#' * *H0: Null hypothesis*: The average salary of the employees is 1,500 euros
#' * *H1: Alternative hypothesis*: The average salary of the employees is different than 1,500 euros
#' 
#' ## Assumptions
#' 
#' There are two assumptions for carrying out the Student's t-test:
#' 
#' **Normality** - All samples should follow a normal distribution. To test the normality, we can plot a histogram or a QQ-plot. Another approach for normality tests is the Shapiro-Wilk and the Kolmogorov-Smirnov test. When we have samples greater than 30, it's unnecessary to analyze the normality due to the central limit theorem.
#' 
#' **Homogeneity of the variances** - Also called homoscedasticity of the variance, and we need to test it when we have two samples. To test the homoscedasticity, we can plot a boxplot or dot plot. Another approach for the homoscedasticity test is the Levene's test and the Fisher's test.
#' 
#' ## Hypotheses testing process
#' 
#' The steps for carrying out a hypothesis testing using the Student's t are:
#' 
#' 1. Test the assumptions of the sample(s)
#' 2. Formulate the null and the alternative hypotheses
#' 3. Define the maximum probability of error that we accept (Significance level)
#' 4. Carry out the Student's test
#' 5. Make a decision based on the p-value
#' 
#' 
#' # Preparing the environment
#' 
#' We need to install and load several packages to analyze our data set with text mining.
#' 
## ---- message=FALSE-------------------------------------------------------------------------------------
library(tidyverse)
library(gridExtra)

#' 
#' The `tidyverse` is an opinionated collection of R packages designed for data science. The `gridExtra`package provides a number of user-level functions to work with "grid" graphics, notably to arrange multiple grid-based plots on a page, and draw tables. 
#' 
#' ## Information of the data set
#' 
#' We have a data set with information of 550 employees in two consecutive years. The data set contains three variables:  
#' 
#' * employeeID: An integer variable with the employee ID
#' * gender: A factor variable with the employee gender (female and male)
#' * salary20: A number variable indicating the employee salary in 2020 in euros 
#' * salary21: A number variable indicating the employee salary in 2021 in euros 
#' * performance20: A number variable indicating the employee performance in 2020. The range is between 0 and 100. 
#' * performance21: A number variable indicating the employee performance in 2021. The range is between 0 and 100. 
#' 
#' 
#' ## Loading and cleaning the data set
#' 
#' The first step is to load the data set and check that everything is correct. Instead of using a standard R data.frame, we have decided to use a tibble because this makes it much easier to work with large data.
#' 
## -------------------------------------------------------------------------------------------------------
employees <- read.csv("t-test-hypotheses.csv", sep = ",") %>% as_tibble()
employees

#' 
#' We can see that the variable `gender` has been defined as a character, but we prefer to describe it as a factor. So, let's change it.
#' 
## -------------------------------------------------------------------------------------------------------
employees$gender <- as.factor(employees$gender)
employees

#' 
#' Now, our data set is ready for analysis.
#' 
#' # Scenarios for the tests
#' 
#' There are several versions of the Student's t-test depending on the number of samples, the definition of the alternative hypothesis, and the relationship between the samples. 
#' 
#' ## Scenario 01: Test one sample and two-sided
#' 
#' Let's start with the most simple test. Consider that we want to know if the average salary of female employees in 2021 is equal to or different than 1,930 euros.
#' 
## -------------------------------------------------------------------------------------------------------
female_employees <- employees %>% filter(gender == "female")
female_employees %>% summarize(mean = mean(salary21))

#' 
#' We can see that the average salary of female employees in the *sample* is 1955.849 euros. Still, we want to know if the average salary of female employees in the *population* is 1,930 euros. To achieve it, we need to carry out the Student's t-test. So, the first step is to analyze the assumptions that the sample has to satisfy.
#' 
#' The first assumption is about normality. As we have a sample greater than 30, it's not necessary. However, we are going to analyze the normality to show to do it. The first way is using a histogram plot or the QQ-plot.
#' 
## -------------------------------------------------------------------------------------------------------
tmp1 <- female_employees %>% ggplot(aes(x = salary21)) +
  geom_histogram(color = "steelblue", fill = "steelblue", binwidth = 58)

tmp2 <- female_employees %>% ggplot(aes(sample = salary21)) +
  geom_qq(color = "steelblue") + geom_qq_line()

grid.arrange(tmp1, tmp2, ncol = 2)

#' 
#' As we can see in both plots, the sample follows a normal distribution. Another approach is a more formal test. In this case, we have decided to carry out a Shapiro-Wilk test. 
#' 
## -------------------------------------------------------------------------------------------------------
female_employees$salary21 %>% shapiro.test()

#' 
#' As the p-value is big, we cannot reject the hypothesis that the sample comes from a normal distribution population. So, we can accept that the population follows a normal distribution. 
#' 
#' After checking the assumption, we can start the following step: defining the null and the alternative hypotheses. We describe the hypotheses in the following way:
#' 
#' * Null Hypothesis: The average salary of female employees in 2021 is equal to 1,930 euros 
#' * Alternative Hypothesis: The average salary of female employees in 2021 is different than 1,930 euros 
#' 
#' We can mathematically write these hypotheses.
#' 
#' * H0: $\mu_{salary} = 1930$ 
#' * H1: $\mu_{salary} \neq 1930$  
#' 
#' The third step is to define the maximum probability of error that we accept (the significance level). It's widespread to use $\alpha=0.05$ as a threshold. In other words, we don't get probabilities to make an error greater than 5%.
#' 
#' Now, it's time to carry out the Student's t-test. the function is `t.test()` carries out the Student's t-test. We need to define the following parameters: 
#' 
#' 1. The data set - In this scenario, the data set is `female_employess$salary21`
#' 2. The value to compare: `mu` - In this scenario, it's $1930$
#' 3. The type of the alternative hypothesis: `alternative` - We can face three types of alternative hypothesis: (1) different to ("two.sided"), (2) greater than ("greater"), and less than ("less"). In this scenario, we have a 'different to' hypothesis.
#' 4. The confidence level of the interval: `conf.level` - Its value is one minus the significance level. In this scenario, the confidence level is $0.95$. 
#' 
## -------------------------------------------------------------------------------------------------------
female_employees$salary21 %>% t.test(mu = 1930, alternative = "two.sided", conf.level = 0.95)

#' 
#' The p-value tells us the probability of making a mistake if we reject the null hypothesis. As the p-value is smaller than 0.05 (our significance level), we can reject the null hypothesis, and so, we can accept the alternative hypothesis. The average salary of female employees in 2021 is different than 1,930 euros.
#' 
#' ## Scenario S2: Test one sample and one-sided
#' 
#' Let's see what happens if we define our alternative hypothesis in a different way. Consider these two new hypotheses.
#' 
#' * Null Hypothesis: The average salary of female employees in 2021 is equal to 1,930 euros 
#' * Alternative Hypothesis: The average salary of female employees in 2021 is less than 1,930 euros 
#' 
#' We can mathematically write these hypotheses.
#' 
#' * H0: $\mu_{salary} = 1930$ 
#' * H1: $\mu_{salary} < 1930$  
#' 
#' In this new scenario, the alternative hypothesis is 'less than', so we need to define the parameter `alternative` as "less". 
#' 
## -------------------------------------------------------------------------------------------------------
female_employees$salary21 %>% t.test(mu = 1930, alternative = "less", conf.level = 0.95)

#' 
#' As the p-value is greater than 0.05 (our significance level), we can't reject the null hypothesis, and so, we can't accept the alternative hypothesis. The average salary of female employees in 2021 is not less than 1,930 euros.
#' 
#' ## Scenario S3: Test two independent samples and two-sided
#' 
#' Let's move on to a more complex scenario. Consider that we want to compare if the average performance of female employees is equal to or different than male employees in 2021.
#' 
## -------------------------------------------------------------------------------------------------------
female_employees <- employees %>% filter(gender == "female")
female_employees %>% summarize(mean = mean(performance21))

male_employees <- employees %>% filter(gender == "male")
male_employees %>% summarize(mean = mean(performance21))

#' 
#' We can see that the average performance of female employees of the sample is greater than male employees. But we want to know if they are different in the population. So, the first step is to check both assumptions in the two samples. 
#' 
## -------------------------------------------------------------------------------------------------------
female_employees$performance21 %>% shapiro.test()
male_employees$performance21 %>% shapiro.test()

#' 
#' In both samples, the p-value is high, so we cannot reject the hypothesis that both samples come from normal distribution populations. So, we can accept that both populations follow a normal distribution. 
#' 
#' The second assumption is about the homoscedasticity of the variance. The first way to test it is by using a boxplot.
#' 
## -------------------------------------------------------------------------------------------------------
tmp1 <- female_employees %>% ggplot(aes(x = performance21)) +
  geom_boxplot(color = "steelblue", fill = "steelblue") + 
  coord_flip() + 
  ylab ("female") 

tmp2 <- male_employees %>% ggplot(aes(x = performance21)) +
  geom_boxplot(color = "tomato", fill = "tomato") + 
  coord_flip() + 
  ylab ("male")  

grid.arrange(tmp1, tmp2, ncol = 2)

#' 
#' As we can see in the plot, the samples have homoscedasticity because they are a very similar shape. Another approach is a more formal test. In this case, we have decided to carry out a Fisher's F-test.
#' 
## -------------------------------------------------------------------------------------------------------
var.test(female_employees$performance21, male_employees$performance21, ratio = 1, alternative = "two.sided", conf.level = 0.95)

#' 
#' As the p-value is greater than 0.05, we cannot reject the hypothesis that the variance of both populations is equal. So, we can accept that both populations have the same (or very similar) variance. 
#' 
#' Following the same approach as in the previous scenarios, we need to define the null and the alternative hypotheses:
#' 
#' * Null Hypothesis: The average performance of female employees is equal to male employees in 2021 
#' * Alternative Hypothesis: The average performance of female employees is different than male employees in 2021  
#' 
#' We can mathematically write these hypotheses.
#' 
#' * H0: $\mu_{perform-female} = \mu_{perform-male}$ 
#' * H1: $\mu_{perform-female} \neq \mu_{perform-male}$  
#' 
#' The third step is to define the maximum probability of error that we accept (the significance level). Again, we define the threshold as 0.05.
#' 
#' Now, it's time to carry out the Student's t-test. the function is `t.test()` carries out the Student's t-test. We need to define the following parameters: 
#' 
#' 1. The data set of the first sample - In this scenario, the data set is `female_employees$performance21`
#' 2. The data set of the second sample - In this scenario, the data set is `male_employees$performance21`
#' 3. The type of the alternative hypothesis: `alternative` - In this scenario, we have a 'different to' hypothesis
#' 4. The relation between the samples: `paired` - In this scenario, there isn't a direct relationship between two samples, so the value is FALSE (see the following scenarios to know more)
#' 4. The relation between variances: `var.equal` - In this scenario, we have tested that both have the same variance
#' 5. The confidence level of the interval: `conf.level` - In this scenario, the confidence level is $0.95$.
#' 
#' Now you may be wondering about the fourth parameter. We said that we need homoscedasticity between both samples to carry out the Student's t-test. So, why do we have to indicate this assumption explicitly? The answer is simple. Internally, the function `t,test` has two sub-functions. If the parameter `var.equal` is TRUE, the function runs a standard Student's t-test. Else, the function is running a Welch (or Satterthwaite) t-test, where the variance of the sample doesn't need to be the same. 
#' 
## -------------------------------------------------------------------------------------------------------
t.test(female_employees$performance21, male_employees$performance21, alternative = "two.sided", paired = FALSE, var.equal = TRUE, conf.level = 0.95)

#' 
#' The p-value tells us the probability of making a mistake if we reject the null hypothesis. As the p-value is smaller than 0.05 (our significance level), we can reject the null hypothesis, and so, we can accept the alternative hypothesis. The average performance of female employees is different from than of male employees in 2021.
#' 
#' ## Scenario S4: Test two independent samples and one-sided
#' 
#' Let's see what happens if we define our alternative hypothesis differently. Consider these two new hypotheses.
#' 
#' * Null Hypothesis: The average performance of female employees is equal to male employees in 2021 
#' * Alternative Hypothesis: The average performance of female employees is greater than male employees in 2021  
#' 
#' We can mathematically write these hypotheses.
#' 
#' * H0: $\mu_{perform-female} = \mu_{perform-male}$ 
#' * H1: $\mu_{perform-female} > \mu_{perform-male}$ 
#' 
#' In this new scenario, the alternative hypothesis is 'greater than', so we need to define the parameter `alternative` as "greater". 
#' 
## -------------------------------------------------------------------------------------------------------
t.test(female_employees$performance21, male_employees$performance21, alternative = "greater", paired = FALSE, var.equal = TRUE, conf.level = 0.95)

#' 
#' As the p-value is less than 0.05 (our significance level), we can reject the null hypothesis, and so, we can accept the alternative hypothesis. The average performance of female employees is greater than male employees in 2021.
#' 
#' ## Scenario S5: Test two related samples and two-sided
#' 
#' Let's move on to a more complex scenario. Consider that we want to compare if the average performance of male employees in 2020 is equal to or different than in 2021. As you can see here, there is a direct relationship between  
#' 
## -------------------------------------------------------------------------------------------------------
male_employees <- employees %>% filter(gender == "male")
male_employees %>% summarize(mean = mean(performance20))
male_employees %>% summarize(mean = mean(performance21))

#' 
#' We can see that the average performance of male employees in both years are not the same. But we want to know if they are different in the population. So, the first step is to check both assumptions in the two samples. Let's start with normality.
#' 
## -------------------------------------------------------------------------------------------------------
male_employees$performance20 %>% shapiro.test()
male_employees$performance21 %>% shapiro.test()

#' 
#' In both samples, the p-value is high, so we cannot reject the hypothesis that both samples come from normal distribution populations. So, we can accept that both populations follow a normal distribution. 
#' 
#' The second assumption is about the homoscedasticity of the variance. In this case, we have decided to carry out the F-test.
#' 
## -------------------------------------------------------------------------------------------------------
var.test(male_employees$performance20, male_employees$performance21,  alternative = "two.sided", conf.level = 0.95)

#' 
#' As the p-value is greater than 0.05, we cannot reject the hypothesis that the variance of both populations is equal. So, we can accept that both populations have the same (or very similar) variance. 
#' 
#' Following the same approach as in the previous scenarios, we need to define the null and the alternative hypotheses:
#' 
#' * Null Hypothesis: The average performance of male employees in 2020 is equal to in 2021 
#' * Alternative Hypothesis: The average performance of male employees in 2020 is different from in 2021   
#' 
#' We can mathematically write these hypotheses.
#' 
#' * H0: $\mu_{perform-male-2020} = \mu_{perform-male-20201}$ 
#' * H1: $\mu_{perform-male-2020} \neq \mu_{perform-male-20201}$ 
#' 
#' The third step is to define the maximum probability of error that we accept (the significance level). Again, we define the threshold with 0.05.
#' 
#' Now, it's time to carry out the Student's t-test. the function is `t.test()` carries out the Student's t-test. We need to define the following parameters: 
#' 
#' 1. The data set of the first sample - In this scenario, the data set is `female_employees$performance21`
#' 2. The data set of the second sample - In this scenario, the data set is `male_employees$performance21`
#' 3. The type of the alternative hypothesis: `alternative` - In this scenario, we have a 'different to' hypothesis
#' 4. The relation between the samples: `paired` - In this scenario, we know that there is a direct relationship between two samples, so the value is TRUE.
#' 5. The relation between variances: `var.equal` - In this scenario, we have tested that both have the same variance
#' 6. The confidence level of the interval: `conf.level` - In this scenario, the confidence level is $0.95$.
#' 
#' Now you may be wondering about the fourth parameter. We said that we need homoscedasticity between both samples to carry out the Students' t-test. So, why do we have to indicate this assumption explicitly? The answer is simple. Internally, the function `t,test` has two sub-functions. If the parameter $var.equal$ is TRUE, the function runs a standard Student's t-test. Else, the function is running a Welch (or Satterthwaite) t-test, where the variance of the sample doesn't need to be the same. 
#' 
## -------------------------------------------------------------------------------------------------------
t.test(male_employees$performance20, male_employees$performance21, alternative = "two.sided", paired = TRUE, var.equal = TRUE, conf.level = 0.95)

#' 
#' The p-value tells us the probability of making a mistake if we reject the null hypothesis. As the p-value is greater than 0.05 (our significance level), we can't reject the null hypothesis, and so, we can't accept the alternative hypothesis. The average performance of male employees in 2020 is equal to in 2021.
#' 
#' ## Scenario S6: Test two related samples and one-sided
#' 
#' Let's see what happens if we define our alternative hypothesis differently with female employees. Consider these two new hypotheses.
#' 
#' * Null Hypothesis: The average performance of female employees in 2020 is equal to in 2021 
#' * Alternative Hypothesis: The average performance of female employees in 2020 is less than in 2021   
#' 
#' We can mathematically write these hypotheses.
#' 
#' * H0: $\mu_{perform-female-2020} = \mu_{perform-female-2021}$ 
#' * H1: $\mu_{perform-female-2020} < \mu_{perform-female-2021}$ 
#' 
#' In this new scenario, the alternative hypothesis is 'less than', so we need to define the parameter `alternative` as "less". 
#' 
## -------------------------------------------------------------------------------------------------------
t.test(female_employees$performance20, female_employees$performance21, alternative = "less", paired = TRUE, var.equal = TRUE, conf.level = 0.95)

#' 
#' As the p-value is less than 0.05 (our significance level), we can reject the null hypothesis, and so, we can accept the alternative hypothesis. The average performance of female employees in 2020 is less than in 2021.
#' 
#' # Conclusions
#' 
#' In the document, we have seen the different ways to use the Student's t-test. We have seen that the samples need to satisfy two criteria: a normality distribution and the homogeneity of the variances. Just two final recommendations:
#' 
#' * If the samples don't show homogeneity, you can use the Welch (or Satterthwaite) t-test by the function `t.test(var.equal = FALSE)`. 
#' * If samples don't follow a normality distribution, you can use the Mann–Whitney–Wilcoxon test by the function `wilcox.test()`.
