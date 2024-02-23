## Introduction

The `topicmodelCV` package allows a user to conduct a 10-fold cross validation method on determine the optimal number of k topics to use when wanting to implement a Latent Dirichlet Allocation (LDA) topic model.
All that the package requires is for the user to insert a pre-processed document-term matrix (dtm) along with other control parameters that are fed to the `tenFoldCV()` function in order to split the dtm into 10 folds, run multiple LDA topic models using the `LDA()` function from the `topicmodels` package, and calculate the models' perplexity, held out likelihood, and semantic coherence (using the `topicdoc` package) for every k topic tested. 
The results for each k topic are then organized and given back to the user in a Data Frame in order to plot them out and compare.
The package also includes functions to plot the results for ease of use (i.e., `plot_perplexity()`, `plot_likelihood()`, and `plot_coherence()`), however, the data can be used to make ones own plots.

## Code Snippets

The following are examples of each function in the package and how to use it.

If a user wants to calculate the optimal k for their topic model:

```{r}
results <- tenFoldCV(dtm, # document-term matrix obtained by pre-processing data
                     range_of_k, # range of topic numbers the user wishes to test
                     burnin_value, iter_value, keep_value, # control parameters that feed into LDA function
                     set_seed) # seed value used for reproducibility
 ```
Once the results are obtained, the user can either plot the data on their own, or use the following built-in functions:

```{r}
plot_perplexity(results)

plot_likelihood(results)

plot_coherence(results)
```

## Sources 

The following are the sources I used to construct the package:

1. Barberá, P. 2021. USC POIR 613 - Computational Social Science Course. University of Southern California, Political Science and International Relations.
2. Brownlee, J. (2023). A Gentle Introduction to k-fold Cross-Validation. https://machinelearningmastery.com/k-fold-cross-validation/.
3. Friedman, D. 2022. `topicdoc` R package documentation. https://github.com/doug-friedman/topicdoc
4. Grün B, Hornik K (2024). topicmodels: Topic Models. R package version 0.2-16, https://CRAN.R-project.org/package=topicmodels.
5. Peter's Stats Stuff - R. (2017). Cross-Validation of Topic Modeling. https://www.r-bloggers.com/2017/01/cross-validation-of-topic-modelling/.
6. Zhang, Z. (2018). Text Mining for Social and Behavioral Research Using R: A Case Study on Teaching Evaluation. https://books.psychstat.org/textmining.


