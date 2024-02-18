#' 10-Fold Cross-Validation for Topic Modeling
#'
#' Run a 10-fold CV for topic modeling using the `topicmodels` package. The method used is Gibbs sampling and the function will return the perplexity, log-likelihood, and topic coherence for each value of k (number of topics) in the candidate_k vector. The function will return the mean and the confidence intervals for each measure in a Data Frame.
#' @param dtm The document term matrix.
#' @param range_of_k Range of topic numbers to test using LDA model.
#' @param burnin_value The number of iterations to discard at the beginning of the MCMC sampling process.
#' @param iter_value The total number of iterations for the MCMC sampling process.
#' @param keep_value The thinning parameter for the MCMC sampling process.
#' @param set_seed The seed used to ensure replicability.
#' @return A Data Frame with the mean and confidence intervals for all three metrics: perplexity, likelihood, and semantic coherence.
#' @examples
#' results <- tenFoldCV(dtm, seq(3,30,1), 500, 1000, 100, 444)
#' @export
tenFoldCV <- function(dtm, range_of_k, burnin_value, iter_value, keep_value,
                      set_seed){

  print("Setting up 10-fold CV")

  # storing values for later
  tdtm <- dtm
  n <- nrow(tdtm)
  burnin <- burnin_value
  iter <- iter_value
  keep <- keep_value
  seed <- set_seed

  # keep three computer cores free for other tasks
  cluster <- makeCluster(detectCores(logical = TRUE) - 3)
  registerDoParallel(cluster)

  # loading proper libraries for each cluster node
  clusterEvalQ(cluster, {
    library(topicmodels)
    library(topicdoc)
  })

  # specify the number of folds
  folds <- 10
  splitfolds <- sample(1:folds, n, replace = TRUE)
  candidate_k <- range_of_k

  print("Commencing cluster export")
  # candidates for how many topics
  clusterExport(cluster, c("tdtm", "burnin", "iter", "keep", "splitfolds",
                           "folds", "candidate_k"), envir=environment())

  print("Initiating loops for each k value")
  system.time({
    results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar% {
      k <- candidate_k[j]
      results_1k <- matrix(0, nrow = folds, ncol = 4)
      colnames(results_1k) <- c("k", "perplexity", "logLikelihood",
                                "topic_coherence")
      for(i in 1:folds){
        train_set <- tdtm[splitfolds != i , ]
        valid_set <- tdtm[splitfolds == i, ]

        fitted <- LDA(train_set, k = k, method = "Gibbs",
                      control = list(burnin = burnin, iter = iter, keep = keep,
                                     seed = seed))
        results_1k[i,] <- c(k,
                            perplexity(fitted, newdata = valid_set),
                            as.numeric(logLik(fitted)),
                            mean(topic_coherence(fitted, train_set,
                                                 top_n_tokens = 10,
                                                 smoothing_beta = 1)))
      }
      return(results_1k)
    }
  })
  stopCluster(cluster)

  print("Storing results in data frame")
  # Note: now that function is done, create data frame out of function results
  results_df <- as.data.frame(results)

  print("Calculating mean and confidence intervals")
  # calculate mean and confidence intervals
  results_df %>%
    group_by(k) %>%
    summarise(mean_perplexity = mean(perplexity),
              sd_perplexity = sd(perplexity),
              mean_logLikelihood = mean(logLikelihood),
              sd_logLikelihood = sd(logLikelihood),
              mean_coherence = mean(topic_coherence),
              sd_coherence = sd(topic_coherence)) -> results_summary

  results_summary %>%
    transmute(se_perplexity = sd_perplexity/sqrt(folds),
              se_logLikelihood = sd_logLikelihood/sqrt(folds),
              se_coherence = sd_coherence/sqrt(folds),
              mean_perplexity, mean_logLikelihood,
              mean_coherence, k) -> results_summary

  results_summary %>%
    mutate(lowCI_perplexity = mean_perplexity - 1.96 * se_perplexity,
           highCI_perplexity = mean_perplexity + 1.96 * se_perplexity,
           lowCI_logLikelihood = mean_logLikelihood - 1.96 * se_logLikelihood,
           highCI_logLikelihood = mean_logLikelihood + 1.96 * se_logLikelihood,
           lowCI_coherence = mean_coherence - 1.96 * se_coherence,
           highCI_coherence = mean_coherence + 1.96 * se_coherence) ->
    results_summary

  # remove se columns
  results_summary <- results_summary %>%
    select(-starts_with("se_"))

  # rearrange columns
  results_summary <- results_summary %>%
    select(k, mean_perplexity, lowCI_perplexity, highCI_perplexity,
           mean_logLikelihood, lowCI_logLikelihood, highCI_logLikelihood,
           mean_coherence, lowCI_coherence, highCI_coherence)

  print("Cross-validation complete")

  return(results_summary)
}

#' Plotting Perplexity Measure
#'
#' Plot perplexity results. For best results, do not change column names.
#' @param results The Data Frame containing the CV results
#' @return A plot showing the mean and lower/upper bounds for each topic number included.
#' @examples
#' plot <- plot_perplexity(results)
#' @export
plot_perplexity <- function(results){
  ggplot(results, aes(x=k)) +
    geom_point(aes(y = mean_perplexity), shape = 19, size=2) +
    geom_line(aes(y = mean_perplexity), size=0.5) +
    geom_errorbar(aes(ymin = lowCI_perplexity, ymax = highCI_perplexity),
                  width = 0.7 ) +
    theme_bw() +
    theme(panel.border = element_rect(color = "black", size = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black")) +
    ggtitle("Model Perplexity\n") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    labs(x = "\nK = number of topics",
         y = "Perplexity measure\n") +
    scale_x_continuous(breaks = unique(results$k)) +
    scale_y_continuous(breaks = pretty_breaks(n = 10))
}

#' Plotting Likelihood Measure
#'
#' Plot likelihood results. For best results, do not change column names.
#' @param results The Data Frame containing the CV results
#' @return A plot showing the mean and lower/upper bounds for each topic number included.
#' @examples
#' plot <- plot_likelihood(results)
#' @export
plot_likelihood <- function(results){
  ggplot(results, aes(x=k)) +
    geom_point(aes(y = mean_logLikelihood), shape = 19, size=2) +
    geom_line(aes(y = mean_logLikelihood), size=0.5) +
    geom_errorbar(aes(ymin = lowCI_logLikelihood,
                      ymax = highCI_logLikelihood), width = 0.7 ) +
    theme_bw() +
    theme(panel.border = element_rect(color = "black", size = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black")) +
    ggtitle("Model Likelihood\n") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    labs(x = "\nK = number of topics",
         y = "Held out likelihood measure\n") +
    scale_x_continuous(breaks = unique(results$k)) +
    scale_y_continuous(breaks = pretty_breaks(n = 10))
}

#' Plotting Coherence Measure
#'
#' Plot coherence results. For best results, do not change column names.
#' @param results The Data Frame containing the CV results
#' @return A plot showing the mean and lower/upper bounds for each topic number included.
#' @examples
#' plot <- plot_coherence(results)
#' @export
plot_coherence <- function(results){
  ggplot(results, aes(x=k)) +
    geom_point(aes(y = mean_coherence), shape = 19, size=2) +
    geom_line(aes(y = mean_coherence), size=0.5) +
    geom_errorbar(aes(ymin = lowCI_coherence, ymax = highCI_coherence),
                  width = 0.7 ) +
    theme_bw() +
    theme(panel.border = element_rect(color = "black", size = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black")) +
    ggtitle("Model Semantic Coherence\n") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    labs(x = "\nK = number of topics",
         y = "Coherence measure\n") +
    scale_x_continuous(breaks = unique(results$k)) +
    scale_y_continuous(breaks = pretty_breaks(n = 10))
}
