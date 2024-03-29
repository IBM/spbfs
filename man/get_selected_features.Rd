% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Main.R
\name{get_selected_features}
\alias{get_selected_features}
\title{Sub-population-based feature selection}
\usage{
get_selected_features(
  DATA_FRAME,
  FEATURE_NAMES,
  OUTCOME_VAR_NAME,
  NUM_ITERATIONS = 100,
  NUM_RANDOM_VARIABLES_FOR_MATCHING = 3,
  FINAL_SELECTION_THRESHOLD = 0.5,
  CALIPER_VALUE = 0.1,
  M_value = 1,
  P_value_threshold = 0.001,
  VERBOSE = 0
)
}
\arguments{
\item{DATA_FRAME}{A data frame with feature columns and an outcome. Features could be either binary or continuous. Outcome variable must be binary. No missing values are allowed.}

\item{FEATURE_NAMES}{Names of features that exist in the data-frame.}

\item{OUTCOME_VAR_NAME}{Name of the outcome variable that exists in the data-frame.}

\item{NUM_ITERATIONS}{Total number of times in which candidate features are selected and ranked (an integer between 1 to 1000). Defaults to 100.}

\item{NUM_RANDOM_VARIABLES_FOR_MATCHING}{Total number of possible randomly selected features that are used to find matched cases and controls (an integer between 1 to the total number of input features). Defaults to 3.}

\item{FINAL_SELECTION_THRESHOLD}{Lower bound for selecting the final list of features (a float between 0 to 1). Only features that exceed the threshold are selected. Defaults to 0.5.}

\item{CALIPER_VALUE}{A statistical standard upper bound threshold indicating the highest allowed standard deviation for each feature used for matching given the matched cases and controls (a float between 0 to 1). Defaults to 0.1.}

\item{M_value}{Total number of controls that are matched per case (an integer between 1 to 10). Defaults to 1.}

\item{P_value_threshold}{P-value threshold used to compare each candidate feature using univariate analysis (a float between 0 to 1). Defaults to 0.001.}

\item{VERBOSE}{Enabling the presentation of matched populations in run-time (0 or 1). Defaults to 0.}
}
\value{
A data-frame with selected features ranked by importance values (0 = no importance; 1 = highest importance). The data frame has two columns named “Feature_Name” and “Frequency” and as many rows as the number of selected features.
}
\description{
The spbfs package contains a function that performs sub-population-based feature selection. The function is applicative to data frames that contain one binary outcome and any number of features (categorical and/or continuous). The function relies on following 4 steps as follows (also highlighted in the figure below). The first step requires specifying knowledge-driven sampling parameters to be used, including propensity score related parameters. In the second step, propensity score matching is applied on the whole population given the specified parameters to identify a homogeneous sub-population of patients with different outcomes. Note that the propensity matching is applied on the outcome variable as the matching variable and not the treatment variable (which is the more typical scenario in propensity score matching). In the third step, features not used to match the sub-population are sorted by an importance criterion (e.g., ascending P values). The features that pass an importance criterion threshold are considered as candidates for final selection. Steps 2 and 3 are repeated a pre-defined number of times; each run samples a different sub-population, and the resulting selected features are given a reward. As a final step, the features that exceed a reward selection threshold across all runs are selected as the final set of features.
}
