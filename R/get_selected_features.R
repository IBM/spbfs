#' Sub-population-based feature selection
#'
#' The spbfs package contains a function that performs sub-population-based feature selection. The function is applicative to data frames that contain one binary outcome and any number of features (categorical and/or continuous). The function relies on following 4 steps as follows (also highlighted in the figure below). The first step requires specifying knowledge-driven sampling parameters to be used, including propensity score related parameters. In the second step, propensity score matching is applied on the whole population given the specified parameters to identify a homogeneous sub-population of patients with different outcomes. Note that the propensity matching is applied on the outcome variable as the matching variable and not the treatment variable (which is the more typical scenario in propensity score matching). In the third step, features not used to match the sub-population are sorted by an importance criterion (e.g., ascending P values). The features that pass an importance criterion threshold are considered as candidates for final selection. Steps 2 and 3 are repeated a pre-defined number of times; each run samples a different sub-population, and the resulting selected features are given a reward. As a final step, the features that exceed a reward selection threshold across all runs are selected as the final set of features.
#' @param DATA_FRAME A data frame with feature columns and an outcome. Features could be either binary or continuous. Outcome variable must be binary. No missing values are allowed.
#' @param FEATURE_NAMES Names of features that exist in the data-frame.
#' @param OUTCOME_VAR_NAME Name of the outcome variable that exists in the data-frame.
#' @param NUM_ITERATIONS Total number of times in which candidate features are selected and ranked (an integer between 1 to 1000). Defaults to 100.
#' @param NUM_RANDOM_VARIABLES_FOR_MATCHING Total number of possible randomly selected features that are used to find matched cases and controls (an integer between 1 to the total number of input features). Defaults to 3.
#' @param FINAL_SELECTION_THRESHOLD Lower bound for selecting the final list of features (a float between 0 to 1). Only features that exceed the threshold are selected. Defaults to 0.5.
#' @param CALIPER_VALUE A statistical standard upper bound threshold indicating the highest allowed standard deviation for each feature used for matching given the matched cases and controls (a float between 0 to 1). Defaults to 0.1.
#' @param M_value Total number of controls that are matched per case (an integer between 1 to 10). Defaults to 1.
#' @param P_value_threshold P-value threshold used to compare each candidate feature using univariate analysis (a float between 0 to 1). Defaults to 0.001.
#' @param VERBOSE Enabling the presentation of matched populations in run-time (0 or 1). Defaults to 0.
#' @return A data-frame with selected features ranked by importance values (0 = no importance; 1 = highest importance). The data frame has two columns named “Feature_Name” and “Frequency” and as many rows as the number of selected features.
#' @import validate
#' @import stats
#' @import utils
#' @import tableone
#' @import splitstackshape
#' @import Matching
#' @import plyr
#' @export
get_selected_features <- function(DATA_FRAME, FEATURE_NAMES, OUTCOME_VAR_NAME, NUM_ITERATIONS = 100, NUM_RANDOM_VARIABLES_FOR_MATCHING = 3, FINAL_SELECTION_THRESHOLD = 0.5, CALIPER_VALUE = 0.1, M_value = 1, P_value_threshold = 0.001, VERBOSE = 0) {

  #Install dependency packages (if not installed already).
  requiredPackages = c('validate', 'Matching', 'tableone', 'splitstackshape', 'plyr')
  for(p in requiredPackages){
    if(!require(p,character.only = TRUE)) install.packages(p)
  }

  for(p in requiredPackages){
    library(p,character.only = TRUE)
  }

  #User input validation.
  if
  ((in_range(NUM_ITERATIONS, 1, 1000) && (NUM_ITERATIONS%%1==0)) == FALSE ||
   (in_range(NUM_RANDOM_VARIABLES_FOR_MATCHING, 1, length(FEATURE_NAMES)) && (NUM_RANDOM_VARIABLES_FOR_MATCHING%%1==0)) == FALSE ||
   in_range(FINAL_SELECTION_THRESHOLD, 0, 1) == FALSE ||
   in_range(CALIPER_VALUE, 0, 1) == FALSE ||
   (in_range(M_value, 1, 10) && (M_value%%1==0)) == FALSE ||
   in_range(P_value_threshold, 0, 1) == FALSE ||
   (in_range(VERBOSE, 0, 1) && (VERBOSE%%1==0)) == FALSE)  {
    print('Input noncompliant. Make sure that all input elements are valid.')
    invokeRestart("abort")
  }
  else
  {
    #Applying sub-population-based feature selection.
    {
      df_feature_selection_1<-NULL
      all_relevant_features <- FEATURE_NAMES

      for(i in 1:NUM_ITERATIONS) #Each iteration samples 2 sub-populations of positive and negative patients.
      {
        sampled_features = sample(all_relevant_features, NUM_RANDOM_VARIABLES_FOR_MATCHING) #Sampling is based on propensity score matching applied on the outcome variable, using a randomly selected sub-set of features.

        feature_str_1 = "";
        for (feature in sampled_features)
        {
          feature_str_1 = paste(feature_str_1, feature, sep = "+")
        }

        feature_str_1 = paste(OUTCOME_VAR_NAME, feature_str_1, sep = "~") #Concatinating a string with the randomly selected features and the outome variable.

        psmodel<-glm(feature_str_1,
                     family = binomial(), data = DATA_FRAME) #Creating a propensity score model.

        pscore = predict(psmodel, DATA_FRAME, type="response", na.action = na.pass) #Calculating a probability for all patients.

        logit_pscore = log(pscore / (1 - pscore))

        psmatch = Match(Tr = DATA_FRAME[[OUTCOME_VAR_NAME]], M = M_value, X = logit_pscore, replace = FALSE, caliper = CALIPER_VALUE) #Applying propensity score matching.

        matched <- DATA_FRAME[unlist(psmatch[c("index.treated", "index.control")]), ] #Creating a dataframe with the matched cases and controls.

        tab1_matched_population = CreateTableOne(vars = all_relevant_features, strata = OUTCOME_VAR_NAME, #Creating a "Table 1" for the matched dataframe.
                                                 data = matched, test = TRUE)

        if (VERBOSE == 1)
        {
          tab1_matched_population_df_1 <- print(tab1_matched_population, printToggle = TRUE);
        }
        else
        {
          tab1_matched_population_df_1 <- print(tab1_matched_population, printToggle = FALSE);
        }

        #String manipulation and counting operations.
        tab1_matched_population_df_2 = data.frame(tab1_matched_population_df_1)
        tab1_matched_population_df_3 = cSplit(tab1_matched_population_df_2, c("X0", "X1"), c("(", "("), drop = TRUE)
        tab1_matched_population_df_3[, 2] <- NULL

        colnames(tab1_matched_population_df_3)[2] <- "Cases_Mean"
        colnames(tab1_matched_population_df_3)[3] <- "Cases_SD"
        colnames(tab1_matched_population_df_3)[4] <- "Controls_Mean"
        colnames(tab1_matched_population_df_3)[5] <- "Controls_SD"

        tab1_matched_population_df_3$Cases_SD <- gsub(')', '', tab1_matched_population_df_3$Cases_SD)
        tab1_matched_population_df_3$Controls_SD <- gsub(')', '', tab1_matched_population_df_3$Controls_SD)

        row_names = data.frame(row.names(tab1_matched_population_df_1))
        tab1_matched_population_df_4 = cbind(row_names, tab1_matched_population_df_3)
        colnames(tab1_matched_population_df_4)[1] <- "Variable"
        tab1_matched_population_df_4$col3 <- tab1_matched_population_df_4$Cases_Mean / tab1_matched_population_df_4$Controls_Mean
        colnames(tab1_matched_population_df_4)[7] <- "Ratio_Means"

        #Converting the low default p-value of CreateTableOne (a string) to 0. This allows the user to choose a p-value parameter.
        tab1_matched_population_df_4$p<-replace(tab1_matched_population_df_4$p, tab1_matched_population_df_4$p == '<0.001', 0)

        tmp_df_1 = tab1_matched_population_df_4[which(as.double(tab1_matched_population_df_4$p) < P_value_threshold),];
        tmp_df_2 = data.frame(tmp_df_1$Variable)
        tmp_df_1$Variable <- gsub(" (mean (SD))","", tmp_df_1$Variable)
        df_feature_selection_1 <- rbind(df_feature_selection_1,data.frame(tmp_df_1$Variable))

        iter_str_1 <- paste0(as.character(i), " out of ", as.character(NUM_ITERATIONS), " iterations completed.")
        print(iter_str_1)
      }
    }

    #Applying final processing steps and presenting a list of selected features ranked by importance.
    {
      df_feature_selection_2 <- count(df_feature_selection_1, c(df_feature_selection_1$Variable))
      df_feature_selection_3 <- df_feature_selection_2[(order(-df_feature_selection_2$freq)),]
      df_feature_selection_3$tmp_df_1.Variable = gsub(" mean"," ",df_feature_selection_3$tmp_df_1.Variable)
      df_feature_selection_3$tmp_df_1.Variable <- gsub(" .*","",df_feature_selection_3$tmp_df_1.Variable)
      df_feature_selection_3$freq <- df_feature_selection_3$freq / NUM_ITERATIONS
      df_feature_selection_4 = df_feature_selection_3[which(df_feature_selection_3$freq > FINAL_SELECTION_THRESHOLD),];
      row.names(df_feature_selection_4) <- NULL
      colnames(df_feature_selection_4) <- c('Feature_Name', 'Frequency')
      print(df_feature_selection_4)
    }

    return(df_feature_selection_4)
  }
}
