install.packages("C:/Temp/spbfs_1.0.tar.gz",
                 repos = NULL,
                 type = "source")
library(spbfs)

#Pre-requirment libraries

#Importing an example database, "The Pima Indians Diabetes". Specifying an initial set of features and an outcome.
{
  library(mlbench)
  data(PimaIndiansDiabetes)
  DATA_FRAME <- PimaIndiansDiabetes
  FEATURE_NAMES = c('pregnant', 'glucose', 'pressure', 'triceps', 'insulin', 'mass', 'pedigree', 'age')
  OUTCOME_VAR_NAME = 'diabetes'
  DATA_FRAME$diabetes<-ifelse(DATA_FRAME$diabetes == "pos", 1, 0) #Converting string labels to binary (1, 0).
}

#Applying sub-population-based feature selection.
{
  Results <- spbfs(DATA_FRAME = DATA_FRAME,
                   FEATURE_NAMES = FEATURE_NAMES,
                   OUTCOME_VAR_NAME = OUTCOME_VAR_NAME,
                   NUM_ITERATIONS = 100,
                   NUM_RANDOM_VARIABLES_FOR_MATCHING = 3,
                   FINAL_SELECTION_THRESHOLD = 0.0,
                   CALIPER_VALUE = 0.1,
                   M_value = 1,
                   P_value_threshold = 0.001,
                   VERBOSE = 0)
}

#Plotting results.
{
  Results_for_plot <- Results[order(Results$Frequency),]
  barplot(Results_for_plot$Frequency,
          horiz = TRUE,
          names.arg = Results_for_plot$Feature_Name,
          las = 1, cex.names = 0.7, cex.axis = 0.7, xlim = c(0, 1),
          xlab = 'Importance')
}
