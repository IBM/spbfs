<!-- This should be the location of the title of the repository, normally the short name -->
# spbfs

## Scope

Sub-population-based feature selection. The spbfs package contains a function that performs sub-population-based feature selection. The function is applicative to data frames that contain one binary outcome and any number of features (categorical and/or continuous). The function relies on following 4 steps as follows. The first step requires specifying knowledge-driven sampling parameters to be used, including propensity score related parameters. In the second step, propensity score matching is applied on the whole population given the specified parameters to identify a homogeneous sub-population of patients with different outcomes. Note that the propensity matching is applied on the outcome variable as the matching variable and not the treatment variable (which is the more typical scenario in propensity score matching). In the third step, features not used to match the sub-population are sorted by an importance criterion (e.g., ascending P values). The features that pass an importance criterion threshold are considered as candidates for final selection. Steps 2 and 3 are repeated a pre-defined number of times; each run samples a different sub-population, and the resulting selected features are given a reward. As a final step, the features that exceed a reward selection threshold across all runs are selected as the final set of features.

## Usage

spbfs(DATA_FRAME, FEATURE_NAMES, OUTCOME_VAR_NAME, NUM_ITERATIONS = 100, NUM_RANDOM_VARIABLES_FOR_MATCHING = 3, FINAL_SELECTION_THRESHOLD = 0.5, CALIPER_VALUE = 0.1, M_value = 1, P_value_threshold = 0.001, VERBOSE = 0)

## License

TBD

## Authors

Uri Kartoun, Kenney Ng
