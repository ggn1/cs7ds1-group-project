# An Evaluation of Accuracy, Overdispersion and Zero-Inflation in Count Regression Trees

This repository is the codebase associated with our work "An Evaluation of Accuracy, Overdispersion and Zero-Inflation in Count Regression Trees".

In order to run all experiments conducted as part of the aforementioned study, please run the `main.R` file.

Following are short descriptions associated with some key files that implement the CORE decision tree algorithm.
* `tree_building.R`: This file contains a function `create_tree(...)` that builds the CORE decision tree, which is "tree building".
* `model_selection.R`: This file contains the `get_best_model(...)` and function as well as some helper functions. These facilitate the first step in the iterative tree building process that CORE follows, which is "model selection". This involves selecting the best GLM to fit the current node of the tree.
* `split_variable_selection.R`: This file contains the `get_split_variable(...)` function and other helper functions. These facilitate the second step in the iterative tree building process that CORE follows, which is "split variable selection". This involves selecting the best feature to split the tree using.
* `split_set_selection.R`: This file contains the `get_split_set(...)` function and other helper functions that facilitae the third step in the iterative tree building process of CORE involving selection of a suitable split condition for selected split variable. This step is called which is "split set selection".
* Files `evaluation.R` and `evaluation_cv.R` contains code that performs 10 fold cross evaluation and computes all metrics that were used to compare performance of various algorithms on this dataset.
* Files `preprocessing.R` and `preliminary-analysis.R` contain steps undertaken as part of exploratory data analysis and cleaning of the data set `data.csv` to produce `data_clean.csv` as found in the `./Data` folder.
* The `helper_functions.R` file contain some common functions that all other files may find useful and need to leverage.