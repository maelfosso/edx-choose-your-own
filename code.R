

# Setup -------------------------------------------------------------------

# Packages to load
packages <- c("devtools", "tidymodels", "MASS", "dplyr", "GGally", "vtable", "mlflow", "gridExtra")

# This function installs a package if it is not present and loads it
install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}

invisible(lapply(packages, install_and_load))

tidymodels_prefer()

# Clear the memory ---------------------------------------------------------------
rm(list = ls(all.names = TRUE))

# Force garbage collection to free up memory
gc(full = TRUE)

# Download data ---------------------------------------------------------------

DATASETS_DIR <- "./datasets"

# URLs of the data to download
urls <- c(
  "https://raw.githubusercontent.com/dataafriquehub/energy_data/refs/heads/main/train.csv",
  "https://raw.githubusercontent.com/dataafriquehub/energy_data/refs/heads/main/test.csv",
  "https://raw.githubusercontent.com/dataafriquehub/energy_data/refs/heads/main/submission.csv"
)

# Name of the corresponding files
files <- c("train.csv", "test.csv", "submission.csv")

# Create 'datasets' folder if not exists
if (!dir.exists(DATASETS_DIR)) {
  dir.create(DATASETS_DIR)
}

# function for download a file
download_file <- function(url, destfile) {
  tryCatch({
    download.file(url, destfile, mode = "wb")
    message(paste("Downloaded :", destfile))
  }, error = function(e) {
    message(paste("Error when downloading", url))
  })
}

# Download each file
for (i in seq_along(urls)) {
  dest_path <- file.path(DATASETS_DIR, files[i])
  download_file(urls[i], dest_path)
}

# Load data ---------------------------------------------------------------

# Locate CSV
LOCAL_TRAIN_URL = file.path(DATASETS_DIR, "train.csv")
LOCAL_TEST_URL = file.path(DATASETS_DIR, "test.csv")
LOCAL_SUBMISSION_URL = file.path(DATASETS_DIR, "submission.csv")
LOCAL_SUBMISSION_FINAL_URL = file.path(DATASETS_DIR, "submission_nationale.csv")

# Read CSV
train <- read.csv(LOCAL_TRAIN_URL)
test <- read.csv(LOCAL_TEST_URL)
submission <- read.csv(LOCAL_SUBMISSION_URL)
submission_final <- read.csv(LOCAL_SUBMISSION_FINAL_URL)


# EDA ---------------------------------------------------------------------

# the size of the different dataset
dim(train)
dim(test)
dim(submission)

### Let's focus on train data sets

## List the different columns
str(train)

## describe it
summary(train)
st(train, out = "return")

# -> categorical: country, types_sols, habit_de_mariage
# -> numerical: all others variables

## Variables with NA
colSums(is.na(train))
# -> variables with NA: taux_adoption_energies_renouvelables (15136)

categorical_features <- c("country", "types_sols", "habit_de_mariage")
numerical_features <- 
  train |> 
  select(-categorical_features) |> 
  names()
numerical_features
# [1] "lat"                                    "lon"                                    "population"                            
# [4] "taux_ensoleillement"                    "demande_energetique_actuelle"           "demande_energetique_projectee"         
# [7] "capacite_installee_actuelle"            "duree_ensoleillement_annuel"            "cout_installation_solaire"             
# [10] "proximite_infrastructures_energetiques" "taux_adoption_energies_renouvelables"   "stabilite_politique"                   
# [13] "taux_acces_energie"                     "niveau_urbanisation"                    "potentiel_investissement"              
# [16] "emissions_co2_evitees"                  "idh"                                    "nombre_animaux_domestiques" 

#### NUMERICAL FEATURES
train |>
  select(numerical_features) |>
  gather() |>
  ggplot(aes(value)) + geom_histogram() + facet_wrap(~key, nrow = 6, ncol = 3, scales = "free")
# -> we can deduce that `nombre_animaux_domestiques`, `potentiel_investissement` are also categorical variables

# Let's update the list of categorical variable
categorical_features <- c(
  "country", "types_sols", "habit_de_mariage", 
  "nombre_animaux_domestiques", "potentiel_investissement"
)
numerical_features <- 
  train |> 
  select(-categorical_features) |> 
  names()
numerical_features
# [1] "lat"                                    "lon"                                    "population"                            
# [4] "taux_ensoleillement"                    "demande_energetique_actuelle"           "demande_energetique_projectee"         
# [7] "capacite_installee_actuelle"            "duree_ensoleillement_annuel"            "cout_installation_solaire"             
# [10] "proximite_infrastructures_energetiques" "taux_adoption_energies_renouvelables"   "stabilite_politique"                   
# [13] "taux_acces_energie"                     "niveau_urbanisation"                    "emissions_co2_evitees"                 
# [16] "idh"

# Let's check the correlation between numerical features and the target
# Visualize the correlation between all the variable and the `demande_energetique_projectee` 
# png("correlation_matrix.png", width = 5000, height = 5000, res = 200)
# plt <- ggpairs(train |> select(numerical_features)) +
#   theme(
#     axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
#     axis.text.y = element_text(size = 6),
#     strip.text = element_text(size = 7, margin = margin(0.1, 0, 0.1, 0, "cm"))
#   )
# print(plt)
# dev.off()
# -> we can notice that the only correlation that exists is the one between `demande_energetique_actuelle` and `demande_energetique_projetee`
# -> all the other variable are independant to the response (`demande_energetique_projetee`)

train |>
  ggplot(aes(sample = demande_energetique_projectee)) + stat_qq() + stat_qq_line()

train |>
  select(numerical_features) |>
  pivot_longer(cols = everything(), names_to = "variable", values_to = "values") |>
  ggplot(aes(sample = values)) + stat_qq() + stat_qq_line(color = "red") +
  facet_wrap(~variable, ncol = 4, nrow = 4, scales = "free")

# Let's calculate Skewness
train |> 
  select(numerical_features) |>
  moments::skewness(na.rm = TRUE) |>
  as.data.frame()
#                                         moments::skewness(select(train, numerical_features), na.rm = TRUE)
# lat                                                                                         -0.3404096052
# lon                                                                                         -0.1602952184
# population                                                                                   3.0299898435
# taux_ensoleillement                                                                          0.0012632486
# demande_energetique_actuelle                                                                -0.0018997061
# demande_energetique_projectee                                                                0.2866971414
# capacite_installee_actuelle                                                                  0.0014673225
# duree_ensoleillement_annuel                                                                  0.0005891623
# cout_installation_solaire                                                                    0.0023512914
# proximite_infrastructures_energetiques                                                       0.0011824656
# taux_adoption_energies_renouvelables                                                         0.0002865192
# stabilite_politique                                                                         -0.0011017794
# taux_acces_energie                                                                           0.0004906154
# niveau_urbanisation                                                                         -0.0056380559
# emissions_co2_evitees                                                                        0.0077048958
# idh                                                                                          0.0001517014
#
# only `population` feature need to be transformed using log


# Missing data ------------------------------------------------------------

# Let's handle missing data
# number of na per country
train %>% 
  group_by(country, lat, lon) %>% summarize(na = sum(is.na(taux_adoption_energies_renouvelables)))
# # A tibble: 53 × 4
# # Groups:   country, lat [53]
# country                     lat    lon    na
# <chr>                     <dbl>  <dbl> <int>
#   1 Algeria                   28.0    1.66   262
# 2 Angola                   -11.2   17.9    282
# 3 Benin                      9.31   2.32   298
# 4 Botswana                 -22.3   24.7    299
# 5 Burkina Faso              12.2   -1.56   286
# 6 Burundi                   -3.37  29.9    253
# 7 Cameroon                   3.85  11.5    298
# 8 Cape Verde                16.5  -23.0    289
# 9 Central African Republic   6.61  20.9    294
# 10 Chad                      15.5   18.7    290
# # ℹ 43 more rows
# # ℹ Use `print(n = ...)` to see more rows

# percentage of na per country
train %>% group_by(country, lat, lon) %>% 
  summarize(na = sum(is.na(taux_adoption_energies_renouvelables)),
            n = n(),
            na_ratio = na/n,
            .groups = 'drop')
# # A tibble: 53 × 6
# country                     lat    lon    na     n na_ratio
# <chr>                     <dbl>  <dbl> <int> <int>    <dbl>
#   1 Algeria                   28.0    1.66   262  2759   0.0950
# 2 Angola                   -11.2   17.9    282  2861   0.0986
# 3 Benin                      9.31   2.32   298  2850   0.105 
# 4 Botswana                 -22.3   24.7    299  2785   0.107 
# 5 Burkina Faso              12.2   -1.56   286  2892   0.0989
# 6 Burundi                   -3.37  29.9    253  2843   0.0890
# 7 Cameroon                   3.85  11.5    298  2877   0.104 
# 8 Cape Verde                16.5  -23.0    289  2863   0.101 
# 9 Central African Republic   6.61  20.9    294  2846   0.103 
# 10 Chad                      15.5   18.7    290  2927   0.0991
# # ℹ 43 more rows
# # ℹ Use `print(n = ...)` to see more rows
#
# almost same number of na per country
# missing values will be replaced by mean


#### CATEGORICAL FEATURES
## Plot categorical variable using a bar chart
train |>
  select(categorical_features) |>
  mutate(
    nombre_animaux_domestiques = as.factor(nombre_animaux_domestiques),
    potentiel_investissement = as.factor(potentiel_investissement)
  ) |>
  pivot_longer(cols = everything(), names_to = "variable", values_to = "category") |>
  ggplot(aes(x = category, fill = variable)) + geom_bar()  + 
  facet_wrap(~variable, ncol = 1, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## Plot each categorical variable according to the feature target
train |>
  select(categorical_features, demande_energetique_projectee) |>
  mutate(
    nombre_animaux_domestiques = as.factor(nombre_animaux_domestiques),
    potentiel_investissement = as.factor(potentiel_investissement)
  ) |>
  pivot_longer(cols = categorical_features, names_to = "variable", values_to = "category") |>
  ggplot(aes(x = category, y = demande_energetique_projectee, fill = variable)) + geom_boxplot() +
  facet_wrap(~variable, ncol = 1, scales = "free") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    strip.text = element_text(size = 10, face = "bold")
  )

## ANOVA TEST
cat_anova <- aov(
  demande_energetique_projectee ~ 
    country + habit_de_mariage + types_sols + 
    nombre_animaux_domestiques + potentiel_investissement, 
  data = train)
summary(cat_anova)
#                                 Df    Sum Sq Mean Sq F value Pr(>F)
# country                        52 2.410e+08 4635417   0.873  0.730
# habit_de_mariage                1 3.522e+05  352246   0.066  0.797
# types_sols                      2 6.290e+06 3144929   0.592  0.553
# nombre_animaux_domestiques      1 4.837e+06 4837120   0.911  0.340
# potentiel_investissement        1 3.019e+05  301874   0.057  0.812
# Residuals                  149942 7.964e+11 5311333    
#
# NO CATEGORICAL FEATURE HAS A P-value lower than 0.05.
# So NO CATEGORICAL FEATURE affect the target feature.
rm(cat_anova)

# Modeling ----------------------------------------------------------------
my_experiment_name = "Energy Demand Prediction with R"
mlflow_set_tracking_uri("http://localhost:8080")
mlflow_set_experiment(my_experiment_name)
my_experiment <- mlflow_get_experiment(name = my_experiment_name)

## Build recipe ----------------------------------------------------------------
# let's build a recipe which contains all our transformations

basic_recipe <- 
  recipe(demande_energetique_projectee ~ ., data = train) |> 
  step_mutate(population = as.numeric(population)) |>
  update_role(all_of(categorical_features), new_role = "removed") |>
  step_rm(has_role("removed")) |>
  step_impute_mean(taux_adoption_energies_renouvelables) |>
  step_normalize(all_numeric_predictors())

basic_recipe |>
  prep() |>
  bake(new_data = NULL)

set.seed(1)

full_data <-
  train |>
  bind_rows(test)


## Tuning ------------------------------------------------

### Common function ----------------------------------

library(mlflow)
library(dplyr)
library(purrr)
library(yardstick)
library(tidymodels)

run_tuning <- function(runName, grid, wf, train, test) {
  mlflow_start_run()
  mlflow_set_tag("mlflow.runName", runName)

  param_cols <- colnames(grid)
  
  results <- grid %>%
    mutate(
      metrics = pmap(
        .l = grid, # transpose(across(all_of(param_cols))),
        .f = function(...) {
          params <- list(...)
          mlflow_start_run(nested = TRUE)
          on.exit(mlflow_end_run())

          walk2(names(params), params, ~ mlflow_log_param(.x, .y))

          fitted <- wf %>%
            finalize_workflow(params) %>%
            fit(data = train)
          
          test_preds <- predict(fitted, test) %>% bind_cols(test)
          train_preds <- predict(fitted, train) %>% bind_cols(train)
          
          test_rmse <- rmse(test_preds, truth = demande_energetique_projectee, estimate = .pred)
          train_rmse <- rmse(train_preds, truth = demande_energetique_projectee, estimate = .pred)
          
          mlflow_log_metric("test_rmse", test_rmse$.estimate)
          mlflow_log_metric("train_rmse", train_rmse$.estimate)
          
          c(train = train_rmse$.estimate, valid = test_rmse$.estimate)
        }
      )
    )
  
  mlflow_end_run()
  
  results %>%
    mutate(
      train_rmse = map_dbl(metrics, "train"),
      valid_rmse = map_dbl(metrics, "valid")
    ) %>%
    select(-metrics)
}

get_mlflow_run_details <- function(runName) {
  experimentId <- mlflow_get_experiment()$experiment_id
  
  parentRun <- 
    mlflow_search_runs(filter = paste0("tags.mlflow.runName = '", runName, "'"), experiment_ids = experimentId)
  
  if (nrow(parentRun) == 0) stop("No run found with that name")
  
  childrenRuns <- 
    mlflow_search_runs(
      filter = paste0("tags.mlflow.parentRunId = '", parentRun$run_id[1], "'"),
      experiment_ids = experimentId,
      run_view_type = "ALL"
    ) |>
    select(-c(run_uuid, status, experiment_id, user_id, artifact_uri, lifecycle_stage, tags))

  return(childrenRuns)
}

distance_from_yx <- function(runs) {
  distances <-
    runs |>
    select(run_name, metrics) |> 
    unnest(metrics) |> 
    select(-c(timestamp, step)) |> 
    pivot_wider(names_from = "key", values_from = "value") |>
    mutate(
      distance = abs(train_rmse - test_rmse)/sqrt(2)
    ) |>
    arrange(distance)
  
  return(distances)
}

plot_distances_bar <- function(distances) {
  distances |>
    head(20) %>%
    ggplot(aes(x = reorder(run_name, distance, decreasing = TRUE), y = distance)) +
    geom_bar(stat = "identity", fill = "#69b3a2") +
    coord_flip() +
    labs(title = "Top 20 Runs by Distance from y = x",
         x = "",
         y = "Distance from perfect generalization") +
    geom_text(aes(label = round(distance, 4)), hjust = -0.1, size = 3) +
    theme_minimal()
}

get_best_model_parameters <- function(distances, runs) {
  params <- 
    runs |> 
    filter(run_name == distances$run_name[1]) |> 
    select(params) |> 
    pull(params)

  params_named <- setNames(as.list(params[[1]]$value), params[[1]]$key)
  
  best_params <- as_tibble(as.list(params_named)) |>
    mutate(across(everything(), as.numeric))
  
  return(best_params)
}

get_best_model_metrics <- function(distances, runs) {
  metrics <- 
    runs |> 
    filter(run_name == distances$run_name[1]) |> 
    select(metrics) |> 
    pull(metrics)
  
  metrics_named <- setNames(as.list(metrics[[1]]$value), metrics[[1]]$key)
  
  best_metrics <- as_tibble(as.list(metrics_named))
  
  return(best_metrics)
}

### Linear regression ------------------------------------------------

lr_model <- linear_reg(
  penalty = tune(),
  mixture = tune()       # from 0 to 1
) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

lr_wf <- workflow() %>%
  add_model(lr_model) %>%
  add_recipe(basic_recipe)

lr_grid <- grid_latin_hypercube(
  penalty(range = c(10^-4, 10^0), trans = log10_trans()),
  mixture(),
  size = 50
)
  
lr_tuned <- run_tuning("Linear Regression", lr_grid, lr_wf, train, test)

lr_runs <- get_mlflow_run_details("Linear Regression")

lr_distance <- distance_from_yx(lr_metrics)

plot_distances_bar(lr_distance)

lr_best_params <- get_best_model_parameters(lr_distance, lr_runs)

get_best_model_metrics(lr_distance, lr_runs)


lr_fit <-
  lr_wf |>
  finalize_workflow(lr_best_params) |>
  fit(data = full_data)

lr_submission_preds <-
  lr_fit |>
  predict(submission) |>
  bind_cols(submission) |>
  bind_cols(submission_final)

rmse(lr_submission_preds, truth = demande_energetique_projectee, estimate = .pred) |>
  as.data.frame()
  
lr_runs <- get_mlflow_run_details("Linear Regression")
lr_distance <- distance_from_yx(lr_runs)

ggplot(data = lr_distance) +
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  geom_point(aes(train_rmse, test_rmse), size = 3) +
  geom_segment(aes(x = train_rmse, y = test_rmse, xend = (train_rmse + test_rmse)/2, yend = (train_rmse + test_rmse)/2), 
               color = "red", linetype = "dashed") +
  geom_text(aes(train_rmse, test_rmse, label = round(distance, 2)),
            vjust = -1) +
  coord_fixed() +
  labs(title = "Distance from Points to Line y = x",
       x = "X", y = "Y") +
  theme_minimal()

lr_distance |>
  head(20) %>%
  ggplot(aes(x = reorder(run_name, distance, decreasing = TRUE), y = distance)) +
  geom_bar(stat = "identity", fill = "#69b3a2") +
  coord_flip() +  # Horizontal bars
  labs(title = "Top 20 Runs by Distance from y = x",
       x = "",
       y = "Distance from perfect generalization") +
  geom_text(aes(label = round(distance, 4)), hjust = -0.1, size = 3) +
  theme_minimal()
#### Forward Selection -------------------------------------------------------

forward_selection_rmse <- function(
    train_data,
    valid_data,
    target,
    predictors,
    tol = 1e-4,
    max_steps = length(predictors)
) {
  
  selected_vars <- character(0)
  remaining_vars <- predictors
  best_rmse <- Inf
  improvement <- Inf 
  step <- 0
  
  
  
  while(length(remaining_vars) > 0 && improvement > tol && step < max_steps) {
    step <- step + 1
    rmse_list <- numeric(length(remaining_vars))
    
    for (i in seq_along(remaining_vars)) {
      vars_to_try <- c(selected_vars, remaining_vars[i])
      
      current_recipe <- 
        recipe(
          as.formula(paste(target, "~", paste(vars_to_try, collapse = "+"))),
          data = train_data
        ) |>
        # step_mutate(population = as.numeric(population)) |>
        # update_role(all_of(categorical_features), new_role = "removed") |>
        # step_rm(has_role("removed")) |>
        # step_impute_mean(taux_adoption_energies_renouvelables) |>
        step_normalize(all_numeric_predictors())
      
      current_lr_model <-
        linear_reg(
          penalty = 1.0,      # à tuner
          mixture = 1.0       # à tuner (de 0 à 1)
        ) %>%
        set_engine("lm") %>%
        set_mode("regression")
      
      current_lr_wf <-
        workflow() |>
        add_recipe(current_recipe) |>
        add_model(current_lr_model)
      
      current_lr_fit <-
        fit(current_lr_wf, data = train_data)
      
      current_preds <-
        predict(current_lr_fit, valid_data) |>
        bind_cols(valid_data)
      
      # cat(current_preds |> names())
      
      current_rmse <- 
        rmse(current_preds, truth = !!sym(target), estimate = .pred)$.estimate
      rmse_list[i] <- current_rmse
    }
    
    min_rmse <- min(rmse_list)
    best_var <- remaining_vars[which.min(rmse_list)]
    
    improvement <- best_rmse - min_rmse
    
    if (improvement > tol) {
      selected_vars <- c(selected_vars, best_var)
      remaining_vars <- setdiff(remaining_vars, best_var)
      
      best_rmse <- min_rmse
      
      cat(sprintf("Step %d: Add variable '%s' with RMSE=%.4f\n", step, best_var, best_rmse))
    } else {
      cat("No significant improvement, stopping selection.\n")
      break
    }
  }
  
  return(selected_vars)
}

target_var <- "demande_energetique_projectee"
all_variables <- setdiff(train |> select(-categorical_features) |> names(), target_var)
selected_variables <-
  forward_selection_rmse(
    train_data = train,
    valid_data = test,
    target = target_var,
    predictors = all_variables,
    tol = 1e-4
  )

lr_best_params <- tibble(
  penalty = 1.0,
  mixture = 1.0
)

lr_fs_recipe <- 
  recipe(
    demande_energetique_projectee ~ .,
    data = train |> select(selected_variables, demande_energetique_projectee)
  ) |>
  step_normalize(all_numeric_predictors())

lr_fs_wf <-
  workflow() |>
  add_recipe(lr_fs_recipe) |>
  add_model(lr_model)

lr_fs_fit <-
  lr_fs_wf |>
  finalize_workflow(lr_best_params) |>
  fit(data = full_data)

lr_submission_preds <-
  lr_fs_fit |>
  predict(submission) |>
  bind_cols(submission) |>
  bind_cols(submission_final)

rmse(lr_submission_preds, truth = demande_energetique_projectee, estimate = .pred) |> as.data.frame()
# .metric .estimator .estimate
# rmse   standard  800.6172

# .metric .estimator .estimate
# rmse   standard  800.6156

# .metric .estimator .estimate
# rmse   standard  800.6134

### Decision Trees Tuning ------------------------------------------------

tree_model <-
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune(),
    min_n = tune()
  ) %>%
  set_engine("rpart") %>%
  set_mode("regression")

tree_grid <- 
  grid_latin_hypercube(
    cost_complexity(range = c(-6, -1)),
    tree_depth(range = c(1, 30)),
    min_n(range = c(2, 40)),
    size = 50
  )
  
tree_wf <-
  workflow() %>%
  add_model(tree_model) %>%
  add_recipe(basic_recipe)
  

tree_tuned <- run_tuning("Decision Trees", tree_grid, tree_wf, train, test)

tree_runs <- get_mlflow_run_details("Decision Trees")

tree_distance <- distance_from_yx(tree_runs)

plot_distances_bar(tree_distance)

tree_best_params <- get_best_model_parameters(tree_distance, tree_runs)
tree_best_params

get_best_model_metrics(tree_distance, tree_runs)

  
tree_fit <-
  tree_wf |>
  finalize_workflow(tree_best_params) |>
  fit(data = full_data)

tree_submission_preds <-
  tree_fit |>
  predict(submission) |>
  bind_cols(submission) |>
  bind_cols(submission_final)

rmse(tree_submission_preds, truth = demande_energetique_projectee, estimate = .pred) |>
  as.data.frame()
# .metric .estimator .estimate
# rmse   standard  802.1377
 


### XGBoost Tuning ------------------------------------------------

xgb_model <- 
  boost_tree(
    trees = tune(),          # Number of trees
    tree_depth = tune(),     # Max tree depth
    learn_rate = tune(),     # Learning rate (eta)
    mtry = tune(),           # Variables per split
    min_n = tune(),          # Min observations in terminal nodes
    loss_reduction = tune()  # Gamma (min loss reduction)
  ) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_wf <- 
  workflow() %>%
  add_recipe(basic_recipe) %>%
  add_model(xgb_model)
  
xgb_grid <- grid_latin_hypercube(
  trees(range = c(100, 2000)),
  tree_depth(range = c(3, 15)),
  learn_rate(range = c(-2, -1)),  # 10^(-2) to 10^(-1)
  mtry(range = c(1, 10)),      # Fraction of predictors
  min_n(range = c(5, 20)),
  loss_reduction(),
  size = 50  # Number of combinations
)
  
xgb_tuned <- run_tuning("XGBoost", xgb_grid, xgb_wf, train, test)

xgb_runs <- get_mlflow_run_details("XGBoost")

xgb_distance <- distance_from_yx(xgb_runs)

plot_distances_bar(xgb_distance)

xgb_best_params <- get_best_model_parameters(xgb_distance, xgb_runs)
xgb_best_params

get_best_model_metrics(xgb_distance, xgb_runs)

xgb_fit <-
  xgb_wf |>
  finalize_workflow(xgb_best_params) |>
  fit(data = full_data)

xgb_submission_preds <-
  xgb_fit |>
  predict(submission) |>
  bind_cols(submission) |>
  bind_cols(submission_final)

rmse(xgb_submission_preds, truth = demande_energetique_projectee, estimate = .pred) |>
  as.data.frame()



mlflow_start_run()
mlflow_set_tag("mlflow.runName", "XGBoost Regularized")

xgb_model <- 
  boost_tree(
    trees = tune(),          
    tree_depth = tune(),     # Max depth (3–8)
    learn_rate = tune(),     
    mtry = tune(),           # Features per split
    min_n = tune(),          # Min observations in nodes
    loss_reduction = tune()  # Gamma
  ) %>%
  set_engine(
    "xgboost",
    lambda = tune(),         # L2 regularization
    alpha = tune(),          # L1 regularization
    subsample = tune(),      # Subsample proportion (0.6–1.0) ← Correct placement
    nthread = 4,
    early_stopping_rounds = 10
  ) %>%
  set_mode("regression")

library(dials)

xgb_params <- extract_parameter_set_dials(xgb_model)

# 3. Adjust any parameter ranges as needed
xgb_params <- xgb_params %>%
  update(
    trees = trees(range = c(100, 2000)),
    tree_depth = tree_depth(range = c(3, 8)),
    learn_rate = learn_rate(range = c(-2, -1)), # 0.01 to 0.1
    mtry = mtry(range = c(1, 10)),
    min_n = min_n(range = c(5, 20)),
    loss_reduction = loss_reduction(range = c(0, 5)),
    lambda = penalty(range = c(0, 1)),  # Use penalty() for lambda
    alpha = penalty(range = c(0, 1)),   # Use penalty() for alpha
    subsample = sample_prop(range = c(0.6, 1.0))
)

xgb_grid <- grid_latin_hypercube(
  xgb_params,
  size = 50
)

xgb_grid

xgb_regularized_tuned <- run_tuning("XGBoost Regularized", xgb_grid, xgb_wf, train, test)
xgb_runs <- get_mlflow_run_details("XGBoost Regularized")

xgb_distance <- distance_from_yx(xgb_runs)

plot_distances_bar(xgb_distance)

xgb_best_params <- get_best_model_parameters(xgb_distance, xgb_runs)
xgb_best_params

xgb_fit <-
  xgb_wf |>
  finalize_workflow(xgb_best_params) |>
  fit(data = full_data)

xgb_submission_preds <-
  xgb_fit |>
  predict(submission) |>
  bind_cols(submission) |>
  bind_cols(submission_final)

rmse(xgb_submission_preds, truth = demande_energetique_projectee, estimate = .pred) |>
  as.data.frame()


## Random forests ----------------------------------------------------------
mlflow_start_run()
mlflow_set_tag("mlflow.runName", "Random forests")

rf_model <- 
  rand_forest(
    mtry = tune(),
    trees = tune(),
    min_n = tune()
  ) |>
  set_mode("regression") |>
  set_engine("ranger")

rf_grid <-
  grid_space_filling(
    mtry(range = c(1, 10)),
    trees(range = c(100, 1000)),
    min_n(range = c(2, 20)),
    size = 50
  )

rf_wf <-
  workflow() |>
  add_model(rf_model) |>
  add_recipe(basic_recipe)

rf_tuned <- run_tuning("Random forests", rf_grid, rf_wf, train, test)

rf_runs <- get_mlflow_run_details("Random forests")

rf_distance <- distance_from_yx(rf_runs)

plot_distances_bar(rf_distance)

rf_best_params <- get_best_model_parameters(rf_distance, rf_runs)
rf_best_params

get_best_model_metrics(rf_distance, rf_runs)


## KNN ---------------------------------------------------------------------

knn_model <-
  nearest_neighbor(neighbors = tune(), weight_func = tune(), dist_power = tune()) %>%
  set_engine('kknn') %>%
  set_mode('regression')

knn_wf <-
  workflow() |>
  add_recipe(basic_recipe) |>
  add_model(knn_model)

knn_grid <-
  grid_space_filling(
    neighbors(),
    weight_func(),
    dist_power(),
    size = 50
  )

knn_tuned <- knn_grid %>%
  mutate(
    fit = pmap(list(neighbors, weight_func, dist_power), ~ {
      neighbors <- ..1
      weight_func <- ..2
      dist_power <- ..3
      
      params <- list(
        neighbors = neighbors,
        weight_func = weight_func,
        dist_power = dist_power
      )
      
      fitted <- knn_wf %>%
        finalize_workflow(params) %>%
        fit(data = train)
      
      return(fitted)
    }),
    metrics = map(fit, ~ {
      test.preds <- predict(.x, test) |> bind_cols(test)
      train.preds <- predict(.x, train) |> bind_cols(train)
      
      test.rmse <- rmse(test.preds, truth = demande_energetique_projectee, estimate = .pred)
      train.rmse <- rmse(train.preds, truth = demande_energetique_projectee, estimate = .pred)
      
      c(train = train.rmse$.estimate, valid = test.rmse$.estimate)
    })
  )


# Conclusion --------------------------------------------------------------

results <- tibble(
  model = c("Linear Regression", "Decision Trees", "XGBoost", "Random Forest"),
  Distance = c(2.3697, 0.4939, 0.1602, 195.5467),
  Training = c(800.107987784175, 799.117689426901, 790.666699758206, 448.017137065657),
  Validation = c(796.703157323657, 798.166633434748, 797.751346067469, 800.538644878489)
)

results_long <- results %>%
  pivot_longer(cols = c(Distance, Training, Validation),
               names_to = "Metric",
               values_to = "Value")

results_long

# Plot Distance
p_distance <- ggplot(filter(results_long, Metric == "Distance"), aes(x = model, y = Value, fill = model)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Distance by Model", y = "Distance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Training
p_training <- ggplot(filter(results_long, Metric == "Training"), aes(x = model, y = Value, fill = model)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Training RMSE by Model", y = "Training RMSE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Validation
p_validation <- ggplot(filter(results_long, Metric == "Validation"), aes(x = model, y = Value, fill = model)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Validation RMSE by Model", y = "Validation RMSE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(p_distance, p_training, p_validation, ncol = 3)
