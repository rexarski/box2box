##################
#     setup      #
##################

gc()
rm(list=ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidymodels, ggthemes, vip, rpart.plot,
               randomForest, rfviz, glue)

ggplot2::theme_set(
    theme_fivethirtyeight() +
        theme(
            text = element_text(family = "Roboto Condensed"),
            title = element_text(size = 14),
            plot.subtitle = element_text(size = 7),
            plot.caption = element_text(size = 10),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            panel.grid.minor.x = element_blank()
        )
)

spotify_green <- "#1DB954"

# not used here though ;)
gini_n_entropy <- function(vec_of_pi) {
    gini <- 1
    entropy <- 0
    for (pi in vec_of_pi) {
        gini <- gini - pi^2
        entropy <- entropy - pi*log2(pi)
    }
    return(c(gini, entropy))
}

dat <- read_csv("data/nba-pbp/pbpstats-tracking-shots-cleaned.csv") %>%
    filter(player=="Stephen Curry") %>%
    select(shot_clock, margin,
           x, y, shottype,
           closest_def_dist, touch_time,
           dribble_range, value,
           made 
           ) %>%
    mutate(shot_clock = as_factor(shot_clock),
           shottype = as_factor(shottype),
           closest_def_dist = as_factor(closest_def_dist),
           touch_time = as_factor(touch_time),
           dribble_range = as_factor(dribble_range),
           value = as_factor(value),
           made = as_factor(made)) %>%
    drop_na()

write_csv(dat, "data/nba-pbp/stephen-curry-3.csv")
    
glimpse(dat)

set.seed(2021)
shot_split <- initial_split(dat, strata = made)
shot_train <- training(shot_split)
shot_test <- testing(shot_split)

set.seed(2022)
shot_folds <- vfold_cv(shot_train, strata = made, v = 5)
shot_folds

tree_spec <- decision_tree(
    cost_complexity = tune(),
    tree_depth = tune(),
    min_n = tune()
) %>%
    set_engine("rpart") %>%
    set_mode("classification")

tree_spec

tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(c(3,12)),
                          min_n(c(5,20)),
                          levels = 4)

tree_grid

doParallel::registerDoParallel()

set.seed(2023)
ptm <- proc.time()
tree_wf <- workflow() %>%
    add_model(tree_spec) %>%
    add_formula(made ~ .)
tree_res <- 
    tree_wf %>% 
    tune_grid(
        resamples = shot_folds,
        grid = tree_grid
    )
proc.time() - ptm

tree_res

tree_res %>% 
    collect_metrics()

model_select <- autoplot(tree_res) + scale_color_viridis_d(option = "plasma", begin = .9, end = 0) +
    labs(
        title = "Model selection of decision tree",
        subtitle = "on Curry's 3pt",
        caption = glue("Rui Qiu (rq47)
                       Data: pbpstats.com")
    )
ggsave("nba-dt/model_selection.png", model_select,
       width=11, height=9)

show_best(tree_res, "accuracy")
show_best(tree_res, "roc_auc")

# tree1
fourth_best_accuracy <- show_best(tree_res, "accuracy")[4,]
final_tree1 <- finalize_model(tree_spec, fourth_best_accuracy)
final_fit1 <- final_tree1 %>%
    fit(made ~ ., shot_train)
final_fit1 %>%
    extract_fit_engine() %>%
    rpart.plot(roundint=FALSE,
               main = "Curry's 3pt decision tree 1 (accuracy)",
               type = 5,
               fallen.leaves = FALSE,
               tweak = 2.5)

tree1_fi <- final_fit1 %>%
    vip(geom = "col", aes = list(fill = spotify_green, alpha = 0.8)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
        title = "Curry's 3pt decision tree 1 feature importance",
        subtitle = "(4th best accuracy)",
        caption = glue("Rui Qiu (rq47)
                       Data: pbpstats.com"))

ggsave("nba-dt/curry-tree1-fi.png", tree1_fi, width = 11, height = 9)

augment(final_fit1, new_data = shot_test) %>%
    accuracy(truth = made, estimate = .pred_class)
augment(final_fit1, new_data = shot_test) %>%
    conf_mat(truth = made, estimate = .pred_class)

# tree2

final_tree2 <- finalize_model(tree_spec, select_best(tree_res, "roc_auc"))
final_fit2 <- final_tree2 %>%
    fit(made ~ ., shot_train)
final_fit2 %>%
    extract_fit_engine() %>%
    rpart.plot(roundint=FALSE,
               main = "Curry's 3pt decision tree 2 (roc_auc)",
               type = 5,
               fallen.leaves = FALSE,
               tweak = 2.5)
tree2_fi <- final_fit2 %>%
    vip(geom = "col", aes = list(fill = spotify_green, alpha = 0.8)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
        title = "Curry's 3pt decision tree 2 feature importance",
        subtitle = "(roc_auc)",
        caption = glue("Rui Qiu (rq47)
                       Data: pbpstats.com"))

ggsave("nba-dt/curry-tree2-fi.png", tree2_fi, width = 11, height = 9)

augment(final_fit2, new_data = shot_test) %>%
    accuracy(truth = made, estimate = .pred_class)
augment(final_fit2, new_data = shot_test) %>%
    conf_mat(truth = made, estimate = .pred_class)

# tree3

classic_tree_spec <- decision_tree() %>%
    set_engine("rpart") %>%
    set_mode("classification")
classic_tree_fit <- classic_tree_spec %>%
    fit(made ~ ., data = dat)
classic_tree_fit
classic_tree_fit %>%
    extract_fit_engine() %>%
    rpart.plot(roundint=FALSE,
               main = "Curry's 3pt decision tree 3",
               type = 5,
               fallen.leaves = FALSE,
               tweak = 2.5)
tree3_fi <- classic_tree_fit %>%
    vip(geom = "col", aes = list(fill = spotify_green, alpha = 0.8)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
        title = "Curry's 3pt decision tree 3 feature importance",
        subtitle = "default settings",
        caption = glue("Rui Qiu (rq47)
                       Data: pbpstats.com"))

ggsave("nba-dt/curry-tree3-fi.png", tree3_fi, width = 11, height = 9)

augment(classic_tree_fit, new_data = shot_test) %>%
    accuracy(truth = made, estimate = .pred_class)
augment(classic_tree_fit, new_data = shot_test) %>%
    conf_mat(truth = made, estimate = .pred_class)


# Random Forest

# p/3 = 3, p = 9
rf_spec <- rand_forest(mtry = tune(),
                       trees = tune(),
                       min_n = tune()) %>%
    set_engine("ranger", importance = "impurity") %>%
    set_mode("classification")

# feature engineering

shot_recipe <- recipe(made ~ ., data = shot_train) %>% 
    step_YeoJohnson(all_numeric(), -all_outcomes()) %>% 
    step_normalize(all_numeric(), -all_outcomes()) %>% 
    step_dummy(all_nominal(), -all_outcomes())

shot_recipe %>%
    prep() %>%
    bake(new_data = shot_train)

rf_wf <- workflow() %>%
    add_model(rf_spec) %>%
    add_recipe(shot_recipe)

rf_grid <- grid_random(mtry() %>% range_set(c(3, 6)),
                       trees(),
                       min_n(),
                       size = 10)
rf_grid

rf_tuning <- rf_wf %>%
    tune_grid(resamples = shot_folds, grid = rf_grid)
rf_tuning %>% show_best("roc_auc")

best_rf <- rf_tuning %>%
    select_best(metric = "roc_auc")
best_rf

final_rf_wf <- rf_wf %>% 
    finalize_workflow(best_rf)

rf_wf_fit <- final_rf_wf %>%
    fit(data = shot_train)

rf_fit <- rf_wf_fit %>%
    extract_fit_parsnip()

rf_fi <- rf_fit %>%
    vip(geom = "col", aes = list(fill = spotify_green, alpha = 0.8)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
        title = "Curry's 3pt random forest feature importance",
        caption = glue("Rui Qiu (rq47)
                       Data: pbpstats.com"))

ggsave("nba-dt/rf-fi.png", rf_fi, width = 11, height = 9)

rf_last_fit <- final_rf_wf %>%
    last_fit(shot_split)

rf_last_fit %>%
    collect_metrics()

rf_predictions <- rf_last_fit %>% collect_predictions()

conf_mat(rf_predictions,
         truth = made,
         estimate = .pred_class)

# retrieve a sample tree (no way to visualize it though)

# rf_model <- randomForest(made ~ .,
#                          data = shot_train,
#                          ntree = 500)
# 
# getTree(rf_model, 10, labelVar=TRUE)

# Decision Tree Feature Importance for different datasets

rfprep <- rf_prep(x=shot_train[,1:9], y=shot_train$made, seed=2021)
rf_viz(rfprep)

