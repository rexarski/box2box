if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidymodels, ggthemes, glue, caret, kernlab, e1071)

ggplot2::theme_set(
    theme_fivethirtyeight() +
        theme(
            text = element_text(family = "Roboto Condensed"),
            title = element_text(size = 14),
            plot.subtitle = element_text(size = 12),
            plot.caption = element_text(size = 10),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            panel.grid.minor.x = element_blank()
        )
)

dat <- read_csv("data/nba-pbp/2020-cleaned-by-team/team-shots-TOR.csv") %>%
    select(player, date, period, time, shot_clock, margin, x, y,
           shottype, assisted, closest_def_dist, touch_time,
           dribble_range, made) %>%
    mutate(assisted = if_else(is.na(assisted), FALSE, assisted),
           date = lubridate::year(date)) %>%
    rename(year = date) %>%
    drop_na() %>%
    mutate(player = factor(player),
           year = factor(year),
           shot_clock = factor(shot_clock),
           shottype = factor(shottype),
           assisted = factor(assisted),
           closest_def_dist = factor(closest_def_dist),
           touch_time = factor(touch_time),
           dribble_range = factor(dribble_range),
           made = factor(made))

glimpse(dat)

# ----------- NB -----------

set.seed(123)
split <- initial_split(dat, prop = .7, strata = made)
train <- training(split)
test <- testing(split)
    
table(train$made) %>% prop.table()
table(test$made) %>% prop.table()

# caret
# 10-fold cross validation

features <- setdiff(names(train), "made")
x <- train[, features]
y <- train$made

train_control <- trainControl(
    method = "cv",
    number = 10
)

nb.m1 <- train(
    x = x,
    y = y,
    method = "nb",
    trControl = train_control
)
confusionMatrix(nb.m1)

# tune the following hyperparameters:

search_grid <- expand.grid(
    usekernel = c(TRUE, FALSE),
    fL = 0:5,
    adjust = seq(0, 5, by = 1)
)

nb.m2 <- train(
    x = x,
    y = y,
    method = "nb",
    trControl = train_control,
    tuneGrid = search_grid,
    preProc = c("center", "scale")
)
nb.m2$results %>%
    top_n(5, wt = Accuracy) %>%
    arrange(desc(Accuracy))

ggplot(nb.m2) +
    labs(
        title = "NB Grid Search Results of Hyperparameters",
        caption = glue("Created by Rui Qiu(rq47)
                       Source: Toronto Raptors' play-by-play data."))

ggsave("nba-nb-svm/nb-tuning.png", plot = last_plot(), height = 6, width = 8)

confusionMatrix(nb.m2)

pred <- predict(nb.m2, newdata = test)
confusionMatrix(pred, test$made)

# estimate variable importance

nb.m2$finalModel

test <- test %>%
    mutate(nb_predicted = pred)

ggplot(test, aes(x=x, y=y, color=made)) +
    geom_point(alpha = 0.5, size = 2) +
    ylim(0, 400) +
    coord_equal() +
    scale_colour_manual(values=c("#A1A1A4", "#B4975A")) +
    labs(
        title = "Toronto Raptors Actual Shot Attempts",
        subtitle = "with Naïve Bayes",
        caption = glue("Created by Rui Qiu(rq47)
                       Source: Toronto Raptors' play-by-play data."))

ggsave("nba-nb-svm/nb-actual-shot-attempts.png", plot=last_plot(), height=8, width=8)

ggplot(test, aes(x=x, y=y, color=nb_predicted)) +
    geom_point(alpha = 0.5, size = 2) +
    ylim(0, 400) +
    coord_equal() +
    scale_colour_manual(values=c("#A1A1A4", "#B4975A")) +
    labs(
        title = "Toronto Raptors Predicted Shot Attempts",
        subtitle = "with Naïve Bayes",
        caption = glue("Created by Rui Qiu(rq47)
                       Source: Toronto Raptors' play-by-play data."))

ggsave("nba-nb-svm/nb-predicted-shot-attempts.png", plot=last_plot(), height=8, width=8)

test <- test %>%
    mutate(nb_accurate = nb_predicted == made)

ggplot(test, aes(x=x, y=y, color=nb_accurate)) +
    geom_point(alpha = 0.5, size = 2) +
    ylim(0, 400) +
    coord_equal() +
    scale_colour_manual(values=c("#000000", "#CE1141")) +
    labs(
        title = "Toronto Raptors Shot Attempts Prediction Accuracy",
        subtitle = "with Naïve Bayes",
        caption = glue("Created by Rui Qiu(rq47)
                       Source: Toronto Raptors' play-by-play data."))

ggsave("nba-nb-svm/nb-prediction-accuracy.png", plot=last_plot(), height=8, width=8)

# ----------- SVM -----------

# same data set

# Linear Kernel

svm.m1 <- train(made ~., data = train, method = "svmLinear", trControl = train_control,
                preProcess = c("center", "scale"),
                tuneGrid = expand.grid(C=seq(0, 2, length = 10)))
svm.m1

ggplot(svm.m1) +
    labs(
        title = "Linear SVM Grid Search Results of Hyperparameters",
        caption = glue("Created by Rui Qiu(rq47)
                       Source: Toronto Raptors' play-by-play data."))

ggsave("nba-nb-svm/svm-linear-tuning.png", plot = last_plot(), height = 6, width = 8)

res1 <- as_tibble(svm.m1$results[which.max(svm.m1$results$Accuracy),])
res1

pred.svm.m1 <- predict(svm.m1, newdata = test)
confusionMatrix(pred.svm.m1, test$made)

# Radial Kernel

svm.m2 <- train(made ~., data = train, method = "svmRadial", trControl = train_control,
                preProcess = c("center", "scale"),
                tuneLength = 10)
svm.m2

ggplot(svm.m2) +
    labs(
        title = "Radial SVM Grid Search Results of Hyperparameters",
        caption = glue("Created by Rui Qiu(rq47)
                       Source: Toronto Raptors' play-by-play data."))

ggsave("nba-nb-svm/svm-radial-tuning.png", plot = last_plot(), height = 6, width = 8)

res2 <- as_tibble(svm.m2$results[which.max(svm.m2$results$Accuracy),])
res2

pred.svm.m2 <- predict(svm.m2, newdata = test)
confusionMatrix(pred.svm.m2, test$made)

# Polynomial Kernel

svm.m3 <- train(made ~., data = train, method = "svmPoly", trControl = train_control,
                preProcess = c("center", "scale"),
                tuneLength = 2)
svm.m3

ggplot(svm.m3) +
    labs(
        title = "Polynomial SVM Grid Search Results of Hyperparameters",
        caption = glue("Created by Rui Qiu(rq47)
                       Source: Toronto Raptors' play-by-play data."))

ggsave("nba-nb-svm/svm-poly-tuning.png", plot = last_plot(), height = 6, width = 8)

res3 <- as_tibble(svm.m3$results[which.max(svm.m3$results$Accuracy),])
res3

pred.svm.m3 <- predict(svm.m3, newdata = test)
confusionMatrix(pred.svm.m3, test$made)

# pick linear svm ;)

test <- test %>%
    mutate(svm_predicted = pred.svm.m1,
           svm_accurate = svm_predicted == made)

ggplot(test, aes(x=x, y=y, color=svm_accurate)) +
    geom_point(alpha = 0.5, size = 2) +
    ylim(0, 400) +
    coord_equal() +
    scale_colour_manual(values=c("#000000", "#CE1141")) +
    labs(
        title = "Toronto Raptors Shot Attempts Prediction Accuracy",
        subtitle = "with Linear SVM",
        caption = glue("Created by Rui Qiu(rq47)
                       Source: Toronto Raptors' play-by-play data."))

ggsave("nba-nb-svm/svm-prediction-accuracy.png", plot=last_plot(), height=8, width=8)

test2 <- test %>%
    select(made, nb_predicted, svm_predicted, nb_accurate, svm_accurate, x, y) %>%
    mutate(`Two classifiers agree` = nb_predicted == svm_predicted)

ggplot(test2, aes(x=x, y=y, color=`Two classifiers agree`)) +
    geom_point(alpha = 0.5, size = 2) +
    ylim(0, 400) +
    coord_equal() +
    scale_colour_manual(values=c("#000000", "#CE1141")) +
    labs(
        title = "Toronto Raptors Shot Attempts Prediction Difference",
        subtitle = "Naïve Bayes vs Linear SVM",
        caption = glue("Created by Rui Qiu(rq47)
                       Source: Toronto Raptors' play-by-play data."))

ggsave("nba-nb-svm/svm-vs-nb-viz.png", plot=last_plot(), height=8, width=8)
