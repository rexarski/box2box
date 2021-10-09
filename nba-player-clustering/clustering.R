#################
#     Setup     #
#################

gc()
rm(list=ls())

if (!requireNamespace('pacman', quietly = TRUE)){
    install.packages('pacman')
}
 
pacman::p_load(tidyverse, ggthemes, cluster, 
               ggrepel, ggpubr, factoextra, glue,
               patchwork, gganimate, ggdendro, NbClust,
               class)

set.seed(2021)

# data comes directly from https://www.basketball-reference.com/leagues/NBA_2021_per_game.html

# this is because our previously cleaned record data from last module consist lots of non-numeric data (you know, used to describe a detailed shot attempt)

dat <- read_csv("./nba-player-clustering/nba-2021-per-game-stats.csv")

################################
#     Preliminary Cleaning     #
################################

# do the cleaning again
# 1. keep the total stats if a player got transferred during within the season.
# 2. clean the player name
# 3. ideally, the label will be the player's position, so we remove some variables having nothing to do with the position such as G, GS, MP, Rk.

dat <- dat %>%
    group_by(Player) %>%
    filter(row_number() == 1 | Tm=="TOT") %>%
    ungroup() %>%
    mutate(Player = str_extract(Player, "[^\\\\]+")) %>%
    select(-c(Rk, Tm, )) %>%
    drop_na()

# now we have a data set with 503 rows, 28 columns (26 variables excluding two "text tags")

dat <- dat %>%
    column_to_rownames(var = "Player") %>%
    select(-Pos)

#########################
#     With Raw Data     #
#########################

iter_kmeans <- function(data, kvalues, flag, caption) {
    for (k in kvalues) {
        km <- kmeans(data, centers = k, nstart = 25)
        
        factormap <- fviz_cluster(km, data = data,
                                  repel = TRUE, # avoid label overlapping
                                  geom = c("text", "point"),
                                  show.clust.cent = TRUE, # show cluster centers
                                  palette = "lancet") +
            theme_fivethirtyeight() +
            theme(
                text = element_text(family = "Roboto Condensed"),
                title = element_text(size = 18),
                plot.subtitle = element_text(size = 16),
                plot.caption = element_text(size = 10),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 12),
                panel.grid.minor.x = element_blank()
            ) +
            labs(
                title = glue("NBA Players Stats K-means Clustering (k=", k, ")"),
                subtitle = "2020-2021 Regular Season",
                caption = caption)
        ggsave(factormap, filename = glue("./nba-player-clustering/05-02-", flag, "-cluster-k", k, ".png"),
               device = "png", height = 9, width = 11, dpi = 100)
    }
}

# since we tend to cluster the data by their playing positions, but these more or less is dominated by playing time. And of course, lots of overlapping suggests the clustering quality is not ideal.

iter_kmeans(data = dat, kvalues = 2:5, flag = "all",
            caption = glue("
                      * All players with valid playing time.
                      Source: basketball-reference
                      By: Rui Qiu (rq47)"))

# Let's do it again with but with a partition of data

#############################
#     With Partial Data     #
#############################

dat2 <- dat %>%
    slice_max(MP, n = 50) %>%
    select(`FG%`, `3PA`, `eFG%`, FT, TRB,
           AST, STL, BLK, TOV, PTS) %>%
    scale()

iter_kmeans(data = dat2, kvalues = 2:5, flag = "limited",
            caption = glue("
                      * Top 50 players ordered by playing time.
                      Source: basketball-reference
                      By: Rui Qiu (rq47)"))

###################################
#     Hierarchical Clustering     #
###################################

# compute distance matrix
res.dist <- dist(dat2, method = "euclidean")

# compute the hierarchical clusterings
hc <- hclust(res.dist, method = "complete")

# in order to colorize the clusters, we dont simple ggdendrogram,
# we use data segments

gden <- fviz_dend(hc, k = 3, cex = .75, color_labels_by_k = TRUE) +
    # coord_flip() +
    # scale_y_reverse(expand=c(0.2, 0)) +
    theme_fivethirtyeight() +
    theme(
        text = element_text(family = "Roboto Condensed", size = 3),
        title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.minor.x = element_blank()
    ) +
    labs(
        title = "Dendrogram",
        subtitle = "Hierarchical clustering with complete linkage",
        caption = glue("
                      Source: basketball-reference
                      By: Rui Qiu (rq47)")) +
    theme_dendro()
    
# dendr <- dendro_data(hc, type = "triangle")
# clust <- cutree(hc, k=3) # find 3 clusters
# clust.df <- tibble(label = names(clust), cluster = factor(clust))
# # dendr[["labels"]] has the labels, merge with clust.df based on label column
# dendr[["labels"]] <- merge(dendr[["labels"]], clust.df, by = "label")
# gden <- ggplot() +
#     geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) +
#     geom_text(data=label(dendr), aes(x, y, label=label, hjust=0,
#                                      color=cluster), size=3) +
#     coord_flip() +
#     scale_y_reverse(expand=c(0.2, 0)) +
#     theme_fivethirtyeight() +
#     theme(
#         text = element_text(family = "Roboto Condensed"),
#         title = element_text(size = 18),
#         plot.subtitle = element_text(size = 16),
#         plot.caption = element_text(size = 10),
#         axis.title = element_text(size = 14),
#         axis.line.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.title.y=element_blank(),
#         panel.grid.minor.x = element_blank()
#     ) +
#     labs(
#         title = "Dendrogram",
#         subtitle = "Hierarchical clustering with complete linkage",
#         caption = glue("
#                       Source: basketball-reference
#                       By: Rui Qiu (rq47)")) +
#     theme_dendro()
ggsave(gden, filename = glue("./nba-player-clustering/05-04-dendrogram-complete.png"),
       device = "png", height = 9, width = 11, dpi = 100)

#######################
#     3 Distances     #
#######################

dist_euc <- factoextra::get_dist(dat2, method = "euclidean")
dist_man <- factoextra::get_dist(dat2, method = "manhattan")

cosine.sim <- function(X) {
    X <- as.matrix(X)
    sim <- X / sqrt(rowSums(X * X))
    return(dist(sim %*% t(sim)))
}

dist_cos <- cosine.sim(dat2)

geuc <- fviz_dist(
    dist_euc,
    gradient = list(low = "red", mid = "white", high = "blue")
) +
    theme_fivethirtyeight() +
    theme(
        text = element_text(family = "Roboto Condensed"),
        title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank()
    ) +
    labs(
        title = "Visualization of Euclidean Distances", 
        caption = glue("
                      Source: basketball-reference
                      By: Rui Qiu (rq47)"))
ggsave(geuc, filename = glue("./nba-player-clustering/05-05-dist-euc.png"),
       device = "png", height = 9, width = 11, dpi = 100)

gman <- fviz_dist(
    dist_man,
    gradient = list(low = "red", mid = "white", high = "blue")
) +
    theme_fivethirtyeight() +
    theme(
        text = element_text(family = "Roboto Condensed"),
        title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank()
    ) +
    labs(
        title = "Visualization of Manhattan Distances", 
        caption = glue("
                      Source: basketball-reference
                      By: Rui Qiu (rq47)"))
ggsave(gman, filename = glue("./nba-player-clustering/05-05-dist-man.png"),
       device = "png", height = 9, width = 11, dpi = 100)

gcos <- fviz_dist(
    dist_cos,
    gradient = list(low = "red", mid = "white", high = "blue")
) +
    theme_fivethirtyeight() +
    theme(
        text = element_text(family = "Roboto Condensed"),
        title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank()
    ) +
    labs(
        title = "Visualization of Cosine Similarities",
        caption = glue("
                      Source: basketball-reference
                      By: Rui Qiu (rq47)"))
ggsave(gman, filename = glue("./nba-player-clustering/05-05-dist-cos.png"),
       device = "png", height = 9, width = 11, dpi = 100)


#####################################
#     Determining the Optimal k     #
#####################################

elbow <- fviz_nbclust(
    dat2, 
    kmeans, 
    k.max = 5,
    method = "wss"
) + 
    # geom_vline(xintercept = 2, linetype = 2) +
    theme_fivethirtyeight() +
    theme(
        text = element_text(family = "Roboto Condensed"),
        title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor.x = element_blank()
    ) +
    labs(
        subtitle = "Elbow method",
        caption = glue("
                      Source: basketball-reference
                      By: Rui Qiu (rq47)"))
ggsave(elbow, filename = glue("./nba-player-clustering/05-03-optimal-k-elbow.png"),
       device = "png", height = 9, width = 11, dpi = 100)

silhouette <- fviz_nbclust(
    dat2, 
    kmeans, 
    k.max = 5,
    method = "silhouette"
) + 
    # geom_vline(xintercept = 2, linetype = 2) +
    theme_fivethirtyeight() +
    theme(
        text = element_text(family = "Roboto Condensed"),
        title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor.x = element_blank()
    ) +
    labs(
        subtitle = "Silhouette method",
        caption = glue("
                      Source: basketball-reference
                      By: Rui Qiu (rq47)"))
ggsave(silhouette, filename = glue("./nba-player-clustering/05-03-optimal-k-silhouette.png"),
       device = "png", height = 9, width = 11, dpi = 100)

gap_stat <- fviz_nbclust(
    dat2, 
    kmeans, 
    k.max = 5,
    method = "gap"
) + 
    # geom_vline(xintercept = 3, linetype = 2) +
    theme_fivethirtyeight() +
    theme(
        text = element_text(family = "Roboto Condensed"),
        title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor.x = element_blank()
    ) +
    labs(
        subtitle = "Gap statistic method",
        caption = glue("
                      Source: basketball-reference
                      By: Rui Qiu (rq47)"))
ggsave(gap_stat, filename = glue("./nba-player-clustering/05-03-optimal-k-gap-stat.png"),
       device = "png", height = 9, width = 11, dpi = 100)

vote <- fviz_nbclust(NbClust(dat2, distance = "euclidean", min.nc = 2,
                     max.nc = 5, method = "kmeans")) +
    theme_fivethirtyeight() +
    theme(
        text = element_text(family = "Roboto Condensed"),
        title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor.x = element_blank()
    ) +
    labs(
        subtitle = "factoextra::fviz_nbclust()",
        caption = glue("
                      Source: basketball-reference
                      By: Rui Qiu (rq47)"))
ggsave(vote, filename = glue("./nba-player-clustering/05-03-optimal-k-vote.png"))

# *** : The Hubert index is a graphical method of determining the number of clusters.
# In the plot of Hubert index, we seek a significant knee that corresponds to a 
# significant increase of the value of the measure i.e the significant peak in Hubert
# index second differences plot. 
# 
# *** : The D index is a graphical method of determining the number of clusters. 
# In the plot of D index, we seek a significant knee (the significant peak in Dindex
#                                                     second differences plot) that corresponds to a significant increase of the value of
# the measure. 
# 
# ******************************************************************* 
#     * Among all indices:                                                
#     * 7 proposed 2 as the best number of clusters 
# * 8 proposed 3 as the best number of clusters 
# * 4 proposed 4 as the best number of clusters 
# * 4 proposed 5 as the best number of clusters 
# 
# ***** Conclusion *****                            
#     
#     * According to the majority rule, the best number of clusters is  3 
# 
# 
# ******************************************************************* 
#     Among all indices: 
#     ===================
#     * 2 proposed  0 as the best number of clusters
# * 1 proposed  1 as the best number of clusters
# * 7 proposed  2 as the best number of clusters
# * 8 proposed  3 as the best number of clusters
# * 4 proposed  4 as the best number of clusters
# * 4 proposed  5 as the best number of clusters
# 
# Conclusion
# =========================
#     * According to the majority rule, the best number of clusters is  3 .
    

###############################
#     New Data Prediction     #
###############################

# Adding some data points are always an imperfect move, since
# clustering is unsupervised learning, it heavily relies on the "training" data. So here's the issue: if the newly added data are not very extreme, nor they are not very influential in quantity (say greater than 10% of the previous data), then they are good. Otherwise, we might want to recalculate the centroids.

# https://stackoverflow.com/questions/21064315/how-do-i-predict-new-datas-cluster-after-clustering-training-data

dat3 <- dat %>%
    slice_max(MP, n = 55) %>%
    select(`FG%`, `3PA`, `eFG%`, FT, TRB,
           AST, STL, BLK, TOV, PTS) %>%
    scale()

# this actually shows nothing

iter_kmeans(data = dat3, kvalues = 3, flag = "predictive",
            caption = glue("
                      * Top 55 players ordered by playing time.
                      ** 51-55th players are added for prediction.
                      Source: basketball-reference
                      By: Rui Qiu (rq47)"))

# we can use knn to decide the cluster of some new observations.

groups <- cutree(hc, k = 3)
table(groups)

knnClust <- knn(train = dat3[1:50,], test = dat3[51:55,], k=1, cl =groups)
knnClust

pca1 <- data.frame(prcomp(dat3[1:50,], scale. = T, center = T)$x[,1:2], cluster = as_factor(groups), factor = "train")
pca2 <- data.frame(prcomp(dat3[51:55,], scale. = T, center = T)$x[,1:2], cluster = as_factor(knnClust), factor = "test")
pca <- rbind(pca1, pca2) %>%
    as_tibble() %>%
    mutate(factor = as_factor(factor),
           player = rownames(dat3))

ppred <- ggplot(pca, aes(x = PC1, y = PC2, color = cluster, alpha = factor, size = factor, label = player)) +
    geom_point(shape=19) +
    ggrepel::geom_text_repel() +
    scale_alpha_discrete(range = c(.65, 1)) +
    scale_size_discrete(range=c(.75, 1.5)) +
    theme_fivethirtyeight() +
    theme(
        text = element_text(family = "Roboto Condensed"),
        title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor.x = element_blank()
    ) +
    labs(
        title = "PCA Clustering",
        subtitle = "with new observations introduced",
        caption = glue("
                      * Top 55 players ordered by playing time.
                      ** 51-55th players are added for prediction.
                      Source: basketball-reference
                      By: Rui Qiu (rq47)"))
ggsave(ppred, filename = glue("./nba-player-clustering/05-02-predictive-new-k3.png"))
