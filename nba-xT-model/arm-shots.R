###################
#     set up      #
###################

gc()
rm(list=ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, arules, arulesViz,
               igraph, networkD3, visNetwork,
               jsonlite)

set.seed(2021)


#########################
#     data cleaning     #
#########################

dat <- read_csv("data/nba-pbp/pbpstats-tracking-shots-cleaned.csv")
glimpse(dat)

dat <- dat %>%
    select(player, period, shottype, value, and1, made)

# Turn all variables into factors:
# including:
# numeric -> factor
# logical -> factor with details
# NA -> factor as well

dat <- dat %>%
    mutate(
           player = as_factor(player),
           period = as_factor(period),
           shottype = as_factor(shottype),
           value = as_factor(value),
           and1 = as_factor(case_when(
               and1 == TRUE ~ 'is_and1',
               and1 == FALSE ~ 'not_and1'
           )),
           made = as_factor(case_when(
               made == TRUE ~ 'is_made',
               made == FALSE ~ 'not_made'
           ))
    )

glimpse(dat)


#################################
#     associate rule mining     #
#################################

shot_trans <- as(dat, "transactions")

inspect(head(shot_trans, 2))

# most frequent items

frequentItems <- eclat(shot_trans,
                       parameter = list(support = 0.0001,
                                        minlen = 6))

inspect(head(frequentItems, by = "support", n = 15))


# for rhs: if not limited, the results are just not meaningful enough to be interpreted

# of course, we can limit the output by adding
# `appearance = list(rhs="made=is_made")` inside apriori()

madeItems <- grep("^made=", itemLabels(shot_trans), value = TRUE)

shot_rules <- apriori(shot_trans,
                      parameter = list(
                          support = 0.0001,
                          confidence = 0.5,
                          minlen = 6
                      ),
                      appearance = list(
                          rhs = madeItems
                      ))

inspect(head(shot_rules, by = "support", n = 15))
inspect(head(shot_rules, by = "confidence", n = 15))
inspect(head(shot_rules, by = "lift", n = 15))

# additional rules

zion_rules <- apriori(shot_trans,
                      parameter = list(
                          support = 0.0001,
                          confidence = 0.5,
                          minlen = 2
                      ),
                      appearance = list(
                          lhs = 'player=Zion Williamson'
                      ))
inspect(zion_rules)

subrules1 <- head(shot_rules, by = "support", n = 50)
subrules2 <- head(shot_rules, by = "confidence", n = 50)
subrules3 <- head(shot_rules, by = "lift", n = 50)


##################
#     igraph     #
##################

# static
png(file="nba-xT-model/06-arm-igraph.png", width=1100, height=900)
plot(subrules3, method = "graph", control = list(verbose = FALSE))
dev.off()

png(file="nba-xT-model/06-arm-igraph-2.png", width=1100, height=900)
plot(subrules3, method = "graph", control = list(verbose = FALSE),
     measure = "lift", shading = "confidence") # static
dev.off()

# interactive with all rules (only top 100 plotted though)
plot(shot_rules, method = "graph", control = list(verbose = FALSE),
     measure = "lift", shading = "confidence", engine = "htmlwidget")

# interactive, readable by GraphML tools
saveAsGraph(shot_rules, file = "nba-xT-model/igraph-rules.graphml")


########################################################
#     data preparation for viz with other packages     #
########################################################

df_rules <- DATAFRAME(subrules3) %>%
    rowid_to_column("rules") %>%
    mutate(rules = paste("Rules", rules),
           RHS = str_remove_all(string = RHS, pattern = "[{}]"))
df_rules

df_items <- df_rules %>%
    mutate(LHS = str_remove_all(string = LHS, pattern = "[{}]")) %>%
    separate(col = LHS, into = c(paste0("item_", 1:3)),
             sep = ",") %>% pivot_longer(cols = c(item_1, item_2, item_3), names_to = "antecedent", values_to = "item") %>%
    select(rules, antecedent, item, RHS, everything()) %>%
    filter(is.na(item) == FALSE)
df_items

nodes <- data.frame(name = unique(c(df_items$item, df_items$RHS, df_items$rules))) %>%
    rowid_to_column("id") %>% mutate(group = ifelse(str_detect(name, "Rules"), "A", "B"), label = name, value = c(rep(NA, n_distinct(c(df_items$item, df_items$RHS))), df_rules$lift), support = c(rep(NA, n_distinct(c(df_items$item, df_items$RHS))), df_rules$support), confidence = c(rep(NA, n_distinct(c(df_items$item, df_items$RHS))), df_rules$confidence), shape = ifelse(group == "A", "circle", "box"), color = ifelse(group == "A", "lightblue", "lightgreen"), title = ifelse(test = group == "A", yes = paste(name, "<br> Lift:", round(value, 2), "<br> Confidence:", round(confidence, 2), "<br> Support:", round(support, 2)), no = as.character(name)))
nodes

edges <- data.frame(from = df_items$item, to = df_items$rules) %>%
    bind_rows(data.frame(from = df_rules$rules, to = df_rules$RHS)) %>%
    left_join(nodes, by = c(from = "name")) %>% select(id, to) %>%
    rename(from = id) %>% left_join(nodes, by = c(to = "name")) %>%
    select(from, id) %>%
    rename(to = id) %>%
    mutate(color = ifelse(to <= 33, "red", "lightgreen"))
edges


#####################
#     networkD3     #
#####################

nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1) %>%
    mutate(value = 1)

forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from",
             Target = "to", NodeID = "label", Group = "group",
             Value = "value", arrows = T, Nodesize = "value",
             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
             linkColour = edges_d3$color, opacity = 0.8,
             fontSize = 24, fontFamily = "Roboto Condensed",
             zoom = TRUE)


######################
#     VisNetwork     #
######################

visNetwork(nodes = nodes, edges = edges, height = "500px", width = "100%") %>%
    visEdges(arrows = "to") %>%
    visOptions(highlightNearest = T) %>%
    visInteraction(tooltipStyle = "position: fixed; visibility: hidden; padding: 5px; white-space: nowrap;
    font-size: 18px; color: black; background-color: white; border-color: orange")


##################
#     Sankey     #
##################

sankeyNetwork(Links = edges_d3, Nodes = nodes_d3,
              Source = "from", Target = "to",
              NodeID = "label", Value = "value")


#################
#     D3.js     #
#################

write_json(list(nodes = nodes_d3, links = edges_d3),
           path = "nba-xT-model/network-data.json")
