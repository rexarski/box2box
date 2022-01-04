# Followed the step-by-step tutorial on "Getting blue fingers":
# https://www.gettingbluefingers.com/tutorials/RadarPizzaChart

if (!require("pacman")) install.packages("pacman")
pacman::p_load(worldfootballR, tidyverse, forcats, glue)

df <- fb_player_scouting_report("https://fbref.com/en/players/5afafbfe/Shinji-Kagawa")

df_selected <- df[c(2,3,9,10,13,28,29,47,73,107,109,116,118,126,148),]

df_selected <- df_selected %>% 
    mutate(stat=case_when(Statistic == "Non-Penalty Goals"|
                          Statistic == "npxG"|
                          Statistic == "Shots Total"|
                          Statistic == "Assists"|
                          Statistic == "xA"|
                          Statistic == "npxG+xA"|
                          Statistic == "Shot-Creating Actions" ~ "Attacking",
                          Statistic == "Passes Attempted"|
                          Statistic == "Pass Completion %"|
                          Statistic == "Progressive Passes"|
                          Statistic == "Progressive Carries"|
                          Statistic == "Dribbles Completed"|
                          Statistic == "Touches (Att Pen)"|
                          Statistic == "Progressive Passes Rec" ~ "Possession",
                          TRUE ~ "Defending"))

ggplot(df_selected,aes(fct_reorder(Statistic,stat),Percentile)) +
    geom_bar(aes(y=100,fill=stat),stat="identity",width=1,colour="white",
             alpha=0.5) +
    geom_bar(stat="identity",width=1,aes(fill=stat),colour="white") +
    coord_polar() +
    geom_label(aes(label=Per90,fill=stat),size=2,color="white",show.legend = FALSE)+
    scale_fill_manual(values=c("Possession" = "#D70232",
                               "Attacking" = "#1A78CF",
                               "Defending" = "#FF9300")) +
    scale_y_continuous(limits = c(-10,100))+
    labs(fill="",
         caption = "Data from StatsBomb via FBref",
         title=df_selected$Player[1])+
    theme_minimal() +
    theme(legend.position = "top",
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          text = element_text(family="Roboto Condensed"),
          plot.title = element_text(hjust=0.5),
          plot.caption = element_text(hjust=0.5,size=6),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())

temp <- (360/(length(df_selected$Player))/2)
myAng <- seq(-temp, -360+temp, length.out = length(df_selected$Player))
ang<-ifelse(myAng < -90, myAng+180, myAng)
ang<-ifelse(ang < -90, ang+180, ang) 

df_selected$Statistic <- gsub(" ","\n",df_selected$Statistic)


ggplot(df_selected,aes(fct_reorder(Statistic,stat),Percentile)) +
    geom_bar(aes(y=100,fill=stat),stat="identity",width=1,colour="white",
             alpha=0.5) +
    geom_bar(stat="identity",width=1,aes(fill=stat),colour="white") +
    coord_polar() +
    geom_label(aes(label=Per90,fill=stat),size=2,color="white",show.legend = FALSE)+
    scale_fill_manual(values=c("Attacking" = "#1A78CF",
                               "Defending" = "#FF9300",
                               "Possession" = "#D70232")) +
    scale_y_continuous(limits = c(-10,100))+
    labs(fill="",
         caption = "Data from StatsBomb via FBref",     
         #remove legend title
         title=glue("{df_selected$Player[1]} | Dortmund"),
         subtitle = glue::glue("{df_selected$season} | Compared to midfielders Top 5 competitions | stats per 90")) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "#F2F4F5",color = "#F2F4F5"),
          panel.background = element_rect(fill = "#F2F4F5",color = "#F2F4F5"),
          legend.position = "top",
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 6, angle = ang),
          text = element_text(family="Roboto Condensed"),
          plot.title = element_text(hjust=0.5,family="Roboto Condensed"),
          plot.subtitle = element_text(hjust=0.5,size=8),
          plot.caption = element_text(hjust=0.5,size=6),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.margin = margin(5,2,2,2))

ggsave("radar-pizza-chart/kagawa.png",bg="#F2F4F5")

# The Athletic/ Tom Worville Style

# ggplot(df_selected,aes(fct_reorder(Statistic,stat),Percentile)) +                       
#     geom_bar(aes(y=100),fill="#131313",stat="identity",width=1,colour="#797979",                 
#              alpha=0.5,show.legend = FALSE) +      
#     
#     
#     geom_bar(stat="identity",width=1,aes(fill=stat),colour="#F3FEFC",alpha=1) +                     
#     coord_polar(clip = "off") +                                                                      
#     geom_hline(yintercept=25, colour="#565656",linetype="longdash",alpha=0.5)+
#     geom_hline(yintercept=50, colour="#565656",linetype="longdash",alpha=0.5)+
#     geom_hline(yintercept=75, colour="#565656",linetype="longdash",alpha=0.5)+ 
#     scale_fill_manual(values=c("Possession" = "#1ADA89",                                   
#                                "Attacking" = "#0F70BF",
#                                "Defending" = "#EC313A")) +                                                        
#     geom_label(aes(label=Percentile,fill=stat),size=2,color="white",show.legend = FALSE)+ 
#     scale_y_continuous(limits = c(-20,100))+                                              
#     labs(fill="",   
#          caption = "Data from StatsBomb via FBref\nStyle copied from The Athletic/@worville",     
#          #remove legend title
#          title=glue("{df_selected$Player[1]} | Leeds United"),
#          subtitle = glue::glue("{df_selected$season} | Compared to midfielders Top 5 competitions | stats per 90"))+                                                
#     theme_minimal() +                                                                     
#     theme(plot.background = element_rect(fill = "#131313",color = "#131313"),
#           panel.background = element_rect(fill = "#131313",color = "#131313"),
#           legend.position = "bottom",
#           axis.title.y = element_blank(),
#           axis.title.x = element_blank(),
#           axis.text.y = element_blank(),
#           axis.text.x = element_text(size = 6,colour = "#FFFFFF"),
#           text = element_text(family="Spartan-Light",colour= "#FEFEFE"),                                   
#           plot.title = element_markdown(hjust=0.5,family="Spartan-Medium"),
#           plot.subtitle = element_text(hjust=0.5,size=8),
#           plot.caption = element_text(hjust=0.5,size=6),
#           panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank(),
#           plot.margin = margin(5,4,2,4)) 
