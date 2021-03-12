pacman::p_load(tidyverse, ggrepel, ggthemes, extrafont, magick, glue)
dat <- read_csv('data/jiangsu/jiangsu_top_scorers.csv')

## add_logo function from Thomas Mock
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
    
    # Requires magick R Package https://github.com/ropensci/magick
    
    # Useful error message for logo position
    if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
        stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
    }
    
    # read in raw images
    plot <- magick::image_read(plot_path)
    logo_raw <- magick::image_read(logo_path)
    
    # get dimensions of plot for scaling
    plot_height <- magick::image_info(plot)$height
    plot_width <- magick::image_info(plot)$width
    
    # default scale to 1/10th width of plot
    # Can change with logo_scale
    logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
    
    # Get width of logo
    logo_width <- magick::image_info(logo)$width
    logo_height <- magick::image_info(logo)$height
    
    # Set position of logo
    # Position starts at 0,0 at top left
    # Using 0.01 for 1% - aesthetic padding
    
    if (logo_position == "top right") {
        x_pos = plot_width - logo_width - 0.01 * plot_width
        y_pos = 0.01 * plot_height
    } else if (logo_position == "top left") {
        x_pos = 0.01 * plot_width
        y_pos = 0.01 * plot_height
    } else if (logo_position == "bottom right") {
        x_pos = plot_width - logo_width - 0.01 * plot_width
        y_pos = plot_height - logo_height - 0.01 * plot_height
    } else if (logo_position == "bottom left") {
        x_pos = 0.01 * plot_width
        y_pos = plot_height - logo_height - 0.01 * plot_height
    }
    
    # Compose the actual overlay
    magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
    
}

scorers <- dat %>%
    ggplot(aes(x = Apps, y = Goals, label = Name)) +
    geom_point(data = dat %>%
                   filter(index <= 10),
               color = "#112987", alpha = 0.8, size = 3) +
    geom_point(data = dat %>%
                   filter (index > 10),
               color = "grey75", size = 3) + 
    ggrepel::geom_text_repel(max.overlaps = 20, seed = 15, size = 4,
                             point.padding = 0.5, color = "grey20",
                             family = "Roboto Condensed") + 
    labs(title = "Jiangsu FC's All-time Top Goalscorers",
         subtitle = "2008-2020",
         caption = glue("
                      * Players with blue dots are top 10 goalscorers.
                      Data: transfermarkt.com
                      By: @rexarski"),
         x = "Appearances",
         y = "Goals") +
    ggthemes::theme_fivethirtyeight() +
    theme(text = element_text(family = "Roboto Condensed"),
          title = element_text(size = 18),
          plot.subtitle = element_text(size = 16),
          plot.caption = element_text(size = 10),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          panel.grid.minor.x = element_blank())
scorers

ggsave(plot = scorers, "a-pipe-dream-jiangsu-fc/jiangsu-top-scorers.png",
       height = 9, width = 11)

plot_logo <- add_logo(
    plot_path = "a-pipe-dream-jiangsu-fc/jiangsu-top-scorers.png",
    logo_path = "data/jiangsu/jiangsu-logo.png",
    logo_position = "top right",
    logo_scale = 12
)
plot_logo

magick::image_write(image = plot_logo,
                    "a-pipe-dream-jiangsu-fc/jiangsu-top-scorers-logo.png")

# yellow = "#f8b501"
# blue = "#112987"

# logo top right
# theme
# point aes
# name and other information next to the point
# what else?
# take a look at Ryo's scatterplots.