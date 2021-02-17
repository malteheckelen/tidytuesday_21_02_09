tuesdata <- tidytuesdayR::tt_load('2021-02-09')
tuesdata <- tidytuesdayR::tt_load(2021, week = 7)

library(tidyverse)
library(ggraph)
library(igraph)
library(magick)
library(cowplot)

# there were duplicates for Asian, Alone, so I averaged those out

income_mean <- tuesdata$income_mean %>%
  group_by(year, income_quintile, race, dollar_type) %>%
  mutate(income_dollars = mean(income_dollars)) %>%
  ungroup()

# filter years and races

prep_data <- income_mean %>%
  filter((year == 1980 | year == 1990 | year == 2000 | year == 2010 | year ==2019)) %>%
  filter(race %in% c("White Alone", "Black Alone", "Hispanic", "Asian Alone")) %>%
  filter(dollar_type == "2019 Dollars") %>%
  group_by(race,year,income_quintile) %>%
  summarise(income_dollars = sum(income_dollars)) %>%
  ungroup()

# define function for plotting of tileplots
# uses prep_data, filter year and filter quintile as inputs

circle_plot_incomequint_x_race <- function(data, filt_year, filt_quint) {

  max_plot_size <- data %>%
    group_by(income_quintile, year) %>%
    mutate(sums = sum(income_dollars)) %>%
    ungroup() %>%
    select(sums) %>%
    max()

  vertices_d_19 <- prep_data %>%
    filter(year == filt_year) %>%
    mutate(name = race) %>%
    mutate(income_quintile = ifelse(income_quintile == 'Top 5%', 'top_5', income_quintile)) %>%
    unite('id', c(race, income_quintile))
  
  vertices_d_19 <- rbind(vertices_d_19, c('Fourth', filt_year, sum(vertices_d_19[grepl('Fourth', vertices_d_19$id),]$income_dollars), 'Fourth'),
                         c('Highest', filt_year, sum(vertices_d_19[grepl('Highest', vertices_d_19$id),]$income_dollars), 'Highest'),
                         c('Lowest', filt_year, sum(vertices_d_19[grepl('Lowest', vertices_d_19$id),]$income_dollars), 'Lowest'),
                         c('Middle', filt_year, sum(vertices_d_19[grepl('Middle', vertices_d_19$id),]$income_dollars), 'Middle'),
                         c('Second', filt_year, sum(vertices_d_19[grepl('Second', vertices_d_19$id),]$income_dollars), 'Second'),
                         c('top_5', filt_year, sum(vertices_d_19[grepl('top_5', vertices_d_19$id),]$income_dollars), 'top_5')) %>%
    mutate(color = ifelse(grepl('^Black', id), '#79fc79', 'black')) %>%
    mutate(color = ifelse(grepl('^Hispanic', id), '#83c8fc', color)) %>%
    mutate(color = ifelse(grepl('^White', id), '#ff6370', color)) %>%
    mutate(color = ifelse(grepl('^Asian', id), '#d78cfa', color)) %>%
    mutate(name = ifelse(name %in% c('Black', 'White', 'Hispanic'), '', name))
  
  vertices_d_19['income_dollars'] <- as.numeric(vertices_d_19$income_dollars)
  
  edges_d_19 <- vertices_d_19 %>%
    mutate(from = ifelse(id != 'top_5', sub('^.*?\\_', '', id), id)) %>%
    mutate(from = ifelse(id != 'top_5', sub('\\_[0-9]+$', '', from), from)) %>%
    mutate(from = ifelse(from == 'top', 'top_5', from)) %>%
    rename('to'='id') %>%
    select(from, to)
  
  # the tiles are sized in proportion to the largest box in the final plot. we got the max_plot_size above
  # since we work with squares, we can consider the sum of income as the area
  # sqrt is then length of one side
  # we relate side length of current box to biggest box on plot and determine, how much to reduce the current box with a margin
  margin_size <- (sqrt(max_plot_size) - sqrt(sum(vertices_d_19 %>% filter(id == filt_quint) %>% select(income_dollars)))) / sqrt(max_plot_size)
  
  vertices_d_19 <- vertices_d_19 %>%
    mutate(income_dollars_text = ifelse(id %in% c('Highest', 'Lowest', 'Fourth', 'Middle', 'Second', 'top_5'), '', as.character(income_dollars))) %>%
    mutate(label_size = max_plot_size / sum(vertices_d_19 %>% filter(id == filt_quint) %>% select(income_dollars)))
  
  mygraph <- graph_from_data_frame(edges_d_19 %>% filter(grepl(filt_quint, to)), vertices=vertices_d_19 %>% filter(grepl(filt_quint, id)))
  mygraph <- set_vertex_attr(mygraph, 'income_dollars', value=as.numeric(V(mygraph)$income_dollars))

  return(ggraph(mygraph, layout = 'treemap', weight=income_dollars) + 
           geom_node_tile(aes(fill=I(color)),colour='white', size = 0.5) +
           theme_void() +
           coord_fixed() +
           theme(legend.position = 'none',
                 plot.margin=unit(c(margin_size, margin_size / 2,0, margin_size / 2),"npc")))
}

columns_grid <- plot_grid(textGrob(''), 
                             textGrob('Lowest', gp=gpar(fontsize=30,fontface='bold',fontfamily='URWBookman'), x=-0.5), 
                             textGrob('Second', gp=gpar(fontsize=30,fontface='bold',fontfamily='URWBookman'), x=-0.3),
                             textGrob('Middle', gp=gpar(fontsize=30,fontface='bold',fontfamily='URWBookman'), x=0.008),
                             textGrob('Fourth', gp=gpar(fontsize=30,fontface='bold',fontfamily='URWBookman'), x=0.07),
                             textGrob('Highest', gp=gpar(fontsize=30,fontface='bold',fontfamily='URWBookman'), x=0.2),
                             textGrob('Top 5%', gp=gpar(fontsize=30,fontface='bold',fontfamily='URWBookman'), x=0.4), 
                          ncol=7)
row_grid <- plot_grid(textGrob('1980', gp=gpar(fontsize=30,fontface='bold',fontfamily='URWBookman', just='bottom'), y=0), 
                         textGrob('1990', gp=gpar(fontsize=30,fontface='bold',fontfamily='URWBookman', just='bottom'), y=0),
                         textGrob('2000', gp=gpar(fontsize=30,fontface='bold',fontfamily='URWBookman', just='bottom'), y=0),
                         textGrob('2010', gp=gpar(fontsize=30,fontface='bold',fontfamily='URWBookman', just='bottom'), y=0),
                         textGrob('2019', gp=gpar(fontsize=30,fontface='bold',fontfamily='URWBookman', just='bottom'), y=0), 
                      ncol=1)
plots_grid <- plot_grid(circle_plot_incomequint_x_race(prep_data, 1980, 'Lowest', palette_races), circle_plot_incomequint_x_race(prep_data, 1980, 'Second', palette_races), circle_plot_incomequint_x_race(prep_data, 1980, 'Middle', palette_races), circle_plot_incomequint_x_race(prep_data, 1980, 'Fourth', palette_races), circle_plot_incomequint_x_race(prep_data, 1980, 'Highest', palette_races), circle_plot_incomequint_x_race(prep_data, 1980, 'top_5', palette_races),
                          circle_plot_incomequint_x_race(prep_data, 1990, 'Lowest', palette_races), circle_plot_incomequint_x_race(prep_data, 1990, 'Second', palette_races), circle_plot_incomequint_x_race(prep_data, 1990, 'Middle', palette_races), circle_plot_incomequint_x_race(prep_data, 1990, 'Fourth', palette_races), circle_plot_incomequint_x_race(prep_data, 1990, 'Highest', palette_races), circle_plot_incomequint_x_race(prep_data, 1990, 'top_5', palette_races),
                          circle_plot_incomequint_x_race(prep_data, 2000, 'Lowest', palette_races), circle_plot_incomequint_x_race(prep_data, 2000, 'Second', palette_races), circle_plot_incomequint_x_race(prep_data, 2000, 'Middle', palette_races), circle_plot_incomequint_x_race(prep_data, 2000, 'Fourth', palette_races), circle_plot_incomequint_x_race(prep_data, 2000, 'Highest', palette_races), circle_plot_incomequint_x_race(prep_data, 2000, 'top_5', palette_races),
                          circle_plot_incomequint_x_race(prep_data, 2010, 'Lowest', palette_races), circle_plot_incomequint_x_race(prep_data, 2010, 'Second', palette_races), circle_plot_incomequint_x_race(prep_data, 2010, 'Middle', palette_races), circle_plot_incomequint_x_race(prep_data, 2010, 'Fourth', palette_races), circle_plot_incomequint_x_race(prep_data, 2010, 'Highest', palette_races), circle_plot_incomequint_x_race(prep_data, 2010, 'top_5', palette_races),
                          circle_plot_incomequint_x_race(prep_data, 2019, 'Lowest', palette_races), circle_plot_incomequint_x_race(prep_data, 2019, 'Second', palette_races), circle_plot_incomequint_x_race(prep_data, 2019, 'Middle', palette_races), circle_plot_incomequint_x_race(prep_data, 2019, 'Fourth', palette_races), circle_plot_incomequint_x_race(prep_data, 2019, 'Highest', palette_races), circle_plot_incomequint_x_race(prep_data, 2019, 'top_5', palette_races), 
                          ncol=6)
bottom <- textGrob('viz by @Boomcrashkapow (Malte Heckelen) | Data by Urban Institute & US Census',
                   x=.8, gp=gpar(fontsize=18,fontfamily='Arial'))
main <- textGrob('\nUnequal Fives: Average U.S. Income per Population Fifths, and Top 5%',
                 gp=gpar(fontsize=55,fontface='bold',fontfamily='Arial'),
                 x=.47)

# images made in GIMP
img <- image_read("images/shadow.png") 
rich_img <- image_read('images/rich_man.png')
poor_img <- image_read('images/poor_man.png')
sub1 <- textGrob('Box size signifies average income in category',
                 x=.08, gp=gpar(just=c('left'), fontsize=18,fontfamily='Arial'))
sub2 <- textGrob('Incomes were sorted and then averages for equal parts of the list were taken, as well as for the top 5%: How much more income does the average person in the Top 5% have, compared to other groups?',
                 x=.409,gp=gpar(just=c('left'), fontsize=18,fontfamily='Arial'))
sub3 <- textGrob('Colors signify share of noted race: Hispanic (red), Black (green), White (blue) and Asian (purple; only data since 2002). If a color has a bigger share of a box, it has the bigger average income.' ,
                 x=.383,gp=gpar(just=c('left'), fontsize=18,fontfamily='Arial'))

png('plot.png', width=2000, height=2000)
ggdraw() +
  draw_image(img, y=.085) +
  draw_image(img, y=-.045) +
  draw_image(img, y=-.175) +
  draw_image(img, y=-.31) +
  draw_image(img, y=-.442) +
  draw_plot(plot_grid(NA, main,
                      NA, sub1,
                      NA, sub2,
                      NA, sub3,
                      NA,NA,
          NA, columns_grid, 
          row_grid,plots_grid, 
          NA, bottom, 
          ncol=2,
          rel_widths=c(0.05,0.9),
          rel_heights=c(0.1,0.02,0.02,0.02,0.1,0.05,0.8,0.1))) +
  draw_image(rich_img,
             width=0.1,
             height=0.1,
             y=.078,
             x=.835) +
  draw_image(poor_img,
             width=0.08,
             height=0.08,
             y=.608,
             x=.08)
dev.off()

