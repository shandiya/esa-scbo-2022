
library(gt)
library(dplyr)

# theme for gt tables -----
gt_mytheme <- function(data, ...){
  data |> 
    opt_table_font(
      font = list(
        google_font("Lato"),
        default_fonts())) |> 
    opt_row_striping() |> 
    tab_options(
      table.font.size = 24,
      table.font.weight = "lighter",
      column_labels.font.size = 28,
      column_labels.font.weight = "lighter",
      row.striping.background_color = "#f3f3f3",
      table_body.hlines.color = "transparent",
      table.font.color = "#413f42",
      data_row.padding = 3,
      column_labels.background.color = "#555555",
      ...
    ) 
}

# years to time periods -----
as_period_string <- function(df) {
  
  df |>
    mutate(
      period = case_when(
        yearStart == 1900 ~ "1900-1970",
        yearStart == 1971 ~ "1971 - 1975",
        yearStart == 1976 ~ "1976 - 1980",
        yearStart == 1981 ~ "1981 - 1985",
        yearStart == 1986 ~ "1986 - 1990",
        yearStart == 1991 ~ "1991 - 1995",
        yearStart == 1996 ~ "1996 - 2000",
        yearStart == 2001 ~ "2001 - 2005",
        yearStart == 2006 ~ "2006 - 2010",
        yearStart == 2011 ~ "2011 - 2015",
        yearStart == 2016 ~ "2016 - 2020"))
  
}

# plot heatmap ------
plot_heatmap <- function(data, region, fill_var, legend_title, pal, n_breaks) {
  
  ggplot(data = data,
         aes(x = period,
             y = reorder({{region}}, desc({{region}})),
             fill = {{fill_var}})) +
    geom_tile() +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_fill_distiller(name = legend_title,
                         type = "seq",
                         palette = pal,
                         direction = 1,
                         trans = "log",
                         breaks = scales::breaks_log(n = n_breaks),
                         guide = guide_colorbar(direction = "horizontal",
                                                label.position = "bottom",
                                                draw.ulim = FALSE, 
                                                draw.llim = FALSE,
                                                title.position = "top",
                                                ticks = FALSE,
                                                barwidth = 16)) +
    
    theme_classic() +
    theme(text = element_text(family = "lato"),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 12, colour = "#444444"),
          legend.title.align = 0.5,
          plot.background = element_rect(fill = "#f3f3f3", colour = NA),
          panel.background = element_rect(fill = "#f3f3f3", colour = NA),
          legend.background = element_rect(fill = "#f3f3f3", colour = NA),
          legend.box.background = element_rect(fill = "#f3f3f3", colour = NA))
  
}

