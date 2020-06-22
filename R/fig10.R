# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Daniel Vaulot
#
# DESCRIPTION:  Phytoplankton diversity
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Function to draw treemaps -----------------------------

long_treemap <- function(df, group1, group2, title, colors = NULL) {
  df <- df %>%
    count({{ group1 }}, {{ group2 }})

  gg <- ggplot(df, aes(
    area = n,
    fill = {{ group2 }},
    label = {{ group2 }},
    subgroup = {{ group1 }}
  )) +
    ggtitle(title) +
    geom_treemap() +
    geom_treemap_subgroup_border(colour = "white") +
    geom_treemap_text(
      colour = "yellow",
      place = "topleft",
      reflow = TRUE,
      padding.x = grid::unit(1, "mm"),
      padding.y = grid::unit(1, "mm")
    ) +
    geom_treemap_subgroup_text(
      place = "centre",
      grow = TRUE,
      alpha = 0.5,
      colour = "white",
      fontface = "italic",
      min.size = 5
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 8, face = "bold"),
      panel.border = element_blank(),
      panel.grid = element_blank()
    )
  if (is.null(colors)) {
    #    gg <- gg + scale_fill_viridis_d()
    gg <-
      gg + paletteer::scale_fill_paletteer_d("ggsci::default_igv")
  } else {
    gg <- gg + scale_fill_manual(values = colors)
  }

  return(gg)
}

# Plot treemaps for cultures -----------------------------

df <- read_excel("data/raw/MALINA cultures 2020.xlsx")
g_cult <- long_treemap(df, class, species, "Cultures")

g_cult

# Plot treemaps for clone libraries -----------------------------

df <- read_excel("data/raw/MALINA sequences 2020.xlsx")
g_seq <- long_treemap(df, class, species, "Clone libraries")

g_seq

# Save --------------------------------------------------------------------


p <- g_seq + g_cult +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  "graphs/fig10.pdf",
  device = cairo_pdf,
  width = 17.5,
  height = 14,
  units = "cm"
)
