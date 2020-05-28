# Figure on DOC, THAA and TDLP9. Data given by Cedric Fichot.

rm(list = ls())

df <- vroom::vroom(fs::dir_ls("data/raw/fig_doc_cedric_fichot/")) %>%
  janitor::clean_names() %>%
  mutate(transect = station %/% 100 * 100) %>%
  mutate(transect = factor(transect, levels = c("600", "300")))

df

lab <- c(
  "600" = "Transect 600",
  "300" = "Transect 300"
)

# DOC ---------------------------------------------------------------------

p1 <- df %>%
  drop_na(doc) %>%
  ggplot(aes(x = salinity, y = doc)) +
  geom_line() +
  geom_point() +
  facet_wrap(~transect, ncol = 2, labeller = labeller(transect = lab)) +
  ggrepel::geom_text_repel(aes(label = station),
    size = 2.5,
    color = "gray50",
    box.padding = unit(0.35, "lines"),
    segment.size = 0.25,
    nudge_x = 0.5,
    nudge_y = 0.5
  ) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8), limits = c(0, 27)) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
  labs(
    x = "Salinity (PSU)",
    y = bquote(DOC ~ (µmol ~ L^{-1}))
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 10, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

# TDLP9 -------------------------------------------------------------------

p2 <- df %>%
  drop_na(tdlp9) %>%
  ggplot(aes(x = salinity, y = tdlp9)) +
  geom_line() +
  geom_point() +
  facet_wrap(~transect, ncol = 2, labeller = labeller(transect = lab)) +
  ggrepel::geom_text_repel(aes(label = station),
    size = 2.5,
    color = "gray50",
    box.padding = unit(0.35, "lines"),
    segment.size = 0.25,
    nudge_x = 0.5,
    nudge_y = 0.5
  ) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8), limits = c(0, 27)) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
  labs(
    x = "Salinity (PSU)",
    y = bquote(TDLP9 ~ (nmol ~ L^{-1}))
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 10, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

# THAA --------------------------------------------------------------------

p3 <- df %>%
  drop_na(thaa) %>%
  ggplot(aes(x = salinity, y = thaa)) +
  geom_line() +
  geom_point() +
  facet_wrap(~transect, ncol = 2, labeller = labeller(transect = lab)) +
  ggrepel::geom_text_repel(aes(label = station),
    size = 2.5,
    color = "gray50",
    box.padding = unit(0.35, "lines"),
    segment.size = 0.25,
    nudge_x = 0.5,
    nudge_y = 0.5
  ) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8), limits = c(0, 27)) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
  labs(
    x = "Salinity (PSU)",
    y = bquote(THAA ~ (nmol ~ L^{-1}))
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 10, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

# Combine plots -----------------------------------------------------------

p <- p1 + p2 + p3 +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  "graphs/fig07.pdf",
  device = cairo_pdf,
  width = 17.5,
  height = 18,
  units = "cm"
)
