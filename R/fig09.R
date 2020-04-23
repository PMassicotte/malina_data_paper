# Figure on photo-oxydation potential.

rm(list = ls())

# https://www.biogeosciences.net/10/3731/2013/bg-10-3731-2013.pdf
# https://www.biogeosciences.net/12/6669/2015/bg-12-6669-2015.pdf

df <- read_csv("data/raw/csv/xie.csv")

df %>%
  count(type)

df_co2 <- df %>%
  filter(depth == "surface") %>%
  drop_na(c02_production_rate_moles_m_3_s_1) %>%
  mutate(c02_production_rate_umoles_m_3_s_1 = c02_production_rate_moles_m_3_s_1 * 1e6) %>%
  mutate(cutoff_wavelength_nm = factor(cutoff_wavelength_nm)) %>%
  mutate(cutoff_wavelength_nm = fct_rev(cutoff_wavelength_nm)) %>%
  mutate(transect = station %/% 100 * 100) %>%
  mutate(transect = factor(transect, c("600", "300")))

df_co2

df_co2 %>%
  filter(cutoff_wavelength_nm == 280) %>%
  ggplot(aes(
    x = factor(station),
    y = c02_production_rate_umoles_m_3_s_1
  )) +
  geom_col(position = "dodge", color = "white") +
  scale_y_continuous(
    labels = scales::label_number_auto(),
    expand = expansion(mult = c(0, 0.02)),
    breaks = scales::breaks_pretty(n = 3)
  ) +
  facet_wrap(~transect, scales = "free") +
  coord_flip() +
  labs(
    y = bquote(CO[2]~production~at~280~nm~(mu*moles~m^{-3}~s^{-1})),
    x = NULL
  ) +
  theme(
    plot.caption = element_text(size = 4),
    legend.key.size = unit(0.5, "cm"),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 14, face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("graphs/fig09.pdf",
  device = cairo_pdf,
  width = 7,
  height = 3
)


# df_co2 %>%
#   ggplot(aes(
#     x = cutoff_wavelength_nm,
#     y = c02_production_rate_umoles_m_3_s_1
#   )) +
#   geom_col() +
#   scale_y_continuous(
#     labels = scales::label_number_auto(),
#     expand = expansion(mult = c(0, 0.02)),
#     breaks = scales::breaks_pretty(n = 3)
#   ) +
#   facet_wrap(~station) +
#   coord_flip() +
#   labs(
#     y = bquote(CO[2]~production~(mu*moles~m^{-3}~s^{-1})),
#     x = "Wavelength (nm)"
#   ) +
#   theme(
#     plot.caption = element_text(size = 4),
#     legend.key.size = unit(0.5, "cm"),
#     legend.position = "none",
#     strip.background = element_blank(),
#     strip.text = element_text(hjust = 0, size = 14, face = "bold"),
#     panel.border = element_blank(),
#     axis.ticks = element_blank()
#   )
