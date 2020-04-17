rm(list = ls())

# https://www.biogeosciences.net/10/3731/2013/bg-10-3731-2013.pdf
# https://www.biogeosciences.net/12/6669/2015/bg-12-6669-2015.pdf

df <- read_csv("data/raw/csv/xie.csv")

df %>%
  count(type)

df_co2 <- df %>%
  filter(depth == "surface") %>%
  drop_na(c02_production_rate_moles_m_3_s_1) %>%
  mutate(c02_production_rate_umoles_m_3_s_1 = c02_production_rate_moles_m_3_s_1 * 1e6)

df_co2

df_co2 %>%
  ggplot(aes(
    x = factor(cutoff_wavelength_nm),
    y = c02_production_rate_umoles_m_3_s_1
  )) +
  geom_col() +
  scale_y_continuous(
    labels = scales::label_number_auto(),
    expand = expansion(mult = c(0, 0.02)),
    breaks = scales::breaks_pretty(n = 3)
  ) +
  facet_wrap(~station) +
  coord_flip() +
  labs(
    y = bquote(CO[2]~production~(mu*moles~m^{-3}~s^{-1})),
    x = "Wavelength (nm)"
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 14, face = "bold")
  )

ggsave("graphs/fig09.pdf",
  device = cairo_pdf,
  width = 6,
  height = 6
)
