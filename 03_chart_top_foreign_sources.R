# =============================================================================
# 03_chart_top_foreign_sources.R
# Horizontal bars: top 10 foreign sources of value added in Bangladeshi
# textiles (c4), latest year, coloured by cumulation group.
# =============================================================================
source("E:/RoO_Research/scripts/00_chart_setup.R")

va <- fread(file.path(tab_dir, "m1_va_origin_by_country.csv"))
va[is.na(group) | group == "", group := "None"]

nm <- c(PRC = "China", IND = "India", RoW = "Rest of World", USA = "United States", PAK = "Pakistan",
        BRA = "Brazil", INO = "Indonesia", TAP = "Taipei, China", JPN = "Japan", KOR = "Korea",
        VIE = "Viet Nam", GER = "Germany", ITA = "Italy", TUR = "Turkiye", SRI = "Sri Lanka",
        THA = "Thailand", AUS = "Australia", MAL = "Malaysia", SIN = "Singapore", UKG = "United Kingdom")

top <- va[sector == "c4" & year == max(year) & group != "Domestic"][order(-va_share)][seq_len(min(10, .N))]
top[, name := ifelse(is.na(nm[country]), country, nm[country])]
top[, name := factor(name, levels = rev(name))]
prc  <- top[country == "PRC", 100 * va_share]
rest <- top[country != "PRC", 100 * sum(va_share)]

p3 <- ggplot(top, aes(100 * va_share, name, fill = group)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%.1f", 100 * va_share)), hjust = -0.2, size = 3.4) +
  scale_fill_manual(values = pal, breaks = c("None", "SAARC", "EU", "Extended"),
                    labels = c(None = "Non-cumulable", SAARC = "SAARC (cumulable)",
                               EU = "EU (cumulable)", Extended = "Extended (cumulable)")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = sprintf("China adds %.1f cents to every US$1 of Bangladeshi textiles,\nmore than the next nine sources combined (%.1f)",
                       prc, rest),
       subtitle = sprintf("Top 10 foreign sources of value added, US cents per US$1 of textile output (c4), %d",
                          max(top$year)),
       x = NULL, y = NULL, caption = src) +
  theme_clean() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#E6E6E6", linewidth = 0.4),
        axis.text.y = element_text(colour = "#121212"))

print(p3)
save_png(p3, "m1_top_foreign_sources.png")
