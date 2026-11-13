# =============================================================================
# 02_chart_export_vs_foreign_driven.R
# Lines: share of textile output exported directly vs share driven by foreign
# final demand (Leontief decomposition), 2018-2024, with the gap shaded.
# =============================================================================
source("E:/RoO_Research/scripts/00_chart_setup.R")

use   <- fread(file.path(tab_dir, "m1_output_use_and_drivers.csv"))
years <- sort(unique(use$year))

w4 <- use[sector == "c4", .(year, exp = 100 * export_share, drv = 100 * foreign_driven_share)]
u4 <- melt(w4, id.vars = "year", variable.name = "m", value.name = "v")
u4[, m := factor(m, levels = c("drv", "exp"),
                 labels = c("Driven by foreign final demand", "Exported directly"))]
ends <- u4[year %in% range(year)]
gap  <- w4[year == max(year), drv - exp]

p2 <- ggplot() +
  geom_ribbon(data = w4, aes(year, ymin = exp, ymax = drv), fill = "#006BA2", alpha = 0.12) +
  geom_line(data = u4, aes(year, v, colour = m), linewidth = 1.2) +
  geom_point(data = u4, aes(year, v, colour = m), size = 2.3) +
  geom_text(data = ends, aes(year, v, label = sprintf("%.0f%%", v), colour = m,
                             hjust = ifelse(year == min(year), 1.4, -0.4)),
            size = 3.6, show.legend = FALSE) +
  # label in the empty space below the lines, arrow pointing into the shaded gap
  annotate("segment", x = 2021, xend = 2021, y = 26, yend = mean(c(w4[year == 2021, drv], w4[year == 2021, exp])),
           colour = "#006BA2", linewidth = 0.4, arrow = arrow(length = unit(2, "mm"))) +
  annotate("text", x = 2021, y = 21,
           label = "Shaded gap = indirect dependence on foreign demand\n(domestic yarn and fabric used in exported garments)",
           size = 3.3, colour = "#006BA2", lineheight = 1) +
  scale_colour_manual(values = c("Driven by foreign final demand" = "#006BA2",
                                 "Exported directly" = "#7FB7D9")) +
  scale_x_continuous(breaks = years, expand = expansion(mult = c(0.06, 0.06))) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 80), breaks = seq(0, 80, 20)) +
  labs(title = sprintf("Foreign demand drives %.0f%% of textile output, %.0f points more than direct exports show",
                       w4[year == max(year), drv], gap),
       subtitle = "Textiles (c4): share of output exported directly vs share pulled by foreign final buyers (Leontief decomposition)",
       x = NULL, y = NULL,
       caption = paste(src, "\nExport levels before 2024 may be overstated in the source data (EPB double counting, corrected in 2024).")) +
  theme_clean() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#E6E6E6", linewidth = 0.4))

print(p2)
save_png(p2, "m1_export_vs_foreign_driven.png")
