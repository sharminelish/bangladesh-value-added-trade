# =============================================================================
# 01_chart_foreign_content_by_group.R
# Horizontal stacked bars: foreign value added per US$1 of textile output (c4),
# by EU cumulation group, 2018-2024. 2020 flagged as a data artefact.
# =============================================================================
source("E:/RoO_Research/scripts/00_chart_setup.R")

grp <- melt(fread(file.path(tab_dir, "m1_va_origin_by_group.csv")),
            id.vars = c("sector", "year"), variable.name = "group", value.name = "va_share")
cum_sum <- fread(file.path(tab_dir, "m1_cumulable_summary.csv"))

f4  <- grp[sector == "c4" & group != "Domestic"]
f4[, group := factor(group, levels = c("None", "Extended", "EU", "SAARC"))]   # cumulable next to the axis
yrs <- sort(unique(f4$year))
f4[, yr := factor(year, levels = rev(yrs))]                                   # 2018 at the top

lab4 <- cum_sum[sector == "c4"]
lab4[, yr := factor(year, levels = rev(yrs))]
lab4[, txt := sprintf("%.0f%% cumulable%s", 100 * cumulable_share_fva, ifelse(year == 2020, " *", ""))]
last <- lab4[year == max(year)]

p1 <- ggplot(f4, aes(100 * va_share, yr, fill = group)) +
  geom_col(width = 0.62) +
  geom_text(data = lab4, aes(100 * foreign_share + 0.4, yr, label = txt),
            inherit.aes = FALSE, hjust = 0, size = 3.4, colour = "#5F6B73") +
  scale_fill_manual(values = pal, breaks = c("SAARC", "EU", "Extended", "None"),
                    labels = c(SAARC = "SAARC", EU = "EU", Extended = "Norway/Switzerland/Turkiye",
                               None = "Non-cumulable (e.g. China)")) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.22)),
                     position = "top") +
  labs(title = sprintf("Only %.0f%% of textiles' foreign content would count under cumulation (%d)",
                       100 * last$cumulable_share_fva, last$year),
       subtitle = "Foreign value added per US$1 of Bangladeshi textile output (c4), by cumulation group",
       x = NULL, y = NULL,
       caption = paste(src, "\n* 2020: likely data artefact in the ADB table; interpret with caution.")) +
  theme_clean() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#E6E6E6", linewidth = 0.4),
        axis.text.y = element_text(colour = "#121212", size = rel(1.05)))

print(p1)
save_png(p1, "m1_foreign_content_by_group.png", h = 5.2)
