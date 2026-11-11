# =============================================================================
# Report charts, version 3 – one clean, consistent style (FT / Economist-inspired)
# "Who Adds the Value? Import Content of Bangladesh's Exports and the Brazil Link"
#
# Eight charts, all from the CSV outputs, all in the same look:
#   01 Exports by sector: domestic vs foreign value
#   02 Marimekko: export size x domestic value share
#   03 Export orientation before/after imported inputs (dumbbell)
#   04 Who supplies the textile sector's imported inputs
#   05 Who earns each US$1 of textile output
#   06 Bangladesh -> Brazil: gross vs value-added exports
#   07 Waterfall: gross textile exports to Brazil -> Bangladeshi value in Brazil
#   08 Trend 2000–2024: value retention vs export orientation
#
# Restyle everything by editing `pal` and `theme_clean()`.
# Packages: install.packages(c("data.table", "ggplot2"))
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

in_dir  <- "E:/BACI Project/gravity_work/mrio_out"   # or "output" inside the GitHub repository
out_dir <- file.path(in_dir, "charts_v3")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Palette -------------------------------------------------------------------
pal <- list(
  blue   = "#006BA2",   # Bangladesh / domestic value / focus
  blue2  = "#7FB7D9",   # light blue
  red    = "#E3120B",   # accent: foreign content, China, textiles in trends
  red2   = "#F4A39E",   # light red
  green  = "#1B9E77",   # increases
  grey   = "#B7C6CF",   # context
  grey2  = "#DCE3E8",   # light context
  ink    = "#121212",
  sub    = "#5F6B73",
  grid   = "#E6E6E6"
)
src_mrio  <- "Source: ADB Multiregional Input-Output Tables 2024; author's calculations"
src_trend <- "Source: ADB input-output tables for Bangladesh; author's calculations"

comma <- function(x) formatC(round(x), format = "d", big.mark = ",")

theme_clean <- function(base = 12, grid = "x") {
  t <- theme_minimal(base_size = base) +
    theme(
      plot.background     = element_rect(fill = "white", colour = NA),
      panel.background    = element_rect(fill = "white", colour = NA),
      text                = element_text(colour = pal$ink),
      plot.title          = element_text(face = "bold", size = rel(1.25), margin = margin(b = 3)),
      plot.subtitle       = element_text(colour = pal$sub, size = rel(0.92), margin = margin(b = 12)),
      plot.caption        = element_text(colour = pal$sub, size = rel(0.68), hjust = 0, margin = margin(t = 10)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      panel.grid          = element_blank(),
      axis.text           = element_text(colour = pal$sub, size = rel(0.85)),
      axis.title          = element_blank(),
      axis.ticks          = element_blank(),
      legend.position     = "top",
      legend.justification = "left",
      legend.title        = element_blank(),
      legend.text         = element_text(size = rel(0.85), colour = pal$ink),
      legend.key.size     = unit(0.8, "lines"),
      legend.margin       = margin(0, 0, 2, -4),
      strip.text          = element_text(face = "bold", hjust = 0, size = rel(0.95), margin = margin(b = 6)),
      plot.margin         = margin(16, 20, 12, 16)
    )
  if (grid %in% c("x", "xy")) t <- t + theme(panel.grid.major.x = element_line(colour = pal$grid, linewidth = 0.4))
  if (grid %in% c("y", "xy")) t <- t + theme(panel.grid.major.y = element_line(colour = pal$grid, linewidth = 0.4))
  t
}

save_chart <- function(p, name, w = 9, h = 5) {
  ggsave(file.path(out_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 300, bg = "white")
  invisible(p)
}

short_names <- c(c4 = "Textiles", c27 = "Post & telecom", c30 = "Business services",
                 c5 = "Leather & footwear", c3 = "Food & beverages", c9 = "Chemicals",
                 c14 = "Electrical & optical", c10 = "Rubber & plastics", c25 = "Air transport",
                 c1 = "Agriculture", c28 = "Finance", c20 = "Wholesale trade")
short <- function(code, name = code) ifelse(is.na(short_names[code]), name, unname(short_names[code]))
country_names <- c(PRC = "China", IND = "India", RoW = "Rest of World", PAK = "Pakistan",
                   USA = "United States", TAP = "Taipei, China", BRA = "Brazil", BAN = "Bangladesh")
nice <- function(x) ifelse(is.na(country_names[x]), x, unname(country_names[x]))

# ---- Data ------------------------------------------------------------------------
sec  <- fread(file.path(in_dir, "ban_sector_import_content_dva.csv"))
srcs <- fread(file.path(in_dir, "ban_direct_input_sources.csv"))
vao  <- fread(file.path(in_dir, "ban_value_added_origin.csv"))
bra  <- fread(file.path(in_dir, "ban_to_bra_gross_vs_va.csv"))
tr   <- fread(file.path(in_dir, "ban_dva_trend_2000_2024.csv"))

# =============================================================================
# 01 Exports by sector: domestic vs foreign
# =============================================================================
top <- sec[order(-exports_usdm)][1:8]
top[, lab := factor(short(sector, name), levels = rev(short(sector, name)))]
long <- melt(top[, .(lab, Domestic = dva_in_exports_usdm, Foreign = fva_in_exports_usdm)],
             id.vars = "lab", variable.name = "part", value.name = "v")
long[, part := factor(part, levels = c("Foreign", "Domestic"))]
c4 <- top[sector == "c4"]

p01 <- ggplot(long, aes(v, lab, fill = part)) +
  geom_col(width = 0.68) +
  geom_text(data = top, inherit.aes = FALSE, aes(exports_usdm, lab, label = sprintf("%.0f%%", 100 * dva_share)),
            hjust = -0.25, size = 3.4, colour = pal$sub) +
  scale_fill_manual(values = c(Domestic = pal$blue, Foreign = pal$red),
                    breaks = c("Domestic", "Foreign"),
                    labels = c("Domestic value added", "Foreign content")) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.08)), position = "top") +
  labs(title = sprintf("Textiles: US$%.1fbn of Bangladeshi value in US$%.1fbn of exports",
                       c4$dva_in_exports_usdm / 1000, c4$exports_usdm / 1000),
       subtitle = "Exports by sector, 2024, US$ million. Labels: domestic share",
       caption = src_mrio) +
  theme_clean(grid = "x") +
  theme(axis.text.y = element_text(colour = pal$ink, size = rel(0.92)))
save_chart(p01, "01_sector_dva_stacked", h = 5.2)

# =============================================================================
# 02 Marimekko
# =============================================================================
mk <- sec[order(-exports_usdm)]
mk[, grp := ifelse(seq_len(.N) <= 5, short(sector, name), "Other")]
mk <- mk[, .(exports = sum(exports_usdm), dva = sum(dva_in_exports_usdm)), by = grp]
mk[, `:=`(share = dva / exports, other = grp == "Other")]
setorder(mk, other, -exports)
mk[, xmax := 100 * cumsum(exports) / sum(exports)][, xmin := shift(xmax, fill = 0)]
mk[, `:=`(xmid = (xmin + xmax) / 2, w = xmax - xmin, tex = grp == "Textiles")]
tx <- mk[tex == TRUE]

p02 <- ggplot(mk) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 100 * share, ymax = 100), fill = pal$red, colour = "white", linewidth = 1) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 100 * share, fill = tex), colour = "white", linewidth = 1) +
  geom_text(data = mk[tex == FALSE], aes(xmid, 100 * share / 2, label = sprintf("%s  %.0f%%", grp, 100 * share)),
            angle = 90, colour = "white", size = 3.3, fontface = "bold") +
  annotate("text", x = tx$xmid, y = 100 * tx$share / 2, label = sprintf("Textiles\n%.0f%% of exports, %.0f%% domestic", tx$w, 100 * tx$share),
           colour = "white", fontface = "bold", size = 4.6, lineheight = 1.1) +
  annotate("text", x = tx$xmid, y = (100 * tx$share + 100) / 2, label = sprintf("Foreign content %.0f%%", 100 * (1 - tx$share)),
           colour = "white", fontface = "bold", size = 3.8) +
  scale_fill_manual(values = c(`TRUE` = pal$blue, `FALSE` = pal$blue2), guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0), breaks = seq(0, 100, 25)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0), breaks = seq(0, 100, 25)) +
  labs(title = "Textiles dominate exports but keep less value per dollar",
       subtitle = "Width: share of total exports. Blue: domestic value share. Red: foreign content. 2024",
       caption = src_mrio) +
  theme_clean(grid = "none")
save_chart(p02, "02_sector_marimekko", h = 5.4)

# =============================================================================
# 03 Dumbbell: alpha vs alpha-tilde
# =============================================================================
db <- sec[sector %in% c("c4", "c14", "c5", "c25", "c9"),
          .(lab = short(sector, name), sector, a = 100 * alpha_export_share, t = 100 * alpha_tilde_full)][order(a)]
db[, lab := factor(lab, levels = lab)]
db[, col := ifelse(sector == "c4", pal$red, pal$blue)]
d4 <- db[sector == "c4"]

p03 <- ggplot(db, aes(y = lab)) +
  geom_segment(aes(x = t, xend = a, yend = lab, colour = col), linewidth = 2.2, alpha = 0.35) +
  geom_point(aes(x = a, colour = col), shape = 21, fill = "white", size = 4.2, stroke = 1.6) +
  geom_point(aes(x = t, colour = col), size = 4.2) +
  geom_text(aes(x = a, label = sprintf("%.1f", a), colour = col), hjust = -0.6, size = 3.4) +
  geom_text(aes(x = t, label = sprintf("%.1f", t), colour = col), hjust = 1.6, size = 3.4) +
  scale_colour_identity() +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 40), position = "top") +
  labs(title = sprintf("Removing imported inputs cuts textiles' export share from %.1f%% to %.1f%%", d4$a, d4$t),
       subtitle = "Exports as % of output. Open dot: gross. Solid dot: net of imported content. 2024",
       caption = src_mrio) +
  theme_clean(grid = "x") +
  theme(axis.text.y = element_text(colour = pal$ink, size = rel(0.95)))
save_chart(p03, "03_alpha_dumbbell", h = 4.4)

# =============================================================================
# 04 Input sources for textiles
# =============================================================================
s4 <- srcs[sector == "c4"][order(-value)]
tot_imp <- sum(s4$value)
s4 <- rbind(s4[1:7, .(country = nice(country), value)],
            data.table(country = "All others", value = sum(s4[-(1:7), value])))
s4[, share := value / tot_imp]
s4[, country := factor(country, levels = rev(country))]
s4[, col := fcase(country == "China", pal$red, country == "Brazil", pal$blue, default = pal$grey)]

p04 <- ggplot(s4, aes(value, country, fill = col)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = sprintf("%.1f%%", 100 * share)), hjust = -0.15, size = 3.4, colour = pal$ink) +
  scale_fill_identity() +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.1)), position = "top") +
  labs(title = sprintf("China supplies %.0f%% of the textile sector's imported inputs", 100 * s4[country == "China", share]),
       subtitle = sprintf("Direct imported inputs to textiles, 2024, US$ million (total US$%s m). Brazil in blue",
                          comma(tot_imp)),
       caption = src_mrio) +
  theme_clean(grid = "x") +
  theme(axis.text.y = element_text(colour = pal$ink, size = rel(0.95)))
save_chart(p04, "04_textile_input_sources", h = 4.8)

# =============================================================================
# 05 Who earns each US$1 of textile output
# =============================================================================
v5 <- vao[sector == "c4"][order(-va_share)]
v5 <- rbind(v5[country %in% c("BAN", "PRC", "IND", "USA", "BRA"), .(country = nice(country), cents = 100 * va_share)],
            data.table(country = "Others", cents = 100 * sum(v5[!country %in% c("BAN", "PRC", "IND", "USA", "BRA"), va_share])))
v5[, country := factor(country, levels = c("Bangladesh", "China", "India", "United States", "Brazil", "Others"))]
setorder(v5, country)
v5[, xmax := cumsum(cents)][, xmin := shift(xmax, fill = 0)][, xmid := (xmin + xmax) / 2]
v5[, col := c(pal$blue, pal$red, pal$red2, pal$grey, "#3A3A3A", pal$grey2)]
small <- v5[cents < 5]
small[, ly := -0.62 - 0.32 * (seq_len(.N) - 1)]

p05 <- ggplot(v5) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = -0.3, ymax = 0.3, fill = col), colour = "white", linewidth = 0.8) +
  geom_text(data = v5[cents >= 5], aes(xmid, 0, label = sprintf("%s\n%.1f cents", country, cents),
                                       colour = ifelse(country == "Others", pal$ink, "white")),
            fontface = "bold", size = 3.6, lineheight = 1) +
  scale_colour_identity() +
  geom_segment(data = small, aes(x = xmid, xend = xmid, y = -0.3, yend = ly), colour = pal$sub, linewidth = 0.3) +
  geom_segment(data = small, aes(x = xmid, xend = 78, y = ly, yend = ly), colour = pal$sub, linewidth = 0.3) +
  geom_text(data = small, aes(77.5, ly, label = sprintf("%s %.1f cents", country, cents),
                              fontface = ifelse(country == "Brazil", "bold", "plain")),
            hjust = 1, size = 3.4, colour = pal$ink) +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, 100.3), ylim = c(-0.5 - 0.32 * nrow(small), 0.35)) +
  labs(title = sprintf("Of every US$1 of textile output, %.0f cents stays in Bangladesh", v5[country == "Bangladesh", cents]),
       subtitle = "Value added by country of origin, US cents, 2024",
       caption = src_mrio) +
  theme_clean(grid = "none") +
  theme(axis.text = element_blank())
save_chart(p05, "05_textile_dollar_origin", h = 4)

# =============================================================================
# 06 Bangladesh -> Brazil: gross vs value added
# =============================================================================
keep <- c("c4", "c1", "c27", "c28", "c20", "c30")
b6 <- bra[sector %in% keep][match(keep, sector)]
b6[, lab := factor(short(sector), levels = rev(short(keep)))]
l6 <- melt(b6[, .(lab, `Gross exports` = gross_exports_usdm, `Bangladeshi value added` = va_absorbed_in_partner_usdm)],
           id.vars = "lab", variable.name = "m", value.name = "v")
l6[, m := factor(m, levels = c("Bangladeshi value added", "Gross exports"))]
t6 <- b6[sector == "c4"]

p06 <- ggplot(l6, aes(v, lab, fill = m)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f", v)), position = position_dodge(width = 0.75),
            hjust = -0.2, size = 3.2, colour = pal$ink) +
  scale_fill_manual(values = c(`Gross exports` = pal$blue2, `Bangladeshi value added` = pal$blue),
                    breaks = c("Gross exports", "Bangladeshi value added"),
                    labels = c("Gross exports to Brazil", "Bangladeshi value in Brazil's final demand")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)), position = "top") +
  labs(title = sprintf("Textile value reaching Brazil is %.0f%% below gross exports",
                       100 * (1 - t6$va_absorbed_in_partner_usdm / t6$gross_exports_usdm)),
       subtitle = "Bangladesh to Brazil, 2024, US$ million. Other sectors reach Brazil indirectly, inside garments",
       caption = src_mrio) +
  theme_clean(grid = "x") +
  theme(axis.text.y = element_text(colour = pal$ink, size = rel(0.95)))
save_chart(p06, "06_brazil_gross_vs_va", h = 5)

# =============================================================================
# 07 Waterfall
# =============================================================================
vs4 <- sec[sector == "c4", import_content_VS]
g4  <- bra[sector == "c4", gross_exports_usdm]; va4 <- bra[sector == "c4", va_absorbed_in_partner_usdm]
va_all <- sum(bra$va_absorbed_in_partner_usdm)
foreign <- vs4 * g4; upstream <- g4 - foreign - va4; others <- va_all - va4
wf <- data.table(step = c("Gross textile\nexports", "Foreign\ncontent", "Upstream\nBangladeshi value",
                          "Textile sector's\nown value", "Other Bangladeshi\nsectors", "Total Bangladeshi\nvalue in Brazil"),
                 type = c("Total", "Decrease", "Decrease", "Total", "Increase", "Total"),
                 delta = c(g4, -foreign, -upstream, va4, others, va_all))
wf[, id := .I]
run <- 0; wf[, `:=`(y0 = 0, y1 = 0)]
for (i in seq_len(nrow(wf))) {
  if (wf$type[i] == "Total") { set(wf, i, c("y0", "y1"), list(0, wf$delta[i])); run <- wf$delta[i] }
  else { set(wf, i, c("y0", "y1"), list(run, run + wf$delta[i])); run <- run + wf$delta[i] }
}
wf[, `:=`(lo = pmin(y0, y1), hi = pmax(y0, y1),
          lab = ifelse(type == "Total", sprintf("%.0f", delta), sprintf("%+.0f", delta)),
          col = fcase(type == "Total", pal$blue, type == "Decrease", pal$red, default = pal$green))]

p07 <- ggplot(wf) +
  geom_segment(data = wf[id < max(id)], aes(x = id + 0.35, xend = id + 0.65, y = y1, yend = y1),
               colour = pal$sub, linewidth = 0.35, linetype = "22") +
  geom_rect(aes(xmin = id - 0.35, xmax = id + 0.35, ymin = lo, ymax = hi, fill = col)) +
  geom_text(aes(id, hi + 8, label = lab, colour = col), fontface = "bold", size = 4) +
  scale_fill_identity() + scale_colour_identity() +
  scale_x_continuous(breaks = wf$id, labels = wf$step) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = sprintf("From US$%.0fm of textile exports to US$%.0fm of Bangladeshi value in Brazil", g4, va_all),
       subtitle = "Bangladesh to Brazil, 2024, US$ million. Red: deductions. Green: value from other sectors",
       caption = src_mrio) +
  theme_clean(grid = "y") +
  theme(axis.text.x = element_text(colour = pal$ink, size = rel(0.82), lineheight = 1))
save_chart(p07, "07_brazil_waterfall", w = 9.5, h = 5.2)

# =============================================================================
# 08 Trend: two panels with direct labels
# =============================================================================
pa <- "Domestic value added share, %"
pb <- "Textiles: exports as % of output"
dd <- rbind(
  tr[, .(year, series, v = 100 * c4_dva_share,          m = "Textiles",           panel = pa)],
  tr[, .(year, series, v = 100 * dva_share_all,         m = "All exports",        panel = pa)],
  tr[, .(year, series, v = 100 * c4_export_share_alpha, m = "Gross",              panel = pb)],
  tr[, .(year, series, v = 100 * c4_alpha_tilde,        m = "Net of imports",     panel = pb)]
)
dd[, g := paste(m, series)]
dd[, panel := factor(panel, levels = c(pa, pb))]
cols <- c(Textiles = pal$red, `All exports` = pal$blue, Gross = pal$red2, `Net of imports` = pal$red)
endp <- dd[year == max(year)]
r18 <- tr[year == 2018]; rl <- tr[year == max(year)]

p08 <- ggplot(dd, aes(year, v, colour = m, group = g)) +
  annotate("rect", xmin = 2017.5, xmax = max(dd$year) + 0.4, ymin = -Inf, ymax = Inf, fill = "#F2F6F9") +
  geom_line(linewidth = 1.1) +
  geom_point(data = endp, size = 2.6) +
  geom_text(data = endp, aes(label = sprintf("%s %.1f", m, v)), hjust = 0, nudge_x = 0.5,
            size = 3.4, fontface = "bold", show.legend = FALSE) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = cols, guide = "none") +
  scale_x_continuous(breaks = c(2000, 2007, 2010, 2013, 2016, 2018, 2021, 2024), limits = c(2000, 2029.5),
                     expand = c(0.01, 0)) +
  scale_y_continuous(expand = expansion(mult = 0.15)) +
  labs(title = sprintf("Textiles keep more value since 2018 (%.1f%% to %.1f%%) but export less of their output",
                       100 * r18$c4_dva_share, 100 * rl$c4_dva_share),
       subtitle = "Shaded: ADB revised series (2018 onward), not directly comparable with earlier years",
       caption = src_trend) +
  theme_clean(grid = "y") +
  theme(panel.spacing = unit(1.4, "lines"))
save_chart(p08, "08_trend_two_panel", w = 9.5, h = 7)

cat("Eight charts saved in:", out_dir, "\n")
