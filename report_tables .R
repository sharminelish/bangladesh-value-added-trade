# =============================================================================
# Publication-quality tables for the report
# "Who Adds the Value? Import Content of Bangladesh's Exports and the Brazil Link"
#
# Builds the report's five tables with {gt}, straight from the CSV outputs of
#   adb_mrio_bangladesh.R  and  ban_iot_trend_2000_2024.R
# Every number and title figure is computed from the CSVs (nothing typed in).
#
# Output: <in_dir>/tables/table1_exports_dva.html (+ .png if webshot2 is installed)
#         ... one file per table
# Packages: install.packages(c("data.table", "gt"))      # required
#           install.packages("webshot2")                  # optional, for PNG export
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(gt)
})

# ---- Settings ------------------------------------------------------------------
in_dir  <- "E:/BACI Project/gravity_work/mrio_out"   # or "output" inside the GitHub repository
out_dir <- file.path(in_dir, "tables")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

src_mrio  <- "Source: ADB Multiregional Input–Output Tables 2024; author's calculations."
src_trend <- "Source: ADB input–output tables for Bangladesh, 2000 and 2007–2024; author's calculations."
accent_soft <- "#E8F1FB"   # light blue highlight, matches the report charts

# ---- Shared look ---------------------------------------------------------------
style_tbl <- function(g) {
  g |>
    opt_align_table_header(align = "left") |>
    tab_options(
      table.font.size               = px(13),
      heading.title.font.size       = px(17),
      heading.subtitle.font.size    = px(13),
      table.border.top.style        = "hidden",
      heading.border.bottom.style   = "hidden",
      column_labels.font.weight     = "bold",
      column_labels.border.bottom.color = "#333333",
      column_labels.border.bottom.width = px(1.5),
      table_body.hlines.color       = "#EEEEEE",
      table.border.bottom.color     = "#333333",
      data_row.padding              = px(5),
      source_notes.font.size        = px(11),
      footnotes.font.size           = px(11)
    )
}

highlight_row <- function(g, rows_expr) {
  tab_style(g, style = cell_fill(color = accent_soft), locations = cells_body(rows = {{ rows_expr }}))
}

total_row <- function(g, rows_expr) {
  g |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_body(rows = {{ rows_expr }})) |>
    tab_style(style = cell_borders(sides = "top", color = "#333333", weight = px(1.5)),
              locations = cells_body(rows = {{ rows_expr }}))
}

save_tbl <- function(g, name) {
  gtsave(g, file.path(out_dir, paste0(name, ".html")))
  if (requireNamespace("webshot2", quietly = TRUE)) {
    gtsave(g, file.path(out_dir, paste0(name, ".png")), expand = 20)
  }
  invisible(g)
}

# ---- Data ----------------------------------------------------------------------------
sec  <- fread(file.path(in_dir, "ban_sector_import_content_dva.csv"))
srcs <- fread(file.path(in_dir, "ban_direct_input_sources.csv"))
vao  <- fread(file.path(in_dir, "ban_value_added_origin.csv"))
bra  <- fread(file.path(in_dir, "ban_to_bra_gross_vs_va.csv"))
tr   <- fread(file.path(in_dir, "ban_dva_trend_2000_2024.csv"))

sec[, label := sprintf("%s (%s)", name, sector)]
bra[, label := sprintf("%s (%s)", name, sector)]
country_names <- c(PRC = "China", IND = "India", RoW = "Rest of World", PAK = "Pakistan",
                   USA = "United States", TAP = "Taipei, China", BRA = "Brazil", BAN = "Bangladesh")
nice_country <- function(x) ifelse(is.na(country_names[x]), x, unname(country_names[x]))

# =============================================================================
# Table 1 – Exports and domestic value added by sector
# =============================================================================
tot_exp <- sum(sec$exports_usdm)
tot_dva <- sum(sec$dva_in_exports_usdm)
t1 <- sec[order(-exports_usdm)][1:8,
          .(label, exports_usdm, share = exports_usdm / tot_exp, dva_share, dva_in_exports_usdm)]
t1 <- rbind(t1, data.table(label = "All sectors", exports_usdm = tot_exp, share = 1,
                           dva_share = tot_dva / tot_exp, dva_in_exports_usdm = tot_dva))
c4_share <- t1[grepl("(c4)", label, fixed = TRUE), share]

g1 <- gt(t1) |>
  tab_header(
    title    = sprintf("Textiles account for %.0f%% of Bangladesh's exports", 100 * c4_share),
    subtitle = "Exports and domestic value added (DVA) by sector, 2024 (top 8 sectors)"
  ) |>
  cols_label(label = "Sector", exports_usdm = "Exports (US$ m)", share = "Share of exports",
             dva_share = "DVA share", dva_in_exports_usdm = "DVA in exports (US$ m)") |>
  fmt_number(columns = c(exports_usdm, dva_in_exports_usdm), decimals = 0) |>
  fmt_percent(columns = c(share, dva_share), decimals = 1) |>
  highlight_row(grepl("(c4)", label, fixed = TRUE)) |>
  total_row(label == "All sectors") |>
  tab_footnote(
    footnote  = "Telecom exports look high for Bangladesh and are likely an estimation artefact; check against balance-of-payments data before use.",
    locations = cells_body(columns = label, rows = grepl("(c27)", label, fixed = TRUE))
  ) |>
  tab_source_note(src_mrio) |>
  style_tbl()
save_tbl(g1, "table1_exports_dva")

# =============================================================================
# Table 2 – Export orientation before and after imported inputs (alpha vs alpha-tilde)
# =============================================================================
t2 <- sec[sector %in% c("c4", "c14", "c5", "c25", "c9"),
          .(label, alpha_export_share, alpha_tilde_direct, alpha_tilde_full, import_content_VS)
          ][order(-alpha_export_share)]
c4_row <- t2[grepl("(c4)", label, fixed = TRUE)]

g2 <- gt(t2) |>
  tab_header(
    title    = sprintf("Removing imported inputs cuts textiles' export orientation from %.1f%% to %.1f%%",
                       100 * c4_row$alpha_export_share, 100 * c4_row$alpha_tilde_full),
    subtitle = "Exports as a share of output, before and after removing imported content, 2024"
  ) |>
  cols_label(label = "Sector", alpha_export_share = "Export share α",
             alpha_tilde_direct = "Direct imports only", alpha_tilde_full = "Full import content",
             import_content_VS = "Import content (VS)") |>
  tab_spanner(label = "Adjusted export share α̃", columns = c(alpha_tilde_direct, alpha_tilde_full)) |>
  fmt_percent(columns = -label, decimals = 1) |>
  highlight_row(grepl("(c4)", label, fixed = TRUE)) |>
  tab_source_note("α = exports ÷ output; α̃ = (exports − import content × exports) ÷ output (UNCTAD–WTO, A Practical Guide to Trade Policy Analysis, eq. 1.3).") |>
  tab_source_note(src_mrio) |>
  style_tbl()
save_tbl(g2, "table2_alpha_vs_alpha_tilde")

# =============================================================================
# Table 3 – Where the textile sector's foreign inputs come from
# =============================================================================
c4_src  <- srcs[sector == "c4"]
tot_imp <- sum(c4_src$value)
c4_va   <- vao[sector == "c4", .(country, cents = 100 * va_share)]
t3 <- merge(c4_src[order(-value)][1:7, .(country, value, share)], c4_va, by = "country")[order(-value)]
t3[, country := nice_country(country)]
ban_cents <- c4_va[country == "BAN", cents]

g3 <- gt(t3) |>
  tab_header(
    title    = sprintf("China supplies %.0f%% of the textile sector's imported inputs",
                       100 * t3[country == "China", share]),
    subtitle = sprintf("Direct imported inputs to textiles (c4), 2024; total US$%s million",
                       format(round(tot_imp), big.mark = ","))
  ) |>
  cols_label(country = "Supplier", value = "Imported inputs (US$ m)",
             share = "Share of imported inputs", cents = "Value added per US$1 of output (US cents)") |>
  fmt_number(columns = value, decimals = 0) |>
  fmt_percent(columns = share, decimals = 1) |>
  fmt_number(columns = cents, decimals = 1) |>
  highlight_row(country == "China") |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(rows = country == "Brazil")) |>
  tab_source_note(sprintf("Last column traces value added through all production stages (global Leontief inverse). Bangladesh itself contributes %.1f cents per US$1.", ban_cents)) |>
  tab_source_note(src_mrio) |>
  style_tbl()
save_tbl(g3, "table3_input_sources")

# =============================================================================
# Table 4 – Bangladesh to Brazil: gross vs value-added exports
# =============================================================================
keep <- c("c4", "c1", "c27", "c28", "c20", "c30", "c5")
t4 <- bra[sector %in% keep][match(keep, sector), .(label, gross_exports_usdm, va_absorbed_in_partner_usdm)]
t4 <- rbind(t4, data.table(label = "All sectors",
                           gross_exports_usdm = sum(bra$gross_exports_usdm),
                           va_absorbed_in_partner_usdm = sum(bra$va_absorbed_in_partner_usdm)))
tx <- t4[grepl("(c4)", label, fixed = TRUE)]

g4 <- gt(t4) |>
  tab_header(
    title    = sprintf("Textile value reaching Brazil is %.0f%% below gross textile exports",
                       100 * (1 - tx$va_absorbed_in_partner_usdm / tx$gross_exports_usdm)),
    subtitle = "Bangladesh to Brazil, selected sectors, 2024, US$ million"
  ) |>
  cols_label(label = "Bangladeshi sector", gross_exports_usdm = "Gross exports to Brazil",
             va_absorbed_in_partner_usdm = "Bangladeshi value added in Brazil's final demand") |>
  fmt_number(columns = -label, decimals = 1) |>
  highlight_row(grepl("(c4)", label, fixed = TRUE)) |>
  total_row(label == "All sectors") |>
  tab_source_note("Value added in final demand follows Johnson and Noguera (2012). Sectors such as agriculture, finance and trade reach Brazil indirectly, embedded in garments.") |>
  tab_source_note(src_mrio) |>
  style_tbl()
save_tbl(g4, "table4_bangladesh_brazil")

# =============================================================================
# Table 5 – Trend, 2000 and 2007–2024
# =============================================================================
t5 <- tr[order(year), .(series, year, exports_bn = exports_usdm / 1000, dva_share_all,
                        c4_dva_share, c5_dva_share, c4_export_share_alpha, c4_alpha_tilde)]
r18 <- t5[year == 2018]; rl <- t5[year == max(year)]

g5 <- gt(t5, groupname_col = "series") |>
  tab_header(
    title    = sprintf("Textile DVA %s from %.1f%% (2018) to %.1f%% (%d)",
                       ifelse(rl$c4_dva_share > r18$c4_dva_share, "rose", "fell"),
                       100 * r18$c4_dva_share, 100 * rl$c4_dva_share, rl$year),
    subtitle = "Domestic value added and export orientation, Bangladesh"
  ) |>
  cols_label(year = "Year", exports_bn = "Exports (US$ bn)", dva_share_all = "All exports",
             c4_dva_share = "Textiles (c4)", c4_export_share_alpha = "α",
             c4_alpha_tilde = "α̃", c5_dva_share = "Leather & footwear (c5)") |>
  tab_spanner(label = "DVA share", columns = c(dva_share_all, c4_dva_share, c5_dva_share)) |>
  tab_spanner(label = "Textiles export share", columns = c(c4_export_share_alpha, c4_alpha_tilde)) |>
  fmt_number(columns = exports_bn, decimals = 1) |>
  fmt_percent(columns = c(dva_share_all, c4_dva_share, c5_dva_share, c4_export_share_alpha, c4_alpha_tilde),
              decimals = 1) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) |>
  highlight_row(year %in% c(2018, max(year))) |>
  tab_footnote(
    footnote  = "Export totals before 2024 are likely overstated: in 2024 Bangladesh Bank found EPB shipment data had been double-counted for years. DVA shares per dollar of output are much less affected.",
    locations = cells_column_labels(columns = exports_bn)
  ) |>
  tab_source_note("ADB revised the tables from 2018, so the earlier and revised series should not be compared directly.") |>
  tab_source_note(src_trend) |>
  style_tbl()
save_tbl(g5, "table5_trend")

# ---- Show in RStudio Viewer ----------------------------------------------------------
print(g1); print(g2); print(g3); print(g4); print(g5)
cat("Tables saved in:", out_dir, "\n")
