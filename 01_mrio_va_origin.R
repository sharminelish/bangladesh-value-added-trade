# =============================================================================
# 01_mrio_va_origin.R
# Research: Rules of Origin, Value Added and Bangladesh's Apparel Exports
#           after LDC Graduation  –  MODULE 1
#
# Question: Where does the foreign value in Bangladesh's textile output come
#           from, and how much of it is CUMULABLE under EU GSP rules of origin?
#
# Input : E:/RoO_Research/data_raw/ADB_MRIO/*.xlsx   (ADB MRIO 62, one file per year;
#         the year must appear in the file name, e.g. ADB-MRIO62-2018_September 2024.xlsx)
#         E:/RoO_Research/data_raw/cumulation/cumulation_groups.csv  (created on first run
#         with a default classification – CHECK IT against the EU rules)
# Output: E:/RoO_Research/output/tables/  m1_*.csv
#         E:/RoO_Research/output/charts/  m1_*.png
#
# Method: global Leontief inverse L = (I - A)^-1, value-added coefficients v.
#   (a) Value-added origin of US$1 of output of Bangladeshi sector j:
#       VA_r = sum over sectors of country r of  v * L[, j]   (sums to 1 across countries)
#   (b) Where Bangladeshi textile output goes: exports / domestic final demand /
#       domestic intermediate use (direct buyers)
#   (c) Which final buyers DRIVE the output (Leontief decomposition x_j = sum_k,g L[j,k] Y[k,g])
#
# Revised version: adds (b) and (c), checks the 2018-2024 file set, prints progress,
#                  and saves a one-row-per-year summary for the paper.
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(data.table)
  library(ggplot2)
})

# ---- 0. Settings -----------------------------------------------------------------
root      <- "E:/RoO_Research"
mrio_dir  <- file.path(root, "data_raw", "ADB_MRIO")
cum_file  <- file.path(root, "data_raw", "cumulation", "cumulation_groups.csv")
clean_dir <- file.path(root, "data_clean")
tab_dir   <- file.path(root, "output", "tables")
fig_dir   <- file.path(root, "output", "charts")
for (d in c(dirname(cum_file), clean_dir, tab_dir, fig_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

focus   <- "BAN"                 # Bangladesh
sectors <- c("c4", "c5")         # c4 Textiles & textile products; c5 Leather & footwear
years   <- 2018:2024

# ---- 1. Cumulation groups (default; VERIFY against EU GSP origin rules) ----------
# Default classification, written to CSV on first run so you can edit it:
#   SAARC     – EU GSP regional cumulation group for South Asia (verify membership)
#   EU        – bilateral cumulation with the EU (EU-27 member states)
#   Extended  – Norway, Switzerland, Türkiye (verify conditions)
#   Domestic  – Bangladesh
#   None      – everyone else (non-cumulable, e.g. China)
if (!file.exists(cum_file)) {
  eu27  <- c("AUT","BEL","BGR","CYP","CZE","GER","DEN","SPA","EST","FIN","FRA","GRC","HRV",
             "HUN","IRE","ITA","LTU","LUX","LVA","MLT","NET","POL","POR","ROM","SVK","SVN","SWE")
  saarc <- c("IND","PAK","SRI","NEP","BHU","MLD")
  ext   <- c("NOR","SWI","TUR")
  fwrite(rbind(data.table(country = focus, group = "Domestic"),
               data.table(country = saarc, group = "SAARC"),
               data.table(country = eu27,  group = "EU"),
               data.table(country = ext,   group = "Extended")), cum_file)
  message("Default cumulation_groups.csv written – please check it against the EU rules: ", cum_file)
}
cum <- fread(cum_file)

# ---- 2. Helpers --------------------------------------------------------------------
is_sec <- function(z) !is.na(z) & grepl("^c[0-9]+$", trimws(z))
num    <- function(m) {
  out <- suppressWarnings(matrix(as.numeric(m), nrow = NROW(m), ncol = NCOL(m)))
  out[is.na(out)] <- 0
  out
}

read_mrio <- function(file, year) {
  cache <- file.path(clean_dir, sprintf("mrio_%d.rds", year))
  if (file.exists(cache)) return(readRDS(cache))
  sheets <- excel_sheets(file)
  sheet  <- sheets[!grepl("legend|content|note", sheets, ignore.case = TRUE)][1]
  message("  reading ", basename(file), " [", sheet, "] (first time only, a few minutes)")
  M <- as.matrix(read_excel(file, sheet = sheet, col_names = FALSE, col_types = "text", na = c("", "-"), .name_repair = "minimal"))

  sec_col <- which.max(colSums(apply(M[, 1:min(6, ncol(M))], 2, is_sec)))
  cty_col <- sec_col - 1
  sec_row <- which.max(rowSums(t(apply(M[1:min(15, nrow(M)), ], 1, is_sec))))
  cty_row <- sec_row - 1
  r_idx <- which(is_sec(M[, sec_col]))
  c_idx <- which(is_sec(M[sec_row, ]))
  stopifnot(length(r_idx) == length(c_idx))
  lab_cty <- trimws(M[r_idx, cty_col]); lab_sec <- trimws(M[r_idx, sec_col])
  stopifnot(all(paste(lab_cty, lab_sec) == paste(trimws(M[cty_row, c_idx]), trimws(M[sec_row, c_idx]))))
  countries <- unique(lab_cty)

  other  <- setdiff(seq_len(ncol(M)), c(seq_len(sec_col), c_idx))
  hdr    <- trimws(M[cty_row, other])
  fd_idx <- other[!is.na(hdr) & hdr %in% countries]
  fd_cty <- trimws(M[cty_row, fd_idx])

  Z <- num(M[r_idx, c_idx])
  F <- num(M[r_idx, fd_idx])
  Y <- sapply(countries, function(k) rowSums(F[, fd_cty == k, drop = FALSE]))
  out <- list(Z = Z, Y = Y, x = rowSums(Z) + rowSums(F), cty = lab_cty, sec = lab_sec, countries = countries)
  saveRDS(out, cache)
  out
}

# ---- 3. One year ---------------------------------------------------------------------
analyse_year <- function(file, year) {
  d <- read_mrio(file, year)
  if (!focus %in% d$countries) stop(year, ": ", focus, " not found")
  N  <- length(d$x)
  xs <- ifelse(d$x > 0, d$x, 1)
  A  <- sweep(d$Z, 2, xs, "/"); A[, d$x <= 0] <- 0
  v  <- 1 - colSums(A);         v[d$x <= 0] <- 0
  message("  inverting ", N, " x ", N, " Leontief matrix")
  L  <- solve(diag(N) - A)
  s  <- which(d$cty == focus)
  exports <- rowSums(d$Z[s, d$cty != focus]) + rowSums(d$Y[s, d$countries != focus, drop = FALSE])

  va_tab <- rbindlist(lapply(sectors, function(k) {
    j  <- s[d$sec[s] == k]
    va <- data.table(country = d$cty, va = v * L[, j])[, .(va_share = sum(va)), by = country]
    di <- data.table(country = d$cty[-s], direct = d$Z[-s, j])[, .(direct_usdm = sum(direct)), by = country]
    res <- merge(va, di, by = "country", all.x = TRUE)
    res[is.na(direct_usdm), direct_usdm := 0]
    res[, `:=`(year = year, sector = k,
               exports_usdm = exports[d$sec[s] == k],
               output_usdm  = d$x[j])]
    res
  }))

  # (b) Where output goes (direct buyers), and (c) which final buyers drive it
  use_tab <- rbindlist(lapply(sectors, function(k) {
    j       <- s[d$sec[s] == k]
    dom_fd  <- d$Y[j, focus]
    dom_int <- sum(d$Z[j, d$cty == focus])
    exp_j   <- exports[d$sec[s] == k]
    driven  <- colSums(L[j, ] * d$Y)                       # output pulled by each buyer country
    data.table(year = year, sector = k, output_usdm = d$x[j],
               exports_usdm = exp_j, dom_final_usdm = dom_fd, dom_intermediate_usdm = dom_int,
               driven_domestic_usdm = driven[focus], driven_foreign_usdm = sum(driven) - driven[focus],
               check_usdm = sum(driven))
  }))
  drv_tab <- rbindlist(lapply(sectors, function(k) {
    j <- s[d$sec[s] == k]
    data.table(year = year, sector = k, buyer = d$countries, driven_usdm = colSums(L[j, ] * d$Y))
  }))
  list(va = va_tab, use = use_tab, drv = drv_tab)
}

# ---- 4. Run all years ---------------------------------------------------------------
files <- list.files(mrio_dir, pattern = "\\.xlsx$", full.names = TRUE)
yr_of <- function(f) as.integer(regmatches(basename(f), regexpr("20[0-9]{2}", basename(f))))
files <- files[sapply(files, yr_of) %in% years]
files <- files[order(sapply(files, yr_of))]
if (!length(files)) stop("No ADB MRIO files found in ", mrio_dir)
missing_years <- setdiff(years, sapply(files, yr_of))
if (length(missing_years)) message("Note: no file for year(s) ", paste(missing_years, collapse = ", "))
files <- files[!grepl("^~\\$", basename(files))]          # skip Excel temp files

res <- lapply(files, function(f) { y <- yr_of(f); message("Year ", y); analyse_year(f, y) })
va  <- rbindlist(lapply(res, `[[`, "va"))
use <- rbindlist(lapply(res, `[[`, "use"))
drv <- rbindlist(lapply(res, `[[`, "drv"))

# Attach cumulation groups; anything not listed is non-cumulable ("None")
va <- merge(va, cum, by = "country", all.x = TRUE)
va[is.na(group), group := "None"]
va[, va_usdm := va_share * exports_usdm]   # value of each country's content in Bangladesh's exports
setorder(va, sector, year, -va_share)
fwrite(va, file.path(tab_dir, "m1_va_origin_by_country.csv"))

# ---- 5. Summaries ---------------------------------------------------------------------
# 5a. Value-added origin by cumulation group (shares of US$1 of output, and US$ m of exports)
grp <- va[, .(va_share = sum(va_share), va_usdm = sum(va_usdm)), by = .(sector, year, group)]
grp_w <- dcast(grp, sector + year ~ group, value.var = "va_share", fill = 0)
fwrite(grp_w, file.path(tab_dir, "m1_va_origin_by_group.csv"))

# 5b. Foreign content only: how much of it is cumulable?
foreign <- grp[group != "Domestic"]
foreign[, share_of_foreign := va_share / sum(va_share), by = .(sector, year)]
cum_sum <- foreign[, .(foreign_share       = sum(va_share),
                       cumulable_share_fva = sum(share_of_foreign[group != "None"]),
                       noncumulable_share_fva = sum(share_of_foreign[group == "None"])),
                   by = .(sector, year)]
china <- va[country == "PRC", .(sector, year, china_share_fva = va_share)]
cum_sum <- merge(cum_sum, china, by = c("sector", "year"))
cum_sum[, china_share_fva := china_share_fva / foreign_share]
fwrite(cum_sum, file.path(tab_dir, "m1_cumulable_summary.csv"))

# 5c. Direct imported inputs by group (first-round imports only)
dir_grp <- va[group != "Domestic", .(direct_usdm = sum(direct_usdm)), by = .(sector, year, group)]
dir_grp[, share := direct_usdm / sum(direct_usdm), by = .(sector, year)]
fwrite(dcast(dir_grp, sector + year ~ group, value.var = "share", fill = 0),
       file.path(tab_dir, "m1_direct_inputs_by_group.csv"))

cat("\nTextiles (c4): foreign value added and how much of it is cumulable\n")
print(cum_sum[sector == "c4", .(year,
                                foreign_content = sprintf("%.1f%%", 100 * foreign_share),
                                cumulable_of_foreign = sprintf("%.1f%%", 100 * cumulable_share_fva),
                                china_of_foreign = sprintf("%.1f%%", 100 * china_share_fva))])

# 5d. Where output goes, and what drives it
use[, `:=`(export_share = exports_usdm / output_usdm,
           foreign_driven_share = driven_foreign_usdm / check_usdm)]
fwrite(use, file.path(tab_dir, "m1_output_use_and_drivers.csv"))
drv[, share := driven_usdm / sum(driven_usdm), by = .(sector, year)]
setorder(drv, sector, year, -driven_usdm)
fwrite(drv, file.path(tab_dir, "m1_output_driven_by_buyer.csv"))

cat("\nTextiles (c4): direct export share vs share of output driven by foreign final demand\n")
print(use[sector == "c4", .(year,
                            output_bn = round(output_usdm / 1000, 1),
                            export_share = sprintf("%.1f%%", 100 * export_share),
                            foreign_driven = sprintf("%.1f%%", 100 * foreign_driven_share),
                            check_ok = abs(check_usdm - output_usdm) / output_usdm < 0.01)])

# 5e. One row per year for the paper
paper <- merge(cum_sum[sector == "c4"], use[sector == "c4", .(year, export_share, foreign_driven_share)], by = "year")
fwrite(paper, file.path(tab_dir, "m1_summary_textiles.csv"))

# ---- 6. Charts ------------------------------------------------------------------------------
pal <- c(Domestic = "#006BA2", SAARC = "#7FB7D9", EU = "#1B9E77", Extended = "#A6D9C6", None = "#E3120B")
theme_clean <- function() theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = rel(1.2)), plot.title.position = "plot",
        plot.subtitle = element_text(colour = "#5F6B73", margin = margin(b = 10)),
        plot.caption = element_text(colour = "#5F6B73", hjust = 0, size = rel(0.7)), plot.caption.position = "plot",
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = "top", legend.justification = "left", legend.title = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA))
src <- "Source: ADB MRIO (62 economies), 2018-2024; author's calculations. Cumulation groups: see cumulation_groups.csv"

# 6a. Foreign content of textiles by cumulation group, over time
f4 <- grp[sector == "c4" & group != "Domestic"]
f4[, group := factor(group, levels = c("None", "SAARC", "EU", "Extended"))]
last <- cum_sum[sector == "c4" & year == max(year)]
p1 <- ggplot(f4, aes(factor(year), 100 * va_share, fill = group)) +
  geom_col(width = 0.65) +
  scale_fill_manual(values = pal, labels = c(None = "Non-cumulable (e.g. China)", SAARC = "SAARC",
                                             EU = "EU", Extended = "Norway/Switzerland/Turkiye")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.08))) +
  labs(title = sprintf("Only %.0f%% of textiles' foreign content would count under cumulation (%d)",
                       100 * last$cumulable_share_fva, last$year),
       subtitle = "Foreign value added per US$1 of Bangladeshi textile output (c4), by cumulation group",
       x = NULL, y = NULL, caption = src) +
  theme_clean()
ggsave(file.path(fig_dir, "m1_foreign_content_by_group.png"), p1, width = 9, height = 5, dpi = 300, bg = "white")

# 6b. Top foreign sources for textiles, latest year
top <- va[sector == "c4" & year == max(year) & group != "Domestic"][order(-va_share)][seq_len(min(10, .N))]
top[, country := factor(country, levels = rev(country))]
p2 <- ggplot(top, aes(100 * va_share, country, fill = group)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%.1f", 100 * va_share)), hjust = -0.2, size = 3.4) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = sprintf("Top foreign sources of value in Bangladeshi textiles, %d", max(top$year)),
       subtitle = "US cents per US$1 of textile output (c4); colour = cumulation group",
       x = NULL, y = NULL, caption = src) +
  theme_clean() + theme(panel.grid.major.y = element_blank())
ggsave(file.path(fig_dir, "m1_top_foreign_sources.png"), p2, width = 9, height = 5, dpi = 300, bg = "white")

# 6c. Direct export share vs foreign-driven share
u4 <- melt(use[sector == "c4", .(year, `Exported directly` = 100 * export_share,
                                 `Driven by foreign final demand` = 100 * foreign_driven_share)],
           id.vars = "year", variable.name = "m", value.name = "v")
p3 <- ggplot(u4, aes(year, v, colour = m)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.2) +
  geom_text(data = u4[year == max(year)], aes(label = sprintf("%.0f%%", v)), hjust = -0.3, size = 3.6, show.legend = FALSE) +
  scale_colour_manual(values = c(`Exported directly` = "#7FB7D9", `Driven by foreign final demand` = "#006BA2")) +
  scale_x_continuous(breaks = years, expand = expansion(mult = c(0.03, 0.08))) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(title = "More of Bangladesh's textile output depends on foreign demand than direct exports show",
       subtitle = "Textiles (c4): share of output exported directly vs share pulled by foreign final buyers (Leontief decomposition)",
       x = NULL, y = NULL, caption = src) +
  theme_clean() + theme(panel.grid.major.x = element_blank())
ggsave(file.path(fig_dir, "m1_export_vs_foreign_driven.png"), p3, width = 9, height = 5, dpi = 300, bg = "white")

cat("\nDone. Tables in", tab_dir, "\nCharts in", fig_dir, "\n")
