# =============================================================================
# Bangladesh national input–output tables (ADB), 2000 and 2007–2024
# Trend in domestic value added (DVA) and import content of exports
#
# Input : Bangladesh ADB IOT workbook, one sheet per year ("2000", "2007", ... "2024")
#         saved as E:/BACI Project/raw_ data/ADB_MRIO/BAN_ADB_IOT_2000_2024.xlsx
# Output: gravity_work/mrio_out/ban_dva_trend_2000_2024.csv
#         gravity_work/mrio_out/ban_textile_dva_trend.png
#
# Note  : ADB revised its series for 2018 onward. 2000–2017 and 2018–2024
#         may not be fully comparable; the chart marks the break.
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(data.table)
  library(ggplot2)
})

root    <- "E:/BACI Project"
in_file <- file.path(root, "raw_ data", "ADB_MRIO", "BAN_ADB_IOT_2000_2024.xlsx")
out_dir <- file.path(root, "gravity_work", "mrio_out")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

is_sec <- function(z) !is.na(z) & grepl("^c[0-9]+$", trimws(z))
num    <- function(m) {
  out <- suppressWarnings(matrix(as.numeric(m), nrow = NROW(m), ncol = NCOL(m)))
  out[is.na(out)] <- 0
  out
}
has <- function(m, pattern) apply(m, 2, function(h) any(grepl(pattern, h, ignore.case = TRUE)))

# ---- One sheet -----------------------------------------------------------------------
analyse_sheet <- function(sheet) {
  M <- as.matrix(read_excel(in_file, sheet = sheet, col_names = FALSE,
                            col_types = "text", na = c("", "-")))

  # Locate sector codes (c1..c35) in rows and columns
  sec_col <- which.max(colSums(apply(M[, 1:min(8, ncol(M))], 2, is_sec)))
  sec_row <- which.max(rowSums(t(apply(M[1:min(20, nrow(M)), ], 1, is_sec))))
  r_all <- which(is_sec(M[, sec_col]))
  c_idx <- which(is_sec(M[sec_row, ]))
  S     <- length(c_idx)
  message(sprintf("  %d coded rows, %d coded columns", length(r_all), S))
  if (length(r_all) < S) stop("fewer coded rows than columns")
  r_idx   <- r_all[1:S]                                    # domestic intermediate block
  r_impbk <- if (length(r_all) >= 2 * S) r_all[(S + 1):(2 * S)] else integer(0)  # imported block, if any
  sec <- trimws(M[r_idx, sec_col])
  stopifnot(all(sec == trimws(M[sec_row, c_idx])))

  # Row labels = text in the columns left of the code column; column headers = rows above codes
  row_lab <- apply(M[, seq_len(sec_col), drop = FALSE], 1, paste, collapse = " ")
  hdr     <- M[seq_len(sec_row), , drop = FALSE]

  # Columns to the right of the intermediate block
  right   <- setdiff(seq(max(c_idx) + 1, ncol(M)), integer(0))
  right   <- right[colSums(num(M[r_idx, right, drop = FALSE]) != 0) > 0]   # non-empty
  tot_col <- right[has(hdr[, right, drop = FALSE], "total")]
  exp_col <- right[has(hdr[, right, drop = FALSE], "export")]
  imp_col <- right[has(hdr[, right, drop = FALSE], "import")]
  fd_cols <- setdiff(right, c(tot_col, imp_col))
  if (length(exp_col) == 0) stop(sheet, ": no exports column found; check headers")

  Z <- num(M[r_idx, c_idx])
  E <- rowSums(num(M[r_idx, exp_col, drop = FALSE]))
  Mi_col <- if (length(imp_col)) abs(rowSums(num(M[r_idx, imp_col, drop = FALSE]))) else 0
  # Output: the table's total-output column if present, else rebuilt from the rows
  x <- if (length(tot_col)) num(M[r_idx, tail(tot_col, 1), drop = FALSE])[, 1] else
         rowSums(Z) + rowSums(num(M[r_idx, fd_cols, drop = FALSE])) - Mi_col
  xs <- ifelse(x > 0, x, 1)

  # Imports: a row under the block (non-competitive table) or a column (competitive table)
  below   <- seq(max(r_all) + 1, nrow(M))
  imp_row <- below[grepl("import", row_lab[below], ignore.case = TRUE)]

  if (length(r_impbk) > 0) {
    table_type <- "non-competitive (imported-input block by product)"
    A_D <- sweep(Z, 2, xs, "/")
    m   <- colSums(num(M[r_impbk, c_idx, drop = FALSE])) / xs
  } else if (length(imp_row) > 0) {
    table_type <- "non-competitive (imports row)"
    A_D <- sweep(Z, 2, xs, "/")                                   # domestic inputs
    m   <- num(M[imp_row[1], c_idx, drop = FALSE])[1, ] / xs      # imported inputs per $1
  } else if (length(imp_col) > 0) {
    table_type <- "competitive (imports column, proportionality)"
    Mi  <- abs(rowSums(num(M[r_idx, imp_col, drop = FALSE])))
    use <- x + Mi - E                                             # domestic absorption
    sh  <- ifelse(use > 0, pmin(Mi / use, 1), 0)                  # import share by product
    A   <- sweep(Z, 2, xs, "/")
    A_D <- (1 - sh) * A
    m   <- colSums(sh * A)
  } else stop(sheet, ": no imports row or column found")

  A_D[, x <= 0] <- 0; m[x <= 0] <- 0
  L_D <- solve(diag(S) - A_D)
  vs  <- as.vector(m %*% L_D)          # import content per $1 of output
  dva <- 1 - vs                        # domestic value added per $1 of output

  j4 <- which(sec == "c4"); j5 <- which(sec == "c5")
  data.table(
    year                 = as.integer(sheet),
    table_type           = table_type,
    exports_usdm         = sum(E),
    dva_share_all        = sum(dva * E) / sum(E),
    c4_exports_usdm      = E[j4],
    c4_share_of_exports  = E[j4] / sum(E),
    c4_export_share_alpha = E[j4] / xs[j4],
    c4_alpha_tilde       = (E[j4] - vs[j4] * E[j4]) / xs[j4],
    c4_dva_share         = dva[j4],
    c4_direct_import_share = m[j4],
    c5_dva_share         = dva[j5]
  )
}

# ---- All years -----------------------------------------------------------------------
sheets <- excel_sheets(in_file)
sheets <- sheets[grepl("^(19|20)[0-9]{2}$", sheets)]
trend  <- rbindlist(lapply(sheets, function(sh) {
  message("Year ", sh)
  tryCatch(analyse_sheet(sh), error = function(e) { message("  skipped: ", conditionMessage(e)); NULL })
}), fill = TRUE)
if (nrow(trend) == 0) stop("No year could be read. Send a screenshot of the top and bottom of one year's sheet.")
setorder(trend, year)
trend[, series := ifelse(year >= 2018, "Revised (2018–2024)", "Earlier (2000–2017)")]
fwrite(trend, file.path(out_dir, "ban_dva_trend_2000_2024.csv"))

print(trend[, .(year, type = substr(table_type, 1, 15),
                exports_bn = round(exports_usdm / 1000, 1),
                dva_all = round(100 * dva_share_all, 1),
                c4_dva = round(100 * c4_dva_share, 1),
                c4_alpha = round(100 * c4_export_share_alpha, 1),
                c4_alpha_tilde = round(100 * c4_alpha_tilde, 1))])

say <- function(a, b) cat(sprintf("Textile DVA %s from %.1f%% (%d) to %.1f%% (%d).\n",
                                  ifelse(b$c4_dva_share > a$c4_dva_share, "rose", "fell"),
                                  100 * a$c4_dva_share, a$year, 100 * b$c4_dva_share, b$year))
rev <- trend[year >= 2018]
if (nrow(rev) > 1) { cat("\nRevised series: "); say(rev[1], rev[.N]) }
cat("Full period:    "); say(trend[1], trend[.N])

# ---- Chart -----------------------------------------------------------------------------
p <- ggplot(trend, aes(year, 100 * c4_dva_share)) +
  annotate("rect", xmin = 2017.5, xmax = max(trend$year) + 0.5, ymin = -Inf, ymax = Inf,
           alpha = 0.06, fill = "#2B7BD6") +
  annotate("text", x = 2017.7, y = Inf, label = "ADB revised series", hjust = 0, vjust = 1.5,
           size = 3.2, colour = "grey40") +
  geom_line(aes(group = series), colour = "#2B7BD6", linewidth = 1) +
  geom_point(colour = "#2B7BD6", size = 2.2) +
  geom_text(data = trend[year %in% c(min(year), 2018, max(year))],
            aes(label = sprintf("%.1f%%", 100 * c4_dva_share)), vjust = -1.1, size = 3.4) +
  scale_x_continuous(breaks = trend$year) +
  scale_y_continuous(expand = expansion(mult = 0.15)) +
  labs(title = "Domestic value added share of Bangladesh's textile exports",
       subtitle = "Sector c4 (textiles and textile products), US$1 of output",
       x = NULL, y = "DVA share (%)",
       caption = "Source: ADB input–output tables for Bangladesh; author's calculations") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), panel.grid.minor = element_blank())
ggsave(file.path(out_dir, "ban_textile_dva_trend.png"), p, width = 9, height = 4.8, dpi = 150)
cat("\nSaved trend CSV and chart in", out_dir, "\n")
