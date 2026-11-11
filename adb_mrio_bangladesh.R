# =============================================================================
# ADB MRIO 2024 – Bangladesh: import content, domestic value added (DVA),
# export orientation (alpha vs alpha-tilde, Practical Guide eq. 1.3),
# sources of foreign inputs, and Bangladesh–Brazil links
#
# Input : E:/BACI Project/raw_ data/ADB_MRIO/ADB_MRIO_2024.xlsx  (sheet "ADB MRIO 2024")
# Output: E:/BACI Project/gravity_work/mrio_out/*.csv
# Units : US$ million, current prices
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(data.table)
})

# ---- 0. Settings -------------------------------------------------------------
root     <- "E:/BACI Project"
in_file  <- file.path(root, "raw_ data", "ADB_MRIO", "ADB_MRIO_2024.xlsx")
sheet    <- "ADB MRIO 2024"
rds_file <- file.path(root, "gravity_work", "adb_mrio_2024_text.rds")
out_dir  <- file.path(root, "gravity_work", "mrio_out")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

focus         <- "BAN"            # Bangladesh (confirm code in the Legend sheet)
partner       <- "BRA"            # Brazil
focus_sectors <- c("c4", "c5")    # c4 Textiles & textile products; c5 Leather & footwear

# ---- 1. Read once, cache as .rds ----------------------------------------------
# Read everything as text so header rows and numbers come in cleanly; "-" -> NA.
if (file.exists(rds_file)) {
  raw <- readRDS(rds_file)
} else {
  message("Reading Excel (first time only, may take 1–3 minutes)...")
  raw <- read_excel(in_file, sheet = sheet, col_names = FALSE,
                    col_types = "text", na = c("", "-"))
  saveRDS(raw, rds_file)
}
M <- as.matrix(raw)
rm(raw); invisible(gc())

is_sec <- function(z) !is.na(z) & grepl("^c[0-9]+$", trimws(z))
num    <- function(m) {
  out <- suppressWarnings(matrix(as.numeric(m), nrow = NROW(m), ncol = NCOL(m)))
  out[is.na(out)] <- 0
  out
}

# ---- 2. Locate the blocks automatically ---------------------------------------
# Column holding row sector codes (c1..c35) = the column with most "cNN" values
sec_col <- which.max(colSums(apply(M[, 1:min(6, ncol(M))], 2, is_sec)))
cty_col <- sec_col - 1                      # country code is just left of it
name_col <- sec_col - 2                     # sector name left of that
# Header row holding column sector codes = the row with most "cNN" values
sec_row <- which.max(rowSums(apply(M[1:min(15, nrow(M)), ], 1, is_sec) |> t()))
cty_row <- sec_row - 1

r_idx <- which(is_sec(M[, sec_col]))                 # intermediate rows
c_idx <- which(is_sec(M[sec_row, ]))                 # intermediate columns
row_lab <- paste(trimws(M[r_idx, cty_col]), trimws(M[r_idx, sec_col]), sep = "_")
col_lab <- paste(trimws(M[cty_row, c_idx]), trimws(M[sec_row, c_idx]), sep = "_")
stopifnot(length(r_idx) == length(c_idx), all(row_lab == col_lab))

lab_cty   <- trimws(M[r_idx, cty_col])
lab_sec   <- trimws(M[r_idx, sec_col])
countries <- unique(lab_cty)
N <- length(r_idx); G <- length(countries); S <- N / G
cat(sprintf("Found %d economies x %d sectors = %d rows\n", G, S, N))
if (!focus %in% countries) {
  print(countries)
  stop("Focus code '", focus, "' not found. Pick the right code from the list above.")
}
sec_names <- setNames(trimws(M[r_idx[1:S], name_col]), lab_sec[1:S])

# Final-demand columns: to the right, headed by a country code
other   <- setdiff(seq_len(ncol(M)), c(seq_len(sec_col), c_idx))
hdr_cty <- trimws(M[cty_row, other])
fd_idx  <- other[!is.na(hdr_cty) & hdr_cty %in% countries]
fd_cty  <- trimws(M[cty_row, fd_idx])
cat(sprintf("Final-demand columns: %d (%.0f per economy)\n",
            length(fd_idx), length(fd_idx) / G))

# ---- 3. Matrices ----------------------------------------------------------------
Z    <- num(M[r_idx, c_idx])                           # N x N intermediates
Fmat <- num(M[r_idx, fd_idx])                          # N x (G*5) final demand
Y    <- sapply(countries, function(k) rowSums(Fmat[, fd_cty == k, drop = FALSE]))  # N x G
x    <- rowSums(Z) + rowSums(Fmat)                     # gross output

# Check against the file's total-output column if present
tot_idx <- other[apply(M[1:sec_row, other, drop = FALSE], 2,
                       function(h) any(grepl("total", h, ignore.case = TRUE)))]
if (length(tot_idx) > 0) {
  x_file <- num(M[r_idx, tail(tot_idx, 1), drop = FALSE])[, 1]
  cat(sprintf("Max |computed output - file total| = %.3f (US$ m)\n", max(abs(x - x_file))))
}
rm(M, Fmat); invisible(gc())

xs <- ifelse(x > 0, x, 1)
A  <- sweep(Z, 2, xs, "/"); A[, x <= 0] <- 0           # technical coefficients
v  <- 1 - colSums(A); v[x <= 0] <- 0                   # value-added coefficients
# (v includes net taxes on products and margins, i.e. all non-intermediate inputs)

# ---- 4. Bangladesh: domestic vs imported content (local Leontief) --------------
s     <- which(lab_cty == focus)
A_ss  <- A[s, s]                                       # domestic inputs
A_ms  <- A[-s, s]                                      # imported inputs
L_ss  <- solve(diag(length(s)) - A_ss)                 # local Leontief inverse
dva_m <- as.vector(v[s] %*% L_ss)                      # DVA per $1 of output
vs_m  <- as.vector(colSums(A_ms) %*% L_ss)             # import content per $1 (Hummels VS)
cat(sprintf("Check DVA + VS = 1: max deviation %.2e\n", max(abs(dva_m + vs_m - 1)[x[s] > 0])))

foreign_cols <- which(lab_cty != focus)
exports <- rowSums(Z[s, foreign_cols]) + rowSums(Y[s, countries != focus, drop = FALSE])
dir_imp <- colSums(Z[-s, s]) / xs[s]                   # direct imported-input share

ban <- data.table(
  sector            = lab_sec[s],
  name              = sec_names[lab_sec[s]],
  output_usdm       = x[s],
  exports_usdm      = exports,
  alpha_export_share = exports / xs[s],                         # eq. 1.2: x/y
  direct_import_share = dir_imp,                                # imported inputs / output
  alpha_tilde_direct = (exports - dir_imp * exports) / xs[s],   # eq. 1.3 (direct)
  import_content_VS  = vs_m,                                    # direct + indirect
  alpha_tilde_full   = (exports - vs_m * exports) / xs[s],      # eq. 1.3 (full)
  dva_share          = dva_m,
  dva_in_exports_usdm = dva_m * exports,
  fva_in_exports_usdm = vs_m * exports
)
setorder(ban, -exports_usdm)
fwrite(ban, file.path(out_dir, "ban_sector_import_content_dva.csv"))

tot <- ban[, .(exports = sum(exports_usdm), dva = sum(dva_in_exports_usdm),
               fva = sum(fva_in_exports_usdm))]
cat(sprintf("\nBangladesh gross exports: US$ %.0f m | DVA %.1f%% | foreign content %.1f%%\n",
            tot$exports, 100 * tot$dva / tot$exports, 100 * tot$fva / tot$exports))
print(ban[sector %in% focus_sectors,
          .(sector, name, alpha_export_share, alpha_tilde_full, import_content_VS, dva_share)])

# ---- 5. Where do Bangladesh's inputs come from? ---------------------------------
# 5a. Direct imported inputs by supplier country
direct_src <- rbindlist(lapply(focus_sectors, function(k) {
  j <- which(lab_cty == focus & lab_sec == k)
  dt <- data.table(country = lab_cty[-s], value = Z[-s, j])[, .(value = sum(value)), by = country]
  dt[, `:=`(sector = k, share = value / sum(value))][order(-value)]
}))
fwrite(direct_src, file.path(out_dir, "ban_direct_input_sources.csv"))

# 5b. Full value-added origin (global Leontief): who adds value to $1 of BAN output
message("Inverting global Leontief matrix (", N, " x ", N, ")...")
L <- solve(diag(N) - A)
va_origin <- rbindlist(lapply(focus_sectors, function(k) {
  j <- which(lab_cty == focus & lab_sec == k)
  dt <- data.table(country = lab_cty, va = v * L[, j])[, .(va_share = sum(va)), by = country]
  dt[, sector := k][order(-va_share)]
}))
fwrite(va_origin, file.path(out_dir, "ban_value_added_origin.csv"))
cat("\nTop value-added sources per $1 of Bangladesh output:\n")
print(va_origin[, head(.SD, 6), by = sector])

# ---- 6. Bangladesh -> Brazil: gross exports vs value-added exports -----------------
if (partner %in% countries) {
  p_cols <- which(lab_cty == partner)
  gross_to_p <- rowSums(Z[s, p_cols]) + Y[s, partner]
  # BAN value added absorbed in Brazil's final demand, by BAN producing sector
  va_to_p <- v[s] * as.vector(L[s, ] %*% Y[, partner])
  bra <- data.table(sector = lab_sec[s], name = sec_names[lab_sec[s]],
                    gross_exports_usdm = gross_to_p,
                    va_absorbed_in_partner_usdm = va_to_p)
  setorder(bra, -gross_exports_usdm)
  fwrite(bra, file.path(out_dir, "ban_to_bra_gross_vs_va.csv"))
  cat(sprintf("\nBAN -> %s: gross exports US$ %.0f m; BAN value added in %s final demand US$ %.0f m\n",
              partner, sum(gross_to_p), partner, sum(va_to_p)))
  cat("(VA exports can exceed gross: BAN value reaching Brazil via third countries.)\n")
}

cat("\nDone. Results saved in:", out_dir, "\n")
