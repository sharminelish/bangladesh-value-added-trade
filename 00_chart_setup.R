# =============================================================================
# 00_chart_setup.R  -  shared settings for the Module 1 charts
# Sourced by charts 01-03. Reads the CSV results of 01_mrio_va_origin.R,
# so the charts can be redrawn any time without re-running the model.
# =============================================================================
suppressPackageStartupMessages({ library(data.table); library(ggplot2) })

root    <- "E:/RoO_Research"
tab_dir <- file.path(root, "output", "tables")
fig_dir <- file.path(root, "output", "charts")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

src <- "Source: ADB MRIO (62 economies), 2018-2024; author's calculations. Cumulation groups: see cumulation_groups.csv"

pal <- c(Domestic = "#006BA2",
         SAARC    = "#7FB7D9",   # light blue
         EU       = "#1B9E77",   # green
         Extended = "#A6D9C6",   # light green
         None     = "#E3887C")   # mild red: non-cumulable (e.g. China)

theme_clean <- function() theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = rel(1.2)), plot.title.position = "plot",
        plot.subtitle = element_text(colour = "#5F6B73", margin = margin(b = 10)),
        plot.caption = element_text(colour = "#5F6B73", hjust = 0, size = rel(0.7)),
        plot.caption.position = "plot",
        panel.grid.minor = element_blank(),
        legend.position = "top", legend.justification = "left", legend.title = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA))

# Save safely: if the PNG is open elsewhere (Windows lock), save under a new name
save_png <- function(p, name, w = 9, h = 5) {
  f <- file.path(fig_dir, name)
  ok <- tryCatch({ ggsave(f, p, width = w, height = h, dpi = 300, bg = "white"); TRUE },
                 warning = function(e) FALSE, error = function(e) FALSE)
  if (!ok) { f <- sub("\\.png$", "_new.png", f); ggsave(f, p, width = w, height = h, dpi = 300, bg = "white") }
  message("Saved: ", f)
}
