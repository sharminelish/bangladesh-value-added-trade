# Who Adds the Value? Import Content of Bangladesh's Exports and the Brazil Link

An input–output analysis of Bangladesh's exports using the Asian Development Bank (ADB) Multiregional Input–Output (MRIO) tables. All code is written in R.

**Author:** Elish Sharmin ([sharminelish.github.io](https://sharminelish.github.io))
**Status:** 2024 analysis and 2000–2024 trend complete

> The views expressed here are the author's own and do not represent the Government of Bangladesh. All data are publicly available.

---

## Question

How much of the value of Bangladesh's exports is created in Bangladesh, and how much comes from imported inputs? Where do those inputs come from? How does Bangladesh's trade with Brazil look when it is measured in value added instead of gross flows?

## Key results (ADB MRIO 2024)

- **About 80% of Bangladesh's exports is domestic value added.** Gross exports are US$45.6 billion, of which domestic value added is 79.9%.
- **Textiles (sector c4) make up 75% of exports.** Their domestic value added share is 76.2%, so US$25.9 billion of their US$34.0 billion in exports is Bangladeshi value.
- **Export orientation is lower once imported inputs are removed.** Textiles export 34.2% of their output; adjusted for imported content, the figure is 26.1%.
- **China supplies 57% of the textile sector's imported inputs**, India 13% and Brazil 2.2% (US$349 million).
- **The Bangladesh–Brazil link runs both ways.** Brazil sells more inputs to Bangladesh's textile sector (US$349m) than it buys in Bangladeshi textiles (US$239m).
- **Value retention is improving.** In ADB's revised series, textile DVA rose from 71.8% in 2018 to 76.2% in 2024. (The earlier 2000–2017 series is not directly comparable.)

![Domestic vs foreign value by sector](figures/01_sector_dva_stacked.png)

![Who earns each US$1 of textile output](figures/05_textile_dollar_origin.png)

![Bangladesh to Brazil waterfall](figures/07_brazil_waterfall.png)

![Trend 2000-2024](figures/08_trend_two_panel.png)

## Data

| Source | Description |
| --- | --- |
| [ADB MRIO tables](https://kidb.adb.org/globalization/current) | 62 economies + Rest of World, 35 sectors, current prices, US$ million. 2024 table for the main analysis. |
| ADB input–output tables for Bangladesh | National tables for 2000 and 2007–2024 (one sheet per year), used for the trend. Series revised from 2018. |

The ADB Excel files are not included in this repository. Download them from the ADB Key Indicators Database (link above).

## Method

1. **Input coefficients:** A = Z · diag(x)⁻¹, where Z is the inter-industry matrix and x is gross output.
2. **Value-added coefficients:** v = 1 − column sums of A.
3. **Domestic value added and import content** of Bangladesh's output, using the domestic Leontief inverse:
   - DVA = v_D (I − A_DD)⁻¹
   - VS = 1′ A_MD (I − A_DD)⁻¹ (Hummels, Ishii and Yi, 2001), with DVA + VS = 1
4. **Adjusted export orientation** (UNCTAD–WTO *Practical Guide to Trade Policy Analysis*, eq. 1.3): α = exports / output; α̃ = (exports − VS × exports) / output.
5. **Value-added trade with Brazil:** v_BAN · L · y_BRA, where L is the global Leontief inverse (Johnson and Noguera, 2012).

## Repository structure

```
bangladesh-value-added-trade/
├── R/
│   ├── adb_mrio_bangladesh.R          # 2024 analysis (main results)
│   ├── ban_iot_trend_2000_2024.R      # trend from the national tables, 2000–2024
│   ├── report_tables.R                # publication-quality tables ({gt}) from the CSVs
│   └── report_charts_v3.R             # all eight report charts in one consistent style (ggplot2)
├── output/
│   ├── ban_sector_import_content_dva.csv   # DVA, import content, alpha and alpha-tilde by sector
│   ├── ban_direct_input_sources.csv        # imported inputs by supplier country (c4, c5)
│   ├── ban_value_added_origin.csv          # value-added origin of US$1 of output (c4, c5)
│   ├── ban_to_bra_gross_vs_va.csv          # Bangladesh–Brazil gross vs value-added exports
│   └── ban_dva_trend_2000_2024.csv         # DVA, alpha and alpha-tilde by year
├── figures/                          # the eight report charts (01–08)
└── README.md
```

## How to run

1. Install R (4.2 or later) and the packages:
   ```r
   install.packages(c("readxl", "data.table", "ggplot2", "gt"))
   ```
2. Download the ADB MRIO Excel file(s) from the ADB Key Indicators Database.
3. Edit the `root` path at the top of each script to point to your folder.
4. Run:
   ```r
   source("R/adb_mrio_bangladesh.R")        # 2024 results
   source("R/ban_iot_trend_2000_2024.R")    # trend (Bangladesh national IOT workbook)
   source("R/report_tables.R")              # formatted tables (needs the gt package)
   source("R/report_charts_v3.R")           # all eight charts (ggplot2)
   ```
The first run reads the Excel file and caches it as `.rds`. Later runs take seconds, apart from the global Leontief inverse (2,205 × 2,205).

## Limitations

- ADB revised its tables from 2018, so the 2017–2018 drop is a data break, not a real change.
- Sector c4 combines spinning, weaving and garments, so results apply to the textile sector, not ready-made garments alone.
- Bangladesh's data in the MRIO tables are partly estimated by ADB.
- Argentina, Uruguay and Paraguay are part of "Rest of World" in the 63-economy table, so the MERCOSUR analysis covers Brazil only.
- Value-added coefficients include net taxes on products.

## References

- Asian Development Bank. Multiregional Input–Output Tables. Key Indicators Database.
- Hummels, D., Ishii, J. and Yi, K.-M. (2001). The nature and growth of vertical specialization in world trade. *Journal of International Economics*, 54(1), 75–96.
- Johnson, R. C. and Noguera, G. (2012). Accounting for intermediates: Production sharing and trade in value added. *Journal of International Economics*, 86(2), 224–236.
- Koopman, R., Wang, Z. and Wei, S.-J. (2014). Tracing value-added and double counting in gross exports. *American Economic Review*, 104(2), 459–494.
- Miller, R. E. and Blair, P. D. (2009). *Input–Output Analysis: Foundations and Extensions*. Cambridge University Press.
- UNCTAD and WTO (2012). *A Practical Guide to Trade Policy Analysis*. Geneva.
