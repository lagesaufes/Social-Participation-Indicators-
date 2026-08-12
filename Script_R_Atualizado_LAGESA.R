# ==============================================
# Supplementary File - Graphs and Analysis
# Lagesa/UFES
# Updated version - includes STEP 5 (consolidated heat map),
# added in response to Reviewer 2 (Round 2), Comments #1, #10, #11:
# streamlining the presentation of results by replacing Tables 3-5
# (Spearman association matrices for PD, PPA, PH) with a single
# three-panel heat map figure.
# ==============================================

# Suppress warnings
options(warn = -1)

# Required packages
library(tidyverse)
library(here)
library(officer)
library(readxl)
library(ggplot2)
library(tidyr)
library(kableExtra)
library(RColorBrewer)
library(dplyr)
library(car)
library(broom)
library(patchwork)
library(pheatmap)   # NEW: required for STEP 5
library(gridExtra)  # NEW: required for STEP 5

# ==============================================
# LOAD DATASETS
# ==============================================
# Accept both possible interpretations of the folder path provided
candidate_base_dirs <- c(
  "C:/Users/User/Documents/Lagesa/artigo/_julianacarneiro",
  "C:/Users/User/Documents/Lagesa/artigo_julianacarneiro"
)

valid_base_dirs <- candidate_base_dirs[
  dir.exists(file.path(candidate_base_dirs, "dados"))
]

if (length(valid_base_dirs) == 0) {
  stop(
    "The data folder was not found. The checked locations were:\n",
    paste(file.path(candidate_base_dirs, "dados"), collapse = "\n")
  )
}

base_dir   <- valid_base_dirs[1]
data_dir   <- file.path(base_dir, "dados")
output_dir <- file.path(base_dir, "Imagens2")

# Locate all Excel files, regardless of their exact filenames
excel_files <- list.files(
  data_dir,
  pattern = "\\.xlsx$",
  full.names = TRUE,
  ignore.case = TRUE
)
excel_files <- excel_files[!grepl("^~\\$", basename(excel_files))]

if (length(excel_files) < 2) {
  stop(
    "Fewer than two .xlsx files were found in:\n", data_dir,
    "\nFiles found:\n",
    paste(basename(excel_files), collapse = "\n")
  )
}

# Identify each workbook from its columns rather than from its filename
headers <- lapply(excel_files, function(file) {
  names(read_excel(file, n_max = 0))
})

indicators_required <- c("CEI", "RI1", "RI2", "RI3", "TRI", "MI", "CCI")
field_required <- unlist(lapply(c("_D", "_PPA", "_PH"), function(suffix) {
  paste0(indicators_required, suffix)
}))

is_GF <- vapply(headers, function(cols) {
  all(indicators_required %in% cols)
}, logical(1))

is_field <- vapply(headers, function(cols) {
  all(field_required %in% cols)
}, logical(1))

if (sum(is_GF) != 1 || sum(is_field) != 1) {
  stop(
    "It was not possible to identify the two workbooks from their columns.\n",
    "Excel files found:\n",
    paste(basename(excel_files), collapse = "\n")
  )
}

file_data_GF <- excel_files[is_GF]
file_data    <- excel_files[is_field]

message("Field data file: ", basename(file_data))
message("Focus-group data file: ", basename(file_data_GF))
message("Output folder: ", output_dir)

data_sheet    <- read_excel(file_data)
data_sheet_GF <- read_excel(file_data_GF)

# Create the requested output folder if it does not exist
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ==============================================
# STEP 1: PROJECT VALIDATION (BARPLOT)
# ==============================================
data_sheet_GF[is.na(data_sheet_GF)] <- 0
means     <- apply(data_sheet_GF, 2, mean)
std_devs  <- apply(data_sheet_GF, 2, sd)

graph_data <- data.frame(
  Column = names(data_sheet_GF),
  Mean = means,
  Std_Dev = std_devs
)

pval_bar <- 0.3402

barplot_spi <- ggplot(graph_data, aes(x = Column, y = Mean)) +
  geom_bar(stat = "identity", fill = "gray", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = Mean - Std_Dev, ymax = Mean + Std_Dev),
                width = 0.4, color = "black", size = 0.8) +
  labs(x = "Indicators", y = "Mean Value") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_blank()) +
  annotate("text", x = 1, y = max(graph_data$Mean + graph_data$Std_Dev) * 0.95,
           label = paste("p =", pval_bar), size = 4, color = "black")

print(barplot_spi)
ggsave(file.path(output_dir, "barplot_spi.png"), barplot_spi, width = 8, height = 6, dpi = 300)
ggsave(file.path(output_dir, "barplot_spi.pdf"), barplot_spi, width = 8, height = 6)

# ==============================================
# STEP 2: HEATMAPS (Pearson & Spearman) - focus group / Phase 1
# This is the source of Fig. 2 in the manuscript (expert
# assessment correlations, NOT the field-application data).
# ==============================================
variables <- data_sheet_GF[, c("CEI", "RI1", "RI2", "RI3", "TRI", "MI", "CCI")]

cor_pearson  <- cor(variables, method = "pearson")
cor_spearman <- cor(variables, method = "spearman")

my_palette <- colorRampPalette(rev(brewer.pal(9, "RdBu")))(100)

png(file.path(output_dir, "heatmap_pearson.png"), width = 2000, height = 1600, res = 300)
heatmap(cor_pearson, col = my_palette, symm = TRUE, margins = c(12, 12),
        cexRow = 2, cexCol = 2, main = "")
dev.off()

pdf(file.path(output_dir, "heatmap_pearson.pdf"), width = 8, height = 6)
heatmap(cor_pearson, col = my_palette, symm = TRUE, margins = c(12, 12),
        cexRow = 1.5, cexCol = 1.5, main = "")
dev.off()

png(file.path(output_dir, "heatmap_spearman.png"), width = 2000, height = 1600, res = 300)
heatmap(cor_spearman, col = my_palette, symm = TRUE, margins = c(12, 12),
        cexRow = 2, cexCol = 2, main = "")
dev.off()

pdf(file.path(output_dir, "heatmap_spearman.pdf"), width = 8, height = 6)
heatmap(cor_spearman, col = my_palette, symm = TRUE, margins = c(12, 12),
        cexRow = 1.5, cexCol = 1.5, main = "")
dev.off()

# ==============================================
# STEP 3: SCATTERPLOTS (D, PPA, PH) - field application data
# ==============================================
# --- Phase D ---
scatter_D <- ggplot(data_sheet, aes(x = MI_D, y = CCI_D)) +
  geom_point() + geom_smooth(method = lm, se = FALSE) +
  theme_light() + theme(panel.grid = element_blank()) +
  labs(x = "MI_D", y = "CCI_D")

print(scatter_D)
ggsave(file.path(output_dir, "scatter_MI_CCI_D.png"), scatter_D, width = 8, height = 6, dpi = 300)
ggsave(file.path(output_dir, "scatter_MI_CCI_D.pdf"), scatter_D, width = 8, height = 6)

# --- Phase PPA (3 combined) ---
p_tri_ri3 <- ggplot(data_sheet, aes(x = TRI_PPA, y = RI3_PPA)) +
  geom_point() + geom_smooth(method = lm, se = FALSE) +
  theme_light() + theme(panel.grid = element_blank()) +
  labs(x = "TRI_PPA", y = "RI3_PPA")

p_cci_mi <- ggplot(data_sheet, aes(x = MI_PPA, y = CCI_PPA)) +
  geom_point() + geom_smooth(method = lm, se = FALSE) +
  theme_light() + theme(panel.grid = element_blank()) +
  labs(x = "MI_PPA", y = "CCI_PPA")

p_ri2_ri3 <- ggplot(data_sheet, aes(x = RI2_PPA, y = RI3_PPA)) +
  geom_point() + geom_smooth(method = lm, se = FALSE) +
  theme_light() + theme(panel.grid = element_blank()) +
  labs(x = "RI2_PPA", y = "RI3_PPA")

combined_p <- p_tri_ri3 / p_cci_mi / p_ri2_ri3
print(combined_p)
ggsave(file.path(output_dir, "scatter_PPA_triplo.png"), combined_p, width = 8, height = 12, dpi = 300)
ggsave(file.path(output_dir, "scatter_PPA_triplo.pdf"), combined_p, width = 8, height = 12)

# --- Phase PH (8 combined) ---
p1 <- ggplot(data_sheet, aes(x = RI1_PH, y = CEI_PH)) +
  geom_point() + geom_smooth(method = lm, se = FALSE) +
  theme_light() + theme(panel.grid = element_blank()) +
  labs(x = "RI1_PH", y = "CEI_PH")

p2 <- ggplot(data_sheet, aes(x = TRI_PH, y = CEI_PH)) +
  geom_point() + geom_smooth(method = lm, se = FALSE) +
  theme_light() + theme(panel.grid = element_blank()) +
  labs(x = "TRI_PH", y = "CEI_PH")

p3 <- ggplot(data_sheet, aes(x = RI2_PH, y = RI3_PH)) +
  geom_point() + geom_smooth(method = lm, se = FALSE) +
  theme_light() + theme(panel.grid = element_blank()) +
  labs(x = "RI2_PH", y = "RI3_PH")

p4 <- ggplot(data_sheet, aes(x = CEI_PH, y = RI3_PH)) +
  geom_point() + geom_smooth(method = lm, se = FALSE) +
  theme_light() + theme(panel.grid = element_blank()) +
  labs(x = "CEI_PH", y = "RI3_PH")

p5 <- ggplot(data_sheet, aes(x = RI2_PH, y = CEI_PH)) +
  geom_point() + geom_smooth(method = lm, se = FALSE) +
  theme_light() + theme(panel.grid = element_blank()) +
  labs(x = "RI2_PH", y = "CEI_PH")

p6 <- ggplot(data_sheet, aes(x = MI_PH, y = CCI_PH)) +
  geom_point() + geom_smooth(method = lm, se = FALSE) +
  theme_light() + theme(panel.grid = element_blank()) +
  labs(x = "MI_PH", y = "CCI_PH")

p7 <- ggplot(data_sheet, aes(x = TRI_PH, y = RI3_PH)) +
  geom_point() + geom_smooth(method = lm, se = FALSE) +
  theme_light() + theme(panel.grid = element_blank()) +
  labs(x = "TRI_PH", y = "RI3_PH")

p8 <- ggplot(data_sheet, aes(x = RI2_PH, y = TRI_PH)) +
  geom_point() + geom_smooth(method = lm, se = FALSE) +
  theme_light() + theme(panel.grid = element_blank()) +
  labs(x = "RI2_PH", y = "TRI_PH")

combined_ph <- (p1 | p2) / (p3 | p4) / (p5 | p6) / (p7 | p8)
print(combined_ph)
ggsave(file.path(output_dir, "scatter_PH_8plots.png"), combined_ph, width = 12, height = 16, dpi = 300)
ggsave(file.path(output_dir, "scatter_PH_8plots.pdf"), combined_ph, width = 12, height = 16)

# ==============================================
# STEP 4: BOXPLOTS PANEL (7 indicators)
# ==============================================
make_boxplot <- function(indicator, df) {
  Data1 <- na.omit(df[[paste0(indicator, "_D")]])
  Data2 <- na.omit(df[[paste0(indicator, "_PPA")]])
  Data3 <- na.omit(df[[paste0(indicator, "_PH")]])
  Data <- c(Data1, Data2, Data3)
  class_data <- factor(c(rep("D", length(Data1)),
                          rep("PPA", length(Data2)),
                          rep("PH", length(Data3))),
                        levels = c("D", "PPA", "PH"))
  base <- cbind.data.frame(Data, class_data)

  anova_result <- aov(Data ~ class_data, data = base)
  pval <- signif(summary(anova_result)[[1]][["Pr(>F)"]][1], 3)

  ggplot(base, aes(x = class_data, y = Data)) +
    geom_boxplot() +
    theme_light() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0, size = 16, face = "bold"),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 14)
    ) +
    labs(x = "", y = "Value", title = indicator) +
    annotate("text", x = 2, y = max(Data, na.rm = TRUE) * 0.9,
             label = paste0("p = ", pval), size = 5, hjust = 0.5, vjust = -0.5)
}

indicators <- c("CEI", "TRI", "MI", "CCI", "RI1", "RI2", "RI3")
plots <- lapply(indicators, make_boxplot, df = data_sheet)

combined_box <- (plots[[1]] | plots[[2]] | plots[[3]] | plots[[4]]) /
  (plots[[5]] | plots[[6]] | plots[[7]] | plot_spacer())
print(combined_box)
ggsave(file.path(output_dir, "boxplots_all.png"), combined_box, width = 16, height = 10, dpi = 300)
ggsave(file.path(output_dir, "boxplots_all.pdf"), combined_box, width = 16, height = 10)

# ==============================================
# STEP 5 (NEW): CONSOLIDATED HEAT MAP - replaces Tables 3, 4, 5
# ==============================================
# Reviewer 2 (Round 2), Comments #1, #10, #11: the manuscript
# previously reported three full Spearman correlation matrices
# (Tables 3-5, one per participatory stage) alongside the
# scatterplots in STEP 3. The reviewer asked for a more narrative
# presentation with fewer tables/figures in the main text and
# less reliance on indicator abbreviations without visual support.
#
# This step reproduces the same three matrices from data_sheet
# (field-application data - NOT the focus-group data used in
# STEP 2 / Fig. 2) and renders them as a single three-panel
# heat map figure, replacing Tables 3-5 in the main text. The
# full numeric matrices move to Supplementary Tables S3-S5.

indicators_order <- c("CEI", "RI1", "RI2", "RI3", "TRI", "MI", "CCI")

get_phase_matrix <- function(df, suffix) {
  cols <- paste0(indicators_order, suffix)
  sub <- df[, cols]
  colnames(sub) <- indicators_order
  cor(sub, method = "spearman", use = "pairwise.complete.obs")
}

cor_D   <- get_phase_matrix(data_sheet, "_D")    # must match former Table 3
cor_PPA <- get_phase_matrix(data_sheet, "_PPA")  # must match former Table 4
cor_PH  <- get_phase_matrix(data_sheet, "_PH")   # must match former Table 5

# Sanity check before plotting: these should reproduce the values
# already reported in the manuscript text, e.g. CCI-MI:
# PD = 0.9484, PPA = 0.6174, PH = 0.9681
round(cor_D, 4)
round(cor_PPA, 4)
round(cor_PH, 4)

# Reuse the same RdBu diverging palette already defined in STEP 2
# for visual consistency with Fig. 2.
heatmap_palette <- colorRampPalette(rev(brewer.pal(9, "RdBu")))(100)
heatmap_breaks  <- seq(-1, 1, length.out = 101)

hm_D <- pheatmap(
  cor_D, color = heatmap_palette, breaks = heatmap_breaks,
  cluster_rows = TRUE, cluster_cols = TRUE,
  display_numbers = FALSE, main = "Participatory Diagnosis (PD)",
  fontsize = 11, silent = TRUE
)

hm_PPA <- pheatmap(
  cor_PPA, color = heatmap_palette, breaks = heatmap_breaks,
  cluster_rows = TRUE, cluster_cols = TRUE,
  display_numbers = FALSE, main = "Planning/Programs/Actions (PPA)",
  fontsize = 11, silent = TRUE
)

hm_PH <- pheatmap(
  cor_PH, color = heatmap_palette, breaks = heatmap_breaks,
  cluster_rows = TRUE, cluster_cols = TRUE,
  display_numbers = FALSE, main = "Public Hearing (PH)",
  fontsize = 11, silent = TRUE
)

png(file.path(output_dir, "heatmap_consolidado_PD_PPA_PH.png"), width = 4800, height = 1800, res = 300)
grid.arrange(hm_D$gtable, hm_PPA$gtable, hm_PH$gtable, ncol = 3)
dev.off()

pdf(file.path(output_dir, "heatmap_consolidado_PD_PPA_PH.pdf"), width = 16, height = 6)
grid.arrange(hm_D$gtable, hm_PPA$gtable, hm_PH$gtable, ncol = 3)
dev.off()

# ==============================================
# STEP 5b (NEW): Export Supplementary Tables S3-S5
# (full numeric matrices, moved out of the main text)
# ==============================================
write.csv(round(cor_D, 4),   file.path(output_dir, "Supplementary_Table_S3_PD.csv"))
write.csv(round(cor_PPA, 4), file.path(output_dir, "Supplementary_Table_S4_PPA.csv"))
write.csv(round(cor_PH, 4),  file.path(output_dir, "Supplementary_Table_S5_PH.csv"))

# ==============================================
# STEP 5c: Export Supplementary Tables S3-S5
# as separate Word files
# ==============================================
matrix_to_word_table <- function(matrix_data) {
  table_data <- data.frame(
    Indicator = rownames(matrix_data),
    round(matrix_data, 4),
    check.names = FALSE
  )
  rownames(table_data) <- NULL
  table_data
}

export_word_table <- function(matrix_data, table_title, file_name) {
  word_document <- officer::read_docx()
  
  word_document <- officer::body_add_par(
    word_document,
    table_title
  )
  
  word_document <- officer::body_add_table(
    word_document,
    value = matrix_to_word_table(matrix_data)
  )
  
  print(
    word_document,
    target = file.path(output_dir, file_name)
  )
}

export_word_table(
  cor_D,
  "Supplementary Table S3 – Participatory Diagnosis (PD)",
  "Supplementary_Table_S3_PD.docx"
)

export_word_table(
  cor_PPA,
  "Supplementary Table S4 – Planning/Programs/Actions (PPA)",
  "Supplementary_Table_S4_PPA.docx"
)

export_word_table(
  cor_PH,
  "Supplementary Table S5 – Public Hearing (PH)",
  "Supplementary_Table_S5_PH.docx"
)

# Reset warnings
options(warn = 0)
