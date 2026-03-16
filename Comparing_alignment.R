
# Comparing alignment methods

library(Morpho)
library(geomorph)
library(abind)
library(rgl)
library(Rvcg)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(geometry)
library(ggpubr)
library(ggrepel)
library(vegan)
library(tidyverse)


### Postprocessing: PAMS  ####
Dog_humerus_PAMS_kPCA <- read.csv("C:/Users/Goswamilab/OneDrive/Dogs/DAA outputs/2025/PAMSDEC2025/kpca.csv",
                                       header = T, stringsAsFactors = FALSE)

Dog_humerus_PAMS_eig <- read.csv("C:/Users/Goswamilab/OneDrive/Dogs/DAA outputs/2025/PAMSDEC2025/eig.csv")


head(Dog_humerus_PAMS_kPCA)

rownames(Dog_humerus_PAMS_kPCA)
Dog_humerus_PAMS_eig$cum..variability..in...


##
### Allometry 

# Want all PCs
Dog_humerus_PAMS_kPCs

humerus_PAMS_df <- Dog_humerus_PAMS_kPCs

humerus_PAMS_df$centroid <- humerus_df$centroid[humerus_df$Specimen.number %in% humerus_PAMS_df$Specimen.number]


## Multivariate regression with centroid size

cbind(humerus_PAMS_df[,c(1:184)])
colnames(humerus_PAMS_df)[c(1:184)]

#multivariate linear model
lm_humerus_PAMS_centroid <- lm(as.matrix((humerus_PAMS_df)[c(1:184)]) ~ log(centroid), data = humerus_PAMS_df)

summary(lm_humerus_allPC_centroid)[[6]]$coefficients[2,4]
summary(lm_humerus_allPC_centroid)[[1]]$adj.r.squared

humerus_PAMS_allometry <- data.frame(row.names = colnames(humerus_PAMS_df)[c(1:108)])

for (i in c(1:dim(humerus_PAMS_allometry)[1])){
  humerus_PAMS_allometry[i,1] <- summary(lm_humerus_PAMS_centroid)[[i]]$coefficients[2,4]
}

for (i in c(1:dim(humerus_PAMS_allometry)[1])){
  humerus_PAMS_allometry[i,2] <- summary(lm_humerus_PAMS_centroid)[[i]]$adj.r.squared
}

colnames(humerus_PAMS_allometry) <- c("pval","adj-rsq")

which(humerus_PAMS_allometry$pval < 0.05) # 1  2  4  8 37

# kPC 1  2  4   significant

kpc1_logcentroid_regression.humPAMS <- 
  ggplot(data = humerus_PAMS_df, aes(x = log(centroid), y = PC1, color = breed)) +
  geom_point(size = 3) +  # Add points
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +  # Add one regression line for all data
  labs(title = "",
       x = "log Centroid size",
       y = "kPC1") +
  scale_color_manual(values = dogcols2) +
  theme_minimal() +
  theme(legend.position = "none")

kpc2_logcentroid_regression.humPAMS <- 
  ggplot(data = humerus_PAMS_df, aes(x = log(centroid), y = PC2, color = breed)) +
  geom_point(size = 3) +  # Add points
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") + 
  labs(title = "",
       x = "log Centroid size",
       y = "kPC2") +
  scale_color_manual(values = dogcols2) +
  theme_minimal() +
  theme(legend.position = "none")

kpc4_logcentroid_regression.humPAMS <- 
  ggplot(data = humerus_PAMS_df, aes(x = log(centroid), y = PC4, color = breed)) +
  geom_point(size = 3) +  # Add points
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") + 
  labs(title = "",
       x = "log Centroid size",
       y = "kPC4") +
  scale_color_manual(values = dogcols2) +
  theme_minimal() +
  theme(legend.position = "none")



combined_plot_allometry_humerus_PAMS <- kpc1_logcentroid_regression.humPAMS + kpc2_logcentroid_regression.humPAMS + kpc4_logcentroid_regression.humPAMS + plot_layout(ncol = 3)
ggsave("combined_allometryplot_humerusPAMS.png", plot = combined_plot_allometry_humerus_PAMS, width = 24, height = 12, dpi = 300)

lm_humerus_PAMS_PC1_centroid <- lm(humerus_PAMS_df$PC1 ~ log(humerus_PAMS_df$centroid))
lm_humerus_PAMS_PC1_centroid$residuals
length(lm_humerus_PAMS_PC1_centroid$residuals)
humerus_PAMS_df$PC1.resid <- lm_humerus_PAMS_PC1_centroid$residuals

lm_humerus_PAMS_PC2_centroid <- lm(humerus_PAMS_df$PC2 ~ log(humerus_PAMS_df$centroid))
lm_humerus_PAMS_PC2_centroid$residuals
length(lm_humerus_PAMS_PC2_centroid$residuals)
humerus_PAMS_df$PC2.resid <- lm_humerus_PAMS_PC2_centroid$residuals

lm_humerus_PAMS_PC4_centroid <- lm(humerus_PAMS_df$PC4 ~ log(humerus_PAMS_df$centroid))
lm_humerus_PAMS_PC4_centroid$residuals
length(lm_humerus_PAMS_PC4_centroid$residuals)
humerus_PAMS_df$PC4.resid <- lm_humerus_PAMS_PC4_centroid$residuals

##

### PAMS - convex hulls 
diff(Dog_humerus_PAMS_eig$cum..variability..in...[c(1:6)])


humerus_PAMS_kpc1.2 <- 
  ggplot(data = humerus_PAMS_df, aes(x = PC1, y = PC2, color = breed)) +
  geom_point(size = 3) +
  stat_chull(aes(color = breed, fill = breed), alpha = 0.1, geom = "polygon", lwd = 0.1) +
  labs(title = "",
       x = "kPC1 (40.6% var)",
       y = "kPC2 (12.4% var)") +
  scale_color_manual(values = dogcols2) +
  scale_fill_manual(values = c(dogcols2)) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("humerus_PAMS_kpc1.2.png", plot = humerus_PAMS_kpc1.2, width = 6, height = 6, dpi = 300)


humerus_PAMS_kpc3.4 <- 
  ggplot(data = humerus_PAMS_df, aes(x = PC3, y = PC4, color = breed)) +
  geom_point(size = 3) +
  stat_chull(aes(color = breed, fill = breed), alpha = 0.1, geom = "polygon", lwd = 0.1) +
  labs(title = "",
       x = "kPC3 (9.2% var)",
       y = "kPC4 (7.7% var)") +
  scale_color_manual(values = dogcols2) +
  scale_fill_manual(values = c(dogcols2)) +
  theme_minimal() +
  theme(legend.position = "none")

humerus_PAMS_kpc5.6 <- 
  ggplot(data = humerus_PAMS_df, aes(x = PC5, y = PC6, color = breed)) +
  geom_point(size = 3) +
  stat_chull(aes(color = breed, fill = breed), alpha = 0.1, geom = "polygon", lwd = 0.1) +
  labs(title = "",
       x = "kPC5 (4.1% var)",
       y = "kPC6 (3.0% var)") +
  scale_color_manual(values = dogcols2) +
  scale_fill_manual(values = c(dogcols2)) +
  theme_minimal() +
  theme(legend.position = "none")


combined_plot_humerus_PAMS <- humerus_PAMS_kpc1.2 + humerus_PAMS_kpc3.4 + humerus_PAMS_kpc5.6 + plot_layout(ncol = 3)
ggsave("combined_kpcplot_humerus_PAMS.png", plot = combined_plot_humerus_PAMS, width = 18, height = 6, dpi = 300)

ggsave("combined_kpcplot_humerus_PAMS.pdf", plot = combined_plot_humerus_PAMS, width = 5, height = 5, dpi = 320, 
       device = "pdf", bg = "transparent")


humerus_PAMS_kpc1resid.2resid <- 
  ggplot(data = humerus_PAMS_df, aes(x = PC1.resid, y = PC2.resid, color = breed)) +
  geom_point(size = 3) +
  stat_chull(aes(color = breed, fill = breed), alpha = 0.1, geom = "polygon", lwd = 0.1) +
  labs(title = "",
       x = "kPC1 (residual)",
       y = "kPC2 (residual)") +
  scale_color_manual(values = dogcols2) +
  scale_fill_manual(values = c(dogcols2)) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("humerus_PAMS_kpc1resid.2resid.png", plot = humerus_PAMS_kpc1resid.2resid, width = 6, height = 6, dpi = 300)


humerus_PAMS_kpc3.4resid <- 
  ggplot(data = humerus_PAMS_df, aes(x = PC3, y = PC4.resid, color = breed)) +
  geom_point(size = 3) +
  stat_chull(aes(color = breed, fill = breed), alpha = 0.1, geom = "polygon", lwd = 0.1) +
  labs(title = "",
       x = "kPC3 (9.2% var)",
       y = "kPC4 (residual)") +
  scale_color_manual(values = dogcols2) +
  scale_fill_manual(values = c(dogcols2)) +
  theme_minimal() +
  theme(legend.position = "none")



combined_plot_humerus_PAMS_resid <- humerus_PAMS_kpc1resid.2resid + humerus_PAMS_kpc3.4resid + humerus_PAMS_kpc5.6 + plot_layout(ncol = 3)
ggsave("combined_kpcresidplot_humerus_PAMS.pdf", plot = combined_plot_humerus_PAMS_resid, width = 18, height = 6, dpi = 300)

setwd("C:/Users/Goswamilab/OneDrive/Dogs/Dog-bones-R/JANAT")
getwd()

#######################################
### Re-run above for all DAA runs ####
#######################################
### Evaluating Landmarks ####

directory_humerus_pts_JA <- "C:/Users/Goswamilab/OneDrive/Dogs/Meshes_2025/humerus/Landmarks"

pts_humerus_files_JA <- list.files(directory_humerus_pts_JA, pattern = "\\.pts", full.names = TRUE)
pts_humerus_JA <- list()

for (i in seq_along(pts_humerus_files_JA)) {
  pts <- read.pts(pts_humerus_files_JA[i])
  pts_humerus_JA[[i]] <- pts
}

pts_humerus_JA
pts_file_names_JA <- basename(pts_humerus_files_JA)

array_humerus_JA <- array(unlist(pts_humerus_JA), dim = c(dim(pts_humerus_JA[[1]])[1], dim(pts_humerus_JA[[1]])[2], length(pts_humerus_JA)))
concatenated_array_humerus_JA <- abind(array_humerus_JA, along = 1)


humerus_proc_JA <- gpagen(concatenated_array_humerus_JA)

LMmean <- humerus_proc_JA$consensus

humerus_pca_LMs <- gm.prcomp(humerus_proc_JA$coords)

pc_plot_LM <- plot(humerus_pca_LMs)


Dog_humerus_LMs_kPCA <- humerus_pca_LMs$x
Dog_humerus_LMs_kPCA <- as.data.frame(Dog_humerus_LMs_kPCA)
Dog_humerus_LMs_kPCA$id <- pts_file_names_JA


Dog_humerus_domestic_PAMS_eig$cum..variability..in...[c(1:6)]

humerus_LMs_pc1.2 <- 
  ggplot(data = pc_tables_1to10_OG_info$LM, aes(x = PC1, y = PC2, color = breed)) +
  geom_point(size = 3) +
  stat_chull(aes(color = breed, fill = breed), alpha = 0.1, geom = "polygon", lwd = 0.1) +
  labs(title = "",
       x = "PC1 (67.2% var)",
       y = "PC2 (12.0% var)") +
  scale_color_manual(values = dogcols2) +
  scale_fill_manual(values = c(dogcols2)) +
  theme_minimal() +
  theme(legend.position = "none")

humerus_LMs_pc3.4 <- 
  ggplot(data = pc_tables_1to10_OG_info$LM, aes(x = PC3, y = PC4, color = breed)) +
  geom_point(size = 3) +
  stat_chull(aes(color = breed, fill = breed), alpha = 0.1, geom = "polygon", lwd = 0.1) +
  labs(title = "",
       x = "PC3 (4.3% var)",
       y = "PC4 (3.1% var)") +
  scale_color_manual(values = dogcols2) +
  scale_fill_manual(values = c(dogcols2)) +
  theme_minimal() +
  theme(legend.position = "none")

humerus_LMs_pc5.6 <- 
  ggplot(data = pc_tables_1to10_OG_info$LM, aes(x = PC5, y = PC6, color = breed)) +
  geom_point(size = 3) +
  stat_chull(aes(color = breed, fill = breed), alpha = 0.1, geom = "polygon", lwd = 0.1) +
  labs(title = "",
       x = "PC3 (2.7% var)",
       y = "PC4 (2.6% var)") +
  scale_color_manual(values = dogcols2) +
  scale_fill_manual(values = c(dogcols2)) +
  theme_minimal() +
  theme(legend.position = "none")

combined_plot_humerus_LMs <- humerus_LMs_pc1.2 + humerus_LMs_pc3.4 + humerus_LMs_pc5.6 + plot_layout(ncol = 3)
ggsave("combined_kpcplot_humerus_LMs_PC1flipped.png", plot = combined_plot_humerus_LMs, width = 18, height = 6, dpi = 300)

ggsave("pc1.2_humerus_LMs.png", plot = humerus_LMs_pc1.2, width = 6, height = 6, dpi = 300)

##

### Landmarks - Allometry 

Dog_humerus_LMs_kPCA

humerus_LMs_df <- Dog_humerus_LMs_kPCA

humerus_proc_JA$Csize

humerus_LMs_df$centroid <- humerus_proc_JA$Csize

humerus_LMs_df <- humerus_LMs_df[,c(1:18)]

## Multivariate regression with centroid size

cbind(humerus_LMs_df[,c(1:14)])
colnames(humerus_LMs_df)[c(1:14)]

#multivariate linear model
lm_humerus_LMs_centroid <- lm(as.matrix((humerus_LMs_df)[c(1:14)]) ~ log(centroid), data = humerus_LMs_df)

summary(lm_humerus_allPC_centroid)[[6]]$coefficients[2,4]
summary(lm_humerus_allPC_centroid)[[1]]$adj.r.squared

humerus_LMs_allometry <- data.frame(row.names = colnames(humerus_LMs_df)[c(1:14)])

for (i in c(1:dim(humerus_LMs_allometry)[1])){
  humerus_LMs_allometry[i,1] <- summary(lm_humerus_LMs_centroid)[[i]]$coefficients[2,4]
}

for (i in c(1:dim(humerus_LMs_allometry)[1])){
  humerus_LMs_allometry[i,2] <- summary(lm_humerus_LMs_centroid)[[i]]$adj.r.squared
}

colnames(humerus_LMs_allometry) <- c("pval","adj-rsq")

which(humerus_LMs_allometry$pval < 0.05) # 1  7

# kPC 1,7 significant

kpc1_logcentroid_regression.humLMs <- 
  ggplot(data = humerus_LMs_df, aes(x = log(centroid), y = PC1, color = breed)) +
  geom_point(size = 3) +  # Add points
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") + 
  labs(title = "",
       x = "log Centroid size",
       y = "kPC1") +
  scale_color_manual(values = dogcols2) +
  theme_minimal() +
  theme(legend.position = "none")


ggsave("combined_allometryplot_humerusLMs.png", plot = kpc1_logcentroid_regression.humLMs, width = 6, height = 6, dpi = 300)

lm_humerus_LMs_PC1_centroid <- lm(humerus_LMs_df$PC1 ~ log(humerus_LMs_df$centroid))
lm_humerus_LMs_PC1_centroid$residuals
length(lm_humerus_LMs_PC1_centroid$residuals)
humerus_LMs_df$PC1.resid <- lm_humerus_LMs_PC1_centroid$residuals


humerus_LMs_pc1resid.2 <- 
  ggplot(data = humerus_LMs_df, aes(x = -PC1.resid, y = PC2, color = breed)) +
  geom_point(size = 3) +
  stat_chull(aes(color = breed, fill = breed), alpha = 0.1, geom = "polygon", lwd = 0.1) +
  labs(title = "",
       x = "PC1 (residual)",
       y = "PC2 (12.0% var)") +
  scale_color_manual(values = dogcols2) +
  scale_fill_manual(values = c(dogcols2)) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("humerus_LMs_pc1resid2_flip.png", plot = humerus_LMs_pc1resid.2, width = 6, height = 6, dpi = 300)


##

### Comparing methods #####

# PERMANOVA

permanova_model_LM_breed <- adonis2(Dog_humerus_LMs_kPCA[,c(1:10)] ~ breed, 
                                    data = Dog_humerus_LMs_kPCA,
                                    permutations = 999,
                                    by = "margin",
                                    method = "euclidean")
permanova_model_LM_breed


permanova_model_SMA_breed <- adonis2(Dog_humerus_SMA_kPCs[,c(1:10)] ~ breed, 
                                     data = Dog_humerus_SMA_kPCs,
                                     permutations = 999,
                                     by = "margin",
                                     method = "euclidean")
permanova_model_SMA_breed



permanova_model_PAMS_breed <- adonis2(Dog_humerus_PAMS_kPCs[,c(1:10)] ~ breed, 
                                      data = Dog_humerus_PAMS_kPCs,
                                      permutations = 999,
                                      by = "margin",
                                      method = "euclidean")
permanova_model_PAMS_breed


permanova_model_PAIS_breed <- adonis2(Dog_humerus_PAMSTRUE_kPCs[,c(1:10)] ~ breed, 
                                      data = Dog_humerus_PAMSTRUE_kPCs,
                                      permutations = 999,
                                      by = "margin",
                                      method = "euclidean")
permanova_model_PAIS_breed


permanova_model_PAMS_breed <- adonis2(Dog_humerus_PAMS_kPCs[,c(1:10)] ~ breed, 
                                      data = Dog_humerus_PAMS_kPCs,
                                      permutations = 999,
                                      by = "margin",
                                      method = "euclidean")
permanova_model_PAMS_breed

permanova_model_VTKA_breed <- adonis2(Dog_humerus_VTK_kPCs[,c(1:10)] ~ breed, 
                                      data = Dog_humerus_VTK_kPCs,
                                      permutations = 999,
                                      by = "margin",
                                      method = "euclidean")
permanova_model_VTKA_breed


permanova_model_GPSA_breed <- adonis2(Dog_humerus_GPSA_kPCs[,c(1:10)] ~ breed, 
                                      data = Dog_humerus_GPSA_kPCs,
                                      permutations = 999,
                                      by = "margin",
                                      method = "euclidean")
permanova_model_GPSA_breed



# Function to extract R2 and P value for the 'breed' term
extract_adonis_stats <- function(adonis_object, term = "breed") {
  tab <- as.data.frame(adonis_object)
  
  data.frame(
    term = term,
    R2 = tab[term, "R2"],
    P_value = tab[term, "Pr(>F)"]
  )
}

# Apply to all PERMANOVA models
results <- rbind(
  extract_adonis_stats(permanova_model_LM_breed),
  extract_adonis_stats(permanova_model_SMA_breed),
  extract_adonis_stats(permanova_model_PAMS_breed),
  extract_adonis_stats(permanova_model_PAIS_breed),
  extract_adonis_stats(permanova_model_VTKA_breed),
  extract_adonis_stats(permanova_model_GPSA_breed)
)

# View results
results

results[,1] <- methods

colnames(results)[1] <- "method"


getwd()

write.csv(results, file = "PERMANOVA_breed_results_alignment.csv")
### PLS ####

methods <- c("LM", "SMA", "PAIS", "PAMS", "VTKA", "GPSA")

colnames(Dog_humerus_LMs_kPCA)[c(1:14)] <- colnames(Dog_humerus_SMA_kPCs)[c(1:14)]
rownames(Dog_humerus_LMs_kPCA) <- rownames(Dog_humerus_SMA_kPCs)
rownames(Dog_humerus_PAMSTRUE_kPCs) <- rownames(Dog_humerus_SMA_kPCs)
rownames(Dog_humerus_PAMS_kPCs) <- rownames(Dog_humerus_SMA_kPCs)
rownames(Dog_humerus_VTK_kPCs) <- rownames(Dog_humerus_SMA_kPCs)
rownames(Dog_humerus_GPSA_kPCs) <- rownames(Dog_humerus_SMA_kPCs)


pc_tables <- list(Dog_humerus_LMs_kPCA[,c(1:10)], # LM
                  Dog_humerus_SMA_kPCs[,c(1:10)], # SMA
                  Dog_humerus_PAMSTRUE_kPCs[,c(1:10)], # PAIS
                  Dog_humerus_PAMS_kPCs[,c(1:10)], # PAMS
                  Dog_humerus_VTK_kPCs[,c(1:10)], #VTKA
                  Dog_humerus_GPSA_kPCs[,c(1:10)]) # GPSA

names(pc_tables) <- methods

pc_tables_1to10

pairs <- combn(names(pc_tables_1to10), 2)

pls_results <- list()

for (i in seq_len(ncol(pairs))) {
  m1 <- pairs[1, i]
  m2 <- pairs[2, i]
  
  pls_results[[paste0(m1, "_vs_", m2)]] <-
    two.b.pls(pc_tables_1to10[[m1]],
              pc_tables_1to10[[m2]],
              iter = 999)
}

pls_results$LM_vs_SMA
pls_results$LM_vs_PAIS
pls_results$LM_vs_PAMS
pls_results$LM_vs_VTKA
pls_results$LM_vs_GPSA

pls_results$SMA_vs_PAIS


rows <- list()
k <- 1

for (nm in names(pls_results)) {
  
  x <- pls_results[[nm]]
  
  if (!all(c("r.pls", "Z", "P.value") %in% names(x))) {
    message("Skipping invalid entry: ", nm)
    next
  }
  
  rows[[k]] <- data.frame(
    comparison  = nm,
    method1     = sub("_vs_.*", "", nm),
    method2     = sub(".*_vs_", "", nm),
    rPLS        = x$r.pls,
    p_value     = x$P.value,
    effect_size = x$Z,
    stringsAsFactors = FALSE
  )
  
  k <- k + 1
}

pls_summary_table <- do.call(rbind, rows)

pls_summary_table

write.csv(pls_summary_table, file = "JANAT/PLS_table.csv")
##
### visualisation
methods <- unique(c(pls_summary_table$method1, pls_summary_table$method2))
rmat <- matrix(NA, nrow = length(methods), ncol = length(methods))
rownames(rmat) <- methods
colnames(rmat) <- methods

for (i in 1:nrow(pls_summary_table)) {
  m1 <- pls_summary_table$method1[i]
  m2 <- pls_summary_table$method2[i]
  r  <- pls_summary_table$rPLS[i]
  
  rmat[m1, m2] <- r
  rmat[m2, m1] <- r
}

diag(rmat) <- 1

rmat

if (!requireNamespace("pheatmap", quietly = TRUE)) {
  install.packages("pheatmap")
}

pheatmap::pheatmap(
  rmat,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  display_numbers = TRUE,
  number_format = "%.2f",
  fontsize_number = 10,
  main = "PLS Correlation Among Alignment Methods"
)


###
### Reordering pcs ####

library(clue) 

pc_tables_1to10_noLM <- pc_tables_1to10_OG[c(2:6)]

methods

ref <- pc_tables_1to10_noLM[[3]]

pc_tables_1to10_noLM_reorder <- pc_tables_1to10_noLM


for (i in seq_along(pc_tables_1to10_noLM)) {
  
  M <- abs(cor(ref, pc_tables_1to10_noLM[[i]]))
  
  perm <- solve_LSAP(M, maximum = TRUE)
  
  pc_tables_1to10_noLM_reorder[[i]] <- pc_tables_1to10_noLM[[i]][ , perm]
}

perms <- lapply(pc_tables_1to10_noLM, function(X) {
  M <- abs(cor(ref, X))
  as.integer(solve_LSAP(M, maximum = TRUE))
})

cor(1:10, perms[[1]], method = "kendall")
cor(1:10, perms[[2]], method = "kendall") #SMA
cor(1:10, perms[[3]], method = "kendall") #PAIS
cor(1:10, perms[[4]], method = "kendall") #PAMS
cor(1:10, perms[[5]], method = "kendall") # VTKA
cor(1:10, perms[[6]], method = "kendall") # GPSA

cor(1:10, perms[[1]], method = "pearson")
cor(1:10, perms[[2]], method = "pearson") #SMA
cor(1:10, perms[[3]], method = "pearson") #PAIS
cor(1:10, perms[[4]], method = "pearson") #PAMS
cor(1:10, perms[[5]], method = "pearson") # VTKA
cor(1:10, perms[[6]], method = "pearson") # GPSA


cor(1:10, perms[[1]], method = "spearman")
cor(1:10, perms[[2]], method = "spearman")
cor(1:10, perms[[3]], method = "spearman")
cor(1:10, perms[[4]], method = "spearman")
cor(1:10, perms[[5]], method = "spearman")

cor.test(1:10, perms[[1]], method = "spearman")
cor.test(1:10, perms[[2]], method = "spearman")
cor.test(1:10, perms[[3]], method = "spearman")
cor.test(1:10, perms[[4]], method = "spearman")
cor.test(1:10, perms[[5]], method = "spearman")


### Keep LMs in ###


library(clue) 

ref <- pc_tables_1to10_OG[[1]]

pc_tables_1to10_reorder <- pc_tables_1to10_OG


for (i in seq_along(pc_tables_1to10_OG)) {
  
  M <- abs(cor(ref, pc_tables_1to10_OG[[i]]))
  
  perm <- solve_LSAP(M, maximum = TRUE)
  
  pc_tables_1to10_reorder[[i]] <- pc_tables_1to10_OG[[i]][ , perm]
}

perms <- lapply(pc_tables_1to10_OG, function(X) {
  M <- abs(cor(ref, X))
  as.integer(solve_LSAP(M, maximum = TRUE))
})

methods
cor(1:10, perms[[2]], method = "kendall")
cor(1:10, perms[[3]], method = "kendall")
cor(1:10, perms[[4]], method = "kendall")
cor(1:10, perms[[5]], method = "kendall")
cor(1:10, perms[[6]], method = "kendall")
###


diag(abs(cor(ref, pc_tables_1to10_OG[[2]])))
diag(abs(cor(ref, pc_tables_1to10_reorder[[2]])))


sum(abs(perms[[6]] - 1:10))


unlist(variance[1,c(1:10)])

var_exp <- list(unlist(variance[1,c(1:10)]), unlist(variance[2,c(1:10)]), unlist(variance[3,c(1:10)]),
                unlist(variance[4,c(1:10)]), unlist(variance[5,c(1:10)]), unlist(variance[6,c(1:10)]))

lambda_ref <- var_exp[[1]] # reference index


lambda_matched <- lapply(seq_along(var_exp), function(i) {
  perm <- perms[[i]]  
  var_exp[[i]][perm]
})

write.csv(perms, file = "perms.csv")
write.csv(lambda_matched, file = "var_perms.csv")

var_cor <- sapply(lambda_matched, function(lam) {
  cor(lambda_ref, lam, method = "spearman")
})

apply(do.call(cbind, lambda_matched), 2, sd)
sd(lambda_ref)



var_dist_euc <- sapply(lambda_matched, function(lam) {
  sqrt(sum((lambda_ref - lam)^2))
})

methods


matplot(
  do.call(cbind, lapply(lambda_matched, unlist)),
  type = "l",
  lty = 1,
  col = methodcols,
  lwd = 1.5,
  ylim = c(0,80),
  xlab = "LSAP reference index",
  ylab = "% variance explained",
  xaxt = "n",
  yaxt = "n" 
)

axis(
  side = 1,
  at = 1:10,
  labels = 1:10
)

axis(
  side = 2,
  at = c(0,10,20,30,40,50,60,70,80),
  labels = c(0,10,20,30,40,50,60,70,80)
)

lines(
  unlist(lambda_ref),
  lwd = 3,
  col = methodcols["LM"]
)

### Centroid sizes ####

### Centroid: VTKA
centroid_humerus_VTK_list <- list()

for (i in c(seq_along(names(meshes_humerus_VTK)))){
  
  csize <- cSize(meshes_humerus_VTK[[i]])
  centroid_humerus_VTK_list[[i]] <- csize
  
}

centroid_humerus_VTK_list

###
### Centroid: GPSA
centroid_humerus_GPSA_list <- list()

for (i in c(seq_along(names(meshes_humerus_GPSA)))){
  
  csize <- cSize(meshes_humerus_GPSA[[i]])
  centroid_humerus_GPSA_list[[i]] <- csize
  
}

centroid_humerus_GPSA_list

###
#### Centroid: PAMS 
names(aligned_humerus_PAMS_scaled) <- vtk_file_names

centroid_humerus_PAMS_list <- list()

for (i in c(seq_along(names(aligned_humerus_PAMS_scaled)))){
  
  csize <- cSize(aligned_humerus_PAMS_scaled[[i]])
  centroid_humerus_PAMS_list[[i]] <- csize
  
}

centroid_humerus_PAMS_list

##
###
#### Centroid: PAIS 

aligned_humerus_proc_notscale

names(aligned_humerus_proc_notscale) <- vtk_file_names

centroid_humerus_PAIS_list <- list()

for (i in c(seq_along(names(aligned_humerus_proc_notscale)))){
  
  csize <- cSize(aligned_humerus_proc_notscale[[i]])
  centroid_humerus_PAIS_list[[i]] <- csize
  
}

centroid_humerus_PAIS_list

##



#### Centroid: SMA

aligned_humerus

names(aligned_humerus) <- vtk_file_names

centroid_humerus_SMA_list <- list()

for (i in c(seq_along(names(aligned_humerus)))){
  
  csize <- cSize(aligned_humerus[[i]])
  centroid_humerus_SMA_list[[i]] <- csize
  
}

centroid_humerus_SMA_list

##



#### Centroid: OG, unaligned meshes 

meshes_humerus_JANAT

centroid_humerus_OG_list <- list()

for (i in c(seq_along(names(meshes_humerus_JANAT)))){
  
  csize <- cSize(meshes_humerus_JANAT[[i]])
  centroid_humerus_OG_list[[i]] <- csize
  
}

centroid_humerus_OG_list

##




####
#### Centroid: LMs 

JA_lm_proc <- humerus_proc_JA$coords
dim(JA_lm_proc)
dimnames(JA_lm_proc)[[3]] <- vtk_file_names


centroid_humerus_LM_list <- list()

for (i in c(seq_along(dimnames(JA_lm_proc)[[3]]))){
  
  csize <- cSize(JA_lm_proc[,,i])
  centroid_humerus_LM_list[[i]] <- csize
  
}

centroid_humerus_LM_list


##
## Summary 

centroid_humerus_VTK_list
centroid_humerus_PAIS_list
centroid_humerus_SMA_list
centroid_humerus_GPSA_list
centroid_humerus_PAMS_list

centroid_humerus_VTK_list_scaled <- log(unlist(centroid_humerus_VTK_list))
centroid_humerus_PAIS_list_scaled <- log(unlist(centroid_humerus_PAIS_list))
centroid_humerus_SMA_list_scaled <- log(unlist(centroid_humerus_SMA_list))
centroid_humerus_GPSA_list_scaled <- log(unlist(centroid_humerus_GPSA_list))
centroid_humerus_PAMS_list_scaled <- log(unlist(centroid_humerus_PAMS_list))

centroid <- as.data.frame(centroid_humerus_VTK_list_scaled)
colnames(centroid) <- "VTKA"
centroid$PAIS <- centroid_humerus_PAIS_list_scaled
centroid$PAMS <- centroid_humerus_PAMS_list_scaled
centroid$SMA <- centroid_humerus_SMA_list_scaled
centroid$GPSA <- centroid_humerus_GPSA_list_scaled
centroid$x <- c(1:185)

library(tidyverse)

centroid_long <- centroid %>%
  pivot_longer(
    cols = c(VTKA, PAIS, PAMS, SMA, GPSA),
    names_to = "variable",
    values_to = "value"
  )

ggplot(centroid_long, aes(x = x, y = value, colour = variable)) +
  geom_point(alpha = 0.7, size = 2) +
  theme_minimal() +
  labs(
    x = "x",
    y = "Value",
    colour = "Variable"
  )


centroid_size_plot <- 
  ggplot(centroid_long, aes(x = x, y = value, colour = variable)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_colour_manual(values = methodcols) +
  theme_minimal() +
  labs(
    x = "x",
    y = "Value",
    colour = "Method"
  )

ggsave("Centroid_sizes.pdf", plot = centroid_size_plot, dpi = 300, width = 6, height = 5)
ggsave("Centroid_sizes.png", plot = centroid_size_plot, dpi = 300, width = 6, height = 5)

write.csv(centroid, file = "centroid-table.csv")
#
### Summary with unaligned meshes 


centroid_humerus_OG_list_log <- log(unlist(centroid_humerus_OG_list))
centroid_humerus_LM_list_log <- log(unlist(centroid_humerus_LM_list))
centroid_humerus_VTK_list_log <- log(unlist(centroid_humerus_VTK_list))
centroid_humerus_PAIS_list_log <- log(unlist(centroid_humerus_PAIS_list))
centroid_humerus_SMA_list_log <- log(unlist(centroid_humerus_SMA_list))
centroid_humerus_GPSA_list_log <- log(unlist(centroid_humerus_GPSA_list))
centroid_humerus_PAMS_list_log <- log(unlist(centroid_humerus_PAMS_list))

centroid_humerus_OG_list_scale <- scale(unlist(centroid_humerus_OG_list))
centroid_humerus_LM_list_scale <- scale(unlist(centroid_humerus_LM_list))
centroid_humerus_VTK_list_scale <- scale(unlist(centroid_humerus_VTK_list))
centroid_humerus_PAIS_list_scale <- scale(unlist(centroid_humerus_PAIS_list))
centroid_humerus_SMA_list_scale <- scale(unlist(centroid_humerus_SMA_list))
centroid_humerus_GPSA_list_scale <- scale(unlist(centroid_humerus_GPSA_list))
centroid_humerus_PAMS_list_scale <- scale(unlist(centroid_humerus_PAMS_list))


centroid_humerus_OG_unlist <- unlist(centroid_humerus_OG_list)
centroid_humerus_LM_unlist <- unlist(centroid_humerus_LM_list)
centroid_humerus_VTK_unlist <- unlist(centroid_humerus_VTK_list)
centroid_humerus_PAIS_unlist <- unlist(centroid_humerus_PAIS_list)
centroid_humerus_SMA_unlist <- unlist(centroid_humerus_SMA_list)
centroid_humerus_GPSA_unlist <- unlist(centroid_humerus_GPSA_list)
centroid_humerus_PAMS_unlist <- unlist(centroid_humerus_PAMS_list)

centroid_all <- as.data.frame(centroid_humerus_OG_list_log)
colnames(centroid_all) <- "unaligned"
centroid_all$LM <- centroid_humerus_LM_list_log
centroid_all$VTKA <- centroid_humerus_VTK_list_log
centroid_all$PAIS <- centroid_humerus_PAIS_list_log
centroid_all$PAMS <- centroid_humerus_PAMS_list_log
centroid_all$SMA <- centroid_humerus_SMA_list_log
centroid_all$GPSA <- centroid_humerus_GPSA_list_log
centroid_all$x <- c(1:185)


model <- lm(centroid_humerus_VTK_unlist ~ centroid_humerus_OG_unlist)
summary(model)
cor(centroid_humerus_OG_unlist, centroid_humerus_VTK_unlist)

plot(centroid_humerus_OG_unlist, centroid_humerus_VTK_unlist)
plot(centroid_humerus_OG_unlist, centroid_humerus_LM_unlist)
plot(centroid_humerus_LM_unlist, centroid_humerus_VTK_unlist)

plot(centroid_humerus_OG_unlist, centroid_humerus_SMA_unlist)
plot(centroid_humerus_OG_unlist, centroid_humerus_PAIS_unlist)
plot(centroid_humerus_OG_unlist, centroid_humerus_PAMS_unlist)
plot(centroid_humerus_OG_unlist, centroid_humerus_GPSA_unlist)

plot(centroid_all$unaligned, centroid_all$PAMS)





centroid_all_long <- centroid_all %>%
  pivot_longer(
    cols = c(unaligned, VTKA, PAIS, PAMS, SMA, GPSA, LM),
    names_to = "variable",
    values_to = "value"
  )


centroid_all_size_plot <- 
  ggplot(centroid_all_long, aes(x = x, y = value, colour = variable)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_colour_manual(values = methodcols) +
  theme_minimal() +
  labs(
    x = "x",
    y = "Value",
    colour = "Method"
  )

ggsave("centroid_all_sizes.pdf", plot = centroid_all_size_plot, dpi = 300, width = 6, height = 5)
ggsave("centroid_all_sizes.png", plot = centroid_all_size_plot, dpi = 300, width = 6, height = 5)

write.csv(centroid_all, file = "centroid_all-table.csv")


##


centroid_all_long2 <- centroid_all_long2 %>%
  mutate(
    variable = factor(
      variable,
      levels = c("SMA", "PAIS", "PAMS", "VTKA", "GPSA", "LM")
    )
  )


plot_centroid_all_long2 <- ggplot(centroid_all_long2,
                                  aes(x = unaligned, y = value, colour = variable)) +
  geom_point(alpha = 0.7, size = 2) +
  facet_wrap(~ variable) +
  scale_colour_manual(values = methodcols) +
  theme_minimal() +
  labs(
    x = "Original centroid size",
    y = "Centroid size after alignment"
  )

ggsave("centroid_against_original.pdf", plot = plot_centroid_all_long2, dpi = 300, width = 6, height = 3.5)




centroid_all_long2 <- centroid_all_long2 %>%
  group_by(variable) %>%
  mutate(value_centered = value - mean(value)) %>%
  ungroup()


ggplot(
  centroid_all_long2,
  aes(x = unaligned, y = value_z, colour = variable)
) +
  geom_point(alpha = 0.7, size = 2) +
  facet_wrap(~ variable) +
  scale_colour_manual(values = methodcols) +
  theme_minimal() +
  labs(
    x = "Original centroid size",
    y = "Standardised centroid size (z-score)"
  )

####