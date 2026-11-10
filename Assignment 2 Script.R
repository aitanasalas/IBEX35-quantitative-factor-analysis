################################################################################
#IBEX35 Factor Analysis
################################################################################

# Define the list of required packages
required_packages <- c(
  "psych", "GPArotation", "ggplot2", "ggpubr", "qgraph", "dplyr",
  "tidyr", "stringr", "FactoMineR", "factoextra", "tidyquant",
  "Rcpp", "readxl", "caret", "lubridate", "ggrepel", "pROC",
  "corrplot"
)

# Install missing packages
installed <- installed.packages()
for (pkg in required_packages) {
  if (!(pkg %in% rownames(installed))) install.packages(pkg)
}

# Load packages and print TRUE/FALSE for each
package_status <- sapply(required_packages, function(pkg) {
  suppressMessages(require(pkg, character.only = TRUE))
})
print(package_status)

# ==============================================================================
# STEP 1: Define Tickers for IBEX35 Firms
# ------------------------------------------------------------------------------

# These are the 13 firms we have selected for analysis out of the 15 assigned
var_names <- c("CABK.MC", "UNI.MC", "BBVA.MC", "MAP.MC", "SAB.MC", "IAG.MC", "ANA.MC", "ELE.MC", "MTS.MC", "ACX.MC", "ENG.MC", "NTGY.MC","AMS.MC")

# ==============================================================================
# STEP 2: Compute Log Returns
# ------------------------------------------------------------------------------


# Load your data
ibex_adj_price <- readRDS("ibex_adj_price.rds")

# Compute log returns (log(P_t) - log(P_{t-1}))
a2_pca <- ibex_adj_price %>%
  mutate(across(all_of(var_names),
                ~ log(.) - log(lag(.)),
                .names = "{.col}.logR"))

# ==============================================================================
# STEP 3: Clean and Prepare Data
# ------------------------------------------------------------------------------

# - Keep only log return columns and date
# - Remove NA from lag operation
# - Rename columns for readability (e.g., CABK.MC.logR → CABK)

a2_pca <- a2_pca %>%
  select(date, ends_with(".logR")) %>%
  filter(!is.na(.[[2]]))  # remove row with NA from lag

# Rename columns for clarity
names(a2_pca)[-1] <- names(a2_pca)[-1] %>%
  str_remove("\\.MC\\.logR")

# Preview your data
head(a2_pca)
str(a2_pca)

# ==============================================================================
# STEP 4: Exploratory Analysis and Suitability for PCA
# ------------------------------------------------------------------------------

# Summary and correlation diagnostics
summary(a2_pca)
df <- a2_pca

# Correlation matrix and plots
corPlot(a2_pca[,2:13],xlas=2)
cor_matrix <- cor(df[,2:13])
corrplot(cor_matrix, method="color", type="lower", tl.col="black", tl.srt=45, 
         col=colorRampPalette(c("blue", "white", "red"))(200))

# Suitability tests for factor analysis
kmo_result <- KMO(df[,2:13])
print(kmo_result)

bartlett_result <- cortest.bartlett(df[,2:13])
print(bartlett_result)

# Scree plot to determine number of factors
psych::VSS.scree(df[,2:13], main = "scree plot") #2 or three factors

# ==============================================================================
# STEP 5: Principal Component Analysis (PCA) and Factor Analysis (EFA)
# ------------------------------------------------------------------------------

# Unrotated factor solution
efa <- psych::principal(df[,c(2:13)], rotate="none", nfactors=3)

# Standard PCA
pca <- stats::prcomp(df[, 2:13], scale = TRUE)
dev.off()
fviz_eig(pca, 
         addlabels = TRUE,   # Adds percentage labels
         barfill = "steelblue", 
         barcolor = "black",
         linecolor = "red", 
         main = "Scree Plot of Principal Components")

# Review results
efa
pca
summary(efa)
efa$values[1:3]
(efa$values[1:3] / sum(efa$values))*100
efa$loadings
unclass(efa$loadings)
efa$communality
efa$residual
efa$scores
scores <- as.data.frame(efa$scores)
scores
summary(scores)
describe(scores)

# Visual inspection of component scores
plot(scores$PC1,scores$PC2)
plot(scores$PC1,scores$PC3)
cor(scores$PC1,scores$PC2)

# Append scores and derive year
efa_scores <-cbind(df,scores)
efa_scores
efa_scores$year <- substr(format(efa_scores$date, "%Y"), 1, 4)
efa_scores$year <- factor(efa_scores$year)
summary(efa_scores)

# Time-series visualization
ggplot(data=efa_scores, aes(x = date, y = PC1)) +
  geom_line() +
  theme_classic() +
  labs(x = 'Date',
       y = "First Principal Component",
       title = "IBEX PCA") +
  scale_y_continuous(breaks = seq(0,300,10))

ggplot(data=efa_scores, aes(x = date, y = PC2)) +
  geom_line() +
  theme_classic() +
  labs(x = 'Date',
       y = "First Principal Component",
       title = "IBEX PCA") +
  scale_y_continuous(breaks = seq(0,300,10))

# Scatter by year
ggplot(efa_scores, aes(x = PC1, y = PC2, color = year)) +
  geom_point()

# Pairwise panel plot
pairs.panels(efa_scores[,15:17])
dev.off()

# Unrotated biplot
biplot(efa,choose=c(1,2),cutl=0.7, smoother=TRUE)


# ==============================================================================
# STEP 6: Rotated Factor Analysis (Varimax)
# ------------------------------------------------------------------------------

efa.rot <- principal(df[,2:13], rotate="varimax", nfactors=3, scores=TRUE)
efa.rot

# Loadings and scores after rotation
loadings_varimax<-as.data.frame(unclass(efa.rot$loadings))
loadings_varimax$firm <- rownames(loadings_varimax)
summary(loadings_varimax)

efa.rot$loadings
efa.rot$communality
scores.rot <- as.data.frame(efa.rot$scores)
summary(scores.rot)
describe(scores.rot)

# Visual diagnostics
pairs.panels(scores.rot)
biplot(efa.rot, choose=c(1,2), cutl=0.9)

# Factor grouping
F1 <- loadings_varimax[abs(loadings_varimax$RC1)>0.6,c("RC1","firm")]
F2 <- loadings_varimax[abs(loadings_varimax$RC2)>0.6,c("RC2","firm")]
F3 <- loadings_varimax[abs(loadings_varimax$RC3)>0.6,c("RC3","firm")]

F1[order(F1$RC1),]
F2[order(F2$RC2),]
F3[order(F3$RC3),]


# Plotting rotated loadings
plot(scores.rot$RC1,scores.rot$RC2)

theta <- seq(0, 2*pi, length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))

loadings_varimax$LoadingStrength <- loadings_varimax$RC1^2 + loadings_varimax$RC2^2

ggplot(loadings_varimax, aes(x = 0, y = 0)) +
  geom_path(data = circle, aes(x = x, y = y), color = "gray", linewidth = 0.8) +  # Correlation circle
  geom_segment(data = loadings_varimax, aes(x = 0, y = 0, xend = RC1, yend = RC2, color = LoadingStrength), 
               arrow = arrow(length = unit(0.2, "cm")), linewidth = 1) +  # Arrows
  geom_text_repel(aes(x = RC1, y = RC2, label = firm), size = 3.5) +  # Labels at arrow tips
  scale_color_gradient2(low = "#00AFBB", mid = "#E7B800", high = "#FC4E07") +  # Color by loading strength
  theme_minimal(base_size = 14) +
  labs(title = "EFA After Rotation", x = "Factor 1", y = "Factor 2", color = "Loading Strength") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Merge scores and explore time evolution
df_scoresrot <- cbind(df,scores.rot)
df_scoresrot$year <- substr(format(df_scoresrot$date, "%Y"), 1, 4)
df_scoresrot$year <- factor(df_scoresrot$year)
summary(df_scoresrot)

ggplot(data=df_scoresrot, aes(x = date, y = RC1)) +
  geom_line() +
  theme_classic() +
  labs(x = 'Date',
       y = "First Principal Component",
       title = "IBEX PCA") +
  scale_y_continuous(breaks = seq(0,300,10))

ggplot(data=df_scoresrot, aes(x = date, y = RC2)) +
  geom_line() +
  theme_classic() +
  labs(x = 'Date',
       y = "First Principal Component",
       title = "IBEX PCA") +
  scale_y_continuous(breaks = seq(0,300,10))

# Scatter and mean trends
mean_values_RC <- df_scoresrot %>%
  group_by(year) %>%
  summarize(mean_RC1 = mean(RC1, na.rm = TRUE),
            mean_RC2 = mean(RC2, na.rm = TRUE))

filtered_RC <- df_scoresrot[df_scoresrot$RC1 >= -3 & df_scoresrot$RC1 <= 3, ]

ggplot(filtered_RC, aes(x = RC1, y = RC2, color = year)) +
  geom_point(alpha = 0.6) +  # Transparency for better visibility
  geom_text(data = mean_values_RC, aes(x = mean_RC1, y = mean_RC2, label = year),
            color = "black", fontface = "bold", size = 5, vjust = -0.5) +  # Year labels
  labs(
    x = "RC1 scores",
    y = "RC2 scores",
    title = "Scatter Plot of First two Rotated Components",
    subtitle = "With yearly mean values labeled",
    color = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    legend.position = "right"
  )

# ==============================================================================
# STEP 7: Link PCA with IBEX Index
# ------------------------------------------------------------------------------

IBEX35 <- tq_get('^IBEX',
                 from = "2017-06-30",
                 to = "2025-03-07",
                 get = "stock.prices")
IBEX35 <- IBEX35[c(2,8)]
IBEX35_adj <- IBEX35 %>%
  mutate(across("adjusted",
                ~ log(.) - log(lag(.))))
IBEX35_adj <- IBEX35_adj %>%
  select(date, adjusted) %>%
  filter(!is.na(.[[2]]))  
complete_IBEX35 <- merge(x=df_scoresrot,y=IBEX35_adj,by="date",all.x=TRUE, all.y=FALSE)
colnames(complete_IBEX35)[19] <- "IBEX.MN"
plot(IBEX.MN~RC1, col=factor(year), pch=19, cex=1,complete_IBEX35)

# Regression model of IBEX on Rotated components
model <- lm(scale(IBEX.MN) ~ RC1+RC2+RC3, data = complete_IBEX35)
summary(model)

# ==============================================================================
# STEP 8: Logistic Regression - Predicting Market Direction
# ------------------------------------------------------------------------------

complete_IBEX35 <- complete_IBEX35 %>%
  arrange(date) %>%  # ensure data is sorted chronologically
  mutate(
    IBEX.MN.next = lead(IBEX.MN),  # tomorrow's index value
    ibex_up = ifelse(IBEX.MN.next > IBEX.MN, 1, 0)  # 1 if it goes up, 0 otherwise
  ) %>%
  filter(!is.na(IBEX.MN.next))  # remove last row (no "next day" to compare)

summary(complete_IBEX35)

# We use the 3 Rotated components as predictors
# The target is 'ibex_up', a binary variable (0 = down or same, 1 = up)

model_logit <- glm(ibex_up ~ RC1 + RC2+RC3,
                   data = complete_IBEX35,
                   family = "binomial")

summary(model_logit)  # check model coefficients and p-values

# ROC and AUC analysis
prob <- predict(model_logit, type = "response")
roc_result <- roc(complete_IBEX35$ibex_up, prob[complete_IBEX35$ibex_up %in% complete_IBEX35$ibex_up])
plot(roc_result)

# Classification performance
complete_IBEX35$prob <- c(NA, prob)
complete_IBEX35$prob <- ifelse(complete_IBEX35$prob >0.5,1,0)
complete_IBEX35$prob[is.na(complete_IBEX35$prob)] <- 0
complete_IBEX35$prob <- as.factor(complete_IBEX35$prob)
complete_IBEX35$ibex_up <- as.factor(complete_IBEX35$ibex_up)
str(complete_IBEX35)
confusionMatrix(complete_IBEX35$prob,complete_IBEX35$ibex_up,positive = "1")

# ==============================================================================
# STEP 9: Correlate PCA with Macroeconomic Indicators
# ------------------------------------------------------------------------------

# Interest Rates
ir <- read_excel("be1901.xls")
ir$date <- as.Date(ir$date)
str(ir)

df_monthly_average <- complete_IBEX35 %>%
  mutate(date = floor_date(date, "month")) %>%  # Adjust date to first day of the month
  group_by(date) %>%
  summarise(mean_value = mean(RC1, na.rm = TRUE))
InterestRate_IBEX <- inner_join(df_monthly_average, ir, by = "date")
model <- lm(mean_value ~ Variation, data = InterestRate_IBEX)
cor.test(InterestRate_IBEX$mean_value,InterestRate_IBEX$Variation)
summary(model)


# GDP
str(complete_IBEX35)
GDP <- read_xlsx("67824.xlsx")
df_quarterly_average <- complete_IBEX35 %>%
  mutate(Trimester = paste0(year(date), "T", quarter(date))) %>%
  group_by(Trimester) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))  #Calculate quarterly average
GDP_IBEX <- inner_join(df_quarterly_average, GDP, by = "Trimester")
model2 <- lm(RC1 ~ Variation, data = GDP_IBEX)
summary(model2)
cor.test(GDP_IBEX$Variation, GDP_IBEX$RC1)

# Natural Gas Prices
gas <- read_xlsx("Dutch TTF Natural Gas Futures Historical Data.xlsx")
str(gas)
gas <- gas %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))
GAS_IBEX <-inner_join(complete_IBEX35,gas, by = "date")
cor.test(GAS_IBEX$RC2,GAS_IBEX$`Change %`)

# Metals
metal <- read_xlsx("metal_prices.xlsx")
metal <- metal %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))
METAL_IBEX <-inner_join(complete_IBEX35,metal, by = "date")

cor.test(METAL_IBEX$RC3,METAL_IBEX$`Metal_Variation`)

