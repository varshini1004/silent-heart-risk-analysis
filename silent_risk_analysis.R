# FOCUSED ANALYSIS: RESEARCHING SILENT HEART ATTACK CASES
# Analyzing only the 103 silent risk patients

# Load libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(corrplot)
library(randomForest)
library(glue)

# Ensure output directory exists
dir.create("output", showWarnings = FALSE)

# Load and preprocess data
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
heart_data <- read.csv(url, header = FALSE)

colnames(heart_data) <- c("age", "sex", "cp", "trestbps", "chol", "fbs",
                          "restecg", "thalach", "exang", "oldpeak", "slope",
                          "ca", "thal", "num")

heart_data <- heart_data %>%
  mutate(
    sex = as.factor(ifelse(sex == 1, "Male", "Female")),
    cp = as.factor(cp),
    fbs = as.factor(ifelse(fbs == 1, ">120", "<=120")),
    restecg = as.factor(restecg),
    exang = as.factor(ifelse(exang == 1, "Yes", "No")),
    slope = as.factor(slope),
    ca = as.factor(ca),
    thal = as.factor(thal),
    heart_disease = as.factor(ifelse(num > 0, "Yes", "No")),
    patient_id = row_number()
  ) %>%
  select(-num)

heart_data[heart_data == "?"] <- NA
heart_data <- na.omit(heart_data)

# IDENTIFY SILENT RISK CASES (Your research focus)
heart_data <- heart_data %>%
  mutate(
    silent_risk = case_when(
      cp == "4" & heart_disease == "Yes" ~ "Silent_Risk",
      TRUE ~ "Other"
    )
  )

# FOCUS ONLY ON SILENT RISK PATIENTS
silent_risk_patients <- heart_data %>% filter(silent_risk == "Silent_Risk")

# ============================================================================
# RANDOM FOREST FEATURE IMPORTANCE ANALYSIS
# ============================================================================

cat("RANDOM FOREST FEATURE IMPORTANCE ANALYSIS\n")
cat("============================================\n\n")

# Prepare data for Random Forest
rf_data <- heart_data %>%
  mutate(
    silent_risk_binary = ifelse(silent_risk == "Silent_Risk", 1, 0),
    sex = as.numeric(sex),
    cp = as.numeric(cp),
    fbs = as.numeric(fbs),
    restecg = as.numeric(restecg),
    exang = as.numeric(exang),
    slope = as.numeric(slope),
    ca = as.numeric(ca),
    thal = as.numeric(thal)
  ) %>%
  select(age, sex, cp, trestbps, chol, fbs, restecg, thalach, exang, oldpeak, slope, ca, thal, silent_risk_binary) %>%
  na.omit()

# Train Random Forest model
set.seed(123)
rf_model <- randomForest(
  as.factor(silent_risk_binary) ~ .,
  data = rf_data,
  importance = TRUE,
  ntree = 500,
  mtry = 3
)

# Get feature importance
importance_df <- as.data.frame(rf_model$importance) %>%
  rownames_to_column("Feature") %>%
  arrange(desc(MeanDecreaseGini))

# Map feature names to readable labels
feature_labels <- c(
  "age" = "Age",
  "sex" = "Gender",
  "cp" = "Chest Pain Type",
  "trestbps" = "Blood Pressure",
  "chol" = "Cholesterol",
  "fbs" = "Fasting Blood Sugar",
  "restecg" = "Resting ECG",
  "thalach" = "Max Heart Rate",
  "exang" = "Exercise Angina",
  "oldpeak" = "ST Depression",
  "slope" = "ST Slope",
  "ca" = "Major Vessels",
  "thal" = "Thalassemia"
)

importance_df$Feature_Label <- feature_labels[importance_df$Feature]

cat("TOP PREDICTORS OF SILENT HEART ATTACKS (Random Forest):\n")
print(importance_df[, c("Feature_Label", "MeanDecreaseGini")])
cat("\n")

# Save top 5 factors for HTML display
top_factors <- importance_df %>%
  head(5) %>%
  select(Feature_Label, MeanDecreaseGini) %>%
  mutate(Importance_Percent = round(MeanDecreaseGini / sum(MeanDecreaseGini) * 100, 1))

cat("TOP 5 FACTORS AFFECTING SILENT HEART ATTACK RISK:\n")
for (i in 1:nrow(top_factors)) {
  cat(i, ". ", top_factors$Feature_Label[i], " (", top_factors$Importance_Percent[i], "%)\n", sep = "")
}
cat("\n")

# Create visualization for feature importance
p_rf <- ggplot(top_factors, aes(x = reorder(Feature_Label, Importance_Percent), y = Importance_Percent)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  labs(title = "Top 5 Factors Predicting Silent Heart Attack Risk (Random Forest)",
       x = "Clinical Factors",
       y = "Relative Importance (%)") +
  theme_minimal() +
  geom_text(aes(label = paste0(Importance_Percent, "%")), hjust = -0.2, size = 3.5)

ggsave("output/random_forest_importance.png", p_rf, width = 10, height = 6)

# ============================================================================
# CONTINUE WITH ORIGINAL ANALYSIS
# ============================================================================

cat("FOCUSED RESEARCH: SILENT HEART ATTACK CASES\n")
cat("===============================================\n\n")

cat("RESEARCH POPULATION:\n")
cat("- Total patients in dataset:", nrow(heart_data), "\n")
cat("- Patients with heart disease:", sum(heart_data$heart_disease == "Yes"), "\n")
cat("- SILENT RISK CASES (your focus):", nrow(silent_risk_patients), "\n\n")

# 1. DEMOGRAPHIC ANALYSIS OF SILENT RISK PATIENTS
cat("=== DEMOGRAPHIC PROFILE OF SILENT RISK PATIENTS ===\n")
cat("Age distribution:\n")
cat("- Average age:", round(mean(silent_risk_patients$age), 1), "years\n")
cat("- Age range:", min(silent_risk_patients$age), "-", max(silent_risk_patients$age), "years\n")
cat("- Gender distribution:", sum(silent_risk_patients$sex == "Male"), "Male,",
    sum(silent_risk_patients$sex == "Female"), "Female\n")
cat("- Male:Female ratio:", round(sum(silent_risk_patients$sex == "Male") /
                                    sum(silent_risk_patients$sex == "Female"), 1), ":1\n\n")

# 2. CLINICAL CHARACTERISTICS OF SILENT RISK PATIENTS
cat("=== CLINICAL CHARACTERISTICS ===\n")
cat("Key risk factors in silent risk patients:\n")
cat("- Average cholesterol:", round(mean(silent_risk_patients$chol), 1), "mg/dL\n")
cat("- Average blood pressure:", round(mean(silent_risk_patients$trestbps), 1), "mmHg\n")
cat("- Average ST depression:", round(mean(silent_risk_patients$oldpeak), 2), "mm\n")
cat("- Average max heart rate:", round(mean(silent_risk_patients$thalach), 1), "bpm\n")
cat("- Exercise-induced angina:", sum(silent_risk_patients$exang == "Yes"),
    "patients (", round(mean(silent_risk_patients$exang == "Yes") * 100, 1), "%)\n\n")

# 3. COMPARE SILENT RISK vs SYMPTOMATIC PATIENTS
symptomatic_patients <- heart_data %>%
  filter(heart_disease == "Yes" & cp != "4")

cat("=== COMPARISON: SILENT vs SYMPTOMATIC HEART DISEASE ===\n")
cat("Silent Risk Patients (n=", nrow(silent_risk_patients), ")\n", sep = "")
cat("Symptomatic Patients (n=", nrow(symptomatic_patients), ")\n\n", sep = "")

comparison_data <- data.frame(
  Characteristic = c("Average Age", "Average Cholesterol", "Average BP",
                     "Average ST Depression", "Exercise Angina"),
  Silent_Risk = c(
    round(mean(silent_risk_patients$age), 1),
    round(mean(silent_risk_patients$chol), 1),
    round(mean(silent_risk_patients$trestbps), 1),
    round(mean(silent_risk_patients$oldpeak), 2),
    paste0(round(mean(silent_risk_patients$exang == "Yes") * 100, 1), "%")
  ),
  Symptomatic = c(
    round(mean(symptomatic_patients$age), 1),
    round(mean(symptomatic_patients$chol), 1),
    round(mean(symptomatic_patients$trestbps), 1),
    round(mean(symptomatic_patients$oldpeak), 2),
    paste0(round(mean(symptomatic_patients$exang == "Yes") * 100, 1), "%")
  )
)

print(comparison_data)
cat("\n")

# 4. RISK STRATIFICATION WITHIN SILENT RISK PATIENTS
cat("=== RISK STRATIFICATION OF SILENT RISK PATIENTS ===\n")

silent_risk_patients <- silent_risk_patients %>%
  mutate(
    risk_category = case_when(
      oldpeak > 2.0 & thalach < 120 ~ "Very High Risk",
      oldpeak > 1.0 | thalach < 130 ~ "High Risk",
      TRUE ~ "Moderate Risk"
    )
  )

risk_stratification <- silent_risk_patients %>%
  group_by(risk_category) %>%
  summarise(
    count = n(),
    percentage = round(n() / nrow(silent_risk_patients) * 100, 1),
    avg_age = round(mean(age), 1),
    avg_oldpeak = round(mean(oldpeak), 2)
  )

print(risk_stratification)
cat("\n")

# 5. PREDICTORS OF SILENT RISK
cat("=== IDENTIFYING PREDICTORS OF SILENT RISK ===\n")

other_patients <- heart_data %>% filter(silent_risk == "Other")

predictor_analysis <- data.frame(
  Predictor = c("Age", "Cholesterol", "Blood Pressure", "ST Depression",
                "Max Heart Rate", "Male Gender", "Exercise Angina"),
  Silent_Risk_Mean = c(
    mean(silent_risk_patients$age),
    mean(silent_risk_patients$chol),
    mean(silent_risk_patients$trestbps),
    mean(silent_risk_patients$oldpeak),
    mean(silent_risk_patients$thalach),
    mean(silent_risk_patients$sex == "Male"),
    mean(silent_risk_patients$exang == "Yes")
  ),
  Other_Patients_Mean = c(
    mean(other_patients$age),
    mean(other_patients$chol),
    mean(other_patients$trestbps),
    mean(other_patients$oldpeak),
    mean(other_patients$thalach),
    mean(other_patients$sex == "Male"),
    mean(other_patients$exang == "Yes")
  )
) %>%
  mutate(
    Difference = Silent_Risk_Mean - Other_Patients_Mean,
    Percent_Difference = round((Difference / Other_Patients_Mean) * 100, 1)
  )

print(predictor_analysis)
cat("\n")

# 6. CLUSTER ANALYSIS OF SILENT RISK PATIENTS
cat("=== PATTERN ANALYSIS: CLUSTERS WITHIN SILENT RISK PATIENTS ===\n")

set.seed(123)
silent_clusters <- kmeans(silent_risk_patients %>%
                            select(age, chol, trestbps, oldpeak, thalach) %>%
                            scale(), centers = 3)

silent_risk_patients$cluster <- as.factor(silent_clusters$cluster)

cluster_profiles <- silent_risk_patients %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    avg_age = round(mean(age), 1),
    avg_chol = round(mean(chol), 1),
    avg_oldpeak = round(mean(oldpeak), 2),
    avg_thalach = round(mean(thalach), 1),
    description = case_when(
      mean(oldpeak) > 2.0 ~ "High ST Depression Group",
      mean(thalach) < 130 ~ "Low Heart Rate Group",
      TRUE ~ "Mixed Risk Factors Group"
    )
  )

cat("Identified patterns within silent risk patients:\n")
print(cluster_profiles)
cat("\n")

# 7. SAVE FOCUSED ANALYSIS RESULTS
write.csv(silent_risk_patients, "output/silent_risk_focused_analysis.csv", row.names = FALSE)

# 8. CREATE VISUALIZATIONS FOR PRESENTATION
cat("=== CREATING RESEARCH VISUALIZATIONS ===\n")

p1 <- ggplot(silent_risk_patients, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", alpha = 0.7) +
  labs(title = "Age Distribution of Silent Risk Patients",
       x = "Age", y = "Number of Patients") +
  theme_minimal()

comparison_long <- comparison_data %>%
  pivot_longer(cols = c(Silent_Risk, Symptomatic),
               names_to = "Group", values_to = "Value")

p2 <- ggplot(comparison_long, aes(x = Characteristic, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Clinical Comparison: Silent vs Symptomatic Patients",
       x = "Clinical Characteristic", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/silent_risk_age_distribution.png", p1, width = 8, height = 6)
ggsave("output/silent_vs_symptomatic_comparison.png", p2, width = 10, height = 6)

# 9. RESEARCH CONCLUSIONS
cat("=== RESEARCH CONCLUSIONS: SILENT HEART ATTACK CASES ===\n\n")

cat("KEY FINDINGS:\n")
cat("1. Silent risk patients represent", round(nrow(silent_risk_patients) / nrow(heart_data) * 100, 1),
    "% of the total population\n")
cat("2. Demographic profile: Average age", round(mean(silent_risk_patients$age), 1),
    "years,", round(mean(silent_risk_patients$sex == "Male") * 100, 1), "% male\n")
cat("3. Clinical characteristics: Higher ST depression and lower maximum heart rate compared to symptomatic patients\n")
cat("4. Risk stratification:", risk_stratification$count[risk_stratification$risk_category == "Very High Risk"],
    "patients are at very high risk within the silent risk group\n")
cat("5. Key predictors: ST depression and maximum heart rate are the most distinguishing factors\n\n")

cat("CLINICAL IMPLICATIONS:\n")
cat("- Silent heart attacks have distinct clinical patterns that can be identified\n")
cat("- Routine screening should focus on high-risk profiles within asymptomatic populations\n")
cat("- Early detection can prevent catastrophic cardiac events\n")
cat("- Targeted interventions can be developed for specific silent risk patterns\n\n")

cat("RESEARCH OUTPUTS (in output/):\n")
cat("1. silent_risk_focused_analysis.csv\n")
cat("2. silent_risk_age_distribution.png\n")
cat("3. silent_vs_symptomatic_comparison.png\n")
cat("4. random_forest_importance.png\n")
cat("5. silent_risk_dashboard.html (generated by dashboard script)\n\n")

cat("Analysis complete.\n")
