# GENERATE SILENT RISK PATIENT DASHBOARD (HTML)
# Run this AFTER silent_risk_analysis.R — it depends on `silent_risk_patients`,
# `top_factors`, and `patient_count` created by that script.
#
# Usage:
#   source("silent_risk_analysis.R")
#   source("generate_dashboard.R")

dir.create("output", showWarnings = FALSE)

# Ensure cluster_name column exists
silent_risk_patients <- silent_risk_patients %>%
  mutate(
    cluster_name = case_when(
      cluster == 1 ~ "Mixed Risk Factors",
      cluster == 2 ~ "High ST Depression",
      cluster == 3 ~ "Low Heart Rate"
    )
  )

cat("Building silent risk dashboard...\n")

patient_count <- nrow(silent_risk_patients)
risk_counts <- list(high = 0, moderate = 0, low = 0)

patient_data <- list()
for (i in 1:patient_count) {
  patient <- silent_risk_patients[i, ]

  heart_rate_status <- ifelse(patient$thalach < 120, "Abnormal", "Normal")
  angina_status <- ifelse(patient$exang == "Yes", "Abnormal", "Normal")
  thalassemia_status <- ifelse(patient$thal %in% c("6", "7"), "Abnormal", "Normal")
  vessels_status <- ifelse(as.numeric(as.character(patient$ca)) > 0, "Abnormal", "Normal")
  st_depression_status <- ifelse(patient$oldpeak > 1.0, "Abnormal", "Normal")

  abnormal_count <- sum(c(
    heart_rate_status == "Abnormal",
    angina_status == "Abnormal",
    thalassemia_status == "Abnormal",
    vessels_status == "Abnormal",
    st_depression_status == "Abnormal"
  ))

  if (abnormal_count >= 3) {
    risk_level <- "high"
    risk_counts$high <- risk_counts$high + 1
  } else if (abnormal_count == 2) {
    risk_level <- "moderate"
    risk_counts$moderate <- risk_counts$moderate + 1
  } else {
    risk_level <- "low"
    risk_counts$low <- risk_counts$low + 1
  }

  patient_data[[i]] <- list(
    id = patient$patient_id,
    age = patient$age,
    sex = patient$sex,
    risk_level = risk_level,
    abnormal_count = abnormal_count,
    factors = list(
      heart_rate = heart_rate_status,
      angina = angina_status,
      thalassemia = thalassemia_status,
      vessels = vessels_status,
      st_depression = st_depression_status
    ),
    values = list(
      heart_rate_val = patient$thalach,
      st_depression_val = patient$oldpeak,
      vessels_val = ifelse(vessels_status == "Abnormal", patient$ca, "0")
    )
  )
}

silent_top_factors <- top_factors
silent_top_factors$Feature_Label[1] <- "ST Depression"

con <- file("output/silent_risk_dashboard.html", "w")

writeLines('<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Silent Heart Attack Risk Dashboard</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; font-family: "Georgia", serif; }
        body { background: linear-gradient(135deg, #0f172a 0%, #1e293b 100%); color: #f1f5f9; min-height: 100vh; }
        .dashboard { max-width: 1800px; margin: 0 auto; padding: 20px; }
        .header { background: rgba(255,255,255,0.95); padding: 30px; border-radius: 20px; margin-bottom: 25px; box-shadow: 0 10px 30px rgba(0,0,0,0.2); }
        .header h1 { color: #dc2626; font-size: 2.5rem; margin-bottom: 10px; }
        .header p { color: #64748b; font-size: 1.2rem; }
        .stats { display: flex; gap: 20px; margin-top: 20px; }
        .stat { background: #dc2626; color: white; padding: 15px; border-radius: 10px; text-align: center; flex: 1; }
        .stat .number { font-size: 2rem; font-weight: bold; }
        .main-content { display: grid; grid-template-columns: 320px 1fr; gap: 25px; }
        .sidebar { background: rgba(255,255,255,0.95); padding: 25px; border-radius: 15px; box-shadow: 0 5px 20px rgba(0,0,0,0.1); color: #1e293b; }
        .factor { background: #f8fafc; padding: 15px; margin-bottom: 10px; border-radius: 10px; border-left: 4px solid #dc2626; }
        .factor-name { font-weight: bold; color: #1e293b; }
        .factor-value { color: #dc2626; font-weight: bold; float: right; }
        .patient-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); gap: 20px; }
        .patient-card { background: rgba(255,255,255,0.95); padding: 20px; border-radius: 12px; box-shadow: 0 5px 15px rgba(0,0,0,0.1); border-top: 4px solid; color: #1e293b; }
        .patient-card.high { border-top-color: #dc2626; }
        .patient-card.moderate { border-top-color: #d97706; }
        .patient-card.low { border-top-color: #059669; }
        .card-header { display: flex; justify-content: space-between; margin-bottom: 15px; }
        .patient-id { font-weight: bold; color: #1e40af; font-size: 1.2rem; }
        .risk-badge { padding: 5px 12px; border-radius: 15px; font-size: 0.8rem; font-weight: bold; color: white; }
        .risk-high { background: #dc2626; }
        .risk-moderate { background: #d97706; }
        .risk-low { background: #059669; }
        .patient-info { display: flex; gap: 10px; margin-bottom: 15px; color: #64748b; }
        .factors-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 8px; margin-bottom: 15px; }
        .factor-indicator { padding: 6px; border-radius: 6px; text-align: center; font-size: 0.8rem; font-weight: 500; }
        .abnormal { background: #fef2f2; color: #dc2626; border: 1px solid #fecaca; }
        .normal { background: #f0fdf4; color: #059669; border: 1px solid #bbf7d0; }
        .clinical-pattern { background: #f1f5f9; padding: 12px; border-radius: 8px; font-size: 0.9rem; }
        .pattern-tag { background: #dc2626; color: white; padding: 3px 8px; border-radius: 10px; font-size: 0.7rem; font-weight: bold; margin-bottom: 5px; display: inline-block; }
        .value-display { font-size: 0.8rem; color: #64748b; margin-top: 2px; }
        @media (max-width: 1200px) { .main-content { grid-template-columns: 1fr; } }
        @media (max-width: 768px) { .patient-grid { grid-template-columns: 1fr; } }
    </style>
</head>
<body>
    <div class="dashboard">
        <div class="header">
            <h1>Silent Heart Attack Risk Intelligence</h1>
            <p>Asymptomatic Risk Analysis - No Chest Pain Symptoms</p>
            <div class="stats">
                <div class="stat">
                    <div class="number" id="high-count">0</div>
                    <div>High Silent Risk</div>
                </div>
                <div class="stat">
                    <div class="number" id="moderate-count">0</div>
                    <div>Moderate Risk</div>
                </div>
                <div class="stat">
                    <div class="number" id="low-count">0</div>
                    <div>Low Risk</div>
                </div>
            </div>
        </div>

        <div class="main-content">
            <div class="sidebar">
                <h3 style="color: #dc2626; margin-bottom: 20px;">Silent Risk Factors</h3>', con)

for (i in 1:nrow(silent_top_factors)) {
  factor <- silent_top_factors[i, ]
  writeLines(paste0('
                <div class="factor">
                    <span class="factor-name">', factor$Feature_Label, '</span>
                    <span class="factor-value">', factor$Importance_Percent, '%</span>
                </div>'), con)
}

writeLines('
                <div style="background: #fef2f2; padding: 20px; border-radius: 10px; margin-top: 20px; border-left: 4px solid #dc2626;">
                    <h4 style="color: #dc2626; margin-bottom: 10px;">Silent Risk Alert</h4>
                    <p style="color: #7f1d1d; font-size: 0.9rem; line-height: 1.5;">
                        <strong>No chest pain symptoms present.</strong> Focus on ST depression, exercise-induced angina,
                        and abnormal heart rate responses as key indicators of silent ischemia.
                    </p>
                </div>
            </div>

            <div class="patient-grid">', con)

cat("Writing silent risk patient cards...\n")
for (i in 1:patient_count) {
  if (i %% 20 == 0) cat("   Processed", i, "of", patient_count, "patients\n")

  p <- patient_data[[i]]

  if (p$factors$st_depression == "Abnormal" && p$factors$angina == "Abnormal") {
    pattern <- "SILENT ISCHEMIA"
    desc <- "ST depression with exercise angina - high silent risk"
  } else if (p$factors$vessels == "Abnormal" && p$factors$heart_rate == "Abnormal") {
    pattern <- "MULTI-VESSEL SILENT"
    desc <- "Vessel disease with abnormal HR response"
  } else if (p$factors$st_depression == "Abnormal") {
    pattern <- "ST DEPRESSION RISK"
    desc <- "Significant ST depression detected"
  } else if (p$abnormal_count >= 3) {
    pattern <- "MULTI-FACTOR SILENT"
    desc <- "Multiple silent risk factors present"
  } else if (p$abnormal_count == 2) {
    pattern <- "DUAL FACTOR RISK"
    desc <- "Two silent risk factors detected"
  } else if (p$abnormal_count == 1) {
    pattern <- "SINGLE FACTOR"
    desc <- "One silent risk factor present"
  } else {
    pattern <- "LOW SILENT RISK"
    desc <- "Minimal silent risk factors"
  }

  writeLines(paste0('
                <div class="patient-card ', p$risk_level, '">
                    <div class="card-header">
                        <div class="patient-id">#', p$id, '</div>
                        <div class="risk-badge risk-', p$risk_level, '">', toupper(p$risk_level), '</div>
                    </div>
                    <div class="patient-info">
                        <span>', p$age, 'y ', p$sex, '</span>
                        <span>', p$abnormal_count, '/5 factors</span>
                    </div>
                    <div class="factors-grid">
                        <div class="factor-indicator ', tolower(p$factors$st_depression), '">
                            ST Depression
                            <div class="value-display">', p$values$st_depression_val, 'mm</div>
                        </div>
                        <div class="factor-indicator ', tolower(p$factors$heart_rate), '">
                            Max Heart Rate
                            <div class="value-display">', p$values$heart_rate_val, 'bpm</div>
                        </div>
                        <div class="factor-indicator ', tolower(p$factors$angina), '">
                            Exercise Angina
                            <div class="value-display">', ifelse(p$factors$angina == "Abnormal", "Present", "Absent"), '</div>
                        </div>
                        <div class="factor-indicator ', tolower(p$factors$thalassemia), '">
                            Thalassemia
                            <div class="value-display">', ifelse(p$factors$thalassemia == "Abnormal", "Abnormal", "Normal"), '</div>
                        </div>
                        <div class="factor-indicator ', tolower(p$factors$vessels), '">
                            Major Vessels
                            <div class="value-display">', p$values$vessels_val, ' vessels</div>
                        </div>
                        <div class="factor-indicator normal">
                            Chest Pain
                            <div class="value-display">Asymptomatic</div>
                        </div>
                    </div>
                    <div class="clinical-pattern">
                        <div class="pattern-tag">', pattern, '</div>
                        <div>', desc, '</div>
                    </div>
                </div>'), con)
}

writeLines(paste0('
            </div>
        </div>
    </div>

    <script>
        document.getElementById("high-count").textContent = "', risk_counts$high, '";
        document.getElementById("moderate-count").textContent = "', risk_counts$moderate, '";
        document.getElementById("low-count").textContent = "', risk_counts$low, '";

        document.addEventListener("DOMContentLoaded", function() {
            const cards = document.querySelectorAll(".patient-card");
            cards.forEach((card, index) => {
                card.style.opacity = "0";
                card.style.transform = "translateY(20px)";
                setTimeout(() => {
                    card.style.transition = "all 0.5s ease";
                    card.style.opacity = "1";
                    card.style.transform = "translateY(0)";
                }, index * 100);
            });
        });
    </script>
</body>
</html>'), con)

close(con)

cat("Dashboard created: output/silent_risk_dashboard.html\n")
cat("Patients analyzed:", patient_count, "\n")

# Uncomment to auto-open locally (not run in CI/headless environments):
# browseURL("output/silent_risk_dashboard.html")
