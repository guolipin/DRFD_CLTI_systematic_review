# ============================================================
# Summary and pooling of study-level mean age and SD
# ============================================================

# Enter study-level data.
# Only studies with usable mean age and SD values in the
# "Age used for calculation" column are included.

age_dat <- data.frame(
  study = c(
    "Adnan and Fatima (2025)",
    "Du et al. (2022)",
    "Gao et al. (2025)",
    "Matsinhe et al. (2024)",
    "Mostafavi et al. (2025)",
    "Poradzka and Czupryniak (2023)",
    "Stefanopoulos et al. (2024)",
    "Tao et al. (2025)",
    "Xie et al. (2022)",
    "Zheng et al. (2025)"
  ),
  
  mean_age = c(
    56.6,
    67.0,
    68.01,
    55.7,
    63.5,
    61.3,
    62.4,
    66.2,
    66.3,
    63.08
  ),
  
  sd_age = c(
    5.18,
    9.80,
    9.72,
    14.0,
    9.9,
    11.0,
    14.1,
    12.2,
    12.1,
    12.08
  ),
  
  n = c(
    319,
    23,
    599,
    114,
    400,
    199,
    326853,
    1035,
    618,
    504
  )
)


# ============================================================
# 1. Check the input data
# ============================================================

print(age_dat)

# Number of included studies
k <- nrow(age_dat)

# Total sample size
total_n <- sum(age_dat$n)


# ============================================================
# 2. Summarise the reported study-level mean ages
# ============================================================

# Median of the study-level mean ages
median_mean_age <- median(age_dat$mean_age)

# Interquartile range of the study-level mean ages
iqr_mean_age <- quantile(
  age_dat$mean_age,
  probs = c(0.25, 0.75),
  names = FALSE
)

# Minimum and maximum study-level mean ages
range_mean_age <- range(age_dat$mean_age)


# ============================================================
# 3. Calculate the unweighted mean of study-level mean ages
# ============================================================

# Each study contributes equally, regardless of sample size
unweighted_mean <- mean(age_dat$mean_age)

# This SD describes variation among the study-level means only.
# It does not incorporate within-study variation in participant age.
sd_across_study_means <- sd(age_dat$mean_age)


# ============================================================
# 4. Calculate the equally weighted pooled SD
# ============================================================

# This calculation treats each study as an equally weighted
# component distribution.
#
# It incorporates:
# 1. Within-study variance: SD_i^2
# 2. Between-study variation in the study-level means
#
# This produces the equally weighted pooled mean age of 63.0 years
# and the equally weighted pooled SD of approximately 12.0 years.

equal_weight_pooled_variance <- mean(
  age_dat$sd_age^2 +
    (age_dat$mean_age - unweighted_mean)^2
)

equal_weight_pooled_sd <- sqrt(
  equal_weight_pooled_variance
)


# ============================================================
# 5. Calculate the sample-size-weighted pooled mean age
# ============================================================

# Each study is weighted according to its sample size
weighted_mean <- weighted.mean(
  x = age_dat$mean_age,
  w = age_dat$n
)


# ============================================================
# 6. Calculate the sample-size-weighted pooled SD
# ============================================================

# Calculate the total within-study sum of squares
within_study_ss <- sum(
  (age_dat$n - 1) * age_dat$sd_age^2
)

# Calculate the between-study sum of squares
between_study_ss <- sum(
  age_dat$n *
    (age_dat$mean_age - weighted_mean)^2
)

# Combine within-study and between-study variation
weighted_pooled_variance <- (
  within_study_ss + between_study_ss
) / (total_n - 1)

# Convert pooled variance to pooled SD
weighted_pooled_sd <- sqrt(
  weighted_pooled_variance
)


# ============================================================
# 7. Calculate the contribution of the dominant study
# ============================================================

# Extract the sample size of Stefanopoulos et al. (2024)
dominant_n <- age_dat$n[
  age_dat$study == "Stefanopoulos et al. (2024)"
]

# Calculate the percentage of the total sample contributed
# by Stefanopoulos et al. (2024)
dominant_percentage <- dominant_n / total_n * 100


# ============================================================
# 8. Sensitivity analysis excluding the dominant study
# ============================================================

# Remove Stefanopoulos et al. (2024)
age_dat_sensitivity <- subset(
  age_dat,
  study != "Stefanopoulos et al. (2024)"
)

# Number of studies in the sensitivity analysis
sensitivity_k <- nrow(age_dat_sensitivity)

# Total sample size after excluding the dominant study
sensitivity_total_n <- sum(age_dat_sensitivity$n)

# Calculate the sample-size-weighted pooled mean age
# after excluding Stefanopoulos et al. (2024)
sensitivity_weighted_mean <- weighted.mean(
  x = age_dat_sensitivity$mean_age,
  w = age_dat_sensitivity$n
)

# Calculate the within-study sum of squares
# for the sensitivity analysis
sensitivity_within_study_ss <- sum(
  (age_dat_sensitivity$n - 1) *
    age_dat_sensitivity$sd_age^2
)

# Calculate the between-study sum of squares
# for the sensitivity analysis
sensitivity_between_study_ss <- sum(
  age_dat_sensitivity$n *
    (
      age_dat_sensitivity$mean_age -
        sensitivity_weighted_mean
    )^2
)

# Combine within-study and between-study variation
# for the sensitivity analysis
sensitivity_pooled_variance <- (
  sensitivity_within_study_ss +
    sensitivity_between_study_ss
) / (
  sensitivity_total_n - 1
)

# Calculate the sample-size-weighted pooled SD
# after excluding Stefanopoulos et al. (2024)
sensitivity_weighted_pooled_sd <- sqrt(
  sensitivity_pooled_variance
)


# ============================================================
# 9. Display all numerical results
# ============================================================

cat(
  "\nAge summary results\n",
  "===================\n",
  
  "Number of studies: ",
  k, "\n",
  
  "Total sample size: ",
  format(total_n, big.mark = ",", scientific = FALSE), "\n\n",
  
  "Median study-level mean age: ",
  round(median_mean_age, 1), " years\n",
  
  "IQR of study-level mean ages: ",
  round(iqr_mean_age[1], 1), "–",
  round(iqr_mean_age[2], 1), " years\n",
  
  "Range of study-level mean ages: ",
  round(range_mean_age[1], 1), "–",
  round(range_mean_age[2], 1), " years\n\n",
  
  "Unweighted mean of study-level mean ages: ",
  round(unweighted_mean, 1), " years\n",
  
  "SD across study-level mean ages: ",
  round(sd_across_study_means, 1), " years\n\n",
  
  "Equally weighted pooled mean age: ",
  round(unweighted_mean, 1), " years\n",
  
  "Equally weighted pooled SD: ",
  round(equal_weight_pooled_sd, 1), " years\n\n",
  
  "Sample-size-weighted pooled mean age: ",
  round(weighted_mean, 1), " years\n",
  
  "Sample-size-weighted pooled SD: ",
  round(weighted_pooled_sd, 1), " years\n\n",
  
  "Sample size of Stefanopoulos et al. (2024): ",
  format(dominant_n, big.mark = ",", scientific = FALSE), "\n",
  
  "Percentage of total sample contributed by ",
  "Stefanopoulos et al. (2024): ",
  round(dominant_percentage, 1), "%\n\n",
  
  "Sensitivity analysis excluding Stefanopoulos et al. (2024)\n",
  "-----------------------------------------------------------\n",
  
  "Number of studies: ",
  sensitivity_k, "\n",
  
  "Total sample size: ",
  format(
    sensitivity_total_n,
    big.mark = ",",
    scientific = FALSE
  ), "\n",
  
  "Sample-size-weighted pooled mean age: ",
  round(sensitivity_weighted_mean, 1), " years\n",
  
  "Sample-size-weighted pooled SD: ",
  round(sensitivity_weighted_pooled_sd, 1), " years\n",
  
  sep = ""
)


# ============================================================
# 10. Create publication-ready statements
# ============================================================

primary_statement <- paste0(
  "Across the ",
  k,
  " studies contributing a mean age to the calculation, ",
  "the median study-level mean age was ",
  round(median_mean_age, 1),
  " years (range ",
  round(range_mean_age[1], 1),
  "–",
  round(range_mean_age[2], 1),
  ")."
)

equal_weight_statement <- paste0(
  "When each study was assigned equal weight, ",
  "the pooled mean age was ",
  round(unweighted_mean, 1),
  " years (pooled SD ",
  round(equal_weight_pooled_sd, 1),
  "; k=",
  k,
  " studies)."
)

weighted_statement <- paste0(
  "The sample-size-weighted pooled mean age was ",
  round(weighted_mean, 1),
  " years (pooled SD ",
  round(weighted_pooled_sd, 1),
  "; k=",
  k,
  " studies; N=",
  format(total_n, big.mark = ",", scientific = FALSE),
  ")."
)

dominance_statement <- paste0(
  "Stefanopoulos et al. (2024) accounted for ",
  round(dominant_percentage, 1),
  "% of the total sample and would therefore have dominated ",
  "the sample-size-weighted estimate."
)

sensitivity_statement <- paste0(
  "In a sensitivity analysis excluding Stefanopoulos et al. (2024), ",
  "the sample-size-weighted pooled mean age was ",
  round(sensitivity_weighted_mean, 1),
  " years (pooled SD ",
  round(sensitivity_weighted_pooled_sd, 1),
  "; k=",
  sensitivity_k,
  " studies; N=",
  format(
    sensitivity_total_n,
    big.mark = ",",
    scientific = FALSE
  ),
  ")."
)

cat(
  "\nPublication-ready statements\n",
  "============================\n",
  primary_statement, "\n",
  equal_weight_statement, "\n",
  weighted_statement, "\n",
  dominance_statement, "\n",
  sensitivity_statement, "\n",
  sep = ""
)
