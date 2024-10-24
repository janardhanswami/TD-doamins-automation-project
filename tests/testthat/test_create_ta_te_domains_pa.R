
# Load necessary libraries
library(dplyr)
library(openxlsx)
library(testthat)


# Source the function (assuming it's in the same directory)
source("R/create_ta_te_domains_pa.R")

# Set up the study parameters
study_id <- "PABC001"
trial_design <- "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS"

# Define the arms_data
arms_data <- list(
  list(
    armcd = "ARM1",
    arm = "Arm 1: Standard Dose",
    epochs = "SCREENING,TREATMENT 1,TREATMENT 2,FOLLOW-UP",
    etcd = "SCRN,TRT1,TRT2,F/U",
    elements = "Screening,Initial Treatment,Extended Treatment,Follow-up",
    testrl = "Informed consent,First dose of standard treatment,First dose of extended treatment,Last dose of study treatment",
    teenrl = "Randomization,Last dose of initial treatment or progression,Last dose of extended treatment or progression,30 days after last dose",
    tedur = "P28D,P12W,P12W,P30D",
    tabranch = "NA,BR1,NA,NA",
    tatrans = "NA,TR1,NA,NA"
  ),
  list(
    armcd = "ARM2",
    arm = "Arm 2: Dose Escalation",
    epochs = "SCREENING,TREATMENT 1,TREATMENT 2,FOLLOW-UP",
    etcd = "SCRN,TRT1,TRT2,F/U",
    elements = "Screening,Initial Treatment,Escalated Treatment,Follow-up",
    testrl = "Informed consent,First dose of initial treatment,First dose of escalated treatment,Last dose of study treatment",
    teenrl = "Randomization,Last dose of initial treatment,Last dose of escalated treatment or progression,30 days after last dose",
    tedur = "P28D,P12W,P12W,P30D",
    tabranch = "NA,BR2,NA,NA",
    tatrans = "NA,TR2,NA,NA"
  )
)

# Call the function
result <- create_ta_te_domains_pa(study_id, trial_design, arms_data)

# Extract the results
PABC001_TA <- result$TA
PABC001_TE <- result$TE

# Print TA Domain
cat("TA Domain:\n")
print(PABC001_TA)

# Print TE Domain
cat("\nTE Domain:\n")
print(PABC001_TE)

# Test assertions
test_that("TA domain is correctly generated", {
  expect_equal(nrow(PABC001_TA), 8)  # 4 elements * 2 arms
  expect_equal(ncol(PABC001_TA), 10)
  expect_equal(unique(PABC001_TA$STUDYID), "PABC001")
  expect_equal(unique(PABC001_TA$DOMAIN), "TA")
  expect_equal(unique(PABC001_TA$ARMCD), c("ARM1", "ARM2"))
  expect_equal(unique(PABC001_TA$TAETORD), 1:4)
  expect_false(all(is.na(PABC001_TA$TABRANCH)))
  expect_false(all(is.na(PABC001_TA$TATRANS)))
})

test_that("TE domain is correctly generated", {
  expect_equal(nrow(PABC001_TE), 5)  # 5 unique elements (SCRN, TRT1, TRT2, F/U, and one extra for the different TRT2 in ARM2)
  expect_equal(ncol(PABC001_TE), 7)
  expect_equal(unique(PABC001_TE$STUDYID), "PABC001")
  expect_equal(unique(PABC001_TE$DOMAIN), "TE")
  expect_equal(PABC001_TE$ETCD, c("SCRN", "TRT1", "TRT2", "TRT2", "F/U"))
  expect_false(any(is.na(PABC001_TE$TESTRL)))
  expect_false(any(is.na(PABC001_TE$TEENRL)))
  expect_false(any(is.na(PABC001_TE$TEDUR)))
})

# Check if Excel files are created
test_that("Excel files are created", {
  expect_true(file.exists(paste0(study_id, "_TA.xlsx")))
  expect_true(file.exists(paste0(study_id, "_TE.xlsx")))
})

cat("All tests passed successfully!\n")


############################## Tests done by me. ################





# Test 1: Empty arms_data list
test_that("create_ta_te_domains_pa handles empty arms_data correctly", {
  expect_error(
    create_ta_te_domains_pa("STUDY001", "PARALLEL DESIGN", list(), list(), data.frame()),
    "arms_data cannot be empty"
  )
})

# Test 2: Mismatched trial_design and arms_data fields
test_that("create_ta_te_domains_pa handles mismatched trial_design and arms_data fields", {
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up"
    )
  )
  expect_error(
    create_ta_te_domains_pa("STUDY002", "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS", arms_data, list(), data.frame()),
    "Missing branch and trans fields for 'PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS'"
  )
})

# Test 3: More epochs than treatments in one arm
test_that("create_ta_te_domains_pa handles more epochs than treatments", {
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Treatment,Follow-Up"
    )
  )
  treatments_list <- list(c("A"))
  expect_error(
    create_ta_te_domains_pa("STUDY003", "PARALLEL DESIGN", arms_data, treatments_list, data.frame()),
    "Mismatch between number of treatments and treatment epochs for arm 1"
  )
})

# Test 4: Unequal treatment lists across arms
test_that("create_ta_te_domains_pa handles unequal treatment lists across arms", {
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up"
    ),
    list(
      armcd = "ARM2",
      epochs = "Screening,Treatment,Follow-Up"
    )
  )
  treatments_list <- list(c("A", "B"), c("C"))
  expect_error(
    create_ta_te_domains_pa("STUDY004", "PARALLEL DESIGN", arms_data, treatments_list, data.frame()),
    "Mismatch between number of treatments across arms"
  )
})

# Test 5: Missing study_id
test_that("create_ta_te_domains_pa handles missing study_id", {
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up"
    )
  )
  expect_error(
    create_ta_te_domains_pa(NULL, "PARALLEL DESIGN", arms_data, list(), data.frame()),
    "study_id cannot be NULL or empty"
  )
})

# Test 6: Invalid te_rules structure
test_that("create_ta_te_domains_pa handles invalid te_rules structure", {
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up"
    )
  )
  te_rules <- data.frame(
    TESTRL = c("Informed consent", "First dose A", "End of treatment"),
    TEENRL = c("End of screening", "End of A", "End of follow-up")
  )
  expect_error(
    create_ta_te_domains_pa("STUDY005", "PARALLEL DESIGN", arms_data, list(), te_rules),
    "te_rules must contain ELEMENT, TESTRL, TEENRL, and TEDUR columns"
  )
})

# Test 7: Inconsistent element durations (TEDUR)
test_that("create_ta_te_domains_pa handles inconsistent element durations", {
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up"
    )
  )
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT A", "FOLLOW-UP"),
    TESTRL = c("Informed consent", "First dose A", "End of treatment"),
    TEENRL = c("End of screening", "End of A", "End of follow-up"),
    TEDUR = c("P7D", "InvalidDuration", "P21D")
  )
  expect_error(
    create_ta_te_domains_pa("STUDY006", "PARALLEL DESIGN", arms_data, list(), te_rules),
    "Invalid duration format in TEDUR"
  )
})

# Test 8: Non-existent output directory
test_that("create_ta_te_domains_pa handles non-existent output directory", {
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up"
    )
  )
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT A", "FOLLOW-UP"),
    TESTRL = c("Informed consent", "First dose A", "End of treatment"),
    TEENRL = c("End of screening", "End of A", "End of follow-up"),
    TEDUR = c("P7D", "P14D", "P21D")
  )
  expect_error(
    create_ta_te_domains_pa("STUDY007", "PARALLEL DESIGN", arms_data, list(), te_rules, "non/existent/dir"),
    "Output directory does not exist"
  )
})

# Test 9: Valid PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS
test_that("create_ta_te_domains_pa works for PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS", {
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up",
      branch = c(NA, "Branch1", NA),
      trans = c(NA, "Trans1", NA)
    )
  )
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT A", "FOLLOW-UP"),
    TESTRL = c("Informed consent", "First dose A", "End of treatment"),
    TEENRL = c("End of screening", "End of A", "End of follow-up"),
    TEDUR = c("P7D", "P14D", "P21D")
  )
  result <- create_ta_te_domains_pa("STUDY008", "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS", arms_data, list(), te_rules)
  expect_equal(nrow(result$TA), 3)
  expect_false(all(is.na(result$TA$TABRANCH)))
  expect_false(all(is.na(result$TA$TATRANS)))
})

# Test 10: Duplicate arm codes in arms_data
test_that("create_ta_te_domains_pa handles duplicate arm codes", {
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up"
    ),
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up"
    )
  )
  expect_error(
    create_ta_te_domains_pa("STUDY009", "PARALLEL DESIGN", arms_data, list(), data.frame()),
    "Duplicate arm codes in arms_data"
  )
})

# Test 11: Identical epochs across arms
test_that("create_ta_te_domains_pa handles identical epochs across arms", {
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up"
    ),
    list(
      armcd = "ARM2",
      epochs = "Screening,Treatment,Follow-Up"
    )
  )
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT A", "FOLLOW-UP"),
    TESTRL = c("Informed consent", "First dose A", "End of treatment"),
    TEENRL = c("End of screening", "End of A", "End of follow-up"),
    TEDUR = c("P7D", "P14D", "P21D")
  )
  result <- create_ta_te_domains_pa("STUDY010", "PARALLEL DESIGN", arms_data, list(), te_rules)
  expect_equal(nrow(result$TA), 6) # 3 epochs * 2 arms
})


# Test 12: Non-character study_id
test_that("create_ta_te_domains_pa handles non-character study_id", {
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up"
    )
  )
  expect_error(
    create_ta_te_domains_pa(12345, "PARALLEL DESIGN", arms_data, list(), data.frame()),
    "study_id must be a character string"
  )
})

# Test 13: Empty treatments_list
test_that("create_ta_te_domains_pa handles empty treatments_list", {
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up"
    )
  )
  expect_error(
    create_ta_te_domains_pa("STUDY011", "PARALLEL DESIGN", arms_data, list(), data.frame()),
    "treatments_list cannot be empty"
  )
})

# Test 14: Invalid trial_design string
test_that("create_ta_te_domains_pa handles invalid trial_design string", {
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up"
    )
  )
})



#Test 15: Check the strings that comes in the

  d <- list(list(
    armdcd = "ARM2",
    epochs = "screening, tratment, followup"
  ))


