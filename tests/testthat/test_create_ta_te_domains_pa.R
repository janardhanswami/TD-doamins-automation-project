

test_that("create_ta_te_domains_pa works correctly for simple PARALLEL DESIGN", {
  study_id <- "STUDY002"
  trial_design <- "PARALLEL DESIGN"
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up"
    ),
    list(
      armcd = "ARM2",
      epochs = "Screening,Treatment,Treatment,Treatment,Follow-Up"
    )
  )
  treatments_list <- list(
    c("A", "B", "C"),
    c("D", "E", "F")
  )
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C",
                "TREATMENT D", "TREATMENT E", "TREATMENT F", "FOLLOW-UP"),
    TESTRL = c("Informed consent", "First dose A", "First dose B", "First dose C",
               "First dose D", "First dose E", "First dose F", "End of treatment"),
    TEENRL = c("End of screening", "End of A", "End of B", "End of C",
               "End of D", "End of E", "End of F", "End of follow-up"),
    TEDUR = c("P7D", "P14D", "P14D", "P14D", "P14D", "P14D", "P14D", "P21D"),
    stringsAsFactors = FALSE
  )

  result <- create_ta_te_domains_pa(study_id, trial_design, arms_data, treatments_list, te_rules)

  # Test TA domain
  expect_equal(nrow(result$TA), 10) # 5 epochs * 2 arms
  expect_equal(unique(result$TA$STUDYID), study_id)
  expect_equal(unique(result$TA$DOMAIN), "TA")
  expect_equal(unique(result$TA$ARMCD), c("ARM1", "ARM2"))
  expect_equal(unique(result$TA$EPOCH), c("SCREENING", "TREATMENT", "FOLLOW-UP"))
  expect_true(all(is.na(result$TA$TABRANCH)))
  expect_true(all(is.na(result$TA$TATRANS)))

  # Check treatment sequence
  arm1_treatments <- result$TA %>%
    filter(ARMCD == "ARM1", EPOCH == "TREATMENT") %>%
    pull(ELEMENT)
  expect_equal(arm1_treatments, c("TREATMENT A", "TREATMENT B", "TREATMENT C"))

  arm2_treatments <- result$TA %>%
    filter(ARMCD == "ARM2", EPOCH == "TREATMENT") %>%
    pull(ELEMENT)
  expect_equal(arm2_treatments, c("TREATMENT D", "TREATMENT E", "TREATMENT F"))

  # Test TE domain
  expect_equal(nrow(result$TE), 8) # Number of unique elements in te_rules
  expect_equal(result$TE$STUDYID, rep(study_id, 8))
  expect_equal(result$TE$DOMAIN, rep("TE", 8))
  expect_equal(result$TE$ETCD, paste0("ET", 1:8))
  expect_equal(result$TE$ELEMENT, te_rules$ELEMENT)
  expect_equal(result$TE$TESTRL, te_rules$TESTRL)
  expect_equal(result$TE$TEENRL, te_rules$TEENRL)
  expect_equal(result$TE$TEDUR, te_rules$TEDUR)
})

test_that("create_ta_te_domains_pa works correctly for PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS", {
  study_id <- "STUDY003"
  trial_design <- "PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS"
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Treatment,Follow-Up",
      branch = c(NA, "Branch1", NA, NA),
      trans = c(NA, "Trans1", "Trans2", NA)
    ),
    list(
      armcd = "ARM2",
      epochs = "Screening,Treatment,Treatment,Follow-Up",
      branch = c(NA, "Branch2", NA, NA),
      trans = c(NA, "Trans3", "Trans4", NA)
    )
  )
  treatments_list <- list(
    c("A", "B"),
    c("C", "D")
  )
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT A", "TREATMENT B", "TREATMENT C", "TREATMENT D", "FOLLOW-UP"),
    TESTRL = c("Informed consent", "First dose A", "First dose B", "First dose C", "First dose D", "End of treatment"),
    TEENRL = c("End of screening", "End of A", "End of B", "End of C", "End of D", "End of follow-up"),
    TEDUR = c("P7D", "P14D", "P14D", "P14D", "P14D", "P21D"),
    stringsAsFactors = FALSE
  )

  result <- create_ta_te_domains_pa(study_id, trial_design, arms_data, treatments_list, te_rules)

  # Test TA domain
  expect_equal(nrow(result$TA), 8) # 4 epochs * 2 arms
  expect_equal(unique(result$TA$STUDYID), study_id)
  expect_equal(unique(result$TA$DOMAIN), "TA")
  expect_equal(unique(result$TA$ARMCD), c("ARM1", "ARM2"))
  expect_equal(unique(result$TA$EPOCH), c("SCREENING", "TREATMENT", "FOLLOW-UP"))
  expect_false(all(is.na(result$TA$TABRANCH)))
  expect_false(all(is.na(result$TA$TATRANS)))

  # Check branches and transitions
  arm1_data <- result$TA %>% filter(ARMCD == "ARM1")
  expect_equal(arm1_data$TABRANCH, c(NA, "Branch1", NA, NA))
  expect_equal(arm1_data$TATRANS, c(NA, "Trans1", "Trans2", NA))

  # Test TE domain
  expect_equal(nrow(result$TE), 6) # Number of unique elements in te_rules
  expect_equal(result$TE$STUDYID, rep(study_id, 6))
  expect_equal(result$TE$DOMAIN, rep("TE", 6))
  expect_equal(result$TE$ETCD, paste0("ET", 1:6))
  expect_equal(result$TE$ELEMENT, te_rules$ELEMENT)
  expect_equal(result$TE$TESTRL, te_rules$TESTRL)
  expect_equal(result$TE$TEENRL, te_rules$TEENRL)
  expect_equal(result$TE$TEDUR, te_rules$TEDUR)
})

test_that("create_ta_te_domains_pa handles errors correctly", {
  study_id <- "STUDY004"
  trial_design <- "PARALLEL DESIGN"
  arms_data <- list(
    list(
      armcd = "ARM1",
      epochs = "Screening,Treatment,Follow-Up"
    )
  )
  treatments_list <- list(c("A"))
  te_rules <- data.frame(
    ELEMENT = c("SCREENING", "TREATMENT A", "FOLLOW-UP"),
    TESTRL = c("Informed consent", "First dose A", "End of treatment"),
    TEENRL = c("End of screening", "End of A", "End of follow-up"),
    TEDUR = c("P7D", "P14D", "P21D"),
    stringsAsFactors = FALSE
  )

  # Test invalid trial design
  expect_error(
    create_ta_te_domains_pa(study_id, "INVALID DESIGN", arms_data, treatments_list, te_rules),
    "This function only supports 'PARALLEL DESIGN' and 'PARALLEL DESIGN WITH BRANCHES AND TRANSITIONS'"
  )

  # Test mismatched epochs and treatments
  bad_treatments_list <- list(c("A", "B"))
  expect_error(
    create_ta_te_domains_pa(study_id, trial_design, arms_data, bad_treatments_list, te_rules),
    "Mismatch between number of treatments and treatment epochs for arm 1"
  )
})


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
  expect_error



#Test 15: Check the strings that comes in the

  d <- list(list(
    armdcd = "ARM2",
    epochs = "screening, tratment, followup"
  )




