# test_create_ti_domain.R

library(stringr)
library(readxl)
library(autoTDD)


###Tests cases written by sai

test_that("Valid PDF method with correct inputs", {
  pdf_path <- system.file("extdata", "Protocol.pdf", package = "autoTDD")

  result <- create_ti_domain(
    study_id = "STUDY001",
    method = "pdf",
    pdf_path = pdf_path,
    incl_range = 41:44,
    excl_range = 44:46,
    incl_section = "4.1.1",
    excl_section = "4.1.2",
    end_section = "4.2"
  )

  expect_type(result, "list")
})




test_that("Throw an error if any of the arguments are missing", {
  pdf_path <- system.file("extdata", "Protocol.pdf", package = "autoTDD")

  expect_error(
    create_ti_domain(
      study_id = "STUDY001",
      method = NULL,
      pdf_path = pdf_path,
      incl_range = NULL,  # Set this to NULL to simulate a missing argument
      excl_range = 44:46,
      incl_section = "4.1.1",
      excl_section = "4.1.2",
      end_section = "4.2"
    )
  )
})



test_that("check if the result excel sheet is being created in the user files pane", {

  pdf_path = system.file("extdata", "Protocol.pdf", package = "autoTDD")
  studyid <- "STUDY001"


  result <- create_ti_domain(
    study_id = "STUDY001",
    method = "pdf",
    pdf_path = pdf_path,
    incl_range = 41:44,
    excl_range = 44:46,
    incl_section = "4.1.1",
    excl_section = "4.1.2",
    end_section = "4.2"
  )

  result_pth <- str_c("~/TD-doamins-automation/tests/testthat","/", studyid, "_TI.xlsx")

  expect_equal(file.exists(result_pth), TRUE)

})





test_that("check the result", {

  studyid <- "STUDY001"

    pdf_path <- system.file("extdata", "Protocol.pdf", package = "autoTDD")

  result <- create_ti_domain(
    study_id = "STUDY001",
    method = "pdf",
    pdf_path = pdf_path,
    incl_range = 41:44,
    excl_range = 44:46,
    incl_section = "4.1.1",
    excl_section = "4.1.2",
    end_section = "4.2"
  )

  result_pth <- str_c("~/TD-doamins-automation/tests/testthat","/", studyid, "_TI.xlsx")

  result_chk <- read_excel(result_pth)
   f <- c("STUDYID" , "DOMAIN"  , "IETESTCD" ,"IETEST" ,  "IECAT"  ,  "IESCAT"  , "IEORRES")

expect_true(all(names(result_chk) %in% f))

})



test_that("Check the length of the IEORRES variable from the result excel sheet", {

  studyid <- "STUDY001"

  pdf_path <- system.file("extdata", "Protocol.pdf", package = "autoTDD")

  result <- create_ti_domain(
    study_id = "STUDY001",
    method = "pdf",
    pdf_path = pdf_path,
    incl_range = 41:44,
    excl_range = 44:46,
    incl_section = "4.1.1",
    excl_section = "4.1.2",
    end_section = "4.2"
  )

  result_pth <- str_c("~/TD-doamins-automation/tests/testthat","/", studyid, "_TI.xlsx")

  result_dataset <- read_excel(result_pth)

  expect_true(max(str_length(result_dataset$IEORRES)) < 200)

})



test_that("check IETEST if it contains both Inclusion criteria and Exclusion criteria",{

  studyid <- "STUDY001"

  pdf_path <- system.file("extdata", "Protocol.pdf", package = "autoTDD")

  result <- create_ti_domain(
    study_id = "STUDY001",
    method = "pdf",
    pdf_path = pdf_path,
    incl_range = 41:44,
    excl_range = 44:46,
    incl_section = "4.1.1",
    excl_section = "4.1.2",
    end_section = "4.2")

  result_pth <- str_c("~/TD-doamins-automation/tests/testthat","/", studyid, "_TI.xlsx")

  result_dataset <- read_excel(result_pth)

  criteria <- c("Inclusion Criteria", "Exclusion Criteria")

  expect_equal(unique(result_dataset$IETEST), criteria)

})




test_that("check if the  IECAT varoable contains only expected values in the result xl sheet", {

  studyid <- "STUDY001"

  pdf_path <- system.file("extdata", "Protocol.pdf", package = "autoTDD")

  result <- create_ti_domain(
    study_id = "STUDY001",
    method = "pdf",
    pdf_path = pdf_path,
    incl_range = 41:44,
    excl_range = 44:46,
    incl_section = "4.1.1",
    excl_section = "4.1.2",
    end_section = "4.2")

  result_pth <- str_c("~/TD-doamins-automation/tests/testthat","/", studyid, "_TI.xlsx")

  result_dataset <- read_excel(result_pth)

  criteria <- c("Inclusion", "Exclusion")

  expect_equal(unique(result_dataset$IECAT), criteria)

})


test_that("check if the number of rows in the result excel data sheet  are not zero",{

  studyid <- "STUDY001"

  pdf_path <- system.file("extdata", "Protocol.pdf", package = "autoTDD")

  result <- create_ti_domain(
    study_id = "STUDY001",
    method = "pdf",
    pdf_path = pdf_path,
    incl_range = 41:44,
    excl_range = 44:46,
    incl_section = "4.1.1",
    excl_section = "4.1.2",
    end_section = "4.2")

  result_pth <- str_c("~/TD-doamins-automation/tests/testthat","/", studyid, "_TI.xlsx")

  result_dataset <- read_excel(result_pth)

  expect_true(nrow(result_dataset) > 0, TRUE)

})







