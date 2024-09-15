# test_create_ti_domain.R



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
      method = "pdf",
      pdf_path = pdf_path,
      incl_range = NULL,  # Set this to NULL to simulate a missing argument
      excl_range = 44:46,
      incl_section = "4.1.1",
      excl_section = "4.1.2",
      end_section = "4.2"
    )
  )
})

