library(here)
library(embraceR)

expected_df <- NULL
setup({
  expected_df <<- embraceR::load_embrace_i(
    file_path = here("data_raw", "embrace_I"),
    mapping_file = here("data_raw", "mapping_table", "mapping_table.xlsx")
  ) %>%
    # Create copies of the original event columns for comparison
    mutate(
      event_localfailure_orig = event_localfailure,
      event_nodalcontrol_incl_pao_orig = event_nodalcontrol_incl_pao,
      event_systemic_excl_pao_orig = event_systemic_excl_pao,
      event_vitalstatus_orig = event_vitalstatus
    )
})

test_that("add_local_failure", {
  result_df <- expected_df %>%
    embraceR::add_local_failure_event()

  # Test that the calculated variable matches the original
  expect_equal(result_df$event_localfailure, result_df$event_localfailure_orig)
})

test_that("add_nodal_failure", {
  result_df <- expected_df %>%
    embraceR::add_nodal_failure_event()

  # Test that the calculated variable matches the original
  expect_equal(result_df$event_nodalcontrol_incl_pao, result_df$event_nodalcontrol_incl_pao_orig)
})

test_that("add_systemic_failure_event", {
  result_df <- expected_df %>%
    embraceR::add_systemic_failure_event()

  # Test that the calculated variable matches the original
  expect_equal(result_df$event_systemic_excl_pao, result_df$event_systemic_excl_pao_orig)
})

test_that("add_os_event", {
  result_df <- expected_df %>%
    add_time_to_diseaseevent() %>%
    embraceR::add_time_to_last_vitalstatus() %>%
    embraceR::add_vitalstatus_event() %>%
    filter(!is.na(event_vitalstatus_orig))

  # Test that the calculated variable matches the original
  expect_equal(result_df$event_vitalstatus, result_df$event_vitalstatus_orig)
})

