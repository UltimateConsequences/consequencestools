test_that("Luminance is calculating correctly", {
  expect_equal(luminance("#ffffff"), 1)
  expect_equal(luminance("#000000"), 0)
  expect_equal(luminance("#0000ff"), 0.114)
  expect_equal(luminance("#00ff00"), 0.587)
  expect_equal(luminance("#ff0000"), 0.299)
})

test_that("Color palettes work", {
  expect_equal(red_pal(c(0, 0.5, 1)), c("#FFE0E0", "#FF3030", "#BB2020"))
  expect_equal(brown_pal(c(0, 0.5, 1)), c("#EFEBE9", "#795548", "#3E2723"))
  expect_equal(bluegray_pal(c(0, 0.5, 1)), c("#ECEFF1", "#607D8B", "#37474F"))
  expect_equal(gray_pal(c(0, 0.5, 1)), c("#EFEFEF", "#8B8B8B", "#4F4F4F"))
})

test_that("Categorical palettes match top colors", {
  expect_equal(perp_pal(1), col2hex(state_resp$colors[["Perpetrator"]]))
  expect_equal(sv_pal(1), col2hex(state_resp$colors[["Victim"]]))
  expect_equal(sep_pal(1), col2hex(state_resp$colors[["Separate"]]))
})
