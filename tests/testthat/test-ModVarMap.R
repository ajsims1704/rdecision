
test_that("ModVarMap initialization rejects illegal parameters", {
  expect_error(ModVarMap$new(), class="missing_description")
  expect_error(ModVarMap$new(42), class="description_not_string")
  expect_silent(ModVarMap$new("My Variables"))
})

test_that("Numeric elements are accepted; other primitives are rejected", {
  M <- ModVarMap$new("My Model")
  expect_error(M$add(), class="missing_key")
  expect_error(M$add("key"), class="missing_value")
  expect_silent(M$add("LUE", 42))
  expect_error(M$add("C", "Forty two"), class="incorrect_type")
  expect_error(M$add("L", FALSE), class="incorrect_type")
  expect_error(M$add("L", NULL), class="incorrect_type")
})

test_that("Empty and duplicate keys are rejected", {
  M <- ModVarMap$new("My Model")
  expect_error(M$add(NULL, 42), class="key_not_string")
  expect_error(M$add(NA, 42), class="key_not_string")
  expect_error(M$add(42, 42), class="key_not_string")
  expect_silent(M$add("LUE", 42))
  expect_error(M$add("LUE", 42), class="key_exists")
})

test_that("ModVars are accepted as elements", {
  M <- ModVarMap$new("My Model")
  SN <- NormModVar$new("Standard normal", "parsec", 0, 1)
  expect_silent(M$add("Astronomical distance", SN))  
})

# example model variable map (Kaminski et al CEJOR 2018;26:135-139, fig 1)
test_that("Example model variable map can be created", {
  M <- ModVarMap$new("Kaminski, Figure 1")
  expect_silent(M$add("y.d.c", 10))
  expect_silent(M$add("y.d.t3", 20))
  expect_silent(M$add("y.c.t1", 20))
  expect_silent(M$add("y.c.t2", 0))
  expect_silent(M$add("p.c.t1", 0.75))
  expect_silent(M$add("p.c.t2", 0.25))
})
