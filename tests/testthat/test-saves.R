context('saves')

# error ------------------------------------------------------------------------


# return -----------------------------------------------------------------------

test_that('to_save() returns a numeric', {
  rr <- to_save(n = 1, sv = 6, ap = 0)
  expect_true(is(rr, 'numeric'))
})

test_that('to_fnp() returns a numeric', {
  rr <- to_fnp(n = 1, fnp = 6)
  expect_true(is(rr, 'numeric'))
})
