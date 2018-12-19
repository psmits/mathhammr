context('wounds')

# error ------------------------------------------------------------------------


# return -----------------------------------------------------------------------

test_that('to_wound() returns a numeric', {
  rr <- to_wound(n = 1, str = 4, tgh = 4)
  expect_true(is(rr, 'numeric'))
})
