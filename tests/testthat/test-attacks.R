context('attacks')

# error ------------------------------------------------------------------------


# return -----------------------------------------------------------------------

test_that('to_attack() returns a numeric', {
  rr <- to_attack(n = 1, skill = 3)
  expect_true(is(rr, 'numeric'))
})
