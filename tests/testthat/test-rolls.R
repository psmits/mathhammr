context('rolls')

# error ------------------------------------------------------------------------

test_that('roll_dice() err for non-numeric', {
  expect_error(roll_dice('a'))
})

test_that('roll_dice() err if less than n < 1', {
  expect_error(roll_dice(0))
})


# return -----------------------------------------------------------------------

test_that('roll_dice() returns a numeric', {
  expect_is(roll_dice(n = 1), 'integer')
})
