context('rolls')

# error ------------------------------------------------------------------------

test_that('roll_dice() err for non-numeric', {
  expect_error(roll_dice('a'))
})


# return -----------------------------------------------------------------------

test_that('roll_dice() returns a numeric', {
  expect_true(is(roll_dice(n = 1), 'numeric'))
})

test_that('explode_dice() returns a numeric',{
  rr <- roll_dice(n = 1)
  expect_true(is(explode_dice(x = rr, lvl = 6), 'numeric'))
})

test_that('reroll_dice() returns a numeric',{
  rr <- roll_dice(n = 1)
  expect_true(is(reroll_dice(x = rr, lvl = 1), 'numeric'))
})

test_that('expand_dice() returns a numeric',{
  rr <- roll_dice(n = 1)
  expect_true(is(expand_dice(x = rr, lvl = 6), 'numeric'))
})

test_that('trigger_dice() returns a numeric',{
  rr <- roll_dice(n = 1)
  expect_true(is(trigger_dice(x = rr, trigger = 1), 'numeric'))
})
