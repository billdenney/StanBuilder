context("stan_code")

test_that("for loops", {
  expect_equal(stan_for_loop(loop_var="n",
                             loop_bounds=list(lower=1, upper=2),
                             contents=list("a=1")),
               structure(
                 list(loop_var="n",
                      lower=structure(
                        list(value=1L, type="int"),
                        class=c("stan_literal_int",
                                "stan_literal",
                                "stan_model_part")),
                      upper=structure(
                        list(value=2L, type="int"),
                        class=c("stan_literal_int",
                                "stan_literal",
                                "stan_model_part")),
                      contents=list("a=1")),
                 class=c("stan_for_loop",
                         "stan_code",
                         "stan_model_part")))
  expect_error(stan_for_loop(loop_var=1),
               regexp="loop_var must be a character scalar",
               info="Test loop_var class")
  expect_error(stan_for_loop(loop_var=c("A", "B")),
               regexp="loop_var must be a character scalar",
               info="Test loop_var length")
  expect_error(stan_for_loop(loop_var="A",
                             contents="A"),
               regexp="contents must be a list",
               info="Test contents class")
  expect_error(stan_for_loop(loop_var="A",
                             contents=list()),
               regexp="contents must have a nonzero length",
               info="Test contents length")
  expect_error(stan_for_loop(loop_var="A",
                             contents=list("A")),
               regexp="argument \"loop_bounds\" is missing, with no default",
               info="Test loop bounds existance")
  expect_error(stan_for_loop(loop_var="A",
                             contents=list("A"),
                             loop_bounds="A"),
               regexp="loop_bounds must be a named list with elements named 'lower' and 'upper'",
               info="Test loop bounds class")
  expect_error(stan_for_loop(loop_var="A",
                             contents=list("A"),
                             loop_bounds=list(lower=factor("B"), upper="C")),
               regexp="loop_bounds\\$lower must be either be a number or a character string.",
               info="Test loop bounds lower class")
  expect_error(stan_for_loop(loop_var="A",
                             contents=list("A"),
                             loop_bounds=list(lower="B", upper=factor("C"))),
               regexp="loop_bounds\\$upper must be either be a number or a character string.",
               info="Test loop bounds upper class")
})

test_that("while loop", {
  expect_equal(stan_while_loop(condition="a == 1",
                               contents=list("A")),
               structure(list(condition="a == 1",
                              contents=list("A")),
                         class=c("stan_while_loop",
                                   "stan_code",
                                   "stan_model_part")))
  expect_error(stan_while_loop(condition=5),
               regexp="condition must be a character scalar",
               info="condition class")
  expect_error(stan_while_loop(condition=c("A", "B")),
               regexp="condition must be a character scalar",
               info="condition length")
  expect_error(stan_while_loop(condition="A", contents="B"),
               regexp="contents must be a list",
               info="contents class")
  expect_error(stan_while_loop(condition="A", contents=list()),
               regexp="contents must have a nonzero length",
               info="contents length")
})

test_that("if statement", {
  expect_equal(stan_if_statement(type="if",
                                 condition="a == 1",
                                 contents=list("A")),
               structure(list(type="if",
                              condition="a == 1",
                              contents=list("A")),
                         class=c("stan_if_statement",
                                   "stan_code",
                                   "stan_model_part")))

  expect_equal(stan_if_statement(type="else",
                                 contents=list("A")),
               structure(list(type="else",
                              condition=NULL,
                              contents=list("A")),
                         class=c("stan_if_statement",
                                 "stan_code",
                                 "stan_model_part")),
               info="else with no condition")
  expect_equal(stan_if_statement(type="else",
                                 condition=NULL,
                                 contents=list("A")),
               structure(list(type="else",
                              condition=NULL,
                              contents=list("A")),
                         class=c("stan_if_statement",
                                 "stan_code",
                                 "stan_model_part")),
               info="else with a NULL condition")
  expect_error(stan_if_statement(type="else",
                                 condition="a == 1",
                                 contents=list("A")),
               regexp="A condition may not be given for an else statement (did you mean 'else if'?)",
               fixed=TRUE,
               info="No condition with else")
  
  expect_error(stan_if_statement(type="if",
                                 condition=factor("a == 1"),
                                 contents=list("A")),
               regexp="condition must be a character scalar",
               fixed=TRUE,
               info="condition class")
  expect_error(stan_if_statement(type="if",
                                 condition=c("B", "a == 1"),
                                 contents=list("A")),
               regexp="condition must be a character scalar",
               fixed=TRUE,
               info="condition length")
  expect_error(stan_if_statement(type="if",
                                 condition="a == 1",
                                 contents="A"),
               regexp="contents must be a list",
               fixed=TRUE,
               info="contents class")
})

test_that("if block", {
  expect_equal(stan_if_block(stan_if_statement(type="if",
                                               condition="a == 1",
                                               contents=list("A"))),
               structure(
                 list(contents=list(
                   structure(list(type="if",
                                  condition="a == 1",
                                  contents=list("A")),
                             class=c("stan_if_statement",
                                     "stan_code",
                                     "stan_model_part")))),
                 class=c("stan_if_block",
                         "stan_code",
                         "stan_model_part")),
               info="if-only if block")
  expect_equal(stan_if_block(stan_if_statement(type="if",
                                               condition="a == 1",
                                               contents=list("A")),
                             stan_if_statement(type="else",
                                               contents=list("B"))),
               structure(
                 list(contents=list(
                   structure(list(type="if",
                                  condition="a == 1",
                                  contents=list("A")),
                             class=c("stan_if_statement",
                                     "stan_code",
                                     "stan_model_part")),
                   structure(list(type = "else",
                                  condition = NULL,
                                  contents = list("B")),
                             class = c("stan_if_statement",
                                       "stan_code",
                                       "stan_model_part")))),
                 class=c("stan_if_block",
                         "stan_code",
                         "stan_model_part")),
               info="if-else block")
  expect_equal(stan_if_block(stan_if_statement(type="if",
                                               condition="a == 1",
                                               contents=list("A")),
                             stan_if_statement(type="else if",
                                               condition="a == 2",
                                               contents=list("C")),
                             stan_if_statement(type="else",
                                               contents=list("B"))),
               structure(
                 list(contents=list(
                   structure(list(type="if",
                                  condition="a == 1",
                                  contents=list("A")),
                             class=c("stan_if_statement",
                                     "stan_code",
                                     "stan_model_part")),
                   structure(list(type="else if",
                                  condition="a == 2",
                                  contents=list("C")),
                             class=c("stan_if_statement",
                                     "stan_code",
                                     "stan_model_part")),
                   structure(list(type = "else",
                                  condition = NULL,
                                  contents = list("B")),
                             class = c("stan_if_statement",
                                       "stan_code",
                                       "stan_model_part")))),
                 class=c("stan_if_block",
                         "stan_code",
                         "stan_model_part")),
               info="if-else if-else block")
  expect_equal(stan_if_block(stan_if_statement(type="if",
                                               condition="a == 1",
                                               contents=list("A")),
                             stan_if_statement(type="else if",
                                               condition="a == 2",
                                               contents=list("C")),
                             stan_if_statement(type="else",
                                               contents=list("B"))),
               structure(
                 list(contents=list(
                   stan_if_statement(type="if",
                                     condition="a == 1",
                                     contents=list("A")),
                   stan_if_statement(type="else if",
                                     condition="a == 2",
                                     contents=list("C")),
                   stan_if_statement(type="else",
                                     contents=list("B")))),
                 class=c("stan_if_block",
                         "stan_code",
                         "stan_model_part")),
               info="if-else if block")
  
  expect_error(stan_if_block(),
               regexp="At least one argument must be given.",
               fixed=TRUE,
               info="require an argument")
  expect_error(stan_if_block("A"),
               regexp="All arguments for a stan_if_block must be stan_if_statement objects",
               fixed=TRUE,
               info="require valid if_statement argument")

  expect_error(stan_if_block(stan_if_statement(type="else if",
                                               condition="a == 2",
                                               contents=list("C"))),
               regexp="The first element of an if block must have type 'if'",
               fixed=TRUE,
               info="must start with if (else if)")
  expect_error(stan_if_block(stan_if_statement(type="else",
                                               contents=list("B"))),
               regexp="The first element of an if block must have type 'if'",
               fixed=TRUE,
               info="must start with if (else)")
  expect_error(stan_if_block(stan_if_statement(type="if",
                                               condition="a == 1",
                                               contents=list("A")),
                             stan_if_statement(type="else",
                                               contents=list("B")),
                             stan_if_statement(type="else if",
                                               condition="a == 2",
                                               contents=list("C"))),
               regexp="Only the last element of an if block may be of type 'else'",
               fixed=TRUE,
               info="may not have else in the middle")
})
