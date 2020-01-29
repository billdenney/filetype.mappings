context("register_data_type")

test_that("register_data_type stores and retrieves lists of data sets", {
  my_instance <- register_data_type()
  expect_equal(
    my_instance(), list(),
    info="initialization is an empty list"
  )
  expect_equal(
    my_instance(A="A"),
    list(A="A")
  )
  expect_equal(
    my_instance(B="B"),
    list(A="A", B="B"),
    info="Values are added to the set."
  )
  my_instance_2 <- register_data_type()
  expect_equal(
    my_instance_2(), list(),
    info="Values are local to each instance (no value sharing occurs between `my_instance` and `my_instance_2`)"
  )
})

test_that("register_data_type gives appropriate warnings and errors", {
  my_instance <- register_data_type()
  expect_warning(
    register_data_type()(A="A", reset=TRUE),
    info="reset does not respect other arguments"
  )
  expect_error(
    my_instance(.l=list("A")),
    info="input must be named (list input with no name)"
  )
  expect_error(
    register_data_type()(.l=list("A", B="B")),
    info="input must be named (list input with some names)"
  )
  expect_error(
    register_data_type()(.l=list(A="A", B="B"), "C"),
    info="input must be named (combined list an non-list input with no names on the non-list)"
  )
  expect_error(
    my_instance("C"),
    info="input must be named (no names on the non-list)"
  )
  expect_error(
    {
      my_instance(reset=TRUE)
      my_instance(A="A")
      my_instance(A="A")
    },
    info="Cannot overwrite without explicitly allowing overwriting"
  )
  expect_equal(
    expect_warning(
      {
        my_instance(reset=TRUE)
        my_instance(A="A")
        my_instance(A="B", overwrite=TRUE)
      },
      info="Attempting to overwrite when specifying overwrite is allowed."
    ),
    list(A="B"),
    info="Overwriting succeeds"
  )
  expect_error(
    register_data_type()(C="A", D.E="B"),
    info="Bad class names are not allowed by default"
  )
  expect_warning(
    register_data_type()(C="A", D.E="B", only_good_names=FALSE),
    info="Bad class names are only warned, when allowed."
  )
})
