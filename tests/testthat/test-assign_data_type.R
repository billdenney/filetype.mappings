test_that("sort_col_names requires the correct input.", {
  expect_error(
    sort_col_names(c("A", "B")),
    regexp="`col_names` must be a list."
  )
  expect_error(
    sort_col_names(list(c("A", "B"), NULL)),
    regexp="`col_names` must be a list of character vectors."
  )
  expect_error(
    sort_col_names(
      list(
        A=c("A", "B"),
        B=c("A", "B")
      )
    ),
    regexp="`col_names` cannot have the same vector of character strings (it would match the same data two ways).  Column names duplicated are: `A`, `B`",
    fixed=TRUE,
    info="Works when the values are in order."
  )
  expect_error(
    sort_col_names(
      list(
        A=c("A", "B"),
        C="C",
        B=c("B", "A")
      )
    ),
    regexp="`col_names` cannot have the same vector of character strings (it would match the same data two ways).  Column names duplicated are: `A`, `B`",
    fixed=TRUE,
    info="Works when values are in order and sorted differently."
  )
  expect_error(
    sort_col_names(list(A=NA_character_)),
    regexp="No value in `col_names` may be NA."
  )
  expect_error(
    sort_col_names(list("A")),
    regexp="`col_names` must be a named list where all elements are named.",
    info="NULL names(col_names)"
  )
  expect_error(
    sort_col_names(list(A="A", "B")),
    regexp="`col_names` must be a named list where all elements are named.",
    info="one element empty names(col_names)"
  )
  expect_error(
    sort_col_names(list()),
    regexp="`col_names` cannot be empty."
  )
  expect_error(
    sort_col_names(
      list(
        A=c("A", "B"),
        A="A"
      )
    ),
    regexp="`names(col_names)` must be unique.",
    fixed=TRUE
  )
})

test_that("sort_col_names correctly sorts the list of inputs.", {
  expect_equal(
    sort_col_names(
      list(
        A=c("A", "B")
      )
    ),
    list(
      A=c("A", "B")
    ),
    info="List scalars return themselves"
  )
  expect_equal(
    sort_col_names(
      list(
        B="A",
        A=c("A", "B")
      )
    ),
    list(
      A=c("A", "B"),
      B="A"
    ),
    info="Values are sorted longest to shortest"
  )
})

test_that("assign_data_type with data.frame input", {
  expect_equal(
    expect_warning(
      assign_data_type(data.frame(C=1, B=2), col_names=list(A=c("A", "B"))),
      regexp="Data did not match any data type for assignment.",
      fixed=TRUE
    ),
    data.frame(C=1, B=2)
  )
  expect_equal(
    assign_data_type(data.frame(A=1, B=2), col_names=list(A=c("A", "B"))),
    local({
      tmp <- data.frame(A=1, B=2)
      class(tmp) <- c("A", "filetype_mappings", "data.frame")
      tmp
    })
  )
  expect_equal(
    assign_data_type(data.frame(A=1, B=2), col_names=list(B=c("A", "B", "C"), A=c("A", "B"), C="A")),
    local({
      tmp <- data.frame(A=1, B=2)
      class(tmp) <- c("A", "filetype_mappings", "data.frame")
      tmp
    })
  )
  expect_equal(
    assign_data_type(data.frame(A=1, B=2, C=3), col_names=list(B=c("A", "B", "C"), A=c("A", "B"), C="A")),
    local({
      tmp <- data.frame(A=1, B=2, C=3)
      class(tmp) <- c("B", "filetype_mappings", "data.frame")
      tmp
    })
  )
  expect_equal(
    assign_data_type(data.frame(A=1, B=2, C=3, D=4), col_names=list(B=c("A", "B", "C"), A=c("A", "B"), C="A")),
    local({
      tmp <- data.frame(A=1, B=2, C=3, D=4)
      class(tmp) <- c("B", "filetype_mappings", "data.frame")
      tmp
    })
  )
})

test_that("assign_data_type with NULL input", {
  expect_null(
    expect_warning(
      assign_data_type(NULL, col_names=list(A=c("A", "B"))),
      regexp="NULL values cannot match a data_type"
    )
  )
  expect_error(
    assign_data_type(NULL, col_names=list(A=c("A", "B")), no_match=stop),
    regexp="NULL values cannot match a data_type"
  )
})

test_that("assign_data_type with list input", {
  expect_error(
    assign_data_type(list("A"), col_names=list(A=c("A", "B"))),
    regexp="no applicable method for 'assign_data_type' applied to an object of class \"character\"",
    fixed=TRUE
  )
  expect_error(
    assign_data_type(
      list(
        data.frame(A=1, B=2),
        data.frame(C=1, B=2)
      ),
      col_names=list(A=c("A", "B")),
      no_match=stop
    ),
    regexp="Data did not match any data type for assignment.",
    fixed=TRUE
  )
  expect_warning(
    assign_data_type(
      list(
        data.frame(A=1, B=2),
        data.frame(C=1, B=2)
      ),
      col_names=list(A=c("A", "B"))
    ),
    list(
      local({
        tmp <- data.frame(A=1, B=2)
        class(tmp) <- c("A", "filetype_mappings", class(tmp))
      }),
      data.frame(C=1, B=2)
    ),
    regexp="Data did not match any data type for assignment.",
    fixed=TRUE
  )
})
