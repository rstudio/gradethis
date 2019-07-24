context("Check Result Condi")

# nolint start
# taken from dput(head(billboard))
# where billboard is taken from:
# https://github.com/hadley/tidy-data/blob/master/data/billboard.csv
billboard <- structure(list(year = c(2000, 2000, 2000, 2000, 2000, 2000), 
    artist.inverted = c("Destiny's Child", "Santana", "Savage Garden", 
    "Madonna", "Aguilera, Christina", "Janet"), track = c("Independent Women Part I", 
    "Maria, Maria", "I Knew I Loved You", "Music", "Come On Over Baby (All I Want Is You)", 
    "Doesn't Really Matter"), time = structure(c(13080, 15480, 
    14820, 13500, 13080, 15420), class = c("hms", "difftime"), units = "secs"), 
    genre = c("Rock", "Rock", "Rock", "Rock", "Rock", "Rock"), 
    date.entered = structure(c(11223, 10999, 10887, 11181, 11174, 
    11125), class = "Date"), date.peaked = structure(c(11279, 
    11055, 10985, 11216, 11244, 11195), class = "Date"), x1st.week = c(78, 
    15, 71, 41, 57, 59), x2nd.week = c(63, 8, 48, 23, 47, 52), 
    x3rd.week = c(49, 6, 43, 18, 45, 43), x4th.week = c(33, 5, 
    31, 14, 29, 30), x5th.week = c(23, 2, 20, 2, 23, 29), x6th.week = c(15, 
    3, 13, 1, 18, 22), x7th.week = c(7, 2, 7, 1, 11, 15), x8th.week = c(5, 
    2, 6, 1, 9, 10), x9th.week = c(1, 1, 4, 1, 9, 10), x10th.week = c(1, 
    1, 4, 2, 11, 5), x11th.week = c(1, 1, 4, 2, 1, 1), x12th.week = c(1, 
    1, 6, 2, 1, 1), x13th.week = c(1, 1, 4, 2, 1, 1), x14th.week = c(1, 
    1, 2, 2, 1, 2), x15th.week = c(1, 1, 1, 4, 4, 2), x16th.week = c(1, 
    1, 1, 8, 8, 3), x17th.week = c(1, 1, 1, 11, 12, 3), x18th.week = c(1, 
    1, 2, 16, 22, 7), x19th.week = c(1, 8, 1, 20, 23, 8), x20th.week = c(2, 
    15, 2, 25, 43, 20), x21st.week = c(3, 19, 4, 27, 44, 25), 
    x22nd.week = c(7, 21, 8, 27, NA, 37), x23rd.week = c(10, 
    26, 8, 29, NA, 40), x24th.week = c(12, 36, 12, 44, NA, 41
    ), x25th.week = c(15, 48, 14, NA, NA, NA), x26th.week = c(22, 
    47, 17, NA, NA, NA), x27th.week = c(29, NA, 21, NA, NA, NA
    ), x28th.week = c(31, NA, 24, NA, NA, NA), x29th.week = c(NA, 
    NA, 30, NA, NA, NA), x30th.week = c(NA, NA, 34, NA, NA, NA
    ), x31st.week = c(NA, NA, 37, NA, NA, NA), x32nd.week = c(NA, 
    NA, 46, NA, NA, NA), x33rd.week = c(NA, NA, 47, NA, NA, NA
    ), x34th.week = c(NA_real_, NA_real_, NA_real_, NA_real_, 
    NA_real_, NA_real_), x35th.week = c(NA_real_, NA_real_, NA_real_, 
    NA_real_, NA_real_, NA_real_), x36th.week = c(NA_real_, NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_), x37th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x38th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x39th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x40th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x41st.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x42nd.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x43rd.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x44th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x45th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x46th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x47th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x48th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x49th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x50th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x51st.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x52nd.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x53rd.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x54th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x55th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x56th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x57th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x58th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x59th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x60th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x61st.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x62nd.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x63rd.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x64th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x65th.week = c(NA_real_, 
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), x66th.week = c(NA, 
    NA, NA, NA, NA, NA), x67th.week = c(NA, NA, NA, NA, NA, NA
    ), x68th.week = c(NA, NA, NA, NA, NA, NA), x69th.week = c(NA, 
    NA, NA, NA, NA, NA), x70th.week = c(NA, NA, NA, NA, NA, NA
    ), x71st.week = c(NA, NA, NA, NA, NA, NA), x72nd.week = c(NA, 
    NA, NA, NA, NA, NA), x73rd.week = c(NA, NA, NA, NA, NA, NA
    ), x74th.week = c(NA, NA, NA, NA, NA, NA), x75th.week = c(NA, 
    NA, NA, NA, NA, NA), x76th.week = c(NA, NA, NA, NA, NA, NA
    )), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, 
-6L))
# nolint end

create_user_df_melt <- function() {
  user <- reshape2::melt(
    head(billboard),
    id.vars = c("year", "artist.inverted", "track", "time",
                "genre", "date.entered", "date.peaked"),
    variable.name = "week",
    value.name = "rank",
    factorAsStrings = TRUE
  )
  user$week <- as.character(user$week)
  return(user)
}

create_solution_df_tidy <- function() {
  solution <- tidyr::gather(head(billboard), "week", "rank", x1st.week:x76th.week)
  return(solution)
}

test_that("Comparing dataframes, testing for null env", {
  skip_if_not_installed("reshape2")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("tibble")

  user <- tibble::as_tibble(create_user_df_melt())
  solution <- tibble::as_tibble(create_solution_df_tidy())

  # check that the results are the same
  testthat::expect_equal(user, solution)

  expect_correct(
    check_result(
        pass_if(~ .result == solution, "This is a correct message"),
        learnr_args = list(last_value = user, envir_prep = new.env())
    )
  )
})
