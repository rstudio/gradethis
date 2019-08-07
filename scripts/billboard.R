library(magrittr)

# function to create the dput output in tests/testthat/billboard.R
billboard <- readr::read_csv("https://raw.githubusercontent.com/hadley/tidy-data/master/data/billboard.csv") %>%
  head()

create_user_df_melt <- function() {
  user <- reshape2::melt(
    billboard,
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
  solution <- tidyr::gather(billboard, "week", "rank", x1st.week:x76th.week)
  return(solution)
}

billboard_file <- file('tests/testthat/billboard.R')
temp1 <- tempfile()
dput(tibble::as_tibble(create_user_df_melt()), file = temp1)

temp2 <- tempfile()
dput(tibble::as_tibble(create_solution_df_tidy()), file = temp2)

cat(
  "# nolint start",
  "\n",
  "billboard_user <-", paste0(readLines(temp1),
                              collapse = "\n"),
  "\n\n",
  "billboard_solution <-", paste0(readLines(temp2),
                                  collapse = "\n"),
  "\n",
  "# nolint end\n",
  file = billboard_file, append = TRUE)

close(billboard_file)
