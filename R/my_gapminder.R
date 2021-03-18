#' Gapminder
#'
#' An excerpt of the data available at Gapminder.org.
#' For each of 142 countries, the package provides values for life expectancy,
#' GDP per capita, and population, every five years, from 1952 to 2007.
#'
#' @source \url{http://www.gapminder.org/data/}
#' @seealso \code{\link[gapminder]{country_colors}} for a nice color scheme for the countries
#' @importFrom tibble tibble
#' @examples
#' gapminder <- STAT302package::my_gapminder
#' str(gapminder)
#' head(gapminder)
#' summary(gapminder)
#' table(gapminder$continent)
#' aggregate(lifeExp ~ continent, gapminder, median)
#' plot(lifeExp ~ year, gapminder, subset = country == "Cambodia", type = "b")
#' plot(lifeExp ~ gdpPercap, gapminder, subset = year == 2007, log = "x")
#'
#' if (require("dplyr")) {
#' gapminder %>%
#'   filter(year == 2007) %>%
#'   group_by(continent) %>%
#'   summarise(lifeExp = median(lifeExp))
#'
#' # how many unique countries does the data contain, by continent?
#' gapminder %>%
#'   group_by(continent) %>%
#'   summarize(n_obs = n(), n_countries = n_distinct(country))
#'
#'
#' # by continent, which country experienced the sharpest 5-year drop in
#' # life expectancy and what was the drop?
#' gapminder %>%
#'   group_by(continent, country) %>%
#'   select(country, year, continent, lifeExp) %>%
#'   mutate(le_delta = lifeExp - lag(lifeExp)) %>%
#'   summarize(worst_le_delta = min(le_delta, na.rm = TRUE)) %>%
#'   filter(min_rank(worst_le_delta) < 2) %>%
#'   arrange(worst_le_delta)
#' }
#'
"my_gapminder"
