# **** Problem Set 1 *****
## Data Visualisation with ggplot2
library(ggplot2)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))
ggplot(mpg) +
  geom_point(aes(displ, hwy, color = class))
ggplot(mpg) +
  geom_point(aes(displ, hwy, size = class))
ggplot(mpg) +
  geom_point(aes(displ, hwy, alpha = class))
ggplot(mpg) +
  geom_point(aes(displ, hwy, shape = class))
ggplot(mpg) +
  geom_point(aes(displ, hwy), color = "blue")
ggplot(mpg) +
  geom_point(aes(displ, hwy), color = "red", size = 2, shape = 7)
ggplot(mpg) + 
  geom_point(aes(displ, hwy)) +
  facet_wrap(~ class, nrow = 2)
ggplot(mpg) +
  geom_point(aes(displ, hwy)) +
  facet_grid(drv ~ cyl)
ggplot(mpg) +
  geom_point(aes(displ, hwy)) +
  facet_grid(. ~ cyl)
ggplot(mpg) +
  geom_smooth(aes(displ, hwy))
ggplot(mpg) +
  geom_smooth(aes(displ, hwy, linetype = drv))
ggplot(mpg) +
  geom_smooth(aes(displ, hwy, group = drv))
ggplot(mpg) +
  geom_smooth(
    aes(displ, hwy, color = drv),
    show.legend = F
    )
ggplot(mpg) +
  geom_point(aes(displ, hwy)) +
  geom_smooth(aes(displ, hwy))
ggplot(mpg, aes(displ, hwy)) + 
  geom_point(color = "red") +
  geom_smooth()
ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(color = class)) +
  geom_smooth()
ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(color = class)) +
  geom_smooth(data = filter(mpg, class == "subcompact"), se = F)
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

ggplot(diamonds) +
  geom_bar(aes(cut))
ggplot(diamonds) + 
  stat_count(aes(cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))
ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))
ggplot(diamonds) +
  geom_bar(aes(cut, colour = clarity))
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()
nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()
# ***** dplyr *******
library ( nycflights13 ) 
library ( tidyverse )
jan1 = filter ( flights ,  month  ==  1 ,  day  ==  1 )
nov_dec  <-  filter ( flights ,  month  %in%  c ( 11 ,  12 ))
filter ( flights ,  month  ==  11  |  month  ==  12 )
filter ( flights ,  ! ( arr_delay  >  120  |  dep_delay  >  120 )) 
filter ( flights ,  arr_delay  <=  120 ,  dep_delay  <=  120 )
df  <-  tibble ( x  =  c ( 1 ,  NA ,  3 )) 
filter ( df ,  x  >  1 )
filter ( df ,  is.na ( x )  |  x  >  1 )
arrange ( flights ,  year ,  month ,  day )
arrange ( flights ,  desc ( arr_delay ))
select ( flights ,  year ,  month ,  day )
select ( flights ,  year : day )
select ( flights ,  - ( year : day ))
rename ( flights ,  tail_num  =  tailnum )
select ( flights ,  time_hour ,  air_time ,  everything ())
flights_sml  <-  select ( flights , year : day , ends_with ( "delay" ), distance , air_time ) 
mutate ( flights_sml , gain  =  arr_delay  -  dep_delay , speed  =  distance  /  air_time  *  60)
mutate ( flights_sml , gain  =  arr_delay  -  dep_delay , hours  =  air_time  /  60 , gain_per_hour  =  gain  /  hours )
transmute ( flights , gain  =  arr_delay  -  dep_delay , hours  =  air_time  /  60 , gain_per_hour  =  gain  /  hours )
transmute ( flights , dep_time , hour  =  dep_time  %/%  100 , minute  =  dep_time  %%  100 )
summarize ( flights ,  delay  =  mean ( dep_delay ,  na.rm  =  TRUE ))
by_day  <-  group_by ( flights ,  year ,  month ,  day ) 
summarize ( by_day ,  delay  =  mean ( dep_delay ,  na.rm  =  TRUE ))
by_dest  <-  group_by ( flights ,  dest ) 
delay  <-  summarize ( by_dest , count  =  n (),dist  =  mean ( distance ,  na.rm  =  TRUE ), delay  =  mean ( arr_delay ,  na.rm  =  TRUE ) ) 
delay  <-  filter ( delay ,  count  >  20 ,  dest  !=  "HNL" )
ggplot ( data  =  delay ,  mapping  =  aes ( x  =  dist ,  y  =  delay ))  + 
  geom_point ( aes ( size  =  count ),  alpha  =  1 / 3 )  + 
  geom_smooth()
delays  <-  flights  %>% group_by ( dest )  %>% summarize ( count  =  n (),ist  =  mean ( distance ,  na.rm  =  TRUE ), delay  =  mean ( arr_delay ,  na.rm  =  TRUE ) )  %>% filter ( count  >  20 ,  dest  !=  "HNL" )
not_cancelled  <-  flights  %>% filter ( ! is.na ( dep_delay ),  ! is.na ( arr_delay )) 
not_cancelled  %>% group_by ( year ,  month ,  day )  %>% summarize ( mean  =  mean ( dep_delay ))
elays  <-  not_cancelled  %>% group_by ( tailnum )  %>% summarize ( delay  =  mean ( arr_delay ) ) 
ggplot ( data  =  delays ,  mapping  =  aes ( x  =  delay ))  + geom_freqpoly ( binwidth  =  10 )
delays  <-  not_cancelled  %>% group_by ( tailnum )  %>% summarize ( delay  =  mean ( arr_delay ,  na.rm  =  TRUE ), n  =  n () ) 
ggplot ( data  =  delays ,  mapping  =  aes ( x  =  n ,  y  =  delay ))  +
  geom_point ( alpha  =  1 / 10 )
delays  %>% 
  filter ( n  >  25 )  %>% 
    ggplot ( mapping  =  aes ( x  =  n ,  y  =  delay ))  +
      geom_point ( alpha  =  1 / 10 )

# *** readr ***
heights  <-  read_csv ( "data/heights.csv" )
str(parse_logical(c("TRUE", "FALSE", "NA")))
str(parse_integer(c("1", "2", "3")))
str(parse_date(c("2010-01-01", "1979-10-14")))
parse_integer ( c ( "1" ,  "231" ,  "." ,  "456" ),  na  =  "." )
parse_double ( "1,23" ,  locale  =  locale ( decimal_mark  =  "," ))
parse_number("$100")
parse_number("20%")
charToRaw("Hadley")
x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
parse_character(x1, locale = locale(encoding = "Latin1"))
parse_character(x2, locale = locale(encoding = "Shift-JIS"))
fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)
parse_datetime("2010-10-01T2010")
parse_datetime("20101010")
library(hms)
parse_time("01:10 am")
parse_time("20:10:01")
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014
t1 <- "1705"
t2 <- "11:15:10.12 PM"
guess_parser("2010-10-01")
guess_parser("15:01")
guess_parser(c("TRUE", "FALSE"))
guess_parser(c("1", "5", "9"))
guess_parser(c("12,352,561"))
str(parse_guess("2010-10-10"))
challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_character()
  )
)
tail(challenge)
challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)
tail(challenge)
challenge2 <- read_csv(readr_example("challenge.csv"), 
                       col_types = cols(.default = col_character())
)
df <- tribble(
  ~x,  ~y,
  "1", "1.21",
  "2", "2.32",
  "3", "4.56"
)
df
type_convert(df)
challenge2 <- read_csv(readr_example("challenge.csv"), guess_max = 1001)
challenge2
challenge2 <- read_csv(readr_example("challenge.csv"), 
                       col_types = cols(.default = col_character())
)
write_csv(challenge, "challenge.csv")
write_csv(challenge, "challenge-2.csv")
read_csv("challenge-2.csv")
write_rds(challenge, "challenge.rds")
read_rds("challenge.rds")

# *** Functions ***

df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
  (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) / 
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) / 
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(c(0, 5, 10))
df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)
x <- c(1:10, Inf)
rescale01(x)
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)

has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}
if (y < 0 && debug) {
  message("Y is negative")
}
x = 2
y = 3
if (y == 0) {
  log(x)
} else {
  y ^ x
}
y <- 10
x <- if (y < 20) "Too low" else "Too high"
if (y < 20) {
  x <- "Too low" 
} else {
  x <- "Too high"
}
mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

x <- runif(100)
mean_ci(x)
mean_ci(x, conf = 0.99)

wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}
wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}
wt_mean(1:6, 6:1, na.rm = "foo")
commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Important output")

show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  
  invisible(df)
}
show_missings(mtcars)
mtcars %>% 
  show_missings() %>% 
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>% 
  show_missings() 