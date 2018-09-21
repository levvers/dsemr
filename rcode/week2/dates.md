Dates in R
----------

The functions from the package
[lubridate](Temperatures%20in%20Eskdalemuir) are very helpful when
working with dates in R. YOu can install the package by entering the
command

    install.packages("lubridate")

or by clicking on the *Install* button in the *Packages* tab in the
bottom-right pane in RStudio.

Create date objects in R
------------------------

### Dates from strings

Often, we obtain the date as a simple character string, like
`"15 October 2018"`. `lubridate` has a number of functions to convert
these to internal date (and datetime) objects. The function names are
combinatins of the letter `d`, `m` and `y` and depend on how the date
has been specified

    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    course.start <- dmy("15 October 2018")
    course.start <- dmy("Monday, 15 October 2018")       # Also works, dmy simply ignores the weekday
    course.start <- ymd("2018/10/15")
    course.start

    ## [1] "2018-10-15"

We can also specify the date and time.

    course.start <- dmy_hms("15 October 2018 9:00:00")
    course.start

    ## [1] "2018-10-15 09:00:00 UTC"

### Dates from sepearate variables for the components

Sometimes, the date is available as separate variables (or columns in a
dataset). We can then create a date (or datetime) oject using

    course.end <- make_datetime(2018, 11, 25, 23, 59, 59)
    course.end

    ## [1] "2018-11-25 23:59:59 UTC"

We can also include time zone information (e.g. `tz="Europe/London"`),
but we will not at this in more detail.

### Relative dates

We can also specify dates relative to one another, so we could have used

    course.end <- make_datetime(2018, 11, 26) - seconds(1)
    course.end

    ## [1] "2018-11-25 23:59:59 UTC"

Decimal dates
-------------

We can create decimal dates by simply using the function

    decimal_date(course.end)

    ## [1] 2018.901

We can go the other way round using

    date_decimal(2018.90136985)

    ## [1] "2018-11-25 23:59:59 UTC"

Periodsm durations and intervals
--------------------------------

To find out the length a time period, we first create a time interval
and then divide by the unit we want to obtain.

    course.start%--%course.end

    ## [1] 2018-10-15 09:00:00 UTC--2018-11-25 23:59:59 UTC

    (course.start%--%course.end) / dyears(1)

    ## [1] 0.1140411

    (course.start%--%course.end) / dweeks(1)

    ## [1] 5.946427

If you want to learn more
-------------------------

The [cheatsheet for
`lubridate`](https://rawgit.com/rstudio/cheatsheets/master/lubridate.pdf)
and the [chapter *Date and time* from *R for Data
Science*](http://r4ds.had.co.nz/dates-and-times.html) contains more
information and examples.
