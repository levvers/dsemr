R
-

R is a command-based language. This means you get R to do something by
entering commands like

    # Create a few variables (which can be a function of an)
    variable <- "I want to learn R"              
    another_variable <- 24
    yet_another_variable <- sqrt(another_variable*6)
    # Print the value of one of the variables
    yet_another_variable

    ## [1] 12

In some way R is nothing than a very fancy calculator. There aren't just
the functions like the square root, which you would find on a
calculator, there are also more complex functions for things like
fitting models or visualising data.

If you are using a front-end like [RStudio](https://www.rstudio.com/),
it is typically easier if you enter the commands into the code editor
and then run these commands.

Extension packages
------------------

R already has a lot of functionality built in, but there are now more
than 13,000 extension packages, available at

Cheat sheets
------------

RStudio have put together a very helpful collection of
[cheatsheets](https://www.rstudio.com/resources/cheatsheets/). They are
not aimed at first-time users, but are the perfect tool if you don't
remember the name of a function or a function argument.

Working with data in R
----------------------

The easiest way of getting data into R is to use text files, such as
`.csv` files, though you can also connect to
[databases](https://cran.r-project.org/web/packages/DBI/), read in more
specialised file formats such as
[XML](https://cran.r-project.org/web/packages/xml2) or
[netcdf](https://cran.r-project.org/web/packages/ncdf4/).
