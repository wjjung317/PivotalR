\name{PivotalR-package}
\alias{PivotalR-package}
\docType{package}
\title{
  An R font-end to PostgreSQL and Greenplum database, and wrapper for in-database
  parallel and distributed machine learning open-source library MADlib
}
\description{
  PivotalR is a package that enables users of R, the most popular open source statistical 
programming language and environment to interact with the Pivotal (Greenplum) 
Database as well as Pivotal HD/HAWQ for Big Data analytics. It does so
by providing an interface to the operations on tables/views in the
database. These operations are almost the same as those of
data.frame. Thus the users of R do not need to learn SQL when they
operate on the objects in the database. The latest code is available at
\url{https://github.com/madlib-internal/PivotalR}. A training video and
a quick-start guide are
available at \url{http://zimmeee.github.io/gp-r/#pivotalr}.
}

\details{
\tabular{ll}{
Package: \tab PivotalR\cr
Type: \tab Package\cr
Version: \tab 0.1.2\cr
Date: \tab 2013-06-11\cr
License: \tab GPL (>= 2)\cr
Depends: \tab methods, DBI, RPostgreSQL\cr
}

This package enables R users to easily develop, refine and deploy R
scripts that leverage the 
parallelism and scalability of the database as well as in-database analytics libraries to 
operate on big data sets that would otherwise not fit in R memory - all this without 
having to learn SQL because the package provides an interface that they are familiar with.

  The package also provides a wrapper for MADlib. MADlib is an open-source
  library for scalable in-database analytics. It provides data-parallel
  implementations of mathematical, statistical and machine-learning
  algorithms for structured and unstructured data. The number of machine
  learning algorithms that MADlib covers is quickly increasing.

As an R front-end to the PostgreSQL-like databases, this package
minimizes the amount of data transferred between the database and R. All
the big data is stored in the database. The
user enters their familiar R syntax, and the package translates it into
SQL queries and sends the SQL query into database for parallel
execution. The computation result, which is small (if it is as big as
the original data, what is the point of big data analytics?), is
returned to R to the user.

On the other hand, this package also gives the usual SQL users the
access of utilizing the powerful analytics and graphics functionalities
of R. Although the database itself has difficulty in plotting, the
result can be analyzed and presented beautifully with R. 

This current version of PivotalR provides the core R infrastructure and data frame functions 
as well as over 50 analytical functions in R that leverage in-database execution. These 
include

* Data Connectivity - db.connect, db.disconnect, db.Rquery

* Data Exploration - db.data.frame, subsets

* R language features - dim, names, min, max, nrow, ncol, summary etc

* Reorganization Functions - merge, by (group-by), samples

* Transformations - as.factor, null replacement

* Algorithms - linear regression and logistic regression wrappers for MADlib
}

\note{
  This package is differernt from PL/R, which is another way of using R
  with PostgreSQL-like databases. PL/R enables the users to run R
  scripts from SQL. In the parallel Greenplum database, one can use PL/R
  to implement parallel algorithms.

  However, PL/R still requires non-trivial knowledge of SQL to use it
  effectively. It is mostly limited to explicitly parallel jobs. And for
  the end user, it is still a SQL interface.

  This package does not require any knowledge of SQL, and it works for
  both explicitly and implicitly parallel jobs by employing the
  open-source MADlib library. It is much more scalable.
  And for the end user, it is a pure R
  interface with the conventional R syntax.
}

\author{
  Author: Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net},  with contributions
  from Woo Jung \email{Woo.Jung@gopivotal.com}
  
  Maintainer: Hai Qian \email{hqian@gopivotal.com}, Predictive Analytics Team at Pivotal
  Inc. \email{user@madlib.net}
}
\references{
  [1] MADlib website, \url{http://madlib.net}
  
  [2] MADlib user docs, \url{http://doc.madlib.net/master}

  [3] MADlib Wiki page, \url{http://github.com/madlib/madlib/wiki}

  [4] MADlib contribution guide,
  \url{https://github.com/madlib/madlib/wiki/Contribution-Guide}

  [5] MADlib on GitHub, \url{https://github.com/madlib/madlib}
}

\seealso{
  \code{\link{madlib.lm}} Linear regression

  \code{\link{madlib.glm}} Linear, logistic and multinomial logistic
  regressions

  \code{\link{madlib.summary}} summary of a table in the database.
}

\examples{
\dontrun{
## get the help for the package
help("PivotalR-package")

## get the help for one method
help("preview,db.Rquery-method")

## get help for a function
help(madlib.lm)

## create multiple connections to different databases
db.connect(port = 5433) # connection 1, use default values for the parameters
db.connect(dbname = "test", user = "qianh1", password = "", host =
"remote.machine.com", madlib = "madlib07", port = 5432) # connection 2

db.list() # list the info for all the connections

## list all tables/views that has "ornst" in the name
db.objects("ornst")

## list all tables/views
db.objects(conn.id = 1)

## create a table and the R object pointing to the table
## using the example data that comes with this package
delete("abalone")
x <- as.db.data.frame(abalone, "abalone")

## OR if the table already exists, you can create the wrapper directly
## x <- db.data.frame("abalone")

dim(x) # dimension of the data table

names(x) # column names of the data table

madlib.summary(x) # look at a summary for each column

preview(x, 20) # look at a sample of the data

## look at a sample sorted by id column
lookat(sort(x, decreasing = FALSE, x$id), 20) 

lookat(sort(x, FALSE, NULL), 20) # look at a sample ordered randomly 

## linear regression Examples --------

## fit one different model to each group of data with the same sex
fit1 <- madlib.lm(rings ~ . - id | sex, data = x)

fit1 # view the result

lookat(mean((x$rings - predict(fit1, x))^2)) # mean square error

## plot the predicted values v.s. the true values
ap <- x$rings # true values
ap$pred <- predict(fit1, x) # add a column which is the predicted values

## If the data set is very big, you do not want to load all the 
## data points into R and plot. We can just plot a random sample.
random.sample <- preview(sort(ap, FALSE, "random"), 1000) # sort randomly

plot(random.sample) # plot a random sample

## fit a single model to all data treating sex as a categorial variable ---------
y <- x # make a copy, y is now a db.data.frame object
y$sex <- as.factor(y$sex) # y becomes a db.Rquery object now
fit2 <- madlib.lm(rings ~ . - id, data = y)

fit2 # view the result

lookat(mean((y$rings - predict(fit2, y))^2)) # mean square error

## logistic regression Examples --------

## fit one different model to each group of data with the same sex
fit3 <- madlib.glm(rings < 10 ~ . - id | sex, data = x, family = "binomial")

fit3 # view the result

## the percentage of correct prediction
lookat(mean((x$rings < 10) == predict(fit3, x)))

## fit a single model to all data treating sex as a categorial variable ----------
y <- x # make a copy, y is now a db.data.frame object
y$sex <- as.factor(y$sex) # y becomes a db.Rquery object now
fit4 <- madlib.glm(rings < 10 ~ . - id, data = y, family = "binomial")

fit4 # view the result

## the percentage of correct prediction
lookat(mean((y$rings < 10) == predict(fit4, y)))

## Group by Examples --------

## mean value of each column except the "id" column
preview(by(x[,-1], x$sex, mean))

## standard deviation of each column except the "id" column
lookat(by(x[,-1], x$sex, sd))

## Merge Examples --------

## create two objects with different rows and columns
key(x) <- "id"
y <- x[1:300, 1:6]
z <- x[201:400, c(1,2,4,5)]

## get 100 rows
m <- merge(y, z, by = c("id", "sex"))

lookat(m, 20)

## operator Examples --------

y <- x$length + x$height + 2.3
z <- x$length * x$height / 3

preview(y < z, 20)

## ------------------------------------------------------------------------
## Deal with NULL values

delete("null_data")
x <- as.db.data.frame(null.data, "null_data")

## OR if the table already exists, you can create the wrapper directly
## x <- db.data.frame("null_data")

dim(x)

names(x)

## ERROR, because of NULL values
fit <- madlib.lm(sf_mrtg_pct_assets ~ ., data = x)

## remove NULL values
y <- x # make a copy
for (i in 1:10) y <- y[!is.na(y[i]),]

dim(y)

fit <- madlib.lm(sf_mrtg_pct_assets ~ ., data = y)

fit

## Or we can replace all NULL values
x[is.na(x)] <- 45
}
}

\keyword{ package }
