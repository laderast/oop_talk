## ------------------------------------------------------------------------
t
#> function (x)
#> UseMethod("t")
#> <bytecode: 0x7fc9474588a0>
#> <environment: namespace:base>

pryr::ftype(t)
#> [1] "s3"      "generic"


## ------------------------------------------------------------------------
pryr::ftype(t.data.frame)
#> [1] "s3"      "method"


## ------------------------------------------------------------------------
methods(summary)


## ------------------------------------------------------------------------
[1] summary.aov                    summary.aovlist*               summary.aspell*
[4] summary.check_packages_in_dir* summary.connection             summary.data.frame
[7] summary.Date                   summary.default                summary.ecdf*
[10] summary.factor                 summary.glm                    summary.infl*
[13] summary.lm                     summary.loess*                 summary.manova
[16] summary.matrix                 summary.mlm*                   summary.nls*
[19] summary.packageStatus*         summary.PDF_Dictionary*        summary.PDF_Stream*
[22] summary.POSIXct                summary.POSIXlt                summary.ppr*
[25] summary.prcomp*                summary.princomp*              summary.proc_time
[28] summary.srcfile                summary.srcref                 summary.stepfun
[31] summary.stl*                   summary.table                  summary.tukeysmooth*
[34] summary.warnings


## ------------------------------------------------------------------------
library(pryr)

# data.frame
otype(mtcars)
#> [1] "S3"

# vector
otype(c("a", "b", "c"))
#> [1] "base"

# factor
otype(as.factor(c("a", "b", "c")))
#> [1] "S3"



## ------------------------------------------------------------------------
methods(foo) #  or methods("foo")


## ------------------------------------------------------------------------
class(5)
class(mtcars)


## ------------------------------------------------------------------------
getS3method("foo", "numeric")
getS3method("foo", "character")
getS3method("foo", "default")


## ------------------------------------------------------------------------
x <- "hello world"
class(x)
#> "character"
is.object(x)
#> [1] FALSE
class(x) <- "saying"
class(x)
#> [1] "character"
is.object(x)
#> [1] TRUE


## ------------------------------------------------------------------------
x <- 5
class(x) <- "my_number"


## ------------------------------------------------------------------------
structure(5, class = "my_number")


## ------------------------------------------------------------------------
structure(5, class = c("foo", "bar"))
# or
x <- 5
class(x) <- c("foo", "bar"))


## ------------------------------------------------------------------------
x <- structure(5, class = "stuff")
class(x)
#> [1] "suff"
class(x) <- NULL
class(x)
#> [1] "numeric"


## ------------------------------------------------------------------------
foo <- function(x) UseMethod("foo")
foo.character <- function(x) paste("hello ", x)
foo.numeric <- function(x) x^2
foo.default <- function(x) stop("no 'foo' method for ", class(x)[1L])


## ------------------------------------------------------------------------
foo(data.frame())
#> Error: no 'foo' method for 'data.frame'


## ------------------------------------------------------------------------
foo('bar')
#> [1] "hello bar"


## ------------------------------------------------------------------------
foo(4)
#> [1] 16


## ------------------------------------------------------------------------
foo <- function(x, y) UseMethod("foo", y)
foo.numeric <- function(x, y) y - x


## ------------------------------------------------------------------------
foo(5, 10)
#> [1] 5


## ------------------------------------------------------------------------
#' My function description
#' @export
#' @parameter x description of x
#' @examples ...
foo <- function(x) UseMethod("foo")

#' @export
foo.character <- function(x) paste("hello ", x)


## ------------------------------------------------------------------------
#' My function description
#' @export
#' @parameter x description of x
#' @examples ...
foo <- function(x) UseMethod("foo")

#' @export
#' @rdname foo
foo.character <- function(x) paste("hello ", x)


## ------------------------------------------------------------------------
attributes("foo bar")
#> NULL
x <- structure("foo bar", class = "stuff")
attributes(x)
#> $class
#> [1] "stuff"


## ------------------------------------------------------------------------
attr(x, "fruit") <- "apple"
attributes(x)
#> $class
#> [1] "stuff"
#> $fruit
#> [1] "apple"


## ------------------------------------------------------------------------
attr(x, "fruit")
#> [1] "apple"


## ------------------------------------------------------------------------
x <- structure(list(foo = 5, bar = 9), class = "stuff")


## ------------------------------------------------------------------------
print.stuff <- function(x, ...) {
cat(paste("foo: ", x$foo), sep = "\n")
cat(paste("bar: ", x$bar), sep = "\n")
}


## ------------------------------------------------------------------------
x
#> foo:  5
#> bar:  9


## ------------------------------------------------------------------------
W  checking S3 generic/method consistency (2s)
print:
 function(x, ...)
print.some_class:
 function(x)


## ------------------------------------------------------------------------
x <- structure(list("hello", "world"), class = "foobar")


## ------------------------------------------------------------------------
lapply(x, function(z) z)
#> [[1]]
#> [1] "hello"
#> 
#> [[2]]
#> [1] "world"

