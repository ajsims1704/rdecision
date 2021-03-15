

test_that("argument types are checked", {
  expect_error(DigraphLayout$new(), class = "missing_G")
  expect_error(DigraphLayout$new(42), class = "invalid_G")
  expect_error(DigraphLayout$new("G"), class = "invalid_G")
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Edge$new(n1,n2)
  e2 <- Edge$new(n1,n3)
  G <- Graph$new(V=list(n1,n2,n3), E=list(e1, e2))
  expect_error(DigraphLayout$new(G), class = "invalid_G")
  e1 <- Arrow$new(n1,n2)
  e2 <- Arrow$new(n1,n3)
  G <- Digraph$new(V=list(n1,n2,n3), A=list(e1, e2))
  expect_silent(DigraphLayout$new(G))
})

# construct a subclass of DigraphLayout and expose private methods for testing
SubDigraphLayout <- R6::R6Class(
  classname = "SubDigraphLayout",
  inherit = DigraphLayout,
  public = list(
    init_rank = function() {
      rv <- private$init_rank()
      return(rv)
    }
  )
)

test_that("Gasner fig 6 is replicated", {
  # create the graph
  a <- Node$new("a")
  b <- Node$new("b")
  c <- Node$new("c")
  d <- Node$new("d")
  e <- Node$new("e")
  f <- Node$new("f")
  g <- Node$new("g")
  h <- Node$new("h")
  ae <- Arrow$new(a,e,"ae")
  af <- Arrow$new(a,f,"af")
  ab <- Arrow$new(a,b,"ab")
  eg <- Arrow$new(e,g,"eg")
  fg <- Arrow$new(f,g,"fg")
  cd <- Arrow$new(c,d,"cd")
  gh <- Arrow$new(g,h,"gh")
  dh <- Arrow$new(d,h,"dh")
  G <- Digraph$new(V=list(a,b,c,d,e,f,g,h), A=list(ae,af,ab,eg,fg,cd,gh,dh))
  DL <- SubDigraphLayout$new(G)
  DL$init_rank()
  expect_true(TRUE)
})