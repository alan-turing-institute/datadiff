purrr::map(constraints, function(x) { class(x) })

purrr::map(1:ncol(df1), .f = function(i) {
  purrr::map(1:ncol(df2), .f = function(j) {
    has_nomatch <- 1 == length(purrr::keep(constraints, function(x) {
      is_constraint_nomatch(x, names(df1)[i], names(df2)[j]) }))

    if (has_nomatch) return(1)

    0

    #    c(names(df1)[i], names(df2)[j])
  })
})

bb15 <- subset(broadband2015, select=c("URBAN2","Nation","DL24hrmean","UL24hrmean","Latency24hr","Web24hr"))

read.csv("C:\\Tomas\\Public\\wrattler\\wrattler-workyard\\broadband\\broadband2013.csv")

inputs = strsplit("a=aa.csv,b=bb.csv", ",")
files <- purrr::map(inputs[[1]], function(x) {
  kvp <- strsplit(x, "=")
  list(key=kvp[[1]][1], value=kvp[[1]][2]) })
getInput <- function(i) {
  purrr::keep(files, function(x) { x$key == i })[[1]]$value
}
getInput("b")

constraints1 = list(
  #constraint_match("Nat.weights", "nat.weights"),
  constraint_nomatch("LLU", "Nation"),
  constraint_nomatch("LLU", "URBAN2"),
  constraint_nomatch("Technology", "Nation"),
  constraint_nomatch("Urban.rural", "Nation"),
)

constraints2 = list(
  constraint_notransform("LLU"),
  constraint_notransform("Technology"),
  constraint_notransform("Urban.rural")
)

constraints = list(
  constraint_notransform("LLU"),
  constraint_notransform("Technology"),
  constraint_match("Urban.rural", "URBAN2")
)

p <- ddiff(broadband2014, bb15, constraints=constraints, verbose=TRUE)
p(broadband2014)



df1 <- broadband2014
df2 <- bb15

cw_candidates <- columnwise_candidates(df1, df2 = df2,
                                       mismatch = diffness,
                                       constraints = constraints,
                                       patch_generators = list(gen_patch_rescale, gen_patch_recode),
                                       patch_penalties = c(12, 12),
                                       break_penalty = 0.95,
                                       penalty_scaling = purrr::partial(linear_scaling, nx = nrow(df1),
                                                                        ny = nrow(df2)),
                                       mismatch_attr = "mismatch",
                                       penalty_attr = "penalty",
                                       scale_break_penalty = FALSE,
                                       verbose = TRUE)

extract_matrix <- function(attr_name) {
  matrix(purrr::map_dbl(unlist(cw_candidates), .f = function(p) {
    attr(p, attr_name)
  }), nrow = ncol(df1), ncol = ncol(df2), byrow = TRUE) }
m_mismatch <- extract_matrix("mismatch")
m_penalty <- extract_matrix("penalty")

m_penalty[4,1] <- 9999

soln <- solve_pairwise_assignment(m_mismatch + m_penalty, verbose = TRUE)

options(max.print = 100)
devtools::load_all()
roxygen2::roxygenise()




bb15 <- read.csv("C:\\Tomas\\Public\\wrattler\\wrattler-workyard\\broadband\\broadband2015.csv")
bb14 <- read.csv("C:\\Tomas\\Public\\wrattler\\wrattler-workyard\\broadband\\broadband2014.csv")
bb15nice <- subset(bb15, select=c("URBAN2","Nation","DL24hrmean","UL24hrmean","Latency24hr","Web24hr"))

p <- ddiff(bb14, bb15nice, verbose=TRUE)

write.csv(bb15nice,"c:/temp/bb15nice.csv",row.names=FALSE)
write.csv(bb14,"c:/temp/bb14.csv",row.names=FALSE)
# clean=c:/temp/bb15nice.csv,dirty=c:/temp/bb14.csv
nsc = names(bb15nice)
nsd = names(bb14)
nsp = names(p(bb14))
pp <- decompose_patch(p)


for(i in 1:length(pp)) {
  typ <- patch_type(pp[[i]])
  if (typ == "recode" || typ == "rescale" || typ == "scale" || typ == "shift") {
    col <- nsd[[get_patch_params(pp[[i]])$cols]]
    cat(paste0("Don't transform '", col, "'\n"))
    cat(paste0("/-", col, "'\n"))
  }
}

for(i in 1:length(names(bb15nice))) {
  cat(paste0("Don't match '", nsp[[i]], "' and '", nsc[[i]], "'\n"))
  cat(paste0("/~", nsp[[i]], "-", nsc[[i]], "\n"))
}

for(i in 1:length(names(bb14))) {
  for(j in 1:length(names(bb15nice))) {
    cat(paste0("Match '", nsd[[i]], "' and '", nsc[[j]], "'\n"))
    cat(paste0("/.", nsd[[i]], "-", nsc[[j]], "\n"))
  }
}



query <- "/.nat.weights-Web24hr/~Web.page..ms.24.hour-Web24hr/-LLU"

clist <- purrr::keep(strsplit(query,"/")[[1]], function(x) { x != "" })
constraints <- purrr::map(clist, function(c) {
  kind <- substr(c, 1, 1)
  args <- strsplit(substring(c, 2), "-")[[1]]
  if (kind == ".") {
    constraint_match(args[1], args[2])
  } else if (kind == "~") {
    constraint_nomatch(args[1], args[2])
  } else if (kind == "-") {
    constraint_notransform(args[1])
  } else stop("Unexpected constraint kind.")
})



strsplit("x y"," ")[[1]][1]
