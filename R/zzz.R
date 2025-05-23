# R/zzz.R
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("id"))
}


if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c("id",           # önceki ek
      "Count", "Group", "Status", "Misfit", "Statistic",
      "a", "Hi", "x", "y")
  )
}
