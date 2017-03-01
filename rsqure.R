rsqure <- function(trv, predv) {
  sse <- sum((trv-predv)^2)
  sst <- sum((trv-mean(predv))^2)
  (1-sst/sse)
}