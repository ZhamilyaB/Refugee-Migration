library(forecast)
library(lambda.tools)
library(futile.logger)


do_forecast <- function(country, df, h=2, plot=FALSE) {
  flog.info("[%s] start", country)
  xs <- as.numeric(colnames(df))
  xs <- c(xs, max(xs) + (1:h))
  ys <- as.numeric(df[country,])

  fc <- forecast(ys, h=h)
  if (plot) plot(fc)

  id <- sprintf("%s.%s", country, tail(xs,h))
  data.frame(Id=id, Prediction=as.numeric(fc$mean))
}

df <- read.csv('training.csv', row.names=1)
colnames(df) <- sub('X','', colnames(df), fixed=TRUE)
out <- fold(rownames(df), function(i,acc) rbind(acc, do_forecast(i,df)), NULL)

# write.csv(out, 'submission.csv', row.names=FALSE, quote=FALSE)
