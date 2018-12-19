# lm_points <- ptsTrainList$LM1
# 
# nest_points <- function(dat){
#   dat <- list(points = dat, tribe = c(), species = c())
#     }
# 
# lm_points <- lapply(lm_points, nest_points)
# 
# for (i in 1:length(lm_points)) {
#   lm_points[[i]][2:3] <- designate[which(designate$ref == names(lm_points[i])), 4:5]
# }

nest_points <- function(dat){
  dat <- list(points = dat, tribe = c(), species = c())
}
pointsstuff <-lapply(pointsstuff, function(junk){lapply(junk, nest_points)})

for (j in 1:6) {
for (i in 1:length(pointsstuff[[j]])) {
  pointsstuff[[j]][[i]][2:3] <- designate[which(designate$ref == names(pointsstuff[[j]][i])), 4:5]
}
}