library(readr)
library(tidyr)
library(dplyr)

uploads <- read_delim("D:/Dropbox/Doctorat/Chapitre 2/YouNiverse/August_version/weekly_uploads_cat_completed.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

upper_bound1 <- quantile(uploads$id_0, 0.975)
upper_bound1

upper_bound2 <- quantile(uploads$id_0, 0.99)
upper_bound2

upper_bound3 <- quantile(uploads$id_0, 0.995)
upper_bound3

upper_bound4 <- quantile(uploads$id_0, 0.999)
upper_bound4

upper_bound5 <- quantile(uploads$id_0, 0.9999)
upper_bound5

outlier_ind1 <- which(uploads$id_0 > upper_bound1)
outliers1 <- uploads[outlier_ind1, ]
n_distinct(outliers1$channel)

outlier_ind2 <- which(uploads$id_0 > upper_bound2)
outliers2 <- uploads[outlier_ind2, ]
n_distinct(outliers2$channel)

outlier_ind3 <- which(uploads$id_0 > upper_bound3)
outliers3 <- uploads[outlier_ind3, ]
n_distinct(outliers3$channel)

outlier_ind4 <- which(uploads$id_0 > upper_bound4)
outliers4 <- uploads[outlier_ind4, ]
n_distinct(outliers4$channel)

outlier_ind5 <- which(uploads$id_0 > upper_bound5)
outliers5 <- uploads[outlier_ind5, ]
n_distinct(outliers5$channel)

write.csv2(outliers3, file="outliers_995.csv", row.names=FALSE)