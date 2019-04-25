library(here)

stat_frame <- data.frame(group=c("setosa", "virginica", "versicolor"), pvalue=c(0.5, 0.2, 0.001))

my_s4_object <- StatPackageResult(data = iris, statistics = stat_frame)

saveRDS(my_s4_object, file = here("data", "s4_stat_result.rds"))