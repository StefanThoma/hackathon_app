library()

source("compare_dfs.R")



# test compare_dfs
df1 <- haven::read_xpt("key/adsl.xpt")
df2 <- haven::read_xpt("adam/adsl.xpt")



score_f(df1, df2)
install.packages("dplyr")
packageVersion("dplyr")
