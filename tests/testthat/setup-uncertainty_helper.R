test_bounds <- function(x){
  all_vars <- colnames(x)
  vars <- all_vars[!grepl("_lower", all_vars)]
  vars <- vars[!grepl("_upper", vars)]

  for(var in vars){
    if(paste0(var, "_lower") %in% all_vars){
      expect_true(all(x[,paste0(var, "_lower")] <= x$var))
    }
    if(paste0(var, "_upper") %in% all_vars){
      expect_true(all(x[,paste0(var, "_upper")] <= x$var))
    }
  }
}
