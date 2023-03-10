pretty.output <- function(summary, independent.var.value, analysisplan, cluster_lookup_table, lookup_table, severity = FALSE, camp=FALSE) {
  subset <- summary[which(summary$independent.var.value == independent.var.value),]
  independent.var <- subset$independent.var[1]
  if(is.na(independent.var)) {
    analplan_subset <- analysisplan
  } else {
    analplan_subset <- analysisplan[which(analysisplan$independent.variable == independent.var),]
  }
  vars <- unique(subset$dependent.var)
  departamentos <- unique(subset$repeat.var.value)
  start <- ifelse(camp, 1, 19)
  df <- data.frame(region = lookup_table$filter[start:nrow(lookup_table)][match(departamentos, lookup_table$name[start:nrow(lookup_table)])],  
                   departamento = departamentos, stringsAsFactors = F)
  df <- df[with(df, order(region, departamento)),]
  for(i in 1:length(vars)){
    var_result <- subset[which(subset$dependent.var == vars[i]),]
    df[,vars[i]] <- var_result[match(df$departamento, var_result$repeat.var.value), "numbers"]
    df[,sprintf("%s_min", vars[i])] <- var_result[match(df$departamento, var_result$repeat.var.value), "min"]
    df[,sprintf("%s_max", vars[i])] <- var_result[match(df$departamento, var_result$repeat.var.value), "max"]
  }
  extra_heading <- data.frame(t(vars), stringsAsFactors = F)
  colnames(extra_heading) <- vars
  extra_heading[1,] <- t(analplan_subset[,1][match(vars, analplan_subset$dependent.variable)])
  extra_heading[2,] <- t(analplan_subset$research.question[match(vars, analplan_subset$dependent.variable)])
  extra_heading[3,] <- t(analplan_subset$sub.research.question[match(vars, analplan_subset$dependent.variable)])
  extra_heading[4,] <- t(analplan_subset$dependent.variable.type[match(vars, analplan_subset$dependent.variable)])
  if (severity){
    extra_heading[5,] <- t(analplan_subset$consequence[match(vars, analplan_subset$dependent.variable)])
  }
  df <- rbind.fill(df, extra_heading)
  df <- df[c((nrow(df)-(nrow(extra_heading) - 1)):nrow(df),1:(nrow(df)-nrow(extra_heading))),]
  df$departamento <- lookup_table$english[match(df$departamento, lookup_table$name)]
  if(!camp){df$region <- lookup_table$english[match(df$region, lookup_table$name)]}
  df[1:nrow(extra_heading), which(is.na(df[1,]))] <- ""
  df
}

correct.zeroes <- function(summary) {
  zeroes <- which(summary$dependent.var.value == 0 & summary$numbers == 1)
  summary$dependent.var.value[zeroes] <- 1
  summary$numbers[zeroes] <- 0
  summary$min[zeroes] <- 0
  summary$max[zeroes] <- 0
  return(summary)
}

severity_for_pin <- function(filename, analysisplan){
  group_data <- read.csv(filename, stringsAsFactors = F)
  indicators <- names(group_data)[-c(1,2,which(endsWith(names(group_data), "min") | endsWith(names(group_data), "max")))]
  ind_sep <- unique(unlist(strsplit(indicators, "_"))[seq(1,length(indicators)*2,2)])
  ind_sep <-ind_sep[!is.na(ind_sep)]
  ind_cols <- purrr:::map(1:length(ind_sep), function(j) {
    which(startsWith(names(group_data), paste0(ind_sep[j], "_")) &
            (!endsWith(names(group_data), "min") &
               !endsWith(names(group_data), "max")))
  }) %>% unlist
    names_ind_cols <- names(group_data)[ind_cols]
    sum_cols <- names_ind_cols[which(endsWith(names_ind_cols, "3") | endsWith(names_ind_cols, "4") | endsWith(names_ind_cols, "5"))]
    new_df <- as.data.frame(group_data[6:nrow(group_data),sum_cols])
    new_df <- apply(new_df,2,FUN=as.numeric)
    group_data[6:nrow(group_data),ind_sep[j]] <- rowSums(new_df)

  group_pin <- group_data[-c(3,4),c("district", "governorate", ind_sep)]
  dap_selection <- unlist(strsplit(analysisplan$dependent.variable, "_"))[seq(1,nrow(analysisplan)*2,2)] %in% ind_sep
  group_pin[1,] <- c("","",analysisplan$Indicator.Group...Sector[dap_selection][seq(1,length(which(dap_selection)),5)])
  group_pin[2,] <- c("","",analysisplan$research.question[dap_selection][seq(1,length(which(dap_selection)),5)])
  group_pin[3,] <- c("","",analysisplan$consequence[dap_selection][seq(1,length(which(dap_selection)),5)])
  return(group_pin)
}

analysisplan_nationwide <- function(analysisplan) {
  analysisplan$repeat.for.variable <- ""
  return(analysisplan)
}
analysisplan_pop_group_aggregated <- function(analysisplan) {
  analysisplan$independent.variable <- ""
  analysisplan$independent.variable.type <- ""
  return(analysisplan)
}

moveme <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], 
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", 
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}
