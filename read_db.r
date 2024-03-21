
read_db <- function(file = "",
                     src.dir = ".",
                     value = "report",
                     simplify = TRUE){
  if (file == "") {
    stop("need to specify file name") }
  file.name.path <- file.path(src.dir, file)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.name.path)
  table.names <- RSQLite::dbListTables(con)
  other.tables <- grep("^_", table.names, value = TRUE)
  report.names <- setdiff(table.names, other.tables)
  if (length(report.names) < 1) {
    stop("No report tables found")}
  if (length(report.names) == 1L) {
    tbl0 <- DBI::dbGetQuery(con, paste("SELECT * FROM ", 
                                       report.names))
    if (nrow(tbl0) == 0) 
      warning("Report table has no data")
    if (any(grepl("Clock.Today", names(tbl0)))) {
      if (nrow(tbl0) > 0) {
        tbl0$Date <- try(as.Date(sapply(tbl0$Clock.Today, 
                                        function(x) strsplit(x, " ")[[1]][1])), silent = TRUE)
      }
    }
    if (all(!grepl("SimulationName", names(tbl0)))) {
      tbl0$SimulationName <- NA
      stn <- grep("Simulation", other.tables, value = TRUE)
      SimulationNamesTable <- DBI::dbGetQuery(con, paste("SELECT * FROM ", 
                                                         stn))
      for (j in seq_along(SimulationNamesTable$ID)) {
        tbl0[tbl0$SimulationID == j, "SimulationName"] <- SimulationNamesTable$Name[j]
      }
    }
  }
  if (length(report.names) > 1L && value %in% c("report", "all")) {
    if (simplify) {
      lst0 <- NULL
      for (i in seq_along(report.names)) {
        tbl0 <- DBI::dbGetQuery(con, paste("SELECT * FROM ", 
                                           report.names[i]))
        if (nrow(tbl0) == 0) 
          warning(paste("Report", report.names[i]), "has no data")
        if (any(grepl("Clock.Today", names(tbl0)))) {
          if (nrow(tbl0) > 0) {
            tbl0$Date <- try(as.Date(sapply(tbl0$Clock.Today, 
                                            function(x) strsplit(x, " ")[[1]][1])), 
                             silent = TRUE)
          }
        }
        if (all(!grepl("SimulationName", names(tbl0)))) {
          tbl0$SimulationName <- NA
          stn <- grep("Simulation", other.tables, value = TRUE)
          SimulationNamesTable <- DBI::dbGetQuery(con, 
                                                  paste("SELECT * FROM ", stn))
          for (j in seq_along(SimulationNamesTable$ID)) {
            tbl0[tbl0$SimulationID == j, "SimulationName"] <- SimulationNamesTable$Name[j]
          }
        }
        dat0 <- data.frame(report = report.names[i], 
                           tbl0)
        lst0 <- try(rbind(lst0, dat0), silent = TRUE)
        if (inherits(lst0, "try-error")) {
          stop("Could not simplify reports into a single data.frame \n\n             Choose simplify = FALSE or modify your reports.")
        }
      }
    }
    else {
      lst0 <- vector("list", length = length(report.names))
      for (i in seq_along(report.names)) {
        tbl0 <- DBI::dbGetQuery(con, paste("SELECT * FROM ", 
                                           report.names[i]))
        if (any(grepl("Clock.Today", names(tbl0)))) {
          tbl0$Date <- try(as.Date(sapply(tbl0$Clock.Today, 
                                          function(x) strsplit(x, " ")[[1]][1])), silent = TRUE)
        }
        if (all(!grepl("SimulationName", names(tbl0)))) {
          tbl0$SimulationName <- NA
          stn <- grep("Simulation", other.tables, value = TRUE)
          SimulationNamesTable <- DBI::dbGetQuery(con, 
                                                  paste("SELECT * FROM ", stn))
          for (j in seq_along(SimulationNamesTable$ID)) {
            tbl0[tbl0$SimulationID == j, "SimulationName"] <- SimulationNamesTable$Name[j]
          }
        }
        lst0[[i]] <- tbl0
      }
      names(lst0) <- report.names
    }
  }
  if (!value %in% c("report", "all") && length(report.names) > 
      1L) {
    if (!value %in% report.names) {
      cat("Available table names: ", report.names, "\n")
      stop("user defined report name is not in the list of available tables", 
           call. = FALSE)
    }
    tbl0 <- DBI::dbGetQuery(con, paste("SELECT * FROM ", 
                                       value))
    if (any(grepl("Clock.Today", names(tbl0)))) {
      tbl0$Date <- try(as.Date(sapply(tbl0$Clock.Today, 
                                      function(x) strsplit(x, " ")[[1]][1])), silent = TRUE)
    }
    if (all(!grepl("SimulationName", names(tbl0)))) {
      tbl0$SimulationName <- NA
      stn <- grep("Simulation", other.tables, value = TRUE)
      SimulationNamesTable <- DBI::dbGetQuery(con, paste("SELECT * FROM ", 
                                                         stn))
      for (j in seq_along(SimulationNamesTable$ID)) {
        tbl0[tbl0$SimulationID == j, "SimulationName"] <- SimulationNamesTable$Name[j]
      }
    }
  }
  if (value == "all") {
    other.tables.list <- vector("list", length = length(other.tables))
    for (i in seq_along(other.tables)) {
      other.tables.list[[i]] <- DBI::dbGetQuery(con, paste("SELECT * FROM ", 
                                                           other.tables[i]))
    }
    names(other.tables.list) <- gsub("_", "", other.tables, 
                                     fixed = TRUE)
  }
  DBI::dbDisconnect(con)
  if (value == "all" && length(report.names) == 1L) {
    lst1 <- list(Report = tbl0)
    ans <- do.call(c, list(lst1, other.tables.list))
  }
  if (value == "all" && length(report.names) > 1L) {
    ans <- do.call(c, list(lst0, other.tables.list))
  }
  if (value == "report" && length(report.names) > 1L) {
    ans <- lst0
  }
  if ((value == "report" && length(report.names) == 1L) || 
      (!value %in% c("report", "all"))) {
    ans <- tbl0
  }
  ans$id_trial <- as.numeric(str_extract(file, "(?<=_)(\\d+)(?=\\.db)"))
  return(ans)
}

