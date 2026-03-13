#' RunManager
#'
#' Manages project IO paths and execution lifecycle.
#'
#' @export
RunManager <- R6::R6Class(
  classname = "RunManager",

  public = list(

    # =================================================
    # Constructor
    # =================================================
    initialize = function(root){

      private$root <- normalizePath(root, mustWork = TRUE)

      private$c_dir <- private$makedir(root, "config")

      private$d.raw_dir    <- private$makedir(root, "data", "raw")
      private$d.usable_dir <- private$makedir(root, "data", "usable")
      private$d.preped_dir <- private$makedir(root, "data", "preped")

      private$o.runs_dir      <- private$makedir(root, "output", "runs")
      private$o.published_dir <- private$makedir(root, "output", "published")

      private$s.R_dir   <- private$makedir(root, "script", "R")
      private$s.exe_dir <- private$makedir(root, "script", "exe")

      private$cli_flags <- list(
        `--runname` = character(0), 
        `--config` = character(0)
      )

      self$logmsg("RunManager Initialized.")

    },

    # =================================================
    # IO PATH HELPERS
    # =================================================
    c_path = function(file=NULL) {
        if (is.null(file)) { return( private$c_dir ) } 
        else { return( file.path(private$c_dir, file) ) }
    },

    d.raw_path = function(file=NULL) {
        if (is.null(file)) { return( private$d.raw_dir ) } 
        else { return( file.path(private$d.raw_dir, file) ) } 
    },

    d.usable_path = function(file=NULL) {
        if (is.null(file)) { return( private$d.usable_dir ) } 
        else { return( file.path(private$d.usable_dir, file) ) }
    },

    d.preped_path = function(file=NULL) {
        if (is.null(file)) { return( private$d.preped_dir ) } 
        else { return( file.path(private$d.preped_dir, file) ) }
    },

    o.runs_path = function(file=NULL) {
        if (is.null(file)) { return( private$o.runs_dir ) } 
        else { return( file.path(private$o.runs_dir, file) ) }
    },

    o.published_path = function(file=NULL) {
        if (is.null(file)) { return( private$o.published_dir ) } 
        else { return( file.path(private$o.published_dir, file) ) }
    },

    s.R_path = function(file=NULL) {
        if (is.null(file)) { return( private$s.R_dir ) } 
        else { return( file.path(private$s.R_dir, file) ) }
    },

    s.exe_path = function(file=NULL) {
        if (is.null(file)) { return( private$s.exe_dir ) } 
        else { return( file.path(private$s.exe_dir, file) ) }
    },

    actrun_path = function(file=NULL) {
      if(is.null(private$active_run_dir)) { stop("Run not initialized.") }

      if (is.null(file)) { return( private$active_run_dir ) } 
      else { return( file.path(private$active_run_dir, file) ) }
    },

    actruninfo_path = function(file=NULL) {
      if(is.null(private$active_run_info_dir)) { stop("Run not initialized.") }

      if (is.null(file)) { return( private$active_run_info_dir ) } 
      else { return( file.path(private$active_run_info_dir, file) ) }
    },

    consolelog_path = function (dir, file) {
        return(file.path(private$o.runs_dir, dir, paste0(file, ".txt")))
    },

    record_git_info = function() {

      repo <- tryCatch(
        git2r::repository(private$root),
        error = function(e) { return(NULL) }
      )
    
      if (is.null(repo)) {
        #warning("Not a git repository; skipping git metadata.")
        return(invisible(NULL))
      }
    
      # git commit ID
      repohead <- git2r::repository_head(repo)
      if (is.null(repohead)) {
        private$git_commit <- "No commits yet"
      } else { 
        private$git_commit <- git2r::sha(repohead)
      }
    
      # git working tree dirty status
      status <- git2r::status(repo)
      private$git_dirty <- any(lengths(status) > 0)
    },

    snapshot_env = function() {

      private$snapshot <- self$actruninfo_path("renv.lock")
      renv::snapshot(prompt = FALSE, lockfile=private$snapshot)

    },

    parse_cliarg = function(args, flag) {
    
      idx <- which(args == flag)
    
      if (length(idx) == 0) { return(character(0)) }
    
      values <- args[(idx + 1):length(args)]
    
      next_flag <- grep("^--", values)
    
      if (length(next_flag) > 0) {
        values <- values[1:(next_flag[1] - 1)]
      }
    
      return(values)
    },

    validate_input = function (valid_args, args_input) {
      # no restriction on input
      if (length(valid_args) == 0) { 
          return(args_input) 
      }
      if ("all" %in% args_input) {
          return(valid_args)
      } else {
          invalid <- setdiff(args_input, valid_args)
          if (length(invalid) > 0) {
              stop("Invalid input: ", paste(invalid, collapse = ", "))
          }
          return(args_input)
      }
    },

    default_config = function() {
        default <- self$c_path("default.yaml")
        if (!file.exists(default)) {
            yaml::write_yaml(NULL, file=default)
        }
        return(default)
    },

    normalize_scripts = function () {

        scripts <- private$scrpaths[!dir.exists(private$scrpaths)]
        if (length(scripts) != 0) {
          scripts <- file.path(self$s.R_path(), scripts)
        }
        private$scrpaths[!dir.exists(private$scrpaths)] <- scripts

        if (!all(file.exists(private$scrpaths))) {
            stop(paste0("Script file/directory does not exist."))
        }

        scriptdirs <- setdiff(private$scrpaths, scripts)
        # if any script dir does not exist
        if (!all(dir.exists(scriptdirs))) {
            stop("Script directory does not exist.")
        }

        for (sd in scriptdirs) {
          files <- list.files(
            path = sd,
            pattern = "\\.R$",
            full.names = TRUE,
            recursive = FALSE
          )
          scripts <- c(scripts, files)
        }
        
        private$scrpaths <- unique(normalizePath(scripts))

    },

    logmsg = function(msg, level = "INFO") {
        time  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        level <- sprintf("%-7s", level)  # left-align level width 7
        content <- sprintf("[%s] [%s] %s", time, level, msg)
        cat(content, "\n", sep="")
        private$log_messages <- c(private$log_messages, content)
    }, 

    log_system_info = function () {
        con <- file(self$actruninfo_path("system_info.txt"), open = "wt")
        on.exit(close(con), add = TRUE)

        # ---- title ----
        cat("========================================\n", file = con)
        cat("R SYSTEM REPORT\n", file = con)
        cat("Generated at:", format(Sys.time()), "\n", file = con)
        if (!is.null(private$runname)) {  
            cat("Run Name:", private$runname, "\n", file = con) 
        }
        cat("========================================\n\n", file = con)

        # --- load libraries ----
        cat("----- load libraries -----\n\n", file = con)
        for (pkg in private$libraries) {
            cat("Loading package \'", pkg, "\'\n", sep = "", file = con)
            capture.output(
                library(pkg, character.only = TRUE), file = con
            )
            self$logmsg(paste0("Imported \'", pkg, "\'." ))
        }
        cat("\n\n", file = con)

        # ---- source scripts ----
        cat("----- source scripts -----\n\n", file = con)
        self$normalize_scripts()
        for (scr in private$scrpaths) {
            cat("Sourcing script \'", scr, "\'\n", sep = "", file = con)
            capture.output(source(scr), file = con)
            self$logmsg(paste0("Sourced \'", scr, "\'." ))
        }
        cat("\n\n", file = con)

        # ---- sessionInfo() ----
        cat("----- sessionInfo() -----\n\n", file = con)
        capture.output(sessionInfo(), file = con)
        cat("\n\n", file = con)

        # ---- devtools::session_info() ----
        cat("----- devtools::session_info() -----\n\n", file = con)
        if (requireNamespace("devtools", quietly = TRUE)) {
            capture.output(devtools::session_info(), file = con)
        } else {
            cat("devtools package is not installed.\n", file = con)
        }
        cat("\n\n", file = con)

        # ---- git info ----
        self$record_git_info()
        cat("----- Git info -----\n\n", file = con)
        cat(paste0("Commit: ", private$git_commit, "\n"), file = con)
        cat(paste0("Dirty working tree: ", private$git_dirty, "\n"), file = con)
        cat("\n\n", file = con)

        # ---- R Environment Snapshot ----
        cat("----- R Environment Snapshot -----\n\n", file = con)
        capture.output(self$snapshot_env(), file = con)
        cat("\n\n", file = con)

        self$logmsg("System execution information logged.")
    },

    get_config = function(name=NULL) {
        if (is.null(name)) {
            return(private$config)
        } else {
            return(private$config[[name]])
        }
    },

    get_cliargs = function(name=NULL) {
        if (is.null(name)) {
            return(private$cli_args)
        } else {
            return(private$cli_args[[name]])
        }
    },

    # =================================================
    # RUN INITIALIZATION
    # =================================================
    initialize_run = function(
      libraries = character(0),
      scriptpath = character(0),
      flags = list()
    ){
      private$start_time <- Sys.time()

      # -----------------------------
      # Parse CLI arguments
      # -----------------------------
      private$cli_flags <- modifyList(private$cli_flags, flags)
      cli_args <- commandArgs(trailingOnly = TRUE)
      private$cli_args <- private$get_all_cliargs(cli_args)
      self$logmsg("CLI arguments parsed.")

      # -----------------------------
      # Create dir for current run
      # -----------------------------
      timestamp <- format(private$start_time, "%Y-%m-%d_%H-%M-%S")
      private$runname <- private$cli_args[["--runname"]]
      if (!is.null(private$runname)) {
          timestamp <- paste(timestamp, private$runname)
      }
      private$active_run_dir <- private$makedir(self$o.runs_path(timestamp))
      private$active_run_info_dir <- private$makedir(self$actrun_path("system"))
      self$logmsg("Timestamp directory for current run created.")

      # -----------------------------
      # Load and archive config
      # -----------------------------
      config_file <- private$cli_args[["--config"]]
      if (length(config_file) == 1) {
        config_path <- self$c_path(config_file)
      } else if (length(config_file > 1)) {
        stop("Multiple config files provided.")
      } else { 
        config_path <- self$default_config()
        self$logmsg("No config file provided. Using default config.")
      }
      private$config <- yaml::read_yaml(config_path)
      file.copy(
        config_path, self$actruninfo_path(paste0("config_", basename(config_path)))
      )
      self$logmsg(paste0("config file \'", config_path, "\' loaded."))

      # -----------------------------
      # Log System info (load libs, source scripts)
      # -----------------------------
      private$libraries <- libraries
      private$scrpaths <- scriptpath
      self$log_system_info()
    },


    # =================================================
    # MAIN EXECUTION WRAPPER
    # =================================================
    run = function(expr){

      expr <- substitute(expr)

      private$runtime <- system.time({
        result <- tryCatch(
          withCallingHandlers(
            eval(expr),
            warning = function(w) {
              private$warnings_log <- c(
                private$warnings_log,
                conditionMessage(w)
              )
              invokeRestart("muffleWarning") # supress console warnings
            }

          ), 
          error = function(e){
            private$error_message <- conditionMessage(e)
            return(invisible(NULL))
          }
        )
      })

      private$end_time <- Sys.time()
      private$write_ending_log()

      if(!is.null(private$error_message)) {
        stop(private$error_message)
      }

      private$finalize_run()
      #invisible(result)
    }

  ),


  private = list(
    # -----------------------------
    # Project root
    # -----------------------------
    root = NULL,

    # -----------------------------
    # Project directories
    # -----------------------------
    c_dir = NULL,

    d.raw_dir = NULL,
    d.usable_dir = NULL,
    d.preped_dir = NULL,

    o.runs_dir = NULL,
    o.published_dir = NULL,

    s.R_dir = NULL,
    s.exe_dir = NULL,

    # -----------------------------
    # Timestamp directory for each run
    # -----------------------------
    active_run_dir = NULL,
    active_run_info_dir = NULL,

    # -----------------------------
    # Run state
    # -----------------------------
    start_time = NULL,
    end_time = NULL,
    runtime = NULL,

    config = NULL,
    cli_flags = NULL,
    cli_args = NULL,
    runname = NULL,

    log_messages = character(),
    warnings_log = character(),
    error_message = NULL,

    # -----------------------------
    # git status
    # -----------------------------
    git_commit = "Not a git repository",
    git_dirty  = "Not a git repository",

    # -----------------------------
    # R environment snapshot
    # -----------------------------
    snapshot = NULL,

    # -----------------------------
    # R environment snapshot
    # -----------------------------
    libraries = NULL,
    scrpaths = NULL,


    makedir = function(...) {
        dir <- file.path(...)
        if (!dir.exists(dir)) { dir.create(dir, recursive=TRUE) }
        return(dir)
    },

    get_all_cliargs = function(args) {
        flags <- names(private$cli_flags)
        res <- vector("list", length(flags))
        names(res) <- flags
        for (i in seq_along(flags)) { 
            f <- flags[i]
            parsed_args <- self$parse_cliarg(args, f) 
            res[[f]] <- self$validate_input(
               private$cli_flags[[f]], parsed_args
            )
        }

        return(res)
    },

    write_ending_log = function(){
      log <- list(
        cli_args = private$cli_args,
        start_time = as.character(private$start_time),
        end_time = as.character(private$end_time),
        runtime = list(
          user = private$runtime["user.self"],
          system = private$runtime["sys.self"],
          elapsed = private$runtime["elapsed"]
        ),
        package_imports = as.list(private$libraries),
        sourced_scripts = as.list(private$scrpaths),
        git_status = list(
          commit = private$git_commit,
          dirty = private$git_dirty
        ),
        logs = as.list(private$log_messages),
        warnings = as.list(private$warnings_log),
        error = private$error_message
      )

      yaml::write_yaml(
        log, self$actruninfo_path("ending_log.yaml")
      )
    },

    finalize_run = function(){
        self$logmsg(paste0("RunManager Begin Time: ", as.character(private$start_time))) 
        self$logmsg(paste0("RunManager End   Time: ", as.character(private$end_time)))
        self$logmsg("RunManager executed sucessfully.") 
    }
  )

)
