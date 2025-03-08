# install.packages(c("summarytools", "git2r", "dplyr", "pipeR", "forcats"))

rm(list = ls())

(root_dir  <- "~/GitHub/st-tests/")
(ref_dir   <- paste0(root_dir, "ref/global/"))
(st_branch <- git2r::repository_head("~/GitHub/summarytools/")$name)
(r_vers    <- as.character(getRversion()))

setwd(root_dir)

if (file.exists(paste0(root_dir, "test_log.Rdata"))) {
  load(file = paste0(root_dir, "test_log.Rdata"))
} else {
  test_log <- data.frame(date = lubridate::today(),
                         datetime = lubridate::now(),
                         st_branch = "",
                         output_dir = "", 
                         ref_dir = "",
                         r_vers = "",
                         stringsAsFactors = FALSE)[-1,]
}

# To use last saved directory, skip this step ------------------------------------
(output_dir <- paste0(root_dir, "output/", 
                   paste(format(Sys.time(), format = "%Y-%m-%d-%Hh%M"),
                         st_branch, "R", r_vers, sep = "-")))
test_log[nrow(test_log) + 1,] <- list(lubridate::today(),
                                      lubridate::now(),
                                      st_branch,
                                      output_dir, 
                                      ref_dir,
                                      r_vers,
                                      0)

save(test_log, file = paste0(root_dir, "test_log.Rdata"))

#test_log <- data.frame(date = lubridate::today(), dir = output_dir, ref = ref_dir, stringsAsFactors = FALSE)
# end skip -----------------------------------------------------------------------

(output_dir <- tail(test_log$output_dir, 1))
(dir.create(output_dir, recursive = TRUE, showWarnings = FALSE))
(testfiles <- grep(dir(paste0(root_dir, "/R")), 
                   pattern = "^\\d{2}b?\\-.+\\.R$",
                   perl = TRUE, value = TRUE)[-1])

cleanup <- function() {
  closeAllConnections()
  cat("\nSink has been stopped for results and messages\n")
}

l <- 1
f <- 1


Time <- list()
Lang <- c("en", "fr", "es", "pt", "tr", "ru")
Tot1 <- Sys.time()

# Following objects will not be deleted after each iteration; all others will.
base_objects <- c(ls(), "base_objects", "reset", "lang", "compare_dirs", "")
#summarytools::st_options(persist = FALSE)

for (l in 1:1) {
  lang <- Lang[l]
  for (f in 1:12) {
    options(width = 200)
    options(warn = 1)
    options(tibble.print_max = 200)
    options(tibble.width = 200)
    Time[[lang]] <- list()
    filename <- testfiles[f]
    (output_subdir <- paste(output_dir, lang, sub("\\.R", "", filename), sep = "/"))
    (dir.create(output_subdir, recursive = TRUE, showWarnings = FALSE))
    suppressWarnings(rm(tobacco, tabagisme, examens, exams, pr_number))
    suppressPackageStartupMessages(library(summarytools))
    st_options(lang = lang)
    if (lang == "ru" && Sys.info()[["sysname"]] == "Windows") {
      Sys.setlocale("LC_CTYPE", "russian")
    } else {
      Sys.setlocale(category = "LC_ALL", locale = "")
    }
    (setwd(output_subdir))
    (outfilename <- paste0(output_subdir, "/", 
                           sub("\\.R", "", filename), ".txt"))
    outfile <- file(outfilename, open = "wt")
    sink(outfile)
    sink(outfile, type = "message")
    t1 <- Sys.time()
    source(file = paste0(root_dir, '/R/', filename), local = FALSE, echo = TRUE,
           spaced = TRUE, prompt.echo = "> ", chdir = FALSE, encoding = "UTF-8",
           continue.echo = ">", max.deparse.length = 200, width.cutoff = 200, 
           keep.source = TRUE, print.eval = TRUE)
    t2 <- Sys.time()
    print(paste("Time elapsed for this script: ", format(t2 - t1)))
    print(paste("Time elapsed (total): ", format(t2 - Tot1)))
    sink(type = "message")
    sink()
    close(outfile)
    
    print(paste("Time elapsed for script", filename, ":", format(t2 - t1)))
    print(paste("Time elapsed (total): ", format(t2 - Tot1)))
    Time[[lang]][[f]] <- t2 - t1
    names(Time[[lang]][f]) <- filename
    
    fff <- file(outfilename, "r")
    content <- readLines(fff)
    close(fff)

    if (lang == "ru" && Sys.info()[["sysname"]] == "Windows") {
      content_utf8 <- iconv(content, "1251", "UTF-8")
    } else {
      content_utf8 <- enc2utf8(content) 
      #iconv(content, "1252", "UTF-8") #enc2utf8(content) 
    }

    content_utf8 <- sub("^> #(.+)$", "# ----------------- \\1", content_utf8)
    content_utf8 <- sub("^(Output file (written|appended)).+$", "\\1", 
                        content_utf8)
    fff <- file(outfilename, "w", encoding = "UTF-8")
    writeLines(content_utf8, fff)
    close(fff)
    
    if (lang == "ru" && Sys.info()[["sysname"]] == "Windows") {
      Sys.setlocale("LC_CTYPE", "")
    }
    
    rm(list = setdiff(ls(), base_objects))
    setwd(root_dir)
    suppressPackageStartupMessages(library(summarytools))
  }
  Tot2 <- Sys.time()
  print(paste("Time elapsed for language", lang, ":", format(Tot2 - Tot1)))
  Time[[lang]]$Total <- Tot2 - Tot1
}

compare_dirs <- function(lang) {
  ref_dir <- normalizePath(
    paste(ref_dir, lang, sep = "/"), 
    mustWork = FALSE)
  output_subdir <- normalizePath(paste(output_dir, lang, sep = "/"))
  if (!dir.exists(ref_dir)) {
    #dir.create(ref_dir, recursive = TRUE)
    return(paste("No ref files exist in", ref_dir))
  }
  if (Sys.info()[['sysname']] == "Linux") {
    system(paste0('meld "', ref_dir, '" "', output_subdir, '"'), wait = FALSE)
    library(summarytools)
    st_options(lang = "en")
  } else {
    cat(ref_dir)
    cat("\n")
    cat(output_subdir)
    #system(paste0('"C:\\Program Files\\Araxis\\Araxis Merge\\compare"', 
    system(paste0('"C:\\Program Files\\Araxis\\Araxis Merge\\compare.exe"', 
                  ' "', ref_dir, '" "', output_subdir, '"'))
  }
}

compare_dirs("en")
#compare_dirs("fr")
#compare_dirs("es")
#compare_dirs("pt")
#compare_dirs("tr")
#compare_dirs("ru")

