# -*- Mode:R; Coding:us-ascii-unix; fill-column:158 -*-

q <- function() {
  quit('no')
}

.First <- function() {
  # Set to FALSE to supress loading 3rd party packages
  mjrLoad3rdpkg <- TRUE

  # Firtst we figure out the home directory we want to use (.Rhistory and package locations)
  for(potHome in c(paste('c:/Users', Sys.info()[["user"]], 'Documents', sep='/'),
                   paste('/Users',   Sys.info()[["user"]],              sep='/'),
                   paste('/home',    Sys.info()[["user"]],              sep='/'),
                   normalizePath('~')))
    if(file.exists(potHome)) {
      mjrHomePath <- potHome
      break
    }
  rm(potHome)   

  if(is.null(mjrHomePath))
    warning("Rprofile: Could not figure out home directory.\n\n")

  # In macOS, we don't always get a good enviornment, so we pull the path here
  if( !(is.na(charmatch('darwin', version$os))))
    Sys.setenv(PATH=system("~/bin/setEnvAndRun.sh /bin/bash -c 'echo $PATH'", ignore.stderr=TRUE, intern=TRUE))

  ## Load up standard libraries
  library(stats)
  library(grid)
  library(lattice)
  library(utils)

  ########################################################################################################################
  ## Construct names for opkgPath -- "others" packages.  On WIndows using Microsoft R Open, we use the default personal path
  opkgPath <- NULL
  if( !(is.null(mjrHomePath)) && file.exists(mjrHomePath) ) {
    if(.Platform$OS.type=="windows") {
      opkgPath <- paste(mjrHomePath,
                        'R',
                        'win-library',
                        gsub('[^a-zA-Z0-9/_.-]+', '_', paste(R.version$platform, paste(version$major, version$minor, sep='.'), sep='-')),
                        'opkgs',
                        sep='/')
    } else {
      for(f in c('/etc/redhat-release',
                 '/etc/SuSE-release',
                 '/etc/debian_version',
                 '/System/Library/CoreServices/SystemVersion.plist')) {
        if(file.exists(f)) {
          con<-file(f, 'r')
          opkgPath <- readChar(con, 5000)
          for(re in list(#c("\n[^\n]*http://.*\n", ''),
                       c('<key>[^<]+</key>', ''),
                       c('</{0,1}(key|string|dict|plist)[^>]*>', ''),
                       c('<?xml[^>]*>', ''),
                       c('<?DOCTYPE[^>]*>', ''),
                       c('[^a-zA-Z0-9]', ''),
                       c('PRETTYNAME', 'pn'),
                       c('Enterprise', 'E'),
                       c('RedHat', 'RH'),
                       c('Linux', 'L'),
                       c('Server', 'S'),
                       c('VERSION', 'Ver'),
                       c('PATCHLEVEL', 'Patch'),
                       c('Workstation', 'W'))) {
          opkgPath <- gsub(re[1], re[2], opkgPath)
        }
        close(con)
        opkgPath <- paste(gsub('[_-].*$', '', gsub('^.*/', '', f)), opkgPath, sep='.')
      }
    }
    opkgPath <- paste(version$major, version$minor, version$arch, opkgPath, sep='.')
    opkgPath <- paste(mjrHomePath, 'R/opkgs', opkgPath, sep='/')
  }

  if( !(is.null(opkgPath))) {
    #print(paste("OPKGS: ", opkgPath))
    if( !(file.exists(opkgPath))) {
      warning("Rprofile: Missing package path: ", opkgPath, "\n\n")
      dir.create(opkgPath, recursive=TRUE, mode = "0700")
    }
    .libPaths(c(opkgPath))
    }
  }
  
  ########################################################################################################################
  ## Setup package paths & figure out what we have

  pAval <- .packages(all.available=TRUE)
  
  ########################################################################################################################
  ## Load up my personal libraries & non-standard, 3rd party libraries

  for(p in c('mjrstdlib', 'mjrPlots', 'mjrEvents'))
    if(p %in% pAval)
      suppressPackageStartupMessages(library(p, character.only=TRUE))
  
  pMiss <- NULL
  for(p in c('knitr',
             'jsonlite',
             'gridExtra',
             'data.table',
             'dplyr',
             'tidyr',
             'ggplot2',
             'scales',
             'gridExtra',
             'usmap',
             #'reshape2',
             #'maps'
             'RColorBrewer'
             ))
    if(mjrLoad3rdpkg && (p %in% pAval))
      suppressPackageStartupMessages(library(p, character.only=TRUE))
    else
      pMiss <- append(pMiss, p)
  
  if(length(pMiss)>0) {
    warning("Rprofile: Missing Favorite packages: ", paste(pMiss, collapse=','), "\n\n")
    cat(paste("Install missing packages with: \n\n",
              '   install.packages(',
              paste(paste('c(', paste('"', pMiss, '"', sep='', collapse=', '), ')', sep=''),
                    paste('lib="', opkgPath, '"', sep=''),
                    ifelse(.Platform$OS.type=="windows", 'method="wininet"', 'method="wget"'), # Diffrent methods for windows vs non-windows
                    'dependencies=TRUE',
                    'repos="https://cran.revolutionanalytics.com/"',
                    sep=', '),
              ")\n\n",
              sep=''))
  }
  
  ## Options
  options(width=200)     # We will almost always have a big window!
  options(prompt="R> ")  # Put 'R' in prompt--so we don't confuse R sessions with LISP ones :) 
  options(printcmd="lp") # How to print

  if("package:dplyr" %in% search())
    options(dplyr.print_max = 1e9)

  if("package:data.table" %in% search()) {
    options(datatable.print.topn  = 250)
    #options(datatable.print.nrows = 500)
    options(datatable.print.nrows = 1e9)
  }

  ## On windows add Rtools to path for R versions 4.0.0 and newer
  if(.Platform$OS.type=="windows") {
    if(R.Version()$major >= 4) {
      RTP <- 'c:/rtools40/usr/bin/'
      if(Sys.getenv('RTOOLS40_HOME') != '') {
        RTP <- paste(Sys.getenv('RTOOLS40_HOME'), 'usr', 'bin', sep=.Platform$file.sep)
      }
      if(dir.exists(RTP)) {
        Sys.setenv(PATH=paste(RTP, Sys.getenv("PATH"), sep=.Platform$path.sep))
      } else {
      warning("Rprofile: Unable to find RTOOLS 4 on Windows")
      }
      rm(RTP)
    }
  }
  
}

.Last <- function() {
}
