#trying to solve the problem with stan
Sys.getenv("BINPREF")
Sys.which("make")
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
install.packages("jsonlite", type = "source")
library(jsonlite)
?.Rprofile

remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")
Sys.setenv(MAKEFLAGS = "-j4") # four cores used
install.packages("rstan", type = "source")

remove.packages("rstan")
install.packages("rstan",dependencies = FALSE)
library(rstan)
set_cppo("fast")

remove.packages("StanHeaders")
remove.packages("rstan")

install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

library(cmdstanr)
install_cmdstan()
set_cmdstan_path()


Sys.getenv('PATH')
Sys.which("gcc.exe")

install.packages("devtools")
devtools::find_rtools()

install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::check_cmdstan_toolchain(fix = TRUE)
cmdstanr::install_cmdstan()

fit <- cmdstanr::cmdstanr_example()
fit

find_rtools()


remove.packages(c("rstan", "StanHeaders"))

install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

Sys.which("make")
remove.packages("jsonlite")
install.packages("jsonlite", type = "source")

remove.packages(c("rstan", "StanHeaders"))

install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

Sys.setenv(BINPREF = "C:/Rtools/mingw_$(WIN)/bin/")

Sys.setenv(MAKEFLAGS = paste0("-j",parallel::detectCores()))

install.packages(c("StanHeaders","rstan"),type="source")
readLines("~/.Rprofile")
file.path(Sys.getenv("HOME"), ".Rprofile")
Sys.getenv("BINPREF")

install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
