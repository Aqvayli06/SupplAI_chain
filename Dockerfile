FROM rocker/r-ver:4.2.0
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libicu-dev libsecret-1-dev libsodium-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc unixodbc-dev zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "1.0.2")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.6.2")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.6.0")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.9")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.2")'
RUN Rscript -e 'remotes::install_version("jsonlite",upgrade="never", version = "1.8.0")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.1")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("waiter",upgrade="never", version = "0.2.5")'
RUN Rscript -e 'remotes::install_version("RSQLite",upgrade="never", version = "2.2.14")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.23")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.4")'
RUN Rscript -e 'remotes::install_version("janitor",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.7.0")'
RUN Rscript -e 'remotes::install_version("bs4Dash",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinyalert",upgrade="never", version = "3.0.0")'
RUN Rscript -e 'remotes::install_version("odbc",upgrade="never", version = "1.3.3")'
RUN Rscript -e 'remotes::install_version("keyring",upgrade="never", version = "1.3.0")'
RUN Rscript -e 'remotes::install_version("shinymanager",upgrade="never", version = "1.0.400")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.2")'
# package dependencies for SaldaeForecasting engine

RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "0.3.4")'
RUN Rscript -e 'remotes::install_version("crosstalk",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("dtwclust",upgrade="never", version = "5.5.10")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.10.0")'
RUN Rscript -e 'remotes::install_version("prophet",upgrade="never", version = "1.0")'
RUN Rscript -e 'remotes::install_github("jcheng5/d3scatter@aba6687f7af974a55ee07b52bea0ccbf110623e8")'

# RUN Rscript -e 'remotes::install_github("Aqvayli06/SaldaeForecasting")'
# end of package dependencies installation
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone

# RUN R -e 'remotes::install_local(upgrade="never")'
# RUN rm -rf /build_zone
# EXPOSE 3838
# CMD  ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');SupplAI::run_app()"]
