FROM 060159139582.dkr.ecr.eu-west-3.amazonaws.com/docker_supplai_base:supplai_base_image

# install home made packages
RUN Rscript -e 'remotes::install_github("Aqvayli06/SaldaeForecasting", dependencies = FALSE)'

RUN Rscript -e 'remotes::install_github("Aqvayli06/SaldaeDataExplorer", dependencies = TRUE,upgrade="never")'

# install the package that contains the App
RUN R -e 'remotes::install_github("Aqvayli06/SupplAI_chain", dependencies = TRUE, upgrade = "never")'
RUN R -e 'library(SupplAI)'
#   copy the remaining elements and set wordking directory
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone



RUN rm -rf /build_zone

# set host and port
COPY Rprofile.site /usr/local/lib/R/etc/

# expose to a specific PORT
EXPOSE 3838
# call main app
CMD  ["R", "-q","-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');SupplAI::run_app()"]
