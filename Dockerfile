FROM 060159139582.dkr.ecr.eu-west-3.amazonaws.com/docker_supplai_base:supplai_base_image

ENV ENV_MONGODB_API_KEY1 = $MONGODB_API_KEY1
# install home made packages

RUN Rscript -e 'print(paste("ENV VAR ist" ,Sys.getenv("ENV_MONGODB_API_KEY1")))'

RUN Rscript -e 'remotes::install_github("Aqvayli06/SaldaeForecasting", dependencies = FALSE)'

#RUN Rscript -e 'remotes::install_github("Aqvayli06/SaldaeDataExplorer", dependencies = FALSE)'

# install the package that contains the App
RUN Rscript -e 'remotes::install_github("Aqvayli06/SupplAI_chain", dependencies = TRUE, upgrade = "never")'

RUN Rscript -e 'library("SupplAI")' # needed as a test and to load pipe operator

#   copy the remaining elements and set wordking directory
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone



RUN rm -rf /build_zone

# enable write in docker container
RUN chmod -R 777 ./

# set host and port
COPY Rprofile.site /usr/local/lib/R/etc/

# expose to a specific PORT
EXPOSE 3838
# call main app
CMD  ["R", "-q","-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');SupplAI::run_app()"]
