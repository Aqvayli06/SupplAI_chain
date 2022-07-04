FROM 060159139582.dkr.ecr.eu-west-3.amazonaws.com/docker_supplai_base:supplai_base_image

# install home made packages
RUN Rscript -e 'remotes::install_github("Aqvayli06/SaldaeForecasting")'

#   copy the remaining elements and set wordking directory
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone

# expose to a specific PORT
EXPOSE 3838
# call main app
CMD  ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');SupplAI::run_app()"]
