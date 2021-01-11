FROM rocker/shiny-verse:latest

RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev 

RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('devtools', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ade4', repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('colbyford/strainhub', subdir='pkg')"

COPY /app/strainhub.Rproj /srv/shiny-server/
COPY /app/app.R /srv/shiny-server/
COPY /data /srv/shiny-server/data

EXPOSE 3838

RUN sudo chown -R shiny:shiny /srv/shiny-server

CMD ["/usr/bin/shiny-server.sh"]