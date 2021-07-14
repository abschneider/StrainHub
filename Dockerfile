# FROM rocker/shiny-verse:latest
FROM rocker/shiny-verse:4.0.0

RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libglpk40 \
    libudunits2-dev \
    libproj-dev \
    libgdal-dev \
    libgeos-dev \
    libnode-dev

## Install R libraries
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('remotes', repos='http://cran.rstudio.com/')"

RUN R -e "install.packages(c('ade4',  'adegenet',  'ape',  'castor', \
          'colourpicker',  'data.table',  'dplyr',  'DT', 'geosphere', \
          'ggplot2', 'ggtree',  'ggtree',  'globe4r',  'hashmap',  'htmltools', \
          'htmlwidgets',  'igraph', 'import',  'knitr',  'leaflet', \
          'magrittr',  'markdown',  'network', 'phangorn', 'plotly',  'plyr',  \
          'randomcoloR',  'rbokeh',  'readr',  'rhandsontable',  \
          'rmarkdown',  'shiny',  'shinycssloaders',  'shinyjqui',  \
          'shinythemes',  'shinyWidgets',  'stringr',  'tibble',  \
          'treeio',  'visNetwork',  'webshot'))"

RUN R -e "remotes::install_github('YuLab-SMU/treeio')"
RUN R -e "remotes::install_github('YuLab-SMU/ggtree')"
RUN R -e "remotes::install_github('nathan-russell/hashmap')"
# RUN R -e "remotes::install_github('rstudio/httpuv')"
# RUN R -e "remotes::install_github('colbyford/strainhub', subdir='pkg', dependencies=TRUE)"
RUN R -e "remotes::install_github('colbyford/strainhub', subdir='pkg', dependencies=FALSE)"

## Copy StrainHub application files
# COPY /app/strainhub.Rproj /srv/shiny-server/
# COPY /app/app.R /srv/shiny-server/
COPY /app /srv/shiny-server/
COPY /data /srv/shiny-server/data

## Open the port for Shiny
EXPOSE 3838

RUN sudo chown -R shiny:shiny /srv/shiny-server
# RUN sudo chown -R shiny:shiny /root/

## Fix PNG export functionality with webshot
RUN R -e "webshot::install_phantomjs()"

## Start Shiny Server
# CMD ["/usr/bin/shiny-server.sh"]
# CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 3838)"]

# #### Enable SSH
# ## Open port 2222 for SSH access
# EXPOSE 80 2222 22

# ## Install OpenSSH and set the password for root to "Docker!". In this example, "apk add" is the install instruction for an Alpine Linux-based image.
# RUN apt install openssh-server sudo -y \
#     && echo "root:Docker!" | chpasswd 

# ## Copy the sshd_config file to the /etc/ssh/ directory
# COPY sshd_config /etc/ssh/

# ## Start the SSH service
# RUN service ssh start
# # CMD ["/usr/sbin/sshd", "-D"]