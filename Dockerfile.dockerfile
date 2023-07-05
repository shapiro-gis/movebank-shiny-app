FROM rocker/shiny
#FROM r-base:3.6.2


## Install packages from CRAN
#r-base:3.6.2
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    gdal-bin \ 
    libgdal-dev \
    libproj-dev \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libgmp3-dev \
    libmpfr-dev \
    libgeos-dev \
    libsqlite3-dev \
    libgsl0-dev \
    zlib1g-dev \
    libudunits2-dev 
    
# install R packages required 
RUN R -e "install.packages('rgdal', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinycssloaders', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('base', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('waiter', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('mapdeck', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leaflet', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('geosphere', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('move', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tibble', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dplyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('adehabitatLT', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('adehabitatHR', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('htmltools', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinythemes', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyBS', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('mapboxer', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sf', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidyverse', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyvalidate', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('lubridate', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('data.table', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('httr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('purrr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('RSQLite', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('raster', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('terra', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"


COPY . /srv/shiny-server/

COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

COPY app.R /srv/shiny-server/app.R

#COPY style.css /srv/shiny-server/style.css

#COPY _auth0.yml /srv/shiny-server/_auth0.yml

#COPY www/ /srv/shiny-server/www/

# Allow permissions
RUN chown -R shiny:shiny /srv/shiny-server
RUN chown -R shiny:shiny /var/log/shiny-server


EXPOSE 8080

#USER shiny

# avoid s6 initialization
# see https://github.com/rocker-org/shiny/issues/79
CMD ["/usr/bin/shiny-server"]