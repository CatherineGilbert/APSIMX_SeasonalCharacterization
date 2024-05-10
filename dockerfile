# Use the official R Shiny server image as the base
FROM rocker/shiny:latest

# Install required system libraries
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    wget \
    tar \
    && rm -rf /var/lib/apt/lists/*

# Install APSIM-X
RUN wget -qO- https://github.com/APSIMInitiative/APSIMClassic/releases/download/apsim-release-X.Y.Z/apsim-X.Y.Z-Linux.tar.gz | tar xvz -C /usr/local/bin
ENV APSIM_PATH=/usr/local/bin/apsim-X.Y.Z
ENV PATH=$APSIM_PATH:$PATH

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'DT', 'readr', 'dplyr', 'pheatmap', 'apsimx', 'tidyverse', 'daymetr', 'data.table', 'RColorBrewer'), repos='http://cran.rstudio.com/')"

# Copy your Shiny app into the Docker image
COPY app.R /srv/shiny-server/

# Make the Shiny server port available to the outside world
EXPOSE 3838

# Run the Shiny server
CMD ["/usr/bin/shiny-server"]
