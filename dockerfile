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

# Copy APSIM-X files to the Docker image
COPY data.tar.gz /tmp/
COPY control.tar.gz /tmp/
COPY debian-binary /tmp/

# Extract APSIM-X files
RUN mkdir -p /usr/local/apsimx && \
    tar -xzf /tmp/data.tar.gz -C /usr/local/apsimx && \
    tar -xzf /tmp/control.tar.gz -C /usr/local/apsimx && \
    rm /tmp/data.tar.gz /tmp/control.tar.gz /tmp/debian-binary

ENV APSIM_PATH=/usr/local/apsimx
ENV PATH=$APSIM_PATH:$PATH

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'DT', 'readr', 'dplyr', 'pheatmap', 'apsimx', 'tidyverse', 'daymetr', 'data.table', 'RColorBrewer', 'janitor', 'esquisse', 'tidyr'), repos='http://cran.rstudio.com/')"

# Copy your Shiny app and supporting files into the Docker image
COPY app.R /srv/shiny-server/
COPY apsimximproved.R /srv/shiny-server/
COPY template_models /srv/shiny-server/template_models/

# Ensure the output directories are created
RUN mkdir -p /srv/shiny-server/apsimx_output/output /srv/shiny-server/apsimx_output/met /srv/shiny-server/apsimx_output/soils /srv/shiny-server/apsimx_output/apsim

# Set working directory
WORKDIR /srv/shiny-server

# Make the Shiny server port available to the outside world
EXPOSE 3838

# Run the Shiny server
CMD ["/usr/bin/shiny-server"]
