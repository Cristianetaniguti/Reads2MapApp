FROM rocker/r-ver:4.0.3
RUN apt-get update && apt-get install -y  libxml2-dev imagemagick libcurl4-openssl-dev libgl1-mesa-dev libglu1-mesa-dev libicu-dev libpng-dev libssl-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("RColorBrewer",upgrade="never", version = "1.1-2")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.3.2")'
RUN Rscript -e 'remotes::install_version("htmlwidgets",upgrade="never", version = "1.5.3")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.2")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.1.2")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.16")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.9.2.1")'
RUN Rscript -e 'remotes::install_version("shinymanager",upgrade="never", version = "1.0.300")'
RUN Rscript -e 'remotes::install_version("pROC",upgrade="never", version = "1.17.0.1")'
RUN Rscript -e 'remotes::install_version("vroom",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("ggpubr",upgrade="never", version = "0.4.0")'
RUN Rscript -e 'remotes::install_version("largeList",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.1")'
RUN Rscript -e 'remotes::install_github("thomasp85/patchwork@79223d3002e7bd7e715a270685c6507d684b2622")'
RUN Rscript -e 'remotes::install_github("tpbilton/GUSbase")'
RUN Rscript -e 'remotes::install_github("tpbilton/GUSMap")'
RUN Rscript -e 'remotes::install_github("Cristianetaniguti/onemap")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');Reads2MapApp::Reads2MapApp()"
