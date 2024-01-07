FROM rocker/r-ver:4.1.0
RUN apt-get update && apt-get install -y  cmake gdal-bin git-core libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libicu-dev libpng-dev libproj-dev libssl-dev libudunits2-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("RColorBrewer",upgrade="never", version = "1.1-3")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.3.6")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.2.4")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.1")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.2")'
RUN Rscript -e 'remotes::install_version("htmlwidgets",upgrade="never", version = "1.5.4")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.9")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.10.0")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.22")'
RUN Rscript -e 'remotes::install_version("ggpubr",upgrade="never", version = "0.4.0")'
RUN Rscript -e 'remotes::install_version("markdown",upgrade="never", version = "1.1")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.8.0")'
RUN Rscript -e 'remotes::install_version("ggVennDiagram",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("shinymanager",upgrade="never", version = "1.0.400")'
RUN Rscript -e 'remotes::install_version("pROC",upgrade="never", version = "1.18.0")'
RUN Rscript -e 'remotes::install_version("vroom",upgrade="never", version = "1.5.3")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.2")'
RUN Rscript -e 'remotes::install_github("tpbilton/GUSbase", ref = "92119b9c57faa7abeede8236d24a4a8e85fb3df7")'
RUN Rscript -e 'remotes::install_github("tpbilton/GUSMap@4d7d4057049819d045750d760a45976c8f30dac6")'
RUN Rscript -e 'remotes::install_github("Cristianetaniguti/onemap@7f5ac29d65d0bd82d9e46fcc2a26e3fc904a0782")'
RUN Rscript -e 'remotes::install_github("Yuchun-Zhang/R_largeList", ref = "v0.3.1", subdir = "largeList")'

RUN apt-get update && apt-get install -y libtcl libtk

RUN Rscript -e 'remotes::install_github("mmollina/mappoly")'
RUN Rscript -e 'remotes::install_github("Cristianetaniguti/Reads2MapApp"  )'

EXPOSE 80
CMD  ["R", "-e", "options('shiny.port'=80,shiny.host='0.0.0.0');Reads2MapApp::run_app()"]

