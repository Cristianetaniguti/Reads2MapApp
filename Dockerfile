FROM rocker/shiny-verse

RUN apt-get update && apt-get install -y  \
    	    	   libxml2-dev \
		   imagemagick \
		   libcurl4-openssl-dev \
		   libgl1-mesa-dev \
		   libglu1-mesa-dev \
		   libicu-dev \
		   libpng-dev \
		   libssl-dev \
		   make \
		   pandoc \
		   pandoc-citeproc \
		   zlib1g-dev

COPY ./ /tmp/app/

RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("shinymanager",upgrade="never", version = "1.0.300")'
RUN Rscript -e 'remotes::install_version("pROC",upgrade="never", version = "1.17.0.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("ggpubr",upgrade="never", version = "0.4.0")'
RUN Rscript -e 'remotes::install_version("largeList",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.1")'
RUN Rscript -e 'remotes::install_github("thomasp85/patchwork@79223d3002e7bd7e715a270685c6507d684b2622")'
RUN Rscript -e 'install.packages("ellipse")'
RUN Rscript -e 'remotes::install_github("tpbilton/GUSbase", ref = "92119b9c57faa7abeede8236d24a4a8e85fb3df7")'
RUN Rscript -e 'remotes::install_github("tpbilton/GUSMap", ref = "4d7d4057049819d045750d760a45976c8f30dac6")'
RUN Rscript -e 'remotes::install_github("Cristianetaniguti/onemap")'

RUN R -e 'remotes::install_local("/tmp/app")'
EXPOSE 80/tcp
RUN rm /srv/shiny-server/index.html
COPY ./inst/app /srv/shiny-server/
COPY ./inst/app/shiny-server.conf /etc/shiny-server/shiny-server.conf

CMD ["/usr/bin/shiny-server"]
