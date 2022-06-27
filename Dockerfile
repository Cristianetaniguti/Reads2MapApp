FROM rocker/shiny-verse

RUN apt-get update
RUN apt-get install -y libudunits2-dev libproj-dev libgdal-dev 

RUN Rscript -e 'remotes::install_github("tpbilton/GUSbase", ref = "92119b9c57faa7abeede8236d24a4a8e85fb3df7")'
RUN Rscript -e 'remotes::install_github("tpbilton/GUSMap", ref = "4d7d4057049819d045750d760a45976c8f30dac6")'
RUN Rscript -e 'remotes::install_github("Cristianetaniguti/onemap")'

RUN Rscript -e 'remotes::install_github("Cristianetaniguti/Reads2MapApp", auth_token = "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")'

COPY ./ /tmp/app/

RUN R -e 'remotes::install_local("/tmp/app", dependencies = "Imports")'

EXPOSE 80/tcp
RUN rm /srv/shiny-server/index.html
COPY ./inst/app /srv/shiny-server/
COPY ./inst/app/shiny-server.conf /etc/shiny-server/shiny-server.conf

CMD ["/usr/bin/shiny-server"]
