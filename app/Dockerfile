FROM rocker/r-base:latest
RUN apt-get update && apt-get install -y --no-install-recommends \
    apt-transport-https \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/* \
## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean
RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
RUN addgroup --system app \
    && adduser --system --ingroup app app
WORKDIR /home/app
COPY . .
RUN chown app:app -R /home/app
USER app
EXPOSE 3838
CMD Rscript runApp.R
