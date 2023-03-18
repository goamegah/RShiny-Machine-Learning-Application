FROM rocker/r-base:4.2.2
RUN apt-get update
RUN apt-get install -y --no-install-recommends \
    apt-transport-https \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    default-jdk \
    && rm -rf /var/lib/apt/lists/*
## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean
RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
RUN addgroup --system app && adduser --system --ingroup app app
WORKDIR /home/app
COPY . .
RUN chown app:app -R /home/app
RUN chown app:app -R /usr/local/lib/R/site-library
RUN chmod 755 /usr/local/lib/R/site-library
RUN Rscript -e 'install.packages("shiny",repos="http://cran.us.r-project.org")'  && \
    Rscript -e 'install.packages("shinydashboard",repos="http://cran.us.r-project.org")'  && \
    Rscript -e 'install.packages("DT",repos="http://cran.us.r-project.org")'  && \
    Rscript -e 'install.packages("ggplot2",repos="http://cran.us.r-project.org")'  && \
    Rscript -e 'install.packages("dplyr",repos="http://cran.us.r-project.org")'  && \
    Rscript -e 'install.packages("treemapify",repos="http://cran.us.r-project.org")'  && \
    Rscript -e 'install.packages("purrr",repos="http://cran.us.r-project.org")'  && \
    Rscript -e 'install.packages("rpart",repos="http://cran.us.r-project.org")'  && \
    Rscript -e 'install.packages("ISLR",repos="http://cran.us.r-project.org")'  && \
    Rscript -e 'install.packages("rpart.plot",repos="http://cran.us.r-project.org")'  && \
    Rscript -e 'install.packages("party",repos="http://cran.us.r-project.org")'  && \
    Rscript -e 'install.packages("fastDummies",repos="http://cran.us.r-project.org")'  && \
    Rscript -e 'install.packages("pROC",repos="http://cran.us.r-project.org")'  && \
    Rscript -e 'install.packages("comprehenr",repos="http://cran.us.r-project.org")'  && \
    Rscript -e 'install.packages("ROSE",repos="http://cran.us.r-project.org")'  && \
    Rscript -e 'install.packages("xlsx",repos="http://cran.us.r-project.org")'  && \
    Rscript -e 'install.packages("readxl",repos="http://cran.us.r-project.org")'

USER app
EXPOSE 3838
CMD Rscript runApp.R 
