FROM rocker/verse as test
LABEL Name=autoggraph Version=0.0.1 

RUN apt-get update && apt-get install -y --no-install-recommends \
  build-essential \
  chrpath \
  libssl-dev \
  libxft-dev \
  libfreetype6 \
  libfreetype6-dev \
  libfontconfig1 \
  libfontconfig1-dev \
  wget && \
  cd ~ && \
  export PHANTOM_JS="phantomjs-2.1.1-linux-x86_64" && \
  wget https://bitbucket.org/ariya/phantomjs/downloads/$PHANTOM_JS.tar.bz2 && \
  mv $PHANTOM_JS.tar.bz2 /usr/local/share/ && \
  cd /usr/local/share/ && \
  tar xvjf $PHANTOM_JS.tar.bz2 && \
  ln -sf /usr/local/share/$PHANTOM_JS/bin/phantomjs /usr/local/share/phantomjs && \
  ln -sf /usr/local/share/$PHANTOM_JS/bin/phantomjs /usr/local/bin/phantomjs && \
  ln -sf /usr/local/share/$PHANTOM_JS/bin/phantomjs /usr/bin/phantomjs && \
  phantomjs --version

COPY . /autoggraph
WORKDIR /autoggraph

RUN install2.r --error \
  -r 'https://cran.rstudio.com' \
  shiny shinyjs shinytest haven rhandsontable

FROM rocker/shiny as deploy

COPY --from=test /autoggraph /srv/shiny-server/autoggraph

RUN  R -e "install.packages(c('readr', 'readxl', 'ggplot2', 'stringr', 'RColorBrewer', 'shinyjs', 'magrittr', 'haven', 'rhandsontable', 'purrr', 'dplyr', 'tidyr'), \
  repos='https://cran.rstudio.com/')"

EXPOSE 3838

CMD ["/usr/bin/shiny-server.sh"]
