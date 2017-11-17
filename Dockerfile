FROM rocker/verse
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

COPY . /home/rstudio/autoggraph
WORKDIR /home/rstudio/autoggraph

RUN R -e 'if (system.file(package="packrat") == "") install.packages("packrat")'
RUN R -e "packrat::packify(); packrat::restore()"