FROM ubuntu:16.04

RUN apt update
RUN apt install -y git

RUN git clone https://github.com/wdebeaum/cogent.git

RUN apt install -y sbcl

RUN apt install -y wget
WORKDIR /usr/local/share/wordnet
RUN wget -nv http://wordnetcode.princeton.edu/3.0/WordNet-3.0.tar.bz2
RUN wget -nv http://wordnetcode.princeton.edu/glosstag-files/WordNet-3.0-glosstag.tar.bz2
RUN ls
RUN apt install -y bzip2
RUN tar -xjf WordNet-3.0.tar.bz2
RUN tar -xjf WordNet-3.0-glosstag.tar.bz2

RUN apt install -y unzip
WORKDIR /usr/local/share/stanford-corenlp/
RUN wget -nv http://nlp.stanford.edu/software/stanford-corenlp-full-2016-10-31.zip
RUN unzip stanford-corenlp-full-2016-10-31.zip

RUN apt install -y openjdk-8-jdk perl
WORKDIR /cogent/src/

RUN apt install -y build-essential
# Set the locale
RUN apt install -y locales locales-all
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8   

RUN cpan DBI
RUN apt -y install sqlite3 libsqlite3-dev
RUN cpan DBD::SQLite

RUN apt-get install -y aspell aspell-en
RUN apt-get install -y xsltproc

RUN ./configure --with-lisp=sbcl
RUN make
RUN make install

CMD ['/cogent/bin/trips-cogent', '-nochat', '-nolisp']
