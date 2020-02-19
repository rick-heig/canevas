FROM ubuntu:18.04

# This is a Dockerfile for running Canevas : https://github.com/rick-heig/canevas
MAINTAINER Rick Wertenbroek <rick.wertenbroek@heig-vd.ch>

ARG OPENJDK_VERSION=11

# Install required software and clean as not to make the layer dirty
RUN apt-get update && apt-get -y upgrade && apt-get install -y \
	apt-utils curl gnupg && \
	apt-get clean && apt-get purge && \
	rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
        
# Add SBT package to manager
RUN echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add

# Install required software and clean as not to make the layer dirty
RUN apt-get update && apt-get -y upgrade && apt-get install -y \
	wget xvfb software-properties-common default-jre unzip glib-networking-common \
        sbt openjdk-$OPENJDK_VERSION-jdk && \
	apt-get clean && apt-get purge && \
	rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Install Canevas

# Add github key so git clone does not complain
RUN mkdir /root/.ssh && \
    ssh-keyscan github.com >> /root/.ssh/known_hosts

# Clone Source Code Repository and build fat jar plus wrapper execution script
RUN mkdir -p /usr/src/ && \
    cd /usr/src/ && \
    git clone https://github.com/rick-heig/canevas && \
    cd /usr/src/canevas/canevas/ && \
    sbt assembly && \
    cp target/scala*/Canevas*.jar /usr/local/ && \
    echo '#!/bin/bash' > canevas && \
    echo 'java -jar /usr/local/Canevas*.jar "$@"' >> canevas && \
    chmod +x canevas && \
    cp canevas /usr/local/bin/ && \
    cd /usr/src/ && \
    rm -r canevas

# Work in this temporary directory
WORKDIR /tmp/work

CMD canevas

# Possible run command :