FROM r-base:latest

RUN apt-get update 

RUN install.r remotes \
    && installGithub.r tsmodels/tsmethods