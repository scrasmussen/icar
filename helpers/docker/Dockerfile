FROM ubuntu:latest
USER root

# WRF-Hydro dependencies
RUN apt-get update \
    && apt-get install -yq --no-install-recommends \
    bzip2 \
    ca-certificates \
    cmake \
    curl \
    emacs-nox \
    g++ \
    gfortran \
    git \
    less \
    libcoarrays-openmpi-dev \
    libcurl4-gnutls-dev \
    libfftw3-dev \
    libnetcdff-dev \
    libnetcdff7 \
    libopenmpi-dev \
    make \
    nano \
    netcdf-bin \
    openmpi-bin \
    python3 \
    python3-pandas \
    python3-netcdf4 \
    python3-xarray \
    wget \
    vim \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# install nccmp manually instead of with conda to save space
ENV NCCMP=nccmp-1.9.1.0
WORKDIR /tmp
RUN wget -q https://gitlab.com/remikz/nccmp/-/archive/1.9.1.0/${NCCMP}.tar.gz \
    && tar zxf ${NCCMP}.tar.gz \
    && cmake -B ${NCCMP}/build -S ${NCCMP} \
    && make -C ${NCCMP}/build -j install

# cleanup
RUN rm -rf /tmp/*

# setup environment
ENV FFTW_DIR=/usr/lib/x86_64-linux-gnu/
ENV FFTW_INC=/usr/include
ENV NETCDF=/usr
ENV CAF_DIR=/usr/lib/x86_64-linux-gnu/open-coarrays/openmpi/lib/
ENV FC=gfortran

# create docker user
RUN useradd -ms /bin/bash docker \
    && usermod -aG sudo docker \
    && chmod -R 777 /home/docker/

USER docker
WORKDIR /home/docker
