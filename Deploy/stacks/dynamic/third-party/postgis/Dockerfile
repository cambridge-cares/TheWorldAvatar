# Overridden with postgis image that has pgrouting added as one of several bundled extensions
FROM imresamu/postgis:15-3.4-bundle-bookworm AS pgrouting_extra

# Copied from https://github.com/pgRouting/docker-pgrouting/blob/dd6672dbe7e6a413a67b8a592f458067771d697d/15-3.3-3.4/extra/Dockerfile
ENV OSM2PGROUTING_VERSION 2.3.8

RUN apt update \
 && apt install -y \
        libpqxx-6.4 \
 && apt install -y \
        build-essential \
        cmake \
        wget \
        libboost-program-options-dev \
        libexpat1 \
        libexpat-dev \
        libosmium2-dev \
        libpqxx-dev \
        zlib1g-dev \
 && cd /usr/local/src \
 && wget https://github.com/pgRouting/osm2pgrouting/archive/refs/tags/v${OSM2PGROUTING_VERSION}.tar.gz \
 && tar xvf v${OSM2PGROUTING_VERSION}.tar.gz \
 && cd osm2pgrouting-${OSM2PGROUTING_VERSION} \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make \
 && make install \
 && cd ../tools/osmium/ \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make \
 && make install \
 && cd /usr/local/src \
 && rm -rf ./* \
 && apt purge -y --autoremove \
        build-essential \
        cmake \
        wget \
        libexpat-dev \
        libosmium2-dev \
        libpqxx-dev \
        zlib1g-dev \
 && apt autoremove -y \
 && rm -rf /var/lib/apt/lists/*
 
# Copied from https://github.com/3dcitydb/3dcitydb/blob/c7ca7df2f953e7b4a4a1d1b88f3fdceeb9d3247e/postgresql/Dockerfile
# 3DCityDB PostGIS Dockerfile #################################################
#   Official website    https://www.3dcitydb.net
#   GitHub              https://github.com/3dcitydb
###############################################################################

# Base image
FROM pgrouting_extra

# Set 3DCityDB version
ARG CITYDB_VERSION='4.4.0'
ENV CITYDBVERSION=${CITYDB_VERSION}

# Copy SQL scripts
WORKDIR /3dcitydb
COPY SQLScripts .
COPY DockerScripts/3dcitydb-initdb.sh /docker-entrypoint-initdb.d/

# Set labels
LABEL maintainer="Bruno Willenborg"
LABEL maintainer.email="b.willenborg(at)tum.de"
LABEL maintainer.organization="Chair of Geoinformatics, Technical University of Munich (TUM)"
LABEL source.repo="https://github.com/3dcitydb/3dcitydb"
