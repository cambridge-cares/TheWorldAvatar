# Use an official Ubuntu as a parent image
FROM ubuntu:20.04

# Set maintainer label
LABEL maintainer="ak2332@cam.ac.uk"

# Set environment variables to non-interactive (this prevents some prompts)
ENV DEBIAN_FRONTEND=non-interactive

# Update & install required packages for building DFTB+
RUN apt-get update && apt-get install -y \
    software-properties-common \
    wget \
    gcc \
    gfortran \
    git \
    cmake \
    python3-pip \
    libblas-dev \
    liblapack-dev \
    && rm -rf /var/lib/apt/lists/*

# Install miniconda for Conda environment
RUN wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O miniconda.sh && \
    bash miniconda.sh -b -p /miniconda && \
    rm miniconda.sh

# Set path to conda
ENV PATH="/miniconda/bin:${PATH}"

# Update Conda, just in case
RUN conda update -n base -c defaults conda

# Create a Conda environment and install DFTB+
RUN conda create -n dftbplus_env python=3.8
RUN echo "source activate dftbplus_env" > ~/.bashrc
ENV PATH /miniconda/envs/dftbplus_env/bin:$PATH
RUN conda install -c conda-forge dftbplus=23.1

# Set a working directory for DFTB calculations
WORKDIR /dftb_calculations

# Command to keep the container running (can be replaced with an actual command if needed)
CMD ["tail", "-f", "/dev/null"]