FROM condaforge/miniforge3

# Expose the port on which our server will run
EXPOSE 5000

# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Set the default working directory, then copy the Python source code into it
WORKDIR /app

RUN git clone https://github.com/cambridge-cares/TheWorldAvatar.git --no-checkout TWA && \
    cd TWA && git sparse-checkout init --cone && git sparse-checkout set thermo/chemutils && \
    git checkout main && cd thermo/chemutils && chmod 777 ./install_script_conda.sh

COPY ./chemaboxwriters/ ./chemaboxwriters
COPY ./install_script_conda.sh .
COPY ./tests/ ./tests/
COPY ./README.md .
COPY ./LICENSE .
COPY ./aboxwriters_config.yml .
COPY ./setup.py .
COPY ./base.yml .
COPY ./dependencies.yml .
COPY ./dev-dependencies.yml .

#------------------------------------
# conda setup
#------------------------------------
ARG conda_env=chemaboxwriters
RUN ./install_script_conda.sh -v -n $conda_env -i -e -c ./TWA/thermo/chemutils
# Make RUN commands use the new environment:
RUN echo "source activate $conda_env" > ~/.bashrc
ENV PATH /opt/conda/envs/$conda_env/bin:$PATH
RUN conda install openjdk

#------------------------------------
# entry point setup
#------------------------------------
ENTRYPOINT ["tail", "-f", "/dev/null"]
#==================================================================================================