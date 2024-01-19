# install the py4ml app
#==================================================================================================
FROM condaforge/miniforge-pypy3 as py4ml_app

# Expose the port on which our server will run
EXPOSE 5000

# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1
SHELL ["/bin/bash", "-c"]

# Set the default working directory, then copy the Python source code into it
WORKDIR /app
COPY ./py4ml /app/py4ml/
COPY ./LICENSE .
COPY ./README.md .
COPY ./setup.py .
COPY ./environment_cpu.yml .
COPY ./environment_gpu.yml .
COPY ./install_script.sh .
COPY ./app_entry_point.sh .
COPY ./tests /app/tests/

# Give execution rights to the script
RUN chmod 0744 /app/app_entry_point.sh

#------------------------------------
# conda setup
#------------------------------------
ARG conda_env=py4ml_venv
# Install the required Python libraries
RUN ./install_script.sh -v -n $conda_env -i -e
# Make RUN commands use the new environment:
RUN echo "source activate $conda_env" > ~/.bashrc
ENV PATH /opt/conda/envs/$conda_env/bin:$PATH

RUN python -m pip install pytest

#------------------------------------
# entry point setup
#------------------------------------
ENTRYPOINT /app/app_entry_point.sh
#==================================================================================================