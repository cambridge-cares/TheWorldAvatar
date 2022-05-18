# install the py4ml app
#==================================================================================================
FROM continuumio/miniconda3 as py4ml_app
# Expose the port on which our server will run
EXPOSE 5000
# Keeps Python from generating .pyc files in the container
#ENV PYTHONDONTWRITEBYTECODE=1
# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1
SHELL ["/bin/bash", "-c"]

#------------------------------------
# cron setup
#------------------------------------
# Get cron
RUN apt-get update && apt-get -y install cron
# Setup cron job
ADD ./cronconfig /etc/cron.d/cronconfig
# Give execution rights on the cron job
RUN chmod 0644 /etc/cron.d/cronconfig
# Apply cron job
RUN crontab /etc/cron.d/cronconfig
# Create the log file to be able to run tail
RUN touch /var/log/cron.log

# Set the default working directory, then copy the Python source code into it
WORKDIR /app
COPY ./py4ml /app/py4ml/
COPY ./LICENSE /app/.
COPY ./README.md /app/.
COPY ./setup.py /app/.
COPY ./cronconfig /app/.
COPY ./environment_cpu.yml /app/.
COPY ./environment_gpu.yml /app/.
COPY ./delete_logs.sh /app/.
COPY ./install_script.sh /app/.
COPY --from=dependency_fetcher /root/${artifact_id} ./retrieved_models/${artifact_id}

# Give execution rights to the script
RUN chmod 0744 /app/delete_logs.sh

#------------------------------------
# conda setup
#------------------------------------
ARG conda_env=py4ml_venv
# Install the required Python libraries
RUN ./install_script.sh -v -n $conda_env -i -e
# Make RUN commands use the new environment:
RUN echo "source activate $conda_env" > ~/.bashrc
ENV PATH /opt/conda/envs/$conda_env/bin:$PATH

#------------------------------------
# entry point setup
#------------------------------------
# Switch to a non-root user before running the server, for security reasons
# (See https://code.visualstudio.com/docs/containers/python-user-rights)
#RUN useradd appuser && chown -R appuser /app
#USER appuser
# Set the entrypoint
RUN chmod 0744 /app/app_entry_point.sh
CMD tail -f /dev/null
#==================================================================================================