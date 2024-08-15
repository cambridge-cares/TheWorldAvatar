# Use the official Jupyter Notebook image as a base
FROM jupyter/base-notebook

HEALTHCHECK NONE

# Install Voilà
RUN pip install voila==0.4.0

COPY ./requirements.txt /home/.
# Install the required Python libraries
RUN python -m pip install -r /home/requirements.txt

COPY ./notebook /home/jovyan/work

COPY ./app_entry_point.sh /home/jovyan/app_entry_point.sh

# Expose the Voilà port
EXPOSE 8866

ENTRYPOINT /home/jovyan/app_entry_point.sh