# Install the stdc app
#==================================================================================================
FROM python:3.7.11-slim-buster

# Meta data
LABEL authors = "danieln@cmclinnovations.com, support@cmclinnovations.com"
LABEL version = "1.0.0-SNAPSHOT"
LABEL description = "Thermodynamic properties calculation agent"

# Install Java
RUN apt update && apt install -y openjdk-11-jdk-headless

# Keeps Python from generating .pyc files in the container
ENV PYTHONDONTWRITEBYTECODE=1

# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Set the default shell
SHELL ["/bin/bash", "-c"]

# Set the default working directory, then copy the Python source code into it
WORKDIR /app
COPY ./stdc /app/stdc/
COPY ./LICENSE /app/.
COPY ./README.md /app/.
COPY ./setup.py /app/.
COPY ./app_entry_point.sh /app/.
COPY ./install_script_pip.sh /app/.

#------------------------------------
# python setup
#------------------------------------
# Install the required Python libraries
RUN python -m pip install -e .
RUN python -m pip install gunicorn

# Expose port
EXPOSE 5000

#------------------------------------
# entry point setup
#------------------------------------
# Set the entrypoint
RUN chmod 0744 /app/app_entry_point.sh
ENTRYPOINT /app/app_entry_point.sh

# Finish
RUN echo -e "Image installation completed."
#==================================================================================================