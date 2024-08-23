ARG PYTHON_VERSION=3.7.12

FROM python:${PYTHON_VERSION}-slim-buster


# Install prerequisite apt packages
RUN apt-get -qq update 

# Copy in entrypoint script, upload script
COPY ./entrypoint.sh .
COPY ./ord2csv.sh .
COPY ./ord2csv.py .
COPY ./schema2labels.py .
COPY ./requirements.txt . 

# Install prerequisite python packages
RUN pip install -r requirements.txt

# Specifying Volumes
VOLUME results inputs

# Run the upload script, via the entrypoint script
ENTRYPOINT /entrypoint.sh