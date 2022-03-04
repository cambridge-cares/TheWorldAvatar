ARG PYTHON_VERSION=3.7.12

FROM python:${PYTHON_VERSION}-slim-buster

# Install prerequisite python packages
RUN pip install csvkit psycopg2-binary

# Install prerequisite apt packages
RUN apt-get -qq update && apt-get -qq --yes install postgresql-client postgis gdal-bin unzip --no-install-recommends

# Copy in entrypoint script, upload script
COPY entrypoint.sh upload_to_pgsql.sh /

# Run the upload script, via the entrypoint script
ENTRYPOINT /entrypoint.sh