ARG PYTHON_VERSION=3.7.12

FROM python:${PYTHON_VERSION}-slim-buster


# Install prerequisite apt packages
RUN apt-get -qq update && apt-get -qq --yes install xsltproc



# Copy in entrypoint script, upload script
COPY ./entrypoint.sh .
COPY ./xml2csv.sh .
COPY ./xml2csv.xslt .
COPY ./fileSplitter.py .

# Run the upload script, via the entrypoint script
ENTRYPOINT /entrypoint.sh