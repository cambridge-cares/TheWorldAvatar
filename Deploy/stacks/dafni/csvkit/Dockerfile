ARG PYTHON_VERSION=3.7.12

FROM python:${PYTHON_VERSION}-slim-buster
RUN pip install csvkit psycopg2-binary

WORKDIR /data

ENTRYPOINT [ "tail", "-f", "/dev/null" ]