FROM python:3.10

WORKDIR /code
COPY . /code
RUN pip install --no-cache-dir --upgrade -r /code/requirements.txt

RUN python ingest_entities.py
RUN python ingest_examples.py

CMD uvicorn main:app --host=0.0.0.0 --log-config /code/log_conf.yaml --workers 4