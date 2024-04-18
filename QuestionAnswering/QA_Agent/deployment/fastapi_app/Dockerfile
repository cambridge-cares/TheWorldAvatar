FROM python:3.10

WORKDIR /code
COPY . /code
RUN pip install --no-cache-dir --upgrade -r /code/requirements.txt

CMD uvicorn main:app --host=0.0.0.0 --log-config /code/log_conf.yaml --workers 4