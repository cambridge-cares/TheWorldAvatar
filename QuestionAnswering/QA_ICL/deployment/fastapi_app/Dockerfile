FROM python:3.10

WORKDIR /code
COPY . /code
RUN pip install --no-cache-dir --upgrade -r requirements.txt

CMD uvicorn main:app --host=0.0.0.0 --log-config log_conf.yaml --workers 4