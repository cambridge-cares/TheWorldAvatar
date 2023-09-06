FROM python:3.10-alpine

WORKDIR /code

COPY requirements.txt /code
RUN pip3 install -r requirements.txt && \
    pip3 install gunicorn

COPY . /code

ENV TRITON_ENDPOINT triton:8000

ENTRYPOINT ["gunicorn"]
CMD ["-w", "4", "-b", "0.0.0.0:5000", "app:app"]