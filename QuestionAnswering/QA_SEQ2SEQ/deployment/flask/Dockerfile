FROM python:3.10-alpine

WORKDIR /code
COPY . /code

RUN pip3 install . && \
    pip3 install gunicorn

ENTRYPOINT ["gunicorn"]
CMD ["--workers=2", "--preload", "--bind=0.0.0.0:5000", "--log-level=debug", "--timeout=0", "marie:app"]