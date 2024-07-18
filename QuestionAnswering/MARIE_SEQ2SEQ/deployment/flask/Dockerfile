FROM python:3.10-alpine

WORKDIR /code

COPY . /code

RUN pip3 install -e . && \
    pip3 install gunicorn

ENV TRITON_ENDPOINT triton:8000

ENTRYPOINT ["gunicorn"]
CMD ["--workers=2", "--preload", "--bind=0.0.0.0:5000", "--log-level=debug", "--timeout=0", "marie:app"]