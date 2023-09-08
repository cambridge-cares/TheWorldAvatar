FROM python:3.10-alpine

WORKDIR /code

COPY . /code

RUN pip3 install -e . && \
    pip3 install gunicorn

ENV TRITON_ENDPOINT triton:8000

ENTRYPOINT ["gunicorn"]
CMD ["-w", "4", "-b", "0.0.0.0:5000", "marie:app"]