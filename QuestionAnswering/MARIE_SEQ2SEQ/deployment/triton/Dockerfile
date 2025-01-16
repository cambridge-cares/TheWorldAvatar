FROM nvcr.io/nvidia/tritonserver:23.08-py3

WORKDIR /workspace

COPY requirements.txt /workspace
RUN pip install -r requirements.txt

COPY model_repository/ /models/

ENTRYPOINT ["tritonserver"]
CMD ["--model-repository=/models"]