FROM nvcr.io/nvidia/tritonserver:24.01-py3

WORKDIR /workspace

COPY model_repository/ /models/

ENTRYPOINT [ "tritonserver" ]
CMD [ "--model-repository=/models" ]