FROM nvcr.io/nvidia/tritonserver:24.01-py3

WORKDIR /workspace

COPY requirements.txt /workspace
RUN pip install -r requirements.txt

COPY backends/ /opt/tritonserver/backends/
COPY model_repository/ /models/
