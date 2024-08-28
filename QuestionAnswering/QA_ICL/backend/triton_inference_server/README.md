# Self-hosting ML models for QA applications

This directory contains resources to serve ML models with Triton Inference Server. The following models are included:

- mpnet: text embedding model

## Installation

### Prerequisites

- [Docker](https://docs.docker.com/engine/install/)

### Required resources

ONNX weights for mpnet model should be placed in [model_repository/mpnet/1](model_repository/mpnet/1/).
- PyTorch weights: [all-mpnet-base-v2](https://huggingface.co/sentence-transformers/all-mpnet-base-v2)
- See [Exporting a Transformers model to ONNX with CLI](https://huggingface.co/docs/transformers/en/serialization#exporting-a--transformers-model-to-onnx-with-cli)

### Steps

```
docker build -t triton:0.1.0 .
docker run -d -p 8000:8000 -p 8001:8001 -p 8002:8002 --shm-size=256m --name inference_server_mpnet triton:0.1.0
```

### Usage

See [Triton Client Libraries and Examples](https://github.com/triton-inference-server/client).

## Development

### Development with Triton image

```
# Launch container for model serving
docker run -it --shm-size=256m --rm -p 8000:8000 -p 8001:8001 -p 8002:8002 -v ${PWD}:/workspace/ -v ${PWD}/model_repository:/models nvcr.io/nvidia/tritonserver:23.08-py3 bash

# Launch the server
tritonserver --model-repository=/models
```

To prepare the [config.pbtxt](https://docs.nvidia.com/deeplearning/triton-inference-server/user-guide/docs/user_guide/model_configuration.html) file,
- Visualise the computational graph with [Netro](https://netron.app/) to view the names, shapes, and datatypes of the input and output nodes,
- [Triton's documentation on datatypes](https://docs.nvidia.com/deeplearning/triton-inference-server/user-guide/docs/user_guide/model_configuration.html#datatypes).

### Testing

```
docker run -it --net=host -v ${PWD}:/workspace/ nvcr.io/nvidia/tritonserver:23.08-py3-sdk bash
python3 client.py
```
