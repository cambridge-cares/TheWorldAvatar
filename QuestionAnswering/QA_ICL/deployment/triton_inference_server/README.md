# Triton inference server

This directory contains resources to serve ML models with Triton Inference Server. The following models are included:

- `mpnet`: text embedding model

## Installation

### Prerequisites

- ONNX weights for mpnet model (see [all-mpnet-base-v2](https://huggingface.co/sentence-transformers/all-mpnet-base-v2) and [Exporting a Transformers model to ONNX with CLI](https://huggingface.co/docs/transformers/en/serialization#exporting-a--transformers-model-to-onnx-with-cli))

### Steps

1. Place `.onnx` file for mpnet model weights in `model_repository/mpnet/1`.
2. Build the image for the triton server and run it.
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

To prepare the [`config.pbtxt`](https://docs.nvidia.com/deeplearning/triton-inference-server/user-guide/docs/user_guide/model_configuration.html) file,
- Visualise the computational graph with [Netro](https://netron.app/) to view the names, shapes, and datatypes of the input and output nodes,
- [Triton's documentation on datatypes](https://docs.nvidia.com/deeplearning/triton-inference-server/user-guide/docs/user_guide/model_configuration.html#datatypes).

### Testing

Given the client script to test the inference server is `client.py`, the script can be executed as follows.

```
docker run -it --net=host -v ${PWD}:/workspace/ nvcr.io/nvidia/tritonserver:23.08-py3-sdk bash
python3 client.py
```
