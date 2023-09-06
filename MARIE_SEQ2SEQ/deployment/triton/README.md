# Triton inference server

## Development

For development from NVIDIA's base image

```
# Launch container for model serving
docker run -it --shm-size=256m --rm -p8000:8000 -p8001:8001 -p8002:8002 -v ${PWD}:/workspace/ -v ${PWD}/model_repository:/models nvcr.io/nvidia/tritonserver:23.08-py3 bash

# Install dependencies
pip install torch transformers optimum[onnxruntime]

# Launch the server
tritonserver --model-repository=/models
```

Build and run docker image

```
docker build -t triton:0.1.0 .
docker run -d -p 8000:8000 --shm-size=256m --name inference_server triton:0.1.0
```

Test inference server with a sample client script.

```
docker run -it --net=host -v ${PWD}:/workspace/ nvcr.io/nvidia/tritonserver:23.08-py3-sdk bash
python3 client.py
```



## Deployment

See [Deployment for Marie](../README.md#deployment-for-marie).