# Triton inference server

## Development

```
# Launch container for model serving
docker run -it --shm-size=256m --rm -p8000:8000 -p8001:8001 -p8002:8002 -v ${PWD}:/workspace/ -v ${PWD}/model_repository:/models nvcr.io/nvidia/tritonserver:23.08-py3 bash

# Install dependencies
pip install torch transformers optimum[onnxruntime]

# Launch the server
tritonserver --model-repository=/models
```

Test inference server with a sample client script.

```
docker run -it --net=host -v ${PWD}:/workspace/ nvcr.io/nvidia/tritonserver:23.08-py3-sdk bash
python3 client.py
```

## Deployment

See [Deployment for Marie](../README.md#deployment-for-marie).