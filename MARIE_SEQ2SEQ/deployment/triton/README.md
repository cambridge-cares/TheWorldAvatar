# Triton inference server

## Local setup

Build the docker image and run the container.

```
docker build -t triton:0.1.0 .
docker run \
    -d \
    -p 8000:8000 \
    --shm-size=256m \
    --name inference_server \
    -e TRANSLATION_MODELPATH=<path_to_translation_model> \
    -e EMBEDDING_MODELPATH=<path_to_embedding_model> \
    triton:0.1.0
```

## Development

For development on NVIDIA's base image

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
