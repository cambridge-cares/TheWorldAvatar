# Triton inference server

## Installation

### Prerequisites

- Docker

### Setup

0. Upload model weights in the ONNX format to [HuggingFace Hub](https://huggingface.co/docs/hub/en/models-the-hub) and note down the model names shown. Only seq2seq models can be used. See HuggingFace's guides on how to [export a Transfomers model to ONNX](https://huggingface.co/docs/transformers/en/serialization#export-to-onnx).

1. For each domain `chemistry`, `kingslynn`, and `singapore`, locate `model_repository/seq2seq_<domain>/1/model.json` file.  Update the `model_path` key with the model name on HuggingFace as its value.

2. Build the docker image.
   ```{bash}
   docker build -t triton:0.1.0 .
   ```

3. Run the container.
   
   To serve all models:
   ```{bash}
   docker run -d -p 8000:8000 --shm-size=256m --name inference_server triton:0.1.0 tritonserver --model-repository=/models
   ```
   
   To serve a subset of models, e.g. to serve only `seq2seq_chemistry`:
   ```{bash}
   docker run -d p 8000:8000 --shm-size=256m --name inference_server triton:0.1.0 tritonserver --model-repository=/models --model-control-mode=explicit --load-model=seq2seq_chemistry
   ```

## Development

For development on NVIDIA's base image

```
# Launch container for model serving
docker run -it --shm-size=256m --rm -p8000:8000 -p8001:8001 -p8002:8002 -v ${PWD}:/workspace/ -v ${PWD}/model_repository:/models nvcr.io/nvidia/tritonserver:23.08-py3 bash

# Install dependencies
pip install torch transformers optimum[onnxruntime] sentence-transformers

# Launch the server
tritonserver --model-repository=/models
```

Test inference server with a sample client script.

```
docker run -it --net=host -v ${PWD}:/workspace/ nvcr.io/nvidia/tritonserver:23.08-py3-sdk bash
python3 client.py
```
