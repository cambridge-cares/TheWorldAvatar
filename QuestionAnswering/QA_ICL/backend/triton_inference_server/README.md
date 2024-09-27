# Self-hosting ML models for QA applications

This directory contains the configuration to serve the Sentence-BERT model for generating text embeddings, which plays a specific role in our Question Answering (QA) system workflow.

## Role of Text Embedding in the QA Workflow
According to the main architecture described in [README](../../README.md#Architecture), the Triton server serves the Sentence-BERT model for text embedding generation via gRPC. Its primary function is:

- Text Embedding Generation
   - The Sentence-BERT model hosted on this Triton server generates text embeddings. These embeddings are used for natural language processing tasks within the FastAPI application, such as semantic similarity comparisons of the QA system.

By serving the Sentence-BERT model through Triton Inference Server, we ensure efficient and scalable generation of text embeddings as required by the FastAPI application in the pipeline.

## Installation

### Prerequisites

- Docker

### Required resources

The ONNX weights for Sentence-BERT model should be placed in [model_repository/mpnet/1](model_repository/mpnet/1/). 

This ONNX file can be obtained by using HuggingFace's [utility](https://huggingface.co/docs/transformers/en/serialization#exporting-a--transformers-model-to-onnx-with-cli) to convert the [Pytorch weights](https://huggingface.co/sentence-transformers/all-mpnet-base-v2) to the ONNX format. Please note that there are several [variants](https://sbert.net/docs/sentence_transformer/pretrained_models.html) to Sentence-BERT. The `all-mpnet-base-v2` variant is recommended.

### Steps

1. Deposit the model weights in the relevant directories as per the section [Required resources](#required-resources).
1. Build the image: `docker build -t triton:0.1.0 .`.
1. Run the container.
   ```
   docker run -d \
     -p 8000:8000 \
     -p 8001:8001 \
     -p 8002:8002 \
     --shm-size=256m \
     --name inference_server_mpnet \ 
     triton:0.1.0
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
