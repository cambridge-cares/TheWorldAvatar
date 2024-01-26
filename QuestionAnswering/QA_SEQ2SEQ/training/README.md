# nl-to-sparql

## Setup

### Prerequisites

- `python3`
- `conda`

### Installation

First, create a conda environment and activate it.

```
conda create --name marie-llama python=3.10
conda activate marie-llama
```

Then, install relevant dependencies with the following commands.

```
conda install pytorch torchvision torchaudio pytorch-cuda=12.1 -c pytorch -c nvidia
conda install --file requriements.txt
```

## Data format

```{json}
[
    {
        "id": str,
        "domain": str,
        "question": str,
        "query": {
            "sparql": str
        }
    }
]
```

## Finetune

See [`finetune.sh`](scripts/finetune.sh) for example usage.

## Inference

See [`inference.sh`](scripts/inference.sh) for example usage.

## Evaluation

The following command computes evaluation metrics for translation (sacreBLEU and exatch match scores) and domain prediction (accuracy).

```
python evaluate.py <path_to_predictions> <output_path>
```

## Utilities for converting model weight formats

- To convert a PyTorch model to ONNX, `python convert_model.py <input_model_path> <output_model_path> --mode to_onnx`.
- To perform avx512_vnni 8-bit quantization on ONNX model weights, `python convert_model.py <input_model_path> <output_model_path> --mode to_8bit`.
