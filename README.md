# nl-to-sparql

## Setup
- `conda` for `python3`
- `conda install pytorch torchvision torchaudio pytorch-cuda=12.1 -c pytorch -c nvidia`
- `conda install --file requriements.txt`

## Finetune

See [`finetune.sh`](scripts/finetune.sh)

## Inference

See [`inference.sh`](scripts/inference.sh)

## Evaluation

```
python evaluate.py <path_to_predictions> <output_path>
```

## Convert HuggingFace PyTorch models to ONNX

```
python convert_model.py <path_to_model_directory> <output_path>
```

