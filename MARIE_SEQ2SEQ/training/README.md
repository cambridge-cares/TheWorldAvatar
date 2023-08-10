## Installation

### Prerequisites
- Linux OS (recommended) . It is not advisable to run this project on Windows as [`bitsandbytes` is not supported on Windows]((https://github.com/TimDettmers/bitsandbytes/issues/30)).
- [conda](https://conda.io/projects/conda/en/latest/index.html) (recommended).
- `python==3.10`.
- CUDA

### Steps
1. Create a conda environment and activate it.
   ```
   conda create --name marie-llama python=3.10
   conda activate marie-llama
   ```
1. Install `torch==2.0.1+cu118`.
   ```
   conda install pytorch torchvision torchaudio pytorch-cuda=11.8 -c pytorch -c nvidia
   ```
1. Install the remaining packages required by the project.

   ````
   pip install -r requirements.txt
   ````
1. For access to any private models or datasets on Hugging Face, ensure that you have the necessary read privileges. Generate an [access token](https://huggingface.co/docs/hub/security-tokens) and make this an environment variable.
   ```
   export HF_ACCESS_TOKEN=<huggingface-access-token>
   ```

## Data Generation/Preparation

```{json}
[
   {
      "question": "",
      "sparql_query": ""
   }
]
```

## Fine-tuning

### Weight & Biases configuration for monitoring

1. [Sign up](https://wandb.ai/site) for a Weights & Biases account. 
1. Locate your API key on the [Authorize page](https://wandb.ai/authorize).
1. Execute the following command and specify the API key.
   ```
   wandb login
   ```
1. Set the Weights & Biases project name as an environment variable.
   ```
   export WANDB_PROJECT=marie
   ```

### Fine-tuning execution

See [`finetune.sh`](./scripts/finetune.sh) for example usage. The script will save the weights of the fine-tuned model in the specified `output_dir`.

For a list all arguments that can be passed to the fine-tune script, see classes `arguments_schema.ModelArguments`, `arguments_schema.DatasetArguments` and `transformers.TrainingArguments`.

## Inference

See [`inference.sh`](./scripts/inference.sh) for example usage. The script will generate a json file containing  

For a list of all arguments that can be passed to the inference script, see classes `arguments_schema.ModelArguments`, `arguments_schema.DatasetArguments`, and `arguments_schema.InferenceArguments`.

## Evaluation


## Running jobs on CSD3
Per [recommendation by CSD3](https://docs.hpc.cam.ac.uk/hpc/user-guide/io_management.html), I/O data should be placed under `/rds`. Cache for models and datasets should thus be placed here. Concretely, the Hugging Face cache directory should be set as follows.
```
export HF_HOME=/rds/user/nmdt2/hpc-work/.cache/huggingface
``` 


## Development

### Testing

#### Installation
```
pip install pytest
```

#### Execute tests
```
pytest
```