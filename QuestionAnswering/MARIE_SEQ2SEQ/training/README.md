## Basic setup

Prerequisites

- Linux OS (recommended) . It is not advisable to run this project on Windows as [`bitsandbytes` is not supported on Windows]((https://github.com/TimDettmers/bitsandbytes/issues/30)).
- [conda](https://conda.io/projects/conda/en/latest/index.html) (recommended).
- `python==3.10`.
- CUDA

Installation steps

1. Create a conda environment and activate it.
   ```
   conda create --name marie-llama python=3.10
   conda activate marie-llama
   ```
1. Install `torch` with `conda install pytorch torchvision torchaudio pytorch-cuda=11.8 -c pytorch -c nvidia`.
1. Install the remaining packages required by the project `pip install -r requirements.txt`.
1. For access to any private models or datasets on HuggingFace (e.g. Llama2), ensure that you have the necessary read privileges. Generate an [access token](https://huggingface.co/docs/hub/security-tokens) and make this an environment variable: `export HF_ACCESS_TOKEN=<huggingface-access-token>`.

## Data format

```{json}
[
   {
      "question": str,
      "sparql_query": str,
      "sparql_query_compact: str
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

### Training

For access to parameter-efficient fine-tuning methods such as LoRA, `pip install bitsandbytes==0.40.2 peft==0.4.0`.

See [`finetune.sh`](./scripts/finetune.sh) for example usage. The script will save the weights of the fine-tuned model in the specified `output_dir`.

For a list all arguments that can be passed to the fine-tune script, see classes `arguments_schema.ModelArguments`, `arguments_schema.DatasetArguments` and `transformers.TrainingArguments`.

## Conversion of Hugging Face models to other formats

```
python convert_model.py <path-to-hf-model-directory> <path-to-output-directory> --target_format [onmt|ov|ort]
```

## Inference

Additional dependencies:
- To run inference on different runtimes:
  - OpenNMT: `pip install ctranslate2 pyonmttok`
  - OpenVINO: `pip install optimum==1.12.0 && pip install  --upgrade-strategy eager optimum[openvino,nncf]`
  - ONNX Runtime for CPU: `pip install optimum==1.12.0 && pip install --upgrade-strategy eager install optimum[onnxruntime]`
  - ONNX Runtime for GPU: `pip install optimum==1.12.0 && pip install optimum[onnxruntime-gpu]`
    - TensorRT: 
    - [CUDA toolkit](https://docs.nvidia.com/cuda/cuda-installation-guide-linux/index.html)
    - [cuDNN](https://docs.nvidia.com/deeplearning/cudnn/install-guide/index.html)
    - [TensorRT](https://docs.nvidia.com/deeplearning/tensorrt/install-guide/index.html) (installation from tar ball recommended)
- To enable memory profiling with the command line argument `--do_profile`, run `pip install memory-profiler==0.61.0`.

See [`inference.sh`](./scripts/inference.sh) for example usage. The script will generate a json file containing the predicted SPARQL queries and inference latency.

For a list of all arguments that can be passed to the inference script, see classes `arguments_schema.ModelArguments`, `arguments_schema.DatasetArguments`, and `arguments_schema.InferenceArguments`.

## Evaluation

To obtain evaluation metrics for translation, including sacreBLEU and exact match scores, execute the following. The optional flags `--do_correct_spans` and `--do_correct_relations` can be set for differential post-processing.

```
python evaluate.py <inference_filepath> <evaluation_result_filepath> [--do_correct_spans] [--do_correct_relations]
```

## Running jobs on CSD3 and similar SLURM environments

### SLURM file

See [`finetune.slurm`](./scripts/finetune.slurm) and [`inference.slurm`](./scripts/inference.slurm) for example SLURM files. Please note the following parameters.

- `#SBATCH --gres=gpu:4`: number of GPUs per node needed; set to 4 for multi-GPU training. 
- `export HF_ACCESS_TOKEN=`: [HuggingFace access](#steps) token if needed.
- `export HF_HOME=/rds/user/nmdt2/hpc-work/.cache/huggingface`: cache directory for HuggingFace models and datasets; per [recommendation by CSD3](https://docs.hpc.cam.ac.uk/hpc/user-guide/io_management.html), I/O data should be placed under `/rds`.
- `export WANDB_PROJECT=marie`: [Weights & Biases project name](#weight--biases-configuration-for-monitoring).

### Model sharing 

Downloading model weights directly from CSD3 with either `scp` or `rsync` is generally slow because these utilities perform data transfer over SSH. It is therefore recommended that fine-tuned weights be uploaded over HTTP/HTTPS to a remote model store, such as the [HuggingFace hub](https://huggingface.co/docs/hub/repositories-getting-started#getting-started-with-repositories).

Should `git-lfs` be required to be installed without `sudo` privileges, one can perform the installation from a tarball. Below is an example for `git-lfs-3.4.0`.
1. `wget https://github.com/git-lfs/git-lfs/releases/download/v3.4.0/git-lfs-linux-amd64-v3.4.0.tar.gz`
1. `tar xvzf git-lfs-linux-amd64-v3.4.0.tar.gz`
1. `cd git-lfs-3.4.0`
1. In the file `install.sh`, update the `prefix` variable to a writable directory e.g. `/home/nmdt2/`.
1. `sh install.sh`

## Development

First, install pytest with `pip install pytest`. To execute tests, run `pytest`.
