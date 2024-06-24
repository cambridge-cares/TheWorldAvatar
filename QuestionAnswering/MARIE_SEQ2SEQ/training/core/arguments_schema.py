from dataclasses import dataclass, field
from typing import Optional


@dataclass
class ModelArguments:
    model_path: str = field(default="google/flan-t5-base")
    model_format: str = field(
        default="hf",
        metadata={
            "help": "`hf` (Hugging Face), `onmt` (OpenNMT), `ov` (OpenVINO), `ort` (ONNX Runtime)."
        },
    )
    use_tensorrt: bool = field(
        default=False, metadata={"help": "Whether to use TensorRT."}
    )
    model_family: str = field(
        default="t5",
        metadata={"help": "`t5`, `mt0`, `llama`, or `bloomz`."},
    )
    device_map: Optional[str] = field(default=None)
    lora_path: Optional[str] = field(default=None)
    # quantization hyperparams
    bits: Optional[int] = field(
        default=None, metadata={"help": "How many bits to use."}
    )
    # lora hyperparams
    lora_r: Optional[int] = field(default=None)
    lora_alpha: Optional[float] = field(default=None)
    lora_dropout: Optional[float] = field(default=None)


@dataclass
class DatasetArguments:
    train_data_path: Optional[str] = field(default=None)
    eval_data_path: Optional[str] = field(default=None)
    source_max_len: int = field(
        default=512,
        metadata={
            "help": "Maximum source sequence length. Sequences will be right padded (and possibly truncated)."
        },
    )
    target_max_len: int = field(
        default=512,
        metadata={
            "help": "Maximum target sequence length. Sequences will be right padded (and possibly truncated)."
        },
    )


@dataclass
class InferenceArguments:
    out_file: str = field(metadata={"help": "File to write predictions to."})
    max_new_tokens: int = field(
        default=512, metadata={"help": "Maximum number of tokens to be generated."}
    )
    embedding_model_path: str = field(metadata={"help": "Path to embedding model for relation correction."})
    do_torch_compile: bool = field(
        default=False,
        metadata={
            "help": "Whether to apply torch.compile() to the pytorch model before running inference."
        },
    )
    do_correct: bool = field(
        default=False,
        metadata={
            "help": "Whether to apply copy correction and relation correction."
        }
    )
    do_profile: bool = field(default=False, metadata={"help": "Whether to monitor memory usage."})
