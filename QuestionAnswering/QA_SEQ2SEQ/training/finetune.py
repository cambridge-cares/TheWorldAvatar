import json
import os

from datasets import Dataset
import transformers
from transformers import (
    DataCollatorForSeq2Seq,
    Seq2SeqTrainingArguments,
    Seq2SeqTrainer,
)

from core.data_processing.constants import T5_PREFIX_DOMAINCLS, T5_PREFIX_NL2SPARQL
from core.data_processing.nl import preprocess_nl
from core.data_processing.sparql import preprocess_sparql
from core.args_schema import DatasetArguments, ModelArguments
from core.model_utils import get_hf_model_and_tokenizer


def get_trainer(
    model_args: ModelArguments,
    data_args: DatasetArguments,
    train_args: Seq2SeqTrainingArguments,
):
    model, tokenizer = get_hf_model_and_tokenizer(model_args)

    def _tokenize(examples):
        model_inputs = tokenizer(
            examples["source"], max_length=data_args.source_max_len, truncation=True
        )
        labels = tokenizer(
            examples["target"], max_length=data_args.target_max_len, truncation=True
        )
        model_inputs["labels"] = labels["input_ids"]
        return model_inputs

    def _get_tokenized_dataset(data_path: str):
        with open(data_path, "r") as f:
            data = json.load(f)

        nlqs = [preprocess_nl(x["question"]) for x in data]
        source = [T5_PREFIX_NL2SPARQL + x for x in nlqs]
        target = [preprocess_sparql(x["query"]["sparql"]) for x in data]

        if data_args.domain == "multi":
            src_for_cls = [T5_PREFIX_DOMAINCLS + x for x in nlqs]
            domains = [x["domain"] for x in data]

            source += src_for_cls
            target += domains

        dataset = Dataset.from_dict(dict(source=source, target=target))
        dataset = dataset.shuffle()
        return dataset.map(_tokenize, batched=True, remove_columns=["source", "target"])

    train_dataset = _get_tokenized_dataset(data_args.train_data_path)
    eval_dataset = _get_tokenized_dataset(data_args.eval_data_path)

    data_collator = DataCollatorForSeq2Seq(tokenizer=tokenizer, model=model)

    return Seq2SeqTrainer(
        model=model,
        args=train_args,
        train_dataset=train_dataset,
        eval_dataset=eval_dataset,
        tokenizer=tokenizer,
        data_collator=data_collator,
    )


def train():
    hfparser = transformers.HfArgumentParser(
        (ModelArguments, DatasetArguments, Seq2SeqTrainingArguments)
    )
    model_args, data_args, train_args = hfparser.parse_args_into_dataclasses()

    trainer = get_trainer(
        model_args=model_args,
        data_args=data_args,
        train_args=train_args,
    )

    trainer.train()

    model_output_dir = os.path.join(train_args.output_dir, "model")
    trainer.model.save_pretrained(model_output_dir)
    trainer.tokenizer.save_pretrained(model_output_dir)


if __name__ == "__main__":
    train()
