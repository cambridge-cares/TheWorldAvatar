import json
import os

from datasets import Dataset
import transformers
from transformers import (
    DataCollatorForSeq2Seq,
    Seq2SeqTrainingArguments,
    Seq2SeqTrainer,
)

from data_processing import preprocess_nl, preprocess_sparql
from args_schema import DatasetArguments, ModelArguments
from model_utils import get_hf_model_and_tokenizer


def get_trainer(
    model_args: ModelArguments,
    data_args: DatasetArguments,
    train_args: Seq2SeqTrainingArguments,
):
    model, tokenizer = get_hf_model_and_tokenizer(model_args.model_path)

    def _tokenize(examples):
        model_inputs = tokenizer(
            examples["source"], max_length=data_args.source_max_len, truncation=True
        )
        labels = tokenizer(
            examples["target"], max_length=data_args.target_max_len, truncation=True
        )
        model_inputs["labels"] = labels["input_ids"]
        return model_inputs

    def _preprocess_examples(examples):
        sources = [preprocess_nl(qn) for qn in examples["question"]]
        targets = [preprocess_sparql(query) for query in examples["sparql"]]
        return dict(source=sources, target=targets)

    def _get_tokenized_dataset(data_path: str):
        with open(data_path, "r") as f:
            data = json.load(f)
        data = [{"question": x["question"], "sparql": x["query"]["sparql_compact"]} for x in data]
        dataset = Dataset.from_list(data)
        dataset = dataset.map(
            _preprocess_examples,
            batched=True,
            remove_columns=[
                x for x in dataset.column_names if x not in ["source", "target"]
            ],
        )
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
