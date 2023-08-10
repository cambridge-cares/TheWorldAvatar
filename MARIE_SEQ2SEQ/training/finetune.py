import os

from datasets import Dataset
import transformers
from transformers import (
    DataCollatorForSeq2Seq,
    Seq2SeqTrainingArguments,
    Seq2SeqTrainer,
)

from marie.data_processing.qn_processing import preprocess_qn
from marie.data_processing.query_processing import preprocess_query

from marie.arguments_schema import DatasetArguments, ModelArguments
from marie.model_utils import get_model_and_tokenizer


def preprocess_examples(examples):
    sources = [preprocess_qn(qn) for qn in examples["question"]]
    targets = [preprocess_query(query) for query in examples["sparql_query"]]
    return dict(source=sources, target=targets)


def train():
    hfparser = transformers.HfArgumentParser(
        (ModelArguments, DatasetArguments, Seq2SeqTrainingArguments)
    )
    model_args, data_args, train_args = hfparser.parse_args_into_dataclasses()

    model, tokenizer = get_model_and_tokenizer(model_args)

    dataset = Dataset.from_json(data_args.data_path)
    dataset = dataset.map(
        preprocess_examples, batched=True, remove_columns=["question", "sparql_query"]
    )

    def _tokenize(examples):
        model_inputs = tokenizer(
            examples["source"], max_length=data_args.source_max_len, truncation=True
        )
        labels = tokenizer(
            examples["target"], max_length=data_args.target_max_len, truncation=True
        )
        model_inputs["labels"] = labels["input_ids"]
        return model_inputs

    tokenized_ds = dataset.map(
        _tokenize, batched=True, remove_columns=["source", "target"]
    )

    data_collator = DataCollatorForSeq2Seq(tokenizer=tokenizer, model=model)

    trainer = Seq2SeqTrainer(
        model=model,
        args=train_args,
        train_dataset=tokenized_ds,
        tokenizer=tokenizer,
        data_collator=data_collator,
    )

    trainer.train()

    model_output_dir = os.path.join(train_args.output_dir, "model")
    trainer.model.save_pretrained(model_output_dir)
    trainer.tokenizer.save_pretrained(model_output_dir)


if __name__ == "__main__":
    train()
