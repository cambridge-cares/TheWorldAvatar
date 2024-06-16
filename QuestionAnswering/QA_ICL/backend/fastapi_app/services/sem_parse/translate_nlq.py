from functools import cache
import json
import logging
from typing import Annotated
from fastapi import Depends
from openai import OpenAI
from pydantic import TypeAdapter

from config import AppSettings, get_app_settings
from model.nlq2datareq import DataRequest
from model.qa import TranslationContext
from utils.rdf import try_make_prefixed_iri


logger = logging.getLogger(__name__)


class Nlq2DataReqLLMCaller:
    SYSTEM_MSG = "You are a SPARQL expert designed to output JSON."
    PROMPT_TEMPLATE = """### Data and object properties:
{properties}

### Examples of translating natural language questions to executable data requests:
{examples}

### Instruction: 
Your task is to translate the following question to an executable data request. Please do not provide any explanation and respond with a single JSON object exactly.

### Question:
{question}"""

    def __init__(
        self,
        openai_base_url: str | None,
        openai_api_key: str | None,
        openai_model: str,
    ):
        self.openai_client = OpenAI(base_url=openai_base_url, api_key=openai_api_key)
        self.openai_model = openai_model
        self.datareq_adapter = TypeAdapter(DataRequest)

    def forward(
        self, nlq: str, translation_context: TranslationContext
    ) -> DataRequest:
        prompt = self.PROMPT_TEMPLATE.format(
            examples="\n".join(
                '"{input}" => {output}'.format(
                    input=example.nlq,
                    output=example.data_req.model_dump_json(
                        exclude_unset=True,
                        exclude_none=True,
                    ),
                )
                for example in translation_context.examples
            ),
            properties=json.dumps(
                [
                    {
                        "IRI": try_make_prefixed_iri(prop.iri),
                        "label": prop.label,
                        "comment": prop.comment,
                    }
                    for prop in translation_context.properties
                ],
                indent=2,
            ),
            question=nlq,
        )
        logger.info("PROMPT:\n" + prompt)

        res = self.openai_client.chat.completions.create(
            model=self.openai_model,
            response_format={"type": "json_object"},
            messages=[
                {"role": "system", "content": self.SYSTEM_MSG},
                {"role": "user", "content": prompt},
            ],
            temperature=0,
        )
        logger.info("LLM's response: " + str(res))

        if not res.choices[0].message.content:
            # TODO: handle empty response from OpenAI
            raise Exception()

        return self.datareq_adapter.validate_json(res.choices[0].message.content)


@cache
def get_nlq2datareq_llmCaller(
    settings: Annotated[AppSettings, Depends(get_app_settings)]
):
    return Nlq2DataReqLLMCaller(
        openai_base_url=settings.translator.base_url,
        openai_api_key=settings.translator.api_key,
        openai_model=settings.translator.model,
    )
