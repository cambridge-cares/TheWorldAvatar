import json
from openai import OpenAI
from .retrieve import Nlq2ActionRetriever


class Nl2SparqlTranslator:
    SYSTEM_MSG = "You are a SPARQL expert designed to output JSON."
    PROMPT_TEMPLATE = """### Examples of translating natural language questions to executable actions:
{examples}

### Instruction: 
Your task is to translate the following question to an executable action. Please do not provide any explanation and respond with a single JSON object exactly.

### Question:
{question}"""

    def __init__(
        self,
        openai_base_url: str,
        openai_api_key: str,
        model: str,
    ):
        self.retriever = Nlq2ActionRetriever()
        self.openai_client = OpenAI(base_url=openai_base_url, api_key=openai_api_key)
        self.model = model

    def translate(self, nlq: str) -> dict:
        examples = self.retriever.retrieve_examples(nlq, k=10)
        prompt = self.PROMPT_TEMPLATE.format(
            examples="\n".join(
                '"{input}" => {output}'.format(
                    input=example.nlq, output=json.dumps(example.action)
                )
                for example in examples
            ),
            question='"{input}" => '.format(input=nlq),
        )
        res = self.openai_client.chat.completions.create(
            model=self.model,
            response_format={"type": "json_object"},
            messages=[
                {"role": "system", "content": self.SYSTEM_MSG},
                {"role": "user", "content": prompt},
            ],
        )
        return json.loads(res.choices[0].message.content)
