from functools import cache
import json
import os
from typing import List, Optional
from openai import OpenAI

from .execute_action.model import ActionBase, FuncAction, SparqlAction
from .retrieve import Nlq2ActionExample


class Nlq2ActionTranslator:
    SYSTEM_MSG = "You are a SPARQL expert designed to output JSON."
    PROMPT_TEMPLATE = """### Examples of translating natural language questions to executable actions:
{examples}

### Instruction: 
Your task is to translate the following question to an executable action. Please do not provide any explanation and respond with a single JSON object exactly.

### Question:
{question}"""

    def __init__(
        self,
        openai_base_url: Optional[str],
        openai_api_key: Optional[str],
        openai_model: str,
    ):
        self.openai_client = OpenAI(base_url=openai_base_url, api_key=openai_api_key)
        self.model = openai_model

    def translate(self, nlq: str, examples: List[Nlq2ActionExample]) -> ActionBase:
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
            temperature=0,
        )
        action = json.loads(res.choices[0].message.content)

        try:
            if "sparql" in action:
                return SparqlAction(**action["sparql"])
            elif "func" in action:
                faction = action["func"]
                return FuncAction(
                    name=faction["name"], args=faction["args"]
                )
        except:
            pass
        raise Exception("Invalid action: " + str(action))


@cache
def get_nlq2action_translator():
    return Nlq2ActionTranslator(
        openai_base_url=os.getenv("TRANSLATOR_OPENAI_BASE_URL"),
        openai_api_key=os.getenv("TRANSLATOR_OPENAI_API_KEY"),
        openai_model=os.environ["TRANSLATOR_OPENAI_MODEL"],
    )
