from functools import cache
import logging
from typing import Annotated

from fastapi import Depends
from openai import OpenAI
import pint

from config import AppSettings, get_app_settings
from model.qt_recog import QtRecogPrediction
from services.stores.qt_recog_example_store import (
    QtRecogExampleStore,
    get_qtRecog_exampleStore,
)


logger = logging.getLogger(__name__)


class QtNormaliser:
    QT_RECOG_PROMPT_TEMPLATE = """### Instruction:
Your task is to detect physical quantities in natural language texts based on the examples given. Please ignore physical quantities with no units and respond with a single JSON object exactly, or `null` if no physical quantities are present.

### Input-output examples:
{examples}

### Input:
{text}"""

    # https://github.com/hgrecco/pint/blob/master/pint/default_en.txt
    TARGET_UNIT = {
        # ontospecies
        "molar_mass": "g/mol",
        "molecular_weight": "g/mol",
        # ontozeolite
        "accessible_area_per_cell": "angstrom^2",
        "accessible_area_per_gram": "angstrom^2/g",
        "accessible_volume_per_cell": "angstrom^3",
        "zeolite_density": "g/cm^3",
        "framework_density": "nm^-3",
        "occupiable_area_per_cell": "angstrom^2",
        "occupiable_area_per_gram": "angstrom^2/g",
        "occupiable_volume_per_cell": "angstrom^3",
        "specific_accessible_area": "m^2/cm^3",
        "specific_occupiable_area": "m^2/cm^3",
    }

    def __init__(
        self,
        qt_recog_example_store: QtRecogExampleStore,
        openai_base_url: str | None,
        openai_api_key: str | None,
        openai_model: str,
    ):
        self.ureg = pint.UnitRegistry()
        self.qt_recog_example_store = qt_recog_example_store
        self.openai_client = OpenAI(base_url=openai_base_url, api_key=openai_api_key)
        self.openai_model = openai_model

    def normalise_qt(self, type: str, value: float, unit: str):
        qt = self.ureg.Quantity(value, unit)
        if type in self.TARGET_UNIT:
            qt = qt.to(self.TARGET_UNIT[type])
        else:
            qt = qt.to_base_units()
        return f"{qt:~P}"

    def normalise(self, text: str):
        prompt = self.QT_RECOG_PROMPT_TEMPLATE.format(
            examples="\n".join(
                '"{input}" => {output}'.format(
                    input=example.text,
                    output=(
                        example.prediction.model_dump_json()
                        if example.prediction
                        else "null"
                    ),
                )
                for example in self.qt_recog_example_store.retrieve()
            ),
            text=text,
        )
        logger.info("PROMPT:\n" + prompt)

        res = self.openai_client.chat.completions.create(
            model=self.openai_model,
            response_format={"type": "json_object"},
            messages=[
                {"role": "user", "content": prompt},
            ],
            temperature=0,
        )
        logger.info("LLM's response: " + str(res))

        content = res.choices[0].message.content
        if not content or content == "null":
            return text

        pred = QtRecogPrediction.model_validate_json(content)
        return pred.template.format(
            *(
                self.normalise_qt(type=qt.type, value=qt.value, unit=qt.unit)
                for qt in pred.quantities
            )
        )


@cache
def get_qt_normaliser(
    qt_recog_example_store: Annotated[
        QtRecogExampleStore, Depends(get_qtRecog_exampleStore)
    ],
    app_settings: Annotated[AppSettings, Depends(get_app_settings)],
):
    return QtNormaliser(
        qt_recog_example_store=qt_recog_example_store,
        openai_base_url=app_settings.qt_recog.base_url,
        openai_api_key=app_settings.qt_recog.api_key,
        openai_model=app_settings.qt_recog.model,
    )
