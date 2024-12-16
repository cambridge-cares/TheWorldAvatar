from decimal import Decimal
import json
import os
import random

import numpy as np

from constants.fs import ROOTDIR
from constants.plot import PLOT_ATTR_2_PRED
from locate_then_ask.model import OmMeasure
from locate_then_ask.ontokin.model import OKSpecies
from locate_then_ask.singapore.ask import OPltPlotAsker
from locate_then_ask.singapore.model import OPltPlot
from utils.numerical import normalize_1d


class OPltPlotSynthesizer:
    FILEPATH = os.path.join(ROOTDIR, "data/singapore/Plot.json")

    def __init__(self, bg_endpoint: str, ontop_endpoint: str):
        if not os.path.exists(self.FILEPATH):
            data = dict()
            from locate_then_ask.kg_client import KgClient

            kg_client = KgClient(bg_endpoint)
            query = """PREFIX ozng: <https://www.theworldavatar.com/kg/ontozoning/>

SELECT DISTINCT ?LandUseTypeType WHERE {{
  SERVICE <{ontop}> {{
    ?Plot ozng:hasLandUseType ?LandUseType
  }}
  ?LandUseType a ?LandUseTypeType
}}""".format(ontop=ontop_endpoint)
            data["LandUseTypeType"] = [
                x["LandUseTypeType"]["value"]
                for x in kg_client.query(query)["results"]["bindings"]
            ]

            query_template = """PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX oplnrgl: <https://www.theworldavatar.com/kg/ontoplanningregulation/>
PREFIX oplt: <https://www.theworldavatar.com/kg/ontoplot/>

SELECT DISTINCT ?NumericalValue ?Unit WHERE {{
  SERVICE <{ontop}> {{
    ?Plot {pred} [
    	om:hasNumericalValue ?NumericalValue ;
        om:hasUnit ?Unit
    ]
  }}
}}
ORDER BY RAND()
LIMIT 100"""
            for key in OPltPlotAsker.NUMERICAL_KEYS:
                query = query_template.format(ontop=ontop_endpoint, pred=PLOT_ATTR_2_PRED[key])
                data[key.value] = [
                    {
                        "NumericalValue": x["NumericalValue"]["value"],
                        "Unit": x["Unit"]["value"],
                    }
                    for x in kg_client.query(query)["results"]["bindings"]
                ]

            os.makedirs(os.path.dirname(self.FILEPATH), exist_ok=True)
            with open(self.FILEPATH, "w") as f:
                json.dump(data, f, indent=4)

        with open(self.FILEPATH, "r") as f:
            data = json.load(f)

        self.data = data

    def make(self):
        sample = random.choice(self.data["GrossPlotRatio"])
        gross_plot_ratio = OmMeasure(
            numerical_value=Decimal(sample["NumericalValue"]), unit_iri=sample["Unit"]
        )
        sample = random.choice(self.data["PlotArea"])
        plot_area = OmMeasure(
            numerical_value=Decimal(sample["NumericalValue"]), unit_iri=sample["Unit"]
        )
        sample = random.choice(self.data["GrossFloorArea"])
        gross_floor_area = OmMeasure(
            numerical_value=Decimal(sample["NumericalValue"]), unit_iri=sample["Unit"]
        )

        return OPltPlot(
            land_use_type_type=random.choice(self.data["LandUseTypeType"]),
            gross_plot_ratio=gross_plot_ratio,
            is_awaiting_detailed_gpr_eval=random.choice([True, False]),
            plot_area=plot_area,
            gross_floor_area=gross_floor_area,
        )
