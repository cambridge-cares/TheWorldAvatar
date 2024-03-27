from datetime import datetime, timedelta
from functools import cache
from typing import Dict, Tuple

import numpy as np

from model.qa import QAData


class SGDispersionAgent:
    JSON_DATETIME_PATTERN = "%Y-%m-%dT%H:%M:%SZ"
    DATA_POINT_NUM = 4
    POLUTANT_BOUNDS: Dict[str, Tuple[float, float]] = {
        "CO": (100, 200),
        "PM2.5": (0.2, 0.6),
        "SO2": (3, 3.4),
        "NOx": (10, 11.5),
        "PM10": (0.35, 0.45),
        "Unburned hydrocarbons": (420, 490),
    }

    def _mock_api_response(self):
        now = datetime.now()
        time = [
            (now - timedelta(minutes=30 * i)).strftime(self.JSON_DATETIME_PATTERN)
            for i in range(self.DATA_POINT_NUM - 1, -1, -1)
        ]
        return {
            k: {
                "time": time,
                "values": np.random.uniform(lower, upper, self.DATA_POINT_NUM),
            }
            for k, (lower, upper) in self.POLUTANT_BOUNDS.items()
        }

    def get_pollutant_concentrations(self, coords: Tuple[float, float]):
        api_response = self._mock_api_response()
        return QAData(
            vars=["pollutant", "timeseries"],
            bindings=[
                {
                    "pollutant": key,
                    "timeseries": list(zip(value["time"], value["values"])),
                }
                for key, value in api_response.items()
            ],
        )

@cache
def get_sgDisperson_agent():
    return SGDispersionAgent()