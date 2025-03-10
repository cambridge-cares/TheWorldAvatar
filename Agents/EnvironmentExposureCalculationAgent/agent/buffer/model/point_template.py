from pydantic import BaseModel
from pydantic.dataclasses import dataclass, model_dump
from utils.string_utils import replace_with_dict


@dataclass
class PointTemplate(BaseModel):
    lng_start: float
    lng_end: float
    lng_step: float
    lat_start: float
    lat_end: float
    lat_step: float

    def get_sql(self):
        with open("/buffer/script/regularly_sample.sql", "r") as file:
            sql = file.read()
            return replace_with_dict(sql, model_dump())
