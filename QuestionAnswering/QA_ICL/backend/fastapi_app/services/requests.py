from typing import Type, TypeVar

from pydantic import BaseModel
import requests


T = TypeVar("T", bound=BaseModel)


def request_get_obj(url: str, params: dict, response_type: Type[T]):
    res = requests.get(url, params=params)
    res.raise_for_status()
    return response_type.model_validate_json(res.text)
