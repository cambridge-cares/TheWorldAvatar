from functools import cache
from typing import Annotated

from fastapi import Depends
from redis import Redis

from config import AppSettings, get_app_settings


@cache
def get_redis_client(settings: Annotated[AppSettings, Depends(get_app_settings)]):
    return Redis(host=settings.redis.host, decode_responses=True)


def does_index_exist(redis_client: Redis, index_name: str):
    try:
        if redis_client.ft(index_name).info():
            return True
        return False
    except:
        return False
