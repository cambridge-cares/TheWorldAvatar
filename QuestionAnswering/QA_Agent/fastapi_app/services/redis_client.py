from functools import cache
import os

from redis import Redis


@cache
def get_redis_client():
    return Redis(host=os.getenv("REDIS_ENDPOINT", "localhost"), decode_responses=True)
