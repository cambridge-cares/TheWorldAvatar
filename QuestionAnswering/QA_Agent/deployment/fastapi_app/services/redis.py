from functools import cache
import os

from redis import Redis


@cache
def get_redis_client():
    return Redis(host=os.getenv("REDIS_HOST", "localhost"), decode_responses=True)

def does_index_exist(redis_client: Redis, index_name: str):
    try:
        if redis_client.ft(index_name).info():
            return True
        return False
    except:
        return False
