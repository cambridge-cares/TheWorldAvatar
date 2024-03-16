from redis import Redis


def does_index_exist(redis_client: Redis, index_name: str):
    try:
        if redis_client.ft(index_name).info():
            return True
        return False
    except:
        return False
