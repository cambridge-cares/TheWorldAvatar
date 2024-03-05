from abc import ABC, abstractmethod
import json
import os
from typing import Any, Dict, Generic, List, TypeVar

from redis import Redis

K = TypeVar("K")
V = TypeVar("V")

class ICache(ABC, Generic[K, V]):
    @abstractmethod
    def set(self, key: K, value: V) -> None:
        pass

    @abstractmethod
    def get(self, key: K) -> V:
        pass

    @abstractmethod
    def exists(self, key: V) -> bool:
        pass


class DictCache(ICache[K, V]):
    def __init__(self):
        self.cache: Dict[K, V] = dict()

    def set(self, key: K, value: V):
        self.cache[key] = value

    def get(self, key: K):
        return self.cache.get(key)

    def exists(self, key: K):
        return key in self.cache


class RedisCache(ICache[K, V]):
    def __init__(self):
        self.client = Redis(host=os.getenv("REDIS_ENDPOINT"), port=6379, decode_responses=True)

    def set(self, key: K, value: V):
        self.client.set(key, json.dumps(value))

    def get(self, key: K) -> V:
        # TODO: perform type-checks
        return json.loads(self.client.get(key))

    def exists(self, key: K):
        return self.client.exists(key) > 0
