from typing import Annotated
from fastapi import Depends
from pydantic import TypeAdapter
from redis import Redis

from model.structured_answer import QARequestArtifact
from services.redis import get_redis_client


class QARequestArtifactStore:
    QA_REQUESTS_COUNTER_KEY = "qaRequestsCounter"
    QA_REQUESTS_KEY_PREFIX = "qaRequests:"
    EXPIRE_DURATION = 5 * 60

    def __init__(self, redis_client: Redis):
        self.redis_client = redis_client
        self.adapter = TypeAdapter(QARequestArtifact)

    def save(self, artifact: QARequestArtifact):
        id = str(self.redis_client.incr(self.QA_REQUESTS_COUNTER_KEY))
        self.redis_client.set(
            self.QA_REQUESTS_KEY_PREFIX + id,
            artifact.model_dump_json(),
            ex=self.EXPIRE_DURATION,
        )
        return id

    def load(self, id: str):
        artifact = self.redis_client.get(self.QA_REQUESTS_KEY_PREFIX + id)
        if artifact is None:
            return None
        return self.adapter.validate_json(artifact)


def get_qaReq_artifactStore(redis_client: Annotated[Redis, Depends(get_redis_client)]):
    return QARequestArtifactStore(redis_client=redis_client)
