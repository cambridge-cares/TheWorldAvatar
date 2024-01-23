import argparse
import json
import time
from typing import List, Optional
import errno
import os
import signal
import functools

from SPARQLWrapper import SPARQLExceptions
from tqdm import tqdm

from core.kg_client import KgClient


class TimeoutError(Exception):
    pass


def timeout(seconds: int, error_message=os.strerror(errno.ETIME)):
    def decorator(func):
        def _handle_timeout(signum, frame):
            raise TimeoutError(error_message)

        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            signal.signal(signal.SIGALRM, _handle_timeout)
            signal.alarm(seconds)
            try:
                result = func(*args, **kwargs)
            finally:
                signal.alarm(0)
            return result

        return wrapper

    return decorator


def get_answer_data(sparql_endpoint: str, data: List[dict]):
    kg_client = KgClient(sparql_endpoint)

    @timeout(20)
    def _query(query: str):
        return kg_client.query(query, limit=100)

    def _exec_query(query: Optional[str]):
        latency = None
        is_query_malformed = False

        if query is None:
            is_query_malformed = True
        else:
            try:
                start = time.time()
                _query(query)
                latency = time.time() - start
            except SPARQLExceptions.QueryBadFormed:
                is_query_malformed = True
            except TimeoutError:
                latency = -1

        return dict(
            latency=latency,
            is_query_malformed=is_query_malformed,
        )

    data_out = []
    for datum in tqdm(data):
        result = _exec_query(datum["prediction_verbose"])
        result["id"] = datum["id"]
        data_out.append(result)

    return data_out


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("input_path", type=str)
    parser.add_argument("output_path", type=str)
    parser.add_argument("--sparql_endpoint", type=str, required=True)
    args = parser.parse_args()

    with open(args.input_path, "r") as f:
        data = json.load(f)

    answer_data = get_answer_data(args.sparql_endpoint, data)

    with open(args.output_path, "w") as f:
        json.dump(answer_data, f, indent=4)


if __name__ == "__main__":
    main()
