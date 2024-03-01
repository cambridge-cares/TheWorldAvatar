from functools import cache

from openai import OpenAI

@cache
def get_openai_client():
    return OpenAI()