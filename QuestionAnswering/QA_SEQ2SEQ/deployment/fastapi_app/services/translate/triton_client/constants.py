import os


TRITON_ENDPOINT = os.getenv("TRITON_ENDPOINT", "localhost:8000")
print("Triton endpoint: " + TRITON_ENDPOINT)