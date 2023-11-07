import argparse
from typing import Optional

from .base import Paraphraser
from .ontospecies import OSParaphraser


def get_paraphraser(domain: Optional[str]):
    if domain == "ontospecies":
        return OSParaphraser()
    else:
        return Paraphraser()

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("filepath")
    parser.add_argument("--domain", type=str, required=False)
    args = parser.parse_args()

    paraphraser = get_paraphraser(args.domain)
    paraphraser.paraphrase_from_file(args.filepath)
