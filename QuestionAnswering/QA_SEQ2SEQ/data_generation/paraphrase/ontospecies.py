import argparse
import csv
import json
import os

from tqdm import tqdm
import pandas as pd

from .base import Paraphraser


class OSParaphraser(Paraphraser):
    def __init__(self):
        super().__init__(openai_kwargs=dict(temperature=0.5, frequency_penalty=1))

    def paraphrase(self, text: str):
        entity_placeholders = ["methanol", "ethanol", "propanol", "butanol", "pentanol", "hexanol", "heptanol", "octanol", "nonanol"]
        entity_placeholders = [x for x in entity_placeholders if x not in text]
        entity_actuals = []

        while True:
            idx_entity_start = text.find("<entity>")
            if idx_entity_start < 0:
                break

            idx_entity_end = text.find("</entity>", idx_entity_start)
            if idx_entity_end < 0:
                raise ValueError("Missing closing entity tag: " + text)
            idx_entity_end += len("</entity>")

            entity = text[idx_entity_start:idx_entity_end]
            text = text.replace(entity, entity_placeholders[len(entity_actuals)])
            entity_actuals.append(entity)

        try_limit = 3
        try_num = 0
        paraphrases = None

        while try_num < try_limit:
            if try_num > 0:
                print("Try {num} (started from 0)".format(num=try_num))

            paraphrases = super().paraphrase(text)

            if len(entity_actuals) == 0:  # no placeholders to replace
                break

            will_retry = False
            for i in range(len(paraphrases)):
                for placeholder, actual in zip(entity_placeholders, entity_actuals):
                    paraphrase = paraphrases[i].replace(placeholder, actual)
                    if paraphrase == paraphrases[i]:
                        will_retry = True
                        break
                    paraphrases[i] = paraphrase
                if will_retry:
                    break

            if will_retry:
                try_num += 1
                continue
            else:
                break

        if paraphrases is None:
            raise Exception("No calls have been made to OpenAI for paraphrasing.")

        if try_num == try_limit:
            raise Exception("Unacceptable paraphrasing results: " + paraphrases)

        return paraphrases
