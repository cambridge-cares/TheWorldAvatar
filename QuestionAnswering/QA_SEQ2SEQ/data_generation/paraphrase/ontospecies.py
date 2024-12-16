import random
from typing import Iterable, List, Tuple

from .base import Paraphraser


class OSParaphraser(Paraphraser):
    ENTITY_PLACEHOLDERS = [
        "[methanol]",
        "[ethanol]",
        "[propanol]",
        "[butanol]",
        "[pentanol]",
        "[hexanol]",
        "[heptanol]",
        "[octanol]",
        "[nonanol]",
    ]

    def _extract_species_labels(self, text: str):
        labels: List[str] = []

        idx = 0
        while True:
            idx_entity_start = text.find("<entity>", idx)
            if idx_entity_start < 0:
                break

            idx_entity_end = text.find("</entity>", idx_entity_start)
            if idx_entity_end < 0:
                raise ValueError("Missing closing entity tag: " + text)
            idx_entity_end += len("</entity>")

            entity = text[idx_entity_start:idx_entity_end]
            labels.append(entity)

            idx = idx_entity_end + 1

        return tuple(labels)

    def _paraphrase(self, text: str, species_labels: Tuple[str], entity_placeholders: Iterable[str]):
        for label, placeholder in zip(species_labels, entity_placeholders):
            text = text.replace(label, placeholder)

        paraphrases = super().paraphrase(text)
        if not species_labels:
            return paraphrases

        processed_paraphrases = []
        for paraphrase in paraphrases:
            valid = True
            for placeholder, actual in zip(entity_placeholders, species_labels):
                processed_paraphrase = paraphrase.replace(placeholder, actual)
                if processed_paraphrase == paraphrase:
                    valid = False
                    break
            if valid:
                processed_paraphrases.append(processed_paraphrase)
        return processed_paraphrases

    def paraphrase(self, text: str):
        species_labels = self._extract_species_labels(text)
        tag2brac = {
            l: "[{x}]".format(x=l[len("<entity>"):-len("</entity>")]) 
            for l in species_labels
        }
        _text = text
        for tag, brac in tag2brac.items():
            _text = _text.replace(tag, brac)
        
        _paraphrases = super().paraphrase(_text)
        paraphrases = []
        for p in _paraphrases:
            for tag, brac in tag2brac.items():
                p = p.replace(brac, tag)
            paraphrases.append(p)

        if len(paraphrases) < 3:
            entity_placeholders = [x for x in self.ENTITY_PLACEHOLDERS if x not in text]

            try_num = 0
            while len(paraphrases) < 3 and try_num < self.TRY_LIMIT:
                random.shuffle(entity_placeholders)
                paraphrases.extend(self._paraphrase(text, species_labels, entity_placeholders))
                try_num += 1

            if len(paraphrases) < 3:
                print(
                    "Unable to generate 3 faithful paraphrases.\nOriginal text: {og}\nParaphrases: {p}".format(
                        og=text, p=paraphrases
                    )
                )

        return paraphrases
