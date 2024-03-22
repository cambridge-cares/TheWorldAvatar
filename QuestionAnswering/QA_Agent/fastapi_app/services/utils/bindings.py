from collections import defaultdict
from typing import Iterable, List, Tuple

from services.core.labels_store import IRIWithLabels


def agg_iri_label_pairs(iri_label_pairs: Iterable[Tuple[str, str]]):
    iri2labels = defaultdict(list)
    for iri, label in iri_label_pairs:
        iri2labels[iri].append(label)
    return [IRIWithLabels(IRI=iri, labels=labels) for iri, labels in iri2labels.items()]
