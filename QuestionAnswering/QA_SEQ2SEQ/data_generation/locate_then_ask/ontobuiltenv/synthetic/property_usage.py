from decimal import Decimal
import random
from typing import Tuple

import numpy as np
from constants.namespaces import OBE
from constants.ontobuiltenv import OBE_PROPERTYUSAGE_LABELS
from locate_then_ask.ontobuiltenv.model import OBEPropertyUsage


class OBEPropertyUsageSynthesizer:
    PROPERTY_USAGE_CONCEPTS = [OBE + x for x in OBE_PROPERTYUSAGE_LABELS]

    def make(self) -> Tuple[OBEPropertyUsage, ...]:
        n = random.randrange(1, 4)
        uses = random.sample(self.PROPERTY_USAGE_CONCEPTS, k=n)
        shares = [random.randint(1, 100) for _ in range(n)]
        shares = np.array(shares) / sum(shares)

        assert len(uses) == len(set(uses))

        return tuple(
            OBEPropertyUsage(
                iri="placeholder",
                concept=concept,
                usage_share=Decimal(str(round(share, 2))),
            )
            for concept, share in zip(uses, shares)
        )
