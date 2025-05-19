from flask import abort
import pandas as pd
from agent.exposure.intersect import Intersect
from agent.utils.table_name_helper import QueryIdHelper


def exposure_calculation(query_id:QueryIdHelper, algorithm: str) -> pd.DataFrame:
    if algorithm == 'intersect':
        return Intersect(query_id).intersect()
    else:
        abort(400, description="Unsupported calculation algorithm method")