from flask import abort
import pandas as pd
from agent.exposure.intersect import Intersect


def exposure_calculation(query_id:str, algorithm: str) -> pd.DataFrame:
    if algorithm == 'intersect':
        return Intersect(query_id).intersect()
    else:
        abort(400, description="Unsupported calculation algorithm method")