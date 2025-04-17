from flask import abort
import pandas as pd
from agent.exposure.intersect import Intersect
from agent.utils.table_name_helper import TableNameHelper


def exposure_calculation(table_name_helper:TableNameHelper, algorithm: str) -> pd.DataFrame:
    if algorithm == 'intersect':
        return Intersect(table_name_helper).intersect()
    else:
        abort(400, description="Unsupported calculation algorithm method")