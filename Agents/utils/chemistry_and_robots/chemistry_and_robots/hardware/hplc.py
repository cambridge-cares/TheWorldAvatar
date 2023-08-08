from chemistry_and_robots.data_model import *
from io import StringIO
import pandas as pd
import numpy as np
import uuid


def read_raw_hplc_report_file(hplc_report_iri: str, file_path: str, filename_extension: str) -> list:
    """This method extracts the chromatogram points from the given raw HPLC report."""
    if filename_extension == XLSFILE_EXTENSION:
        # TODO this is a shortcut that we hardcoded the sheet_name and columns
        retention_time = np.array(pd.read_excel(file_path, sheet_name='IntResults1', usecols="E"))
        peak_area = np.array(pd.read_excel(file_path, sheet_name='IntResults1', usecols="F"))
    elif filename_extension == TXTFILE_EXTENSION:
        # TODO this is also a shortcut that hardcodes the required information to process the raw report
        start_mark = "Peak#"
        end_mark = "[Compound Results(AD2)]"

        hplc_report = open(file_path, 'r').read()
        report_str = hplc_report[(hplc_report.find(start_mark)):(hplc_report.find(end_mark) - 2)]
        hplc_table = pd.read_csv(StringIO(report_str), sep='\t')

        peak_area = hplc_table['Area'].to_numpy()
        retention_time = hplc_table['R.Time'].to_numpy()
    else:
        raise NotImplementedError(f"Handling raw HPLC report that has <{filename_extension}> as filename extension is not implemented yet.")

    if len(retention_time) != len(peak_area):
        raise Exception("""HPLC raw report is not processed correctly, the retention time and peak area doesn't match. 
        Processed retention time: %s. Processed peak area: %s""" % (str(retention_time), str(peak_area)))

    list_point = []
    for i in range(len(retention_time)):
        rt_instance = RetentionTime(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            namespace_for_init=getNameSpace(hplc_report_iri),
            hasValue=OM_Measure(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=getNameSpace(hplc_report_iri),
                hasUnit=OM_MINUTETIME,
                hasNumericalValue=float(retention_time[i])
            )
        )
        pa_instance = PeakArea(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            namespace_for_init=getNameSpace(hplc_report_iri),
            hasValue=OM_Measure(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=getNameSpace(hplc_report_iri),
                hasUnit=OM_MILLIABSORBANCEUNITMULTIPLIESMINUTE,
                hasNumericalValue=float(peak_area[i])
            )
        )
        list_point.append({ONTOHPLC_RETENTIONTIME:rt_instance, ONTOHPLC_PEAKAREA: pa_instance})

    return list_point
