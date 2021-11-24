from a_kg_overview.analytics_output import analytics1_csv
from a_kg_overview.analytics_operations import assemblyModelGroups
from a_kg_overview.analytics_operations import mopsoverview

def kgoverview():
    listofMOPs = mopsoverview()
    uniques = assemblyModelGroups(listofMOPs)
    analytics1_csv(uniques[0])
    return uniques