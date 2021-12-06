from a_kg_overview.analytics_output import kgoverview_csv
from a_kg_overview.analytics_operations import assemblyModelGroups
from a_kg_overview.analytics_operations import mopsoverview

def kgoverview():
    """This function returns overview on the available assembly Models in the KG
    it also returns back two lists needed for the overall workflow, namely
    [0] = List_AM_GBU and [1] = List_preR2"""
    listofMOPs = mopsoverview()
    uniques = assemblyModelGroups(listofMOPs)
    kgoverview_csv(uniques[0])
    return uniques