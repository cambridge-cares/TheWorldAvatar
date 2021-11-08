from analytics1.analytics_operations import kgoverview
from analytics1.analytics_output import analytics1_csv
from analytics1.analytics_operations import assembliesOverview
from radius_1.assembly_operations import assemblyModelGroups
from radius_1.assembly_radius1 import searchRadius1

# Running a querry to get all mops and save this as a list of MOPs

def workflow(x):
    ### Level 1 is starting with kg_analytics
    display = None
    if x == '1':
        display = kganalytics_a()
        analytics1_csv(display)
    if x == '2':
        display = mops_radius1_workflow()
    else:
        kganalytics_b()
        mops_radius2()
    return display

def kganalytics_a():
    """Queries the KG and returns information on the assembly models existing in the KG."""
    listofMOPs = kgoverview()
    uniques = assembliesOverview(listofMOPs)
    return uniques

def mops_radius1_workflow():
    """Projects which new MOPs can be derived using the KG (search radius 1)."""
    listofMOPs = kgoverview()
    uniques = assemblyModelGroups(listofMOPs)
    analytics = searchRadius1(uniques)
    return analytics

def kganalytics_b():
    """Analyses the relationship between different assembly models."""
    pass

def mops_radius2():
    """Projects which new MOPs can be derived using the KG (search radius 2)."""
    pass

