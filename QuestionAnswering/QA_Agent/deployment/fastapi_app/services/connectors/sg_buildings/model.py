from enum import Enum


class PropertyUsage(Enum):
    BANK = "Bank"
    CLINIC = "Clinic"
    CULTURAL_FACILITY = "CulturalFacility"
    DOMESTIC = "Domestic"
    DRINKING_ESTABLISHMENT = "DrinkingEstablishment"
    EATING_ESTABLISHMENT = "EatingEstablishment"
    EDUCATION = "Education"
    FIRESTATION = "FireStation"
    HOSPITAL = "Hospital"
    HOTEL = "Hotel"
    INDUSTRIAL_FACILITY = "IndustrialFacility"
    MEDICAL = "Medical"
    MULTIRESIDENTIAL = "MultiResidential"
    NONDOMESTIC = "Non-Domestic"
    OFFICE = "Office"
    PHARMACY = "Pharmacy"
    POLICE_STATION = "PoliceStation"
    RELIGIOUS_FACILITY = "ReligiousFacility"
    RETAIL_ESTABLISHMENT = "RetailEstablishment"
    SCHOOL = "School"
    SINGLE_RESIDENTIAL = "SingleResidential"
    SPORTS_FACILITY = "SportsFacility"
    TRANSPORT_FACILITY = "TransportFacility"
    UNIVERSITY = "University"


class BuildingAttrKey(Enum):
    HEIGHT = "Height"
    FOOTPRINT = "FootPrint"