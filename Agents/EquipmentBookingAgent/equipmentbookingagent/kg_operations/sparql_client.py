# The purpose of this module is to provide utility functions
# to interact with the knowledge graph
#============================================================
from datetime import datetime
from typing import Tuple
from rdflib import Graph, Literal, URIRef
from rdflib.namespace import RDF
from rdflib import XSD
from pathlib import Path
import collections
import requests
from requests import status_codes
import uuid
import time
import os

from pyderivationagent.kg_operations import PySparqlClient
from pyderivationagent.data_model import *
from equipmentbookingagent.data_model import *

from py4jps import agentlogging
logger = agentlogging.get_logger('dev')


class EquipmentBookingSparqlClient(PySparqlClient):

    def get_all_bookable_equipment_iri(self):
        query = f"""SELECT ?eq WHERE {{ ?eq <{OAM_HASBOOKINGSYSTEM}> ?bs . }}"""
        response = self.performQuery(query)
        return [list(res.values())[0] for res in response]
    
    def get_all_bookable_equipment(self):
        query = f"""{PREFIX_RDFS} {PREFIX_RDF} SELECT ?bs ?eqIri ?eqInventoryId ?eqLabel ?eqManufacturer ?eqSupplier ?eqLocation ?eqType WHERE {{ 
                        ?eqIri <{OAM_HASBOOKINGSYSTEM}> ?bs ;
                                rdfs:label ?eqLabel ;
                                rdf:type ?eqType .
                        OPTIONAL {{ ?eqIri <{OAM_HASITEMINVENTORYIDENTIFIER}> ?eqInventoryId . }} 
                        OPTIONAL {{ ?eqIri <{OAM_ISMANUFACTUREDBY}>/<{OMG_HASNAME}>/rdfs:label ?eqManufacturer . }}
                        OPTIONAL {{ ?eqIri <{OAM_ISSUPPLIEDBY}>/<{OMG_HASNAME}>/rdfs:label ?eqSupplier . }}
                        OPTIONAL {{ ?eqIri ^<{BOT_CONTAINSELEMENT}> ?eqLocation. }}
                    }}
                    ORDER BY ASC(?eqLabel)"""
        response = self.performQuery(query)
        equipment_list = list()
        for res in response:
            res['eqManufacturer'] = 'unknown' if not('eqManufacturer' in res) else res['eqManufacturer']
            res['eqSupplier'] = 'unknown' if not('eqSupplier' in res) else res['eqSupplier']
            res['eqLocation'] = 'unknown' if not('eqLocation' in res) else res['eqLocation']
            res['eqInventoryId'] = 'unknown' if not('eqInventoryId' in res) else res['eqInventoryId']
            res['eqType'] = trimIRI(res['eqType'])

            equipment_list.append(Equipment(
                instance_iri=res['eqIri'],
                clz=res['eqType'],
                label=res['eqLabel'],
                hasItemInventoryIdentifier=res['eqInventoryId'],
                hasBookingSystem=BookingSystem(instance_iri=res['bs']),
                isManufacturedBy=res['eqManufacturer'],
                isSuppliedBy=res['eqSupplier'],
                isLocatedIn=res['eqLocation']
            ))
        return equipment_list
    
    def get_all_users(self):
        query = f"""{PREFIX_RDFS} SELECT ?userIri ?userName WHERE {{
             ?userIri a <{FIBO_PERSON}> ;
                    <{OMG_HASNAME}>/rdfs:label  ?userName .
             }}
             ORDER BY ASC(?userName)"""
        response = self.performQuery(query)
        user_list = list()
        for res in response:
            user_list.append(Fibo_Person(
                instance_iri=res['userIri'],
                name=res['userName']
            ))
        return user_list

    def get_all_bookings_in_system(self, booking_system_iri: str):
        booking_system_iri = trimIRI(booking_system_iri)
        query = f"""SELECT ?booking WHERE {{ <{booking_system_iri}> <{OAM_HASBOOKING}>  ?booking . }}"""
        response = self.performQuery(query)
        return [list(res.values())[0] for res in response]
    
    def get_all_conflicting_bookings(self, booking_system_iri: str, bookingStart: int, bookingEnd: int):
        booking_system_iri = trimIRI(booking_system_iri)
        query = f"""SELECT ?booking WHERE {{
            <{booking_system_iri}> <{OAM_HASBOOKING}> ?booking .
            ?booking <{OAM_HASBOOKINGPERIOD}>/<{TIME_HASBEGINNING}>/<{TIME_INTIMEPOSITION}>/<{TIME_NUMERICPOSITION}> ?bookingStart ;
                    <{OAM_HASBOOKINGPERIOD}>/<{TIME_HASEND}>/<{TIME_INTIMEPOSITION}>/<{TIME_NUMERICPOSITION}> ?bookingEnd .
            FILTER ( ( ?bookingEnd >= {bookingStart} && ?bookingEnd <= {bookingEnd} ) || ( ?bookingStart <= {bookingEnd} && ?bookingStart >= {bookingStart}) || (?bookingStart < {bookingStart} && ?bookingEnd > {bookingEnd}) )
         }}"""
        response = self.performQuery(query)
        return [list(res.values())[0] for res in response]

    def create_technicalystem_of_devices(self, equipment_iris: list, system_label):
        g=Graph()
        namespace_iri = getNameSpace(equipment_iris[0])
        system_iri=initialiseInstanceIRI(namespace_iri,ONTOCAPE_TECHNICALSYSTEM)
        g.add((URIRef(system_iri), RDF.type, URIRef(ONTOCAPE_TECHNICALSYSTEM)))

        composed_string = ""
        for equipment_iri in equipment_iris:
            equipment_iri = trimIRI(equipment_iri)
            composed_string = composed_string + "<" + OTS_COMPOSEDOF + "> <" + equipment_iri + "> ; "
            g.add((URIRef(system_iri), URIRef(OTS_COMPOSEDOF), URIRef(equipment_iri)))

        update = f"""{PREFIX_RDF} {PREFIX_RDFS} INSERT DATA {{
            <{system_iri}> rdf:type <{ONTOCAPE_TECHNICALSYSTEM}> ;
                {composed_string} 
                rdfs:label "{system_label}" .
        }}"""
        self.performUpdate(update)
        logger.info(f"Created technical system {system_iri}.")
        return g


    def create_booking_system_for_equipment(self, equipment_iri: str, booking_system_label: str):
        g=Graph()
        equipment_iri = trimIRI(equipment_iri)
        namespace_iri = getNameSpace(equipment_iri)

        booking_system_iri = initialiseInstanceIRI(namespace_iri, OAM_BOOKINGSYSTEM)
        g.add((URIRef(booking_system_iri), RDF.type, URIRef(OAM_BOOKINGSYSTEM)))

        update = f"""{PREFIX_RDF} {PREFIX_RDFS} INSERT DATA {{
            <{booking_system_iri}> rdf:type <{OAM_BOOKINGSYSTEM}> ;
                rdfs:label "{booking_system_label}" .
            <{equipment_iri}> <{OAM_HASBOOKINGSYSTEM}> <{booking_system_iri}> .
        }}"""
        self.performUpdate(update)
        logger.info(f"Booking system {booking_system_iri} created for equipment {equipment_iri}.")
        return g

    def create_booking_within_system(self, booking_system_iri: str, booker_person_iri: str, booking_start: str, booking_end: str):
        g = Graph()
        booking_system_iri = trimIRI(booking_system_iri)
        booker_person_iri = trimIRI(booker_person_iri)
        namespace_iri = getNameSpace(booking_system_iri)

        booking_iri = initialiseInstanceIRI(namespace_iri, OAM_BOOKING)
        g.add((URIRef(booking_iri), RDF.type, URIRef(OAM_BOOKING)))

        start_pos_iri = initialiseInstanceIRI(namespace_iri, TIME_TIMEPOSITION)
        g.add((URIRef(start_pos_iri), RDF.type, URIRef(TIME_TIMEPOSITION)))
        start_iri = initialiseInstanceIRI(namespace_iri, TIME_INSTANT)
        g.add((URIRef(start_iri), RDF.type, URIRef(TIME_INSTANT)))

        end_pos_iri = initialiseInstanceIRI(namespace_iri, TIME_TIMEPOSITION)
        g.add((URIRef(end_pos_iri), RDF.type, URIRef(TIME_TIMEPOSITION)))
        end_iri = initialiseInstanceIRI(namespace_iri, TIME_INSTANT)
        g.add((URIRef(end_iri), RDF.type, URIRef(TIME_INSTANT)))

        period_iri = initialiseInstanceIRI(namespace_iri, TIME_INTERVAL)
        g.add((URIRef(period_iri), RDF.type, URIRef(TIME_INTERVAL)))

        # TODO figure out time zone conversion
        update = f"""{PREFIX_RDF} INSERT DATA {{
            <{start_pos_iri}> rdf:type <{TIME_TIMEPOSITION}> ;
                <{TIME_HASTRS}> "{DBPEDIA_UNIX}" ;
                <{TIME_NUMERICPOSITION}> {booking_start} .
            <{start_iri}> rdf:type <{TIME_INSTANT}> ;
                <{TIME_INTIMEPOSITION}> <{start_pos_iri}> .

            <{end_pos_iri}> rdf:type <{TIME_TIMEPOSITION}> ;
                <{TIME_HASTRS}> "{DBPEDIA_UNIX}" ;
                <{TIME_NUMERICPOSITION}> {booking_end} .
            <{end_iri}> rdf:type <{TIME_INSTANT}> ;
                <{TIME_INTIMEPOSITION}> <{end_pos_iri}> .

            <{period_iri}> rdf:type <{TIME_INTERVAL}> ;
                <{TIME_HASBEGINNING}> <{start_iri}> ;
                <{TIME_HASEND}> <{end_iri}> .

            <{booking_iri}> rdf:type <{OAM_BOOKING}> ;
                <{OAM_HASBOOKER}> <{booker_person_iri}> ;
                <{OAM_HASBOOKINGPERIOD}> <{period_iri}> .
            <{booking_system_iri}> <{OAM_HASBOOKING}> <{booking_iri}> .}}"""
        self.performUpdate(update)
        logger.info(f"Booking {booking_iri} was added to booking system {booking_system_iri}.")
        #return g
        return booking_iri

    

    def remove_booking_from_system (self, booking_iri: str):
        booking_iri = trimIRI(booking_iri)
        update = f"""{PREFIX_RDF} DELETE WHERE{{
            ?bookingsystem <{OAM_HASBOOKING}> <{booking_iri}> .
            <{booking_iri}> ?bookingpred ?bookingobj ;
                <{OAM_HASBOOKINGPERIOD}> ?period .
            ?period ?periodpred ?periodobj ;
                rdf:type <{TIME_INTERVAL}> .
            ?periodobj <{TIME_INTIMEPOSITION}> ?timepos ;
                rdf:type <{TIME_INSTANT}> .
            ?timepos ?timepospred ?timeposobj .
        }}"""
        self.performUpdate(update)
        logger.info(f"Booking {booking_iri} was removed.")


    def modify_booking_in_system (self, booking_iri: str, booker_iri: str, booker_person_iri: str, booking_start: datetime, booking_end: datetime):
        booking_iri = trimIRI(booking_iri)
        booker_iri = trimIRI(booker_iri)

        update = f""" DELETE {{
            <{booking_iri}> <{OAM_HASBOOKER}> ?booker .
            ?startpos <{TIME_NUMERICPOSITION}> ?starttime .
            ?endpos <{TIME_NUMERICPOSITION}> ?endtime .
        }} INSERT {{
            <{booking_iri}> <{OAM_HASBOOKER}> <{booker_iri}> .
            ?startpos <{TIME_NUMERICPOSITION}> {booking_start.timestamp()} .
            ?endpos <{TIME_NUMERICPOSITION}> {booking_end.timestamp()} .
        }} WHERE {{
            <{booking_iri}> <{OAM_HASBOOKER}> ?booker ;
                <{OAM_HASBOOKINGPERIOD}>/<{TIME_HASBEGINNING}>/<{TIME_INTIMEPOSITION}> ?startpos ;
                <{OAM_HASBOOKINGPERIOD}>/<{TIME_HASEND}>/<{TIME_INTIMEPOSITION}> ?startpos .
            ?startpos <{TIME_NUMERICPOSITION}> ?starttime .
            ?endpos <{TIME_NUMERICPOSITION}> ?endtime .
        }}"""
        self.performUpdate(update)
        logger.info(f"Booking {booking_iri} was updated.")
