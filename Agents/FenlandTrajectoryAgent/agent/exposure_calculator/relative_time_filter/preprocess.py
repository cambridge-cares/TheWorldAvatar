#!/usr/bin/env python3
"""
Preprocess OpenStreetMap opening_hours data into OntoService TTL.

Configuration (edit values here):
  API_URL       = "https://qlever.cs.uni-freiburg.de/api/osm-planet"
  SPARQL_FILE   = "opening_hours_cam.sparql"
  START_YEAR    = 2013
  END_YEAR      = 2028
  COUNTRY_CODE  = "GB"
  OUTPUT_FILE   = "results/ontoservice_opening_hours.ttl"

Dependencies:
  pip install rdflib requests holidays
"""

import requests, re, holidays
from rdflib import Graph, Namespace, URIRef, Literal
from rdflib.namespace import RDF, RDFS, XSD
from rdflib.term import _toPythonMapping

# disable xsd:time auto-conversion
_toPythonMapping[XSD.time] = lambda s: s

# ---------------------------------------
# Configuration
# ---------------------------------------
API_URL       = "https://qlever.cs.uni-freiburg.de/api/osm-planet"
SPARQL_FILE   = "opening_hours_cam.sparql"
START_YEAR    = 2013
END_YEAR      = 2028
COUNTRY_CODE  = "GB"
OUTPUT_FILE   = "results/ontoservice_opening_hours.ttl"

# ---------------------------------------
# Namespaces
# ---------------------------------------
OS      = Namespace("https://theworldavatar.io/kg/")
FIBO_FD = Namespace("https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/FinancialDates/")
CMNS_DT = Namespace("https://www.omg.org/spec/Commons/DatesAndTimes/")
CMNS_COL= Namespace("https://www.omg.org/spec/Commons/Collections/")
OSMKEY  = Namespace("https://www.openstreetmap.org/wiki/Key:")

WEEKDAYS  = ["Mo","Tu","We","Th","Fr","Sa","Su"]
MONTH_MAP = {'Jan':1,'Feb':2,'Mar':3,'Apr':4,'May':5,'Jun':6,
             'Jul':7,'Aug':8,'Sep':9,'Oct':10,'Nov':11,'Dec':12}

def normalize_time(t):
    m = re.match(r'(\d{1,2}):(\d{2})', t)
    if not m: return None
    hh, mm = m.groups()
    return f"{int(hh):02d}:{mm}"

def parse_rule(text):
    text = text.strip().replace('–','-')
    parts = text.split(None,1)
    date_part = parts[0]
    time_part = parts[1] if len(parts)>1 else ""

    # OFF rules
    if 'off' in time_part.lower():
        if date_part=='PH':
            return {'type':'adhoc_ph','key':'PH_off'}
        m = re.match(r'([A-Za-z]{3})[- ](\d{1,2})', date_part+' '+time_part)
        if m:
            mon,day = m.groups()
            mm = MONTH_MAP.get(mon)
            if mm:
                return {'type':'adhoc_md','key':f"MD_{mon}{int(day):02d}",'month':mm,'day':int(day)}
        return {'type':'unsupported','key':date_part}

    # parse days (comma separated)
    days=[]
    for item in date_part.split(','):
        item=item.strip()
        if '-' in item:
            a,b=item.split('-',1)
            if a in WEEKDAYS and b in WEEKDAYS:
                i1,i2=WEEKDAYS.index(a),WEEKDAYS.index(b)
                if i1<=i2: days+=WEEKDAYS[i1:i2+1]
                else:      days+=WEEKDAYS[i1:]+WEEKDAYS[:i2+1]
        elif item in WEEKDAYS:
            days.append(item)

    if days:
        m = re.search(r'(\d{1,2}:\d{2})\s*-\s*(\d{1,2}:\d{2})', time_part)
        if not m:
            return {'type':'unsupported','key':date_part}
        st_raw,en_raw = m.groups()
        st=normalize_time(st_raw); en=normalize_time(en_raw)
        if not st or not en:
            return {'type':'unsupported','key':date_part}
        # determine key: range vs single
        if days[0]!=days[-1] and len(days)>1:
            key = f"{days[0]}To{days[-1]}"
        else:
            key = days[0]
        return {'type':'weekly','key':key,'days':days,
                'start_time':st,'end_time':en}

    return {'type':'unsupported','key':date_part}

def main():
    # read SPARQL
    query = open(SPARQL_FILE).read()
    resp = requests.get(API_URL,params={'query':query},headers={'Accept':'text/turtle'})
    resp.raise_for_status()

    g = Graph().parse(data=resp.text, format='turtle')
    out = Graph()
    for p,ns in [('twa',OS),('fibo-fnd-dt-fd',FIBO_FD),
                 ('cmns-dt',CMNS_DT),('cmns-col',CMNS_COL),
                 ('osmkey',OSMKEY)]:
        out.bind(p,ns)

    # process each entity
    for entity,oh in g.subject_objects(OSMKEY.opening_hours):
        oh_str = str(oh)
        print(f"Entity {entity} OH: {oh_str}")
        rules = [r for r in oh_str.split(';') if r.strip()]
        parsed = [parse_rule(r) for r in rules]
        for r,pr in zip(rules,parsed):
            print(f"  rule: {r!r} -> {pr}")

        eid = str(entity).rsplit('/',1)[-1]
        base=OS[f"schedule/{eid}"]

        adhocs={}
        for pr in parsed:
            if pr['type']=='weekly':
                uri = URIRef(f"{base}/{pr['key']}")
                out.add((entity,FIBO_FD.hasSchedule,uri))
                out.add((uri,RDF.type,FIBO_FD.RegularSchedule))
                out.add((uri,CMNS_DT.hasStartDate,Literal(f"{START_YEAR}-01-01",datatype=XSD.date)))
                out.add((uri,CMNS_DT.hasEndDate,  Literal(f"{END_YEAR}-12-31",datatype=XSD.date)))
                tp=URIRef(f"{uri}/timeperiod")
                out.add((uri,CMNS_DT.hasTimePeriod,tp))
                out.add((tp,RDF.type,CMNS_DT.ExplicitTimePeriod))
                st_uri, en_uri = URIRef(f"{uri}/startTime"),URIRef(f"{uri}/endTime")
                out.add((tp,CMNS_DT.hasStart,st_uri))
                out.add((tp,CMNS_DT.hasEndTime,en_uri))
                out.add((st_uri,RDF.type,CMNS_DT.TimeOfDay))
                out.add((en_uri,RDF.type,CMNS_DT.TimeOfDay))
                out.add((st_uri,CMNS_DT.hasTimeValue,Literal(pr['start_time'],datatype=XSD.time)))
                out.add((en_uri,CMNS_DT.hasTimeValue,Literal(pr['end_time'],  datatype=XSD.time)))
                for wd in pr['days']:
                    out.add((uri,FIBO_FD.hasRecurrenceInterval,FIBO_FD[wd]))
            elif pr['type'].startswith('adhoc'):
                adhocs[pr['key']]=pr

        # build adhoc schedules
        for key,info in adhocs.items():
            dates=[]
            if info['type']=='adhoc_ph':
                hols=holidays.CountryHoliday(COUNTRY_CODE,years=range(START_YEAR,END_YEAR+1))
                dates=sorted(d.isoformat() for d in hols)
            elif info['type']=='adhoc_md':
                for y in range(START_YEAR,END_YEAR+1):
                    dates.append(f"{y:04d}-{info['month']:02d}-{info['day']:02d}")
            uri=URIRef(f"{base}/{key}")
            out.add((entity,FIBO_FD.hasSchedule,uri))
            out.add((uri,RDF.type,FIBO_FD.AdHocSchedule))
            out.add((uri,RDFS.label,Literal(f"Ad hoc: {key}")))
            for d in dates:
                euri=URIRef(f"{uri}/entry/{d}")
                out.add((uri,CMNS_COL.hasConstituent,euri))
                out.add((euri,RDF.type,FIBO_FD.AdHocScheduleEntry))
                out.add((euri,FIBO_FD.hasDate,Literal(d,datatype=XSD.date)))

    out.serialize(destination=OUTPUT_FILE,format='turtle')
    print(f"Serialized to {OUTPUT_FILE}")

if __name__=='__main__':
    main()
