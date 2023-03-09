from spiral import ronin
import requests
import json
import logging
import rdflib

class GeoAttrFinder():
    def __init__(self, featureSelect, initialDict = None):
        self.dictFile = initialDict
        self.geoNameDict = {}
        if featureSelect == '3F':
            self.featureSelect = self.featureSelect3F
        else:
            raise Exception("feature Select method not defined")

        if initialDict is not None:
            with open(initialDict) as f:
                self.geoNameDict = json.load(f)

    def save(self):
        if self.dictFile is not None:
            with open(self.dictFile, 'w') as f:
                json.dump(self.geoNameDict, f)

    def findExtraGeoAttrSingleOnto(self, onto, leveled=False):
        #add new list to existing list, how?
        map = []

        for idx in range(0, len(onto.individualNames)):#for every individual in onto
            # get name
            instanceName = onto.individualNames[idx]
            country = self.findCountryInfo(onto.valueMap.map[idx])
            if country is not None:
                geoValues = self.getCoordiIfGeoname(instanceName, country)
                #add datatype to geovalues
                #print(geoValues)
                valuesLiteral  = [ ('coordi', rdflib.term.Literal(float(v), datatype=rdflib.namespace.XSD.double)) for v in geoValues]
                map.append(valuesLiteral)
            else:
                map.append(None)
        return map

    '''
    Get all coordinates for geoName tokens contained in the instanceName if it is a geo name
    input instancename, string
    output
    '''
    def getCoordiIfGeoname(self, instanceName,country):
        if instanceName is None or country is None:
            return []
        tokens = ronin.split(instanceName)  #tokenize
        geoVs = []
        for token in tokens:
            if len(token)<=3:
                continue
            token = token.lower()
            if token in self.geoNameDict:  #already in pre-saved
                #geoVs.extend(self.geoNameDict[token])
                coordi = self.featureSelect(self.geoNameDict[token])
                if len(coordi) >= 2:
                    geoVs.extend(coordi)
                else:
                    logging.warning('empty coordinates for token=%s, instanceName=%s', token, instanceName)


            else:
                newVs = self.requestGeonameSingleToken(token, country)
                if newVs is not None and len(newVs) > 0:#found new geonames
                    self.geoNameDict[token] = newVs #add to pre-save
                    geoVs.extend(self.featureSelect(newVs))
        return geoVs


    #TODO: select multiple features (add noise by increase chance of matching?)
    '''
    Feature select by three tiers of trustworthiness: ADM>PPL>others
    input: [(x, y, featurecode),]
    output: (float, float)
    '''
    def featureSelect3F(self, listOfFeature):
        for feature in listOfFeature:
            x,y,fcode = feature
            x = float(x)
            y = float(y)
            if "ADM" in fcode:
                return (x,y)
            elif "PPL" in fcode:
                return (x,y)
        if len(listOfFeature)>0:
            return float(listOfFeature[0][0]), float(listOfFeature[0][1])
        else:
            return ()



    '''
    find if any value in value map is a country name
    input  list of value
    output country string in standard format, or None if not exist
    '''
    def findCountryInfo(self, valuelist):
        country_list = ["Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Anguilla", "Antigua &amp; Barbuda",
                        "Argentina", "Armenia", "Aruba", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain",
                        "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bermuda", "Bhutan",
                        "Bolivia", "Bosnia &amp; Herzegovina", "Botswana", "Brazil", "British Virgin Islands", "Brunei",
                        "Bulgaria", "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Cape Verde", "Cayman Islands",
                        "Chad", "Chile", "China", "Colombia", "Congo", "Cook Islands", "Costa Rica", "Cote D Ivoire",
                        "Croatia", "Cruise Ship", "Cuba", "Cyprus", "Czech Republic", "Denmark", "Djibouti", "Dominica",
                        "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Estonia",
                        "Ethiopia", "Falkland Islands", "Faroe Islands", "Fiji", "Finland", "France",
                        "French Polynesia", "French West Indies", "Gabon", "Gambia", "Georgia", "Germany", "Ghana",
                        "Gibraltar", "Greece", "Greenland", "Grenada", "Guam", "Guatemala", "Guernsey", "Guinea",
                        "Guinea Bissau", "Guyana", "Haiti", "Honduras", "Hong Kong", "Hungary", "Iceland", "India",
                        "Indonesia", "Iran", "Iraq", "Ireland", "Isle of Man", "Israel", "Italy", "Jamaica", "Japan",
                        "Jersey", "Jordan", "Kazakhstan", "Kenya", "Kuwait", "Kyrgyz Republic", "Laos", "Latvia",
                        "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Macau",
                        "Macedonia", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Mauritania",
                        "Mauritius", "Mexico", "Moldova", "Monaco", "Mongolia", "Montenegro", "Montserrat", "Morocco",
                        "Mozambique", "Namibia", "Nepal", "Netherlands", "Netherlands Antilles", "New Caledonia",
                        "New Zealand", "Nicaragua", "Niger", "Nigeria", "Norway", "Oman", "Pakistan", "Palestine",
                        "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal",
                        "Puerto Rico", "Qatar", "Reunion", "Romania", "Russia", "Rwanda", "Saint Pierre &amp; Miquelon",
                        "Samoa", "San Marino", "Satellite", "Saudi Arabia", "Senegal", "Serbia", "Seychelles",
                        "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "South Africa", "South Korea", "Spain",
                        "Sri Lanka", "St Kitts &amp; Nevis", "St Lucia", "St Vincent", "St. Lucia", "Sudan", "Suriname",
                        "Swaziland", "Sweden", "Switzerland", "Syria", "Taiwan", "Tajikistan", "Tanzania", "Thailand",
                        "Timor L'Este", "Togo", "Tonga", "Trinidad &amp; Tobago", "Tunisia", "Turkey", "Turkmenistan",
                        "Turks &amp; Caicos", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "Uruguay",
                        "Uzbekistan", "Venezuela", "Vietnam", "Virgin Islands (US)", "Yemen", "Zambia", "Zimbabwe"]

        #find country info from value lsit, maybe have a list of country as it is fixed, btw, the country is defined as IRI in pp file, how is it extracted?
        for value in valuelist:#convert Literal type to string
            valueStr = value[1].toPython()
            if not isinstance(valueStr,str):
                continue
            for country in country_list:
                if country in valueStr:
                    return country
        return None

    '''
    request from geoNames API and get the coordinate for single token by filtering by country and FCODE
    input: token, country
    output: coordinates if exist
    '''
    def requestGeonameSingleToken(self, token, country):
        FCODE_TAG = "fcode"
        COUNTRY_TAG = "countryName"
        res = requests.get(
            "http://api.geonames.org/search?name_equals=" + token + "&maxRows=50&username=szhang012&type=json")
        gNames = json.loads(res.text)
        coordis = []
        if "totalResultsCount" not in gNames:
            # print(res.text)
            logging.warning('request to Geonames failed for token=%s, reason: %s', token, res.text)
            return None
        if "totalResultsCount" in gNames and gNames["totalResultsCount"] is not 0:
            for geoRecord in gNames["geonames"]:
                if FCODE_TAG in geoRecord and COUNTRY_TAG in geoRecord and "lat" in geoRecord and "lng" in geoRecord:
                    if geoRecord[COUNTRY_TAG].lower() == country.lower():  # same country, ADM kind
                        coordis.append((geoRecord["lat"], geoRecord["lng"], geoRecord[FCODE_TAG]))
        return coordis

if __name__ == "__main__":
    from ontologyWrapper import Ontology
    a=GeoAttrFinder()
    addr = "C:/Users/Shaocong/WORK/ontoMatchData/simMatch/temp/kwltest3.owl"
    onto1 = Ontology(addr)
    b = a.findExtraGeoAttrSingleOnto(onto1)
    print(b)
    print("wait")

