#This probably is better formalized as an owl
#there is a standard format though
#better have a converting module to convert our rather orthordox way of datatype definition
import rdflib
class UnitConverter():

    map = {}
    map["http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#MW"] = 1000000

    @classmethod
    def unitConvert(cls, unit,value):
        if unit in cls.map.keys():
            print(value,value.value, dir(value))
            v = float(value.value) * cls.map[unit]
            return rdflib.term.Literal(v)
        return value






if __name__ == '__main__':
    print(UnitConverter.map)
    print(UnitConverter.unitConvert("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#MW", 4 ))


