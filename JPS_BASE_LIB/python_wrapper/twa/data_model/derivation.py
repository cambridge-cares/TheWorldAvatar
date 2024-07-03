import ast
from typing import Type, List, Union
from rdflib import Graph, RDF, Literal

from twa.data_model.base_ontology import BaseClass

class Derivation():
    """This is a wrapper class for uk.ac.cam.cares.jps.base.derivation.Derivation.java.
    Only two methods are provided here for developers to use in their python code when
    handling the Derivation object returned when creating synchronous derivation for
    new information. The methods are getIri() and getBelongsToIris(outputRdfType).
    All other methods in Java can be accessed via self.derivation.javaMethod(args).
    """

    def __init__(
        self,
        derivation_java
    ):
        self.derivation = derivation_java

    def getIri(self) -> str:
        """Returns the IRI of the Derivation instance.

        Returns:
            str: IRI of the Derivation instance
        """
        return self.derivation.getIri()

    def getBelongsToIris(self, outputRdfType: str) -> List[str]:
        """Returns the IRIs of the entities that belongsTo the Derivation instance.

        Args:
            outputRdfType (str): IRI of the rdf:type of the entities that belongsTo the Derivation instance

        Returns:
            List[str]: List of IRIs of the entities that belongsTo the Derivation instance
        """
        return self.derivation.getBelongsToIris(outputRdfType)


class DerivationInputs():
    """This is a warpper class for uk.ac.cam.cares.jps.base.derivation.DerivationInputs.java.
    All methods provided here are wrapped around the corresponding methods in the java class,
    as references for developers to handle derivations within python derivation agents."""

    def __init__(self, derivationInputs) -> None:
        self.derivation_inputs = derivationInputs

    def getDerivationIRI(self):
        return self.derivation_inputs.getDerivationIRI()

    def getInputs(self):
        return ast.literal_eval(str(self.derivation_inputs.getInputs()))

    def getIris(self, rdfType):
        iris = self.derivation_inputs.getIris(rdfType)
        return list(iris) if iris is not None else None

    def get_inputs_ogm_by_rdf_type(
        self,
        rdf_type: str,
        sparql_client,
        recursive_depth: int = 0
    ) -> List[BaseClass]:
        return BaseClass.pull_from_kg(
            iris=self.getIris(rdf_type),
            sparql_client=sparql_client,
            recursive_depth=recursive_depth
        )

    def get_inputs_ogm(
        self,
        clz: Type[BaseClass],
        sparql_client,
        recursive_depth: int = 0
    ) -> List[BaseClass]:
        return clz.pull_from_kg(
            iris=self.getIris(clz.get_rdf_type()),
            sparql_client=sparql_client,
            recursive_depth=recursive_depth
        )

    def get_inputs_ogm_assume_one(
        self,
        clz: Type[BaseClass],
        sparql_client,
        recursive_depth: int = 0
    ) -> BaseClass:
        objects = self.get_inputs_ogm(clz=clz, sparql_client=sparql_client, recursive_depth=recursive_depth)
        if len(objects) != 1:
            raise Exception(f"""Input type {clz.get_rdf_type()} assumed one for derivation {self.getDerivationIRI()},
                encounterred {len(objects)}: {' '.join([o.triples() for o in objects])}""")
        return next(iter(objects))


class DerivationOutputs():
    """This is a warpper class for uk.ac.cam.cares.jps.base.derivation.DerivationOutputs.java.
    All methods provided here are wrapped around the relevant methods in the java class,
    as references for developers to handle derivations within python derivation agents."""

    def __init__(self, derivationOutputs) -> None:
        self.derivation_outputs = derivationOutputs

    def createNewEntity(self, iri, rdfType):
        self.derivation_outputs.createNewEntity(iri, rdfType)

    def createNewEntityWithBaseUrl(self, baseUrl, rdfType):
        """This method initialises an IRI with the given baseUrl and rdfType, adds the new entity to derivation outputs, then returns the initialised IRI."""
        return self.derivation_outputs.createNewEntityWithBaseUrl(baseUrl, rdfType)

    def addTriple(self, s, p, o):
        """Only one addTriple is provided here, the two functions taking TriplePattern is NOT provided for simplicity of java-python data structure conversion."""
        self.derivation_outputs.addTriple(s, p, o)

    def addLiteral(self, s, p, o):
        """Only one addLiteral is provided here as the correct method to use will be decided by java automatically."""
        self.derivation_outputs.addLiteral(s, p, o)

    def addLiteralWithDataType(self, s, p, o, dataType):
        """This method corresponds to addLiteral(String, String, String, String) in DerivationOutputs.java, but renamed in python due to limitations of overloading in python."""
        self.derivation_outputs.addLiteral(s, p, o, dataType)

    def addGraph(self, g: Graph):
        """Add a whole rdflib.Graph to derivation outputs."""
        for s, p, o in g:
            try:
                if p.toPython() == RDF.type.toPython():
                    self.createNewEntity(s.toPython(), o.toPython())
                else:
                    # add data properties
                    if isinstance(o, Literal):
                        if isinstance(o.toPython(), Literal):
                            # if o.toPython() is a Literal instance, then it's returning itself
                            # this means the datatype provided to initialise o is NOT presented in rdflib.term.XSDToPython
                            # therefore, it cannot be cast to native python type
                            # but str(o) will return the lexical_or_value used
                            self.addLiteralWithDataType(s.toPython(), p.toPython(), str(o), o._datatype.toPython())
                        else:
                            # .toPython() works out what's the most suitable python class and cast to it
                            self.addLiteral(s.toPython(), p.toPython(), o.toPython())
                    # add object properties
                    else:
                        self.addTriple(s.toPython(), p.toPython(), o.toPython())
            except Exception as exc:
                raise Exception(f"Failed to add: {s.n3()} {p.n3()} {o.n3()}") from exc

    def add_outputs_ogm(self, objects: Union[BaseClass, List[BaseClass]]):
        if isinstance(objects, BaseClass):
            objects = [objects]
        for o in objects:
            self.addGraph(o.graph())
