import ast
from typing import Type, Union, List, Dict
from rdflib import Graph, RDF, Literal

from twa.data_model.base_ontology import BaseClass, KnowledgeGraph

class Derivation():
    """
    Wrapper class for `uk.ac.cam.cares.jps.base.derivation.Derivation.java`.

    This class provides a simplified interface for interacting with the Derivation object
    returned when creating synchronous derivations for new information.

    Only two methods are provided here, all other methods in Java can be accessed via `self.derivation.nameOfJavaMethod(args)`.

    Methods:
        getIri(): Returns the IRI of the Derivation instance
        getBelongsToIris(outputRdfType): Returns the IRIs of the entities that belong to the Derivation instance
    """

    def __init__(self, derivation_java):
        """
        Initialises the Derivation instance.

        Args:
            derivation_java: The Java derivation object
        """
        self.derivation = derivation_java

    def getIri(self) -> str:
        """
        Returns the IRI of the Derivation instance.

        Returns:
            str: IRI of the Derivation instance
        """
        return self.derivation.getIri()

    def getBelongsToIris(self, outputRdfType: str) -> List[str]:
        """
        Returns the IRIs of the entities that belongsTo the Derivation instance.

        Args:
            outputRdfType (str): IRI of the rdf:type of the entities that belongsTo the Derivation instance

        Returns:
            List[str]: List of IRIs of the entities that belongsTo the Derivation instance
        """
        return self.derivation.getBelongsToIris(outputRdfType)


class DerivationInputs():
    """
    Wrapper class for `uk.ac.cam.cares.jps.base.derivation.DerivationInputs.java`.

    This class provides methods to handle derivations within Python derivation agents,
    referencing to the corresponding methods in the Java class.
    For implementation details, please refer to the Java code.

    Methods:
        getDerivationIRI(): Returns the IRI of the derivation.
        getInputs(): Returns the inputs of the derivation as a dictionary.
        getIris(rdfType): Returns the IRIs of the inputs of the specified rdf:type.
        get_inputs_ogm_by_rdf_type(rdf_type, sparql_client, recursive_depth): Returns the inputs as objects of the specified rdf:type.
        get_inputs_ogm(clz, sparql_client, recursive_depth): Returns the inputs as objects of the specified class.
        get_inputs_ogm_assume_one(clz, sparql_client, recursive_depth): Returns a single input object of the specified class.
    """

    def __init__(self, derivationInputs) -> None:
        """
        Initialises the DerivationInputs instance.

        Args:
            derivationInputs: The Java DerivationInputs object
        """
        self.derivation_inputs = derivationInputs

    def getDerivationIRI(self) -> str:
        """
        Returns the IRI of the derivation.

        Returns:
            str: IRI of the derivation
        """
        return self.derivation_inputs.getDerivationIRI()

    def getInputs(self) -> Dict[str, List[str]]:
        """
        Returns the inputs of the derivation as a dictionary.

        Returns:
            Dict[str, List[str]]: Inputs of the derivation in the format of {rdf_type: [iris]}
        """
        return ast.literal_eval(str(self.derivation_inputs.getInputs()))

    def getIris(self, rdfType) -> Union[List[str], None]:
        """
        Returns the IRIs of the inputs of the specified rdf:type.

        Args:
            rdfType (str): IRI of the rdf:type of the inputs

        Returns:
            List[str]: List of IRIs of the inputs, or None if no inputs are found
        """
        iris = self.derivation_inputs.getIris(rdfType)
        return list(iris) if iris is not None else None

    def get_inputs_ogm_by_rdf_type(
        self,
        rdf_type: str,
        sparql_client,
        recursive_depth: int = 0
    ) -> List[BaseClass]:
        """
        Returns the inputs as objects of the specified rdf:type (when using object graph mapper).

        Args:
            rdf_type (str): IRI of the rdf:type
            sparql_client: The SPARQL client to query the knowledge graph
            recursive_depth (int): The depth of recursive queries (default is 0)

        Returns:
            List[BaseClass]: List of objects of the specified rdf:type
        """
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
        """
        Returns the inputs as objects of the specified class.

        Args:
            clz (Type[BaseClass]): The class of the objects to return
            sparql_client: The SPARQL client to query the knowledge graph
            recursive_depth (int): The depth of recursive queries (default is 0)

        Returns:
            List[BaseClass]: List of objects of the specified class
        """
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
        """
        Returns a single input object of the specified class.

        Args:
            clz (Type[BaseClass]): The class of the object to return
            sparql_client: The SPARQL client to query the knowledge graph
            recursive_depth (int): The depth of recursive queries (default is 0)

        Raises:
            Exception: If the number of objects found is not exactly one

        Returns:
            BaseClass: The single object of the specified class
        """
        objects = self.get_inputs_ogm(clz=clz, sparql_client=sparql_client, recursive_depth=recursive_depth)
        if len(objects) != 1:
            raise Exception(f"""Input type {clz.get_rdf_type()} assumed one for derivation {self.getDerivationIRI()},
                encounterred {len(objects)}: {' '.join([o.triples() for o in objects])}""")
        return next(iter(objects))


class DerivationOutputs():
    """
    Wrapper class for `uk.ac.cam.cares.jps.base.derivation.DerivationOutputs.java`.

    This class provides methods to handle derivations within Python derivation agents,
    referencing to the corresponding methods in the Java class.
    For implementation details, please refer to the Java code.

    Methods:
        createNewEntity(iri, rdfType): Creates a new entity with the given IRI and rdf:type.
        createNewEntityWithBaseUrl(baseUrl, rdfType): Creates a new entity with a base URL and rdf:type.
        addTriple(s, p, o): Adds a triple to the derivation outputs.
        addLiteral(s, p, o): Adds a literal to the derivation outputs.
        addLiteralWithDataType(s, p, o, dataType): Adds a literal with a specified data type to the derivation outputs.
        addGraph(g): Adds a whole rdflib.Graph to the derivation outputs.
        add_outputs_ogm(objects): Adds objects of a specified class to the derivation outputs.
    """

    def __init__(self, derivationOutputs) -> None:
        """
        Initialises the DerivationOutputs instance.

        Args:
            derivationOutputs: The Java DerivationOutputs object
        """
        self.derivation_outputs = derivationOutputs

    def createNewEntity(self, iri, rdfType):
        """
        Creates a new entity with the given IRI and rdf:type.

        Args:
            iri (str): IRI of the new entity
            rdfType (str): IRI of the rdf:type of the new entity
        """
        self.derivation_outputs.createNewEntity(iri, rdfType)

    def createNewEntityWithBaseUrl(self, baseUrl, rdfType):
        """
        Creates a new entity with the given base URL and rdf:type, adds the new entity to derivation outputs, then returns the initialised IRI.

        Args:
            baseUrl (str): Base URL for the new entity
            rdfType (str): IRI of the rdf:type of the new entity

        Returns:
            str: IRI of the newly created entity.
        """
        return self.derivation_outputs.createNewEntityWithBaseUrl(baseUrl, rdfType)

    def addTriple(self, s, p, o):
        """
        Adds a triple to the derivation outputs.
        Note that only one addTriple function is provided here, the two functions taking TriplePattern is NOT provided for simplicity of java-python data structure conversion.

        Args:
            s (str): Subject of the triple
            p (str): Predicate of the triple
            o (str): Object of the triple
        """
        self.derivation_outputs.addTriple(s, p, o)

    def addLiteral(self, s, p, o):
        """
        Adds a literal to the derivation outputs.
        Note that only one addLiteral is provided here as the correct method to use will be decided by java automatically.

        Args:
            s (str): Subject of the triple
            p (str): Predicate of the triple
            o (Union[str, Literal]): Literal object of the triple
        """
        self.derivation_outputs.addLiteral(s, p, o)

    def addLiteralWithDataType(self, s, p, o, dataType):
        """
        Adds a literal with a specified data type to the derivation outputs.
        Note that this method corresponds to addLiteral(String, String, String, String) in `DerivationOutputs.java`,
        but renamed in python due to limitations of overloading in python.

        Args:
            s (str): Subject of the triple
            p (str): Predicate of the triple
            o (str): Literal object of the triple
            dataType (str): Data type of the literal
        """
        self.derivation_outputs.addLiteral(s, p, o, dataType)

    def addGraph(self, g: Graph):
        """
        Adds a whole rdflib.Graph to the derivation outputs.

        Args:
            g (Graph): The graph to add to the derivation outputs
        """
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
        """
        Adds objects of a specified class to the derivation outputs.

        Args:
            objects (Union[BaseClass, List[BaseClass]]): The objects to add to the derivation outputs
        """
        if isinstance(objects, BaseClass):
            objects = [objects]
        iris = [o.instance_iri for o in objects]
        self.addGraph(KnowledgeGraph.all_triples_of_nodes(iris))
