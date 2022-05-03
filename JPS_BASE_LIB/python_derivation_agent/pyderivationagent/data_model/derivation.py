class DerivationInputs():
    """This is a warpper class for uk.ac.cam.cares.jps.base.derivation.DerivationInputs.java.
    All methods provided here are wrapped around the corresponding methods in the java class,
    as references for developers to handle derivations within python derivation agents."""

    def __init__(self, derivationInputs) -> None:
        self.derivation_inputs = derivationInputs

    def getInputs(self):
        return self.derivation_inputs

    def getIris(self, rdfType):
        return self.derivation_inputs.getIris(rdfType)

class DerivationOutputs():
    """This is a warpper class for uk.ac.cam.cares.jps.base.derivation.DerivationOutputs.java.
    All methods provided here are wrapped around the corresponding methods in the java class,
    as references for developers to handle derivations within python derivation agents."""

    def __init__(self, derivationOutputs) -> None:
        self.derivation_outputs = derivationOutputs

    def createNewEntity(self, iri, rdfType):
        self.derivation_outputs.createNewEntity(iri, rdfType)

    def addTriple(self, s, p, o):
        """Only one addTriple is provided here as the correct method to use will be decided by java automatically."""
        self.derivation_outputs.addTriple(s, p, o)
