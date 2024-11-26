class SparqlBase:
    def tolines(self):
        return [str(self)]


class GraphPattern(SparqlBase):
    pass


class Constraint(SparqlBase):
    pass
