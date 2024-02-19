class NodeInserter:
    """
    The NodeInsert provides functions to insert nodes for embedding direct connection between entity and values (lateral)
    It also need to create ID for such value and upload the values (perhaps units) to the database we choose.

    Wikidata and Pubchem data are the first two examples that needs a node inserter
    """

    def __init__(self):
        pass

    def insert_node(self, database_addr=0.0):
        """Insert a node for shallow triples.

        Keyword arguments:
        database_addr -- the address of the database (default 0.0)
        """
        pass
