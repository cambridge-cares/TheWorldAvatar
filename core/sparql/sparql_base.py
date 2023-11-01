PRINT_TEMPLATE = "{classname}[{properties}]"


class SparqlBase:
    def __repr__(self):
        return PRINT_TEMPLATE.format(
            classname=type(self).__name__, properties=self.__dict__.__repr__()
        )
