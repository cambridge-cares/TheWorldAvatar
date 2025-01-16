def replace_sparql_literal(query_sparql: str, old: str, new: str):
    return query_sparql.replace('"{x}"'.format(x=old), '"{x}"'.format(x=new))


def replace_nlq_literal(nlq: str, old: str, new: str):
    return nlq.replace("[{x}]".format(x=old), "[{x}]".format(x=new))
