from ontomatch.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from ontomatch.kgoperations.querykg import querykg
from ontomatch.utils.blackboard import Agent, LOCAL_BLACKBOARD_DIR



#TODO, add this as an extra step to coordinator
def namespaceOnEP(namespace):
    return SPARQL_ENDPOINTS['ontopowerplant'] + "/"+namespace

#TODO: use this function as an option to coordinator config
def downloadDataKg(addr, namespace):
    #Query str to get all related data
    qstr = '''
    select * FROM <{}> where {{?A ?b ?C}}
    '''
    #TODO: for now use a dummy function that returns from local DB
    #TODO: define this tmp file address
    res = querykg(namespaceOnEP(namespace), qstr.format(namespace))
    #TODO: construct a graph
    handle_turtle = Agent.create_handle(addr) + '.ttl'
    path = LOCAL_BLACKBOARD_DIR + '/' + handle_turtle
    #graph.serialize(path, format='turtle')
    return res



if __name__ == '__main__':
    downloadDataKg("http://Germany")