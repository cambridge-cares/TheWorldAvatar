if __name__ == "__main__":
    from AgentCaller import AgentCaller
    from AgentQueryParser import AgentQueryParser
    from AgentRequestConstructor import AgentRequestConstructor

else:
    from .AgentCaller import AgentCaller
    from .AgentQueryParser import AgentQueryParser
    from .AgentRequestConstructor import AgentRequestConstructor
    