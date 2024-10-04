class UnexpectedVerbalizationType(ValueError):
    def __init__(self, arg: str):
        super().__init__(f"Unexpected verbalization type: {arg}.")