
def COP(T_c):
    return 0.35*((45+273.15)/(45-T_c))


def T_from_COP(COP):
    return 45 - ((45+273.15)/(COP/0.35))