import re

if __name__ == '__main__':
    from Lookup import find_nearest_match
    from MarieLogger import MarieMessage
else:
    from .Lookup import find_nearest_match
    from .MarieLogger import MarieMessage


def c_k(t):  # celsius to kelvin
    return t + 273.15


def f_k(t):  # fahrenheit to kelvin
    return t * 5 / 9 + 273.15


def k_k(t):
    return t


def a_pa(p):
    return p * 101325


def b_pa(p):
    return p * 100000


def pa_pa(p):
    return p


def psi_pa(p):
    return p * 6894.76


def kpa_pa(p):
    return p * 1000


def mpa_pa(p):
    return p * 1000 * 1000


def splitValueAndUnits(variable):
    variable = str(variable).lower().strip().replace('temperature', '').replace('pressure', '')
    # e.g -1002 degrees celsius, 223 c, 2343 K, 3432 degrees, -2132
    re_numerical = r'[-]*[0-9]+'
    re_unit = r'[a-z ]+'
    # split the numerical value out
    n_rst = re.search(re_numerical, variable)
    variable_numerical = n_rst.group(0) if n_rst else ""
    # split the unit out
    u_rst = re.findall(re_unit, variable)
    variable_unit = ''.join(u_rst) if u_rst else ""
    return variable_numerical, variable_unit


def convertPressure(pressure):
    default_unit = 'pa'
    a = ['atm', 'atmospheric pressure']
    b = ['bar', 'barye', 'ba', ]
    pa = ['pascal', 'pa']
    psi = ['pound per square inch', 'psi']
    kpa = ['kpa', 'kilo pascal', 'k pascal']
    mpa = ['mpa', 'million pascal', 'm pascal']

    p_dict = {'a': {'keys': a, 'func': a_pa},
              'b': {'keys': b, 'func': b_pa},
              'pa': {'keys': pa, 'func': pa_pa},
              'psi': {'keys': psi, 'func': psi_pa},
              'kpa': {'keys': kpa, 'func': kpa_pa},
              'mpa': {'keys': mpa, 'func': mpa_pa}
              }

    keys = a + b + pa + psi + kpa + mpa
    p_numerical, p_unit = splitValueAndUnits(pressure)
    p_numerical = number_conversion(p_unit, p_numerical, p_dict, default_unit, keys, 100000)
    return p_numerical


def number_conversion(t_unit, t_numerical, d, default_unit, keys, default_number):
    for unit_key in d:
        if t_unit == '' or t_unit == 'room':
            t_unit = default_unit
        matched_unit, score = find_nearest_match(keys=keys, term=t_unit)
        try:
            t_numerical = float(t_numerical)
        except ValueError:
            t_numerical = default_number  # assume the value is 0 if failed to convert .
        _keys = d[unit_key]['keys']
        _func = d[unit_key]['func']
        if matched_unit in _keys:
            t_numerical = _func(t_numerical)
            return t_numerical


def convertTemperature(temperature):
    default_unit = 'k'
    c = ['c', 'celsius', 'degrees celsius']
    f = ['f', 'fahrenheit', 'degrees fahrenheit']
    k = ['k', 'kelvin', 'degrees kelvin', '', 'room']
    keys = c + f + k
    t_dict = {'c': {'keys': c, 'func': c_k},
              'f': {'keys': f, 'func': f_k},
              'k': {'keys': k, 'func': k_k}
              }
    t_numerical, t_unit = splitValueAndUnits(temperature)
    t_numerical = number_conversion(t_unit, t_numerical, t_dict, default_unit, keys, 300)
    return t_numerical


if __name__ == '__main__':
    t_variables = ['', '1232', 'somenoise 1010 K', 'K', '-255k', '-244C', '0 K', '0 Dgrees celcsisu', '233 F',
                   '324 Farhenheit degress']
    print('=========================== TEMPERATURE ==============================')
    for t in t_variables:
        r_t = convertTemperature(t)
        print(t, '--->', r_t)
        print('========')

    print('=========================== PRESSURE ==============================')
    p_variables = ['', '1232', 'some Bar', '-32142 Bar', '343 pa', '231kpa', 'pascal', '2 atm', '23 pressure ']
    for p in p_variables:
        r_p = convertPressure(p)
        print(p, '--->', r_p)
        print('========')
