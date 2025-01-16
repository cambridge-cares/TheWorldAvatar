# PostGIS does not like non-UTF-8 characters. This script reads in
# the pumping station csv file, replaces all umlauts with UTF-8 ones,
# and writes this into another csv file.

basename = r'Hebewerke-PS-2022'
ext = r'.csv'
addendum = r'-bereinigt'

with open(basename+ext, 'rb') as f_in:
    with open(basename+addendum+ext, 'w') as f_out:
        for b in f_in:
            line = b.decode('utf-8', 'backslashreplace')
            line_sanitised = line.replace(r'\xdf','ß').replace(r'\xfc','ü').replace(r'\xe4','ä').replace(r'\xdc','Ü').replace(r'\xf6','ö')
            f_out.write(line_sanitised)
