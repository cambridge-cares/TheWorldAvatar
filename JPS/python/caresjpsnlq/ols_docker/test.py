import urllib.request
with urllib.request.urlopen('http://www.ebi.ac.uk/efo/efo.owl') as f:
	print(f.read(300))
