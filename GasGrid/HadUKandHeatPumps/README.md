### HadUK Ontology

This lightweight ontology represents the HadUK-Grid dataset. It is based upon the [Office for National Statistics representation of 'Statistical Geography' areas](http://statistics.data.gov.uk/home)

### HadUK Parsing

Pointwise measurements are given a bounding region of validity based on surrounding measurements. 

HadUK_to_RDF.py contains functionality to parse .nc files, and subsequently create an assertional-box of the climate data expressed using the HadUK ontology. 

Each variable, for each month is around 10mb when in .nc form, however expands to around 2.5gb when expressed as linked data. 

