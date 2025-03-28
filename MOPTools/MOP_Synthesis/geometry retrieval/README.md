# MOP Geometry retrieval
This is a minimal software package to retrieve metal-organic polyhedra information from the TWA knowledge graph. Each MOP has a XYZ geometry file attached which can be downloaded.
TWA holds knowledge on over 2000 MOP structures, 166 of which are experimentally verified. The remaining structures are proposed via algorithms presented in Kondinski, 2022 (DOI: 10.1021/jacs.2c03402).

## Prerequisites
The TWA library needs to be installed via `pip install twa`. This requires an installation of Python version >= 3.8 and the latest version of Java (e.g., via OpenJDK).
Running the example notebook also requires installation of jupyter and pandas library.

## Credentials file
Please keep the contents of this file confidential as it includes login credentials for our file server as well as an endpoint to unpublished data.
**To Do:** Change the GEO_FOLDER parameter to whatever local folder path you want to save MOP Geometry files.

## Jupyter Notebook
The example notebook (retrieve_structures.ipynb) establishes a connection to TWA, retrieves a number of MOPs based on specific criteria, and downloads the related geometry files. The current example gets all original (i.e., experimentally found) MOPs with a total charge of zero.  

The three blocks can be run separately:
1. First block imports relevant libraries and establishes a connection to the knowledge graph.

2. Second block calls a function in queryMOPs.py to retrieve MOPs. The function `construct(known,charge,shape,cbu,gbu)` can be called without parameters to retrieve all MOPs or with any number of the shown optional arguments to restrict the number of MOPs: 
    - setting known to true restricts the search to the original MOPs
    - setting charge to an integer restricts the search to MOPs of that total charge
    - setting shape to a short form denotion of a polyhedral shape (tet, cub, oct, ico, dod, cuo, rco, rdo, twc, tbp, tri, trp) restricts the search to MOPs of that geometric appearance
    - setting CBU or GBU to the name of a chemical or generic building unit (see 10.1021/jacs.2c03402) restricts the search to MOPs built from that CBU or GBU
A table appears with the MOPs retrieved by the given criteria together with their shape, total charge, and XYZ file name.

3. Third block downloads the XYZ files of all the retrieved MOPs to the folder specified in credentials.py. This is kept separate from the earlier block so you can first check the MOP details before starting the file download.