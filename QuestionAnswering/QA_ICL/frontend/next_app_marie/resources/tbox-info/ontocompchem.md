---
sortKey: 2
id: ontocompchem
title: OntoCompChem
---

OntoCompChem is an ontology designed to represent the input and output processes of quantum mechanical (QM) calculations, with a primary focus on molecular systems. This ontology delineates calculations through various detailed parameters: the objective (e.g., single point calculation, geometry optimization, frequency calculation), the utilized software (e.g., Gaussian16), the theoretical level employed (e.g., B3LYP, 6-31G(d)), and additional details such as overall charge and spin polarization. Furthermore, OntoCompChem includes data on calculated frontier orbitals, the final converged self-consistent field (SCF) energy, optimized geometries for geometry optimizations, and for frequency calculations, it records the zero-point energy correction and a comprehensive list of computed vibrational frequencies, linking back to their corresponding stationary geometries and calculations.

### Download

- [OntoCompChem.owl](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontocompchem/OntoCompChem.owl)
  
### Access

- Direct access via [Blazegraph Workbench](https://theworldavatar.io/chemistry/blazegraph/ui/#query) (select "ontocompchem" namespace)
- SPARQL endpoint: https://theworldavatar.io/chemistry/blazegraph/namespace/ontocompchem

### Publications

[1] N. Krdzavac, S. Mosbach, D. Nurkowski, P. Buerger, J. Akroyd, J. W. Martin, A. Menon, and M. Kraft, ["An ontology and semantic web service for quantum chemistry calculations"](https://doi.org/10.1021/acs.jcim.9b00227), Journal of Chemical Information and Modeling, 59(7), 3154-3165, (2019).

[2] F. Farazi, N. Krdzavac, J. Akroyd, S. Mosbach, A. Menon, D. Nurkowski, and M. Kraft, ["Linking reaction mechanisms and quantum chemistry: An ontological approach"](https://doi.org/10.1016/j.compchemeng.2020.106813), Computers & Chemical Engineering, 137, 106813, (2020).

[3] L. Pascazio, D. Tran, S. D. Rihm, Jiaru Bai, J. Akroyd, S. Mosbach, and M. Kraft, ["Question-answering system for combustion kinetics"](https://doi.org/10.1016/j.proci.2024.105428), Proceedings of the Combustion Institute, 40, 105428, (2024).

### Preprints

[1] N. Krdzavac, S. Mosbach, D. Nurkowski, P. Buerger, J. Akroyd, J. W. Martin, A. Menon, and M. Kraft, "An ontology and semantic web service for quantum chemistry calculations", Technical Report 223, c4e-Preprint Series, Cambridge, 2019 ([PDF](https://como.ceb.cam.ac.uk/media/preprints/c4e-Preprint-223.pdf)).

[2] F. Farazi, N. Krdzavac, J. Akroyd, S. Mosbach, A. Menon, D. Nurkowski, and M. Kraft, "Linking reaction mechanisms and quantum chemistry: An ontological approach", Technical Report 236, c4e-Preprint Series, Cambridge, 2019 ([PDF](https://como.ceb.cam.ac.uk/media/preprints/c4e-preprint-236.pdf)).

[3] L. Pascazio, D. Tran, S. D. Rihm, Jiaru Bai, J. Akroyd, S. Mosbach, and M. Kraft, "Question-answering system for combustion kinetics", Technical Report 315, c4e-Preprint Series, Cambridge, 2023 ([PDF](https://como.ceb.cam.ac.uk/media/preprints/c4e-preprint-315.pdf)).