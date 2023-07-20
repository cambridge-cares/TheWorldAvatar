## Evaluation Dataset creation 
To create the dataset for evaluation, the user needs to set up the full running 
environment for the Marie system and have all the files prepared. For details, 
see [Main readme file](../readme.md).


### Cross graph
To create the evaluation dataset for test cross graph query as well as for ablation test of the 
score alignment model, run `Evaluation/CreateCrossGraphEvaluationSet.py`. The script
will create the files under `DATA/CrossGraph`

### Other ontologies
For other ontologies, under `Evalaution` directory, the user can find a set of scripts
with the name 
`Create[Ontology Name]EvaluationSet.py`. By running them, the according dataset for evaluation 
will be created under folder `MARIE_AND_BERT/DATA/CrossGraph/[ontology name]`. 


 
## Running 
To run the evaluation script, the user needs to set up the full running 
environment for the Marie system and have all the files prepared. For details, 
see [Main readme file](../readme.md). 

To run single ontology evaluation, run `python Evaluator.py --mode single --ontology [ontology name]`

To run numerical evaluation (numerical filtering questions), run `python Evaluator.py --mode numerical`

To run cross graph evaluation: 
1. Without score alignment, run `python Evaluation.py --mode cross --ablation true`
2. With score alignment, run `python Evaluation.py --mode cross --ablation false`


