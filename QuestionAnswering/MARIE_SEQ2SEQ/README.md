# Marie4

Marie is a question-answering system that exposes chemical data from the World Avatar Knowledge Graph via a natural language interface. In this iteration of Marie, a text-to-text transformer model is employed to translate natural language questions to SPARQL queries, which are then executed against the underlying knowledge graph to yield the desired answers.

## Contents

- [data_generation](./data_generation/): to generate training data
- [training](./training/): for fine-tuning and evaluating translation models
- [deployment](./deployment/): for deployment

## Shared data

Experimental data, dataset for fine-tuning, and fine-tuned weights can be found [here](https://www.dropbox.com/scl/fo/ob34gbuiy5mzorqtsnxdr/h?rlkey=20ggivx8tardw2wgq1vx8azqo&dl=0).