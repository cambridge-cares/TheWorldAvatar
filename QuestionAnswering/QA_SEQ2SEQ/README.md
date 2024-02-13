# Marie4 over distributed knowledge graphs

Marie is a question-answering system that exposes chemical data from the World Avatar Knowledge Graph via a natural language interface. In this iteration of Marie, a pre-trained transformer model is fine-tuned for two independent tasks: natural language-to-SPARQL translation and domain classificationn. This enables the system to operate over multiple knowledge graphs to meet various information needs in the chemistry domain. 

## Contents

- [data_generation](./data_generation/): to generate training data
- [training](./training/): for fine-tuning and evaluating translation models
- [deployment](./deployment/): for deployment
