## KG Embedding 

The KG embedding is still in its experiment stage (2022/05/24). The experiment will 
start with PubChem data, which contains nearly 1 million species while each 
species contains 22 attributes. 

It is estimated that converting the PubChem data into training data (plain triples without T-Box) will 
create about 40 million triples. As tested, the full-size training data is about 1.09 GB. Besides the full-size 
training data, a mini and a medium training set is created, which contains about **10000** triples and **410000** triples. 
The main purpose of the experiments contains 3 parts: 
- To find the optimal configuration/hyper-parameters: batch size, loss function (model), epoch
- To estimate the computation cost of training the pubchem data and hence the computation cost of training the whole TWA KG. 
- To estimate the approximate accuracy we can achieve using KG embedding. 
- To estimate the effectiveness of fine-tuning the model after the KG is updated. 

### Computation expense
It is critical to estimate the computation expense of embedding the TWA KG.   

The current experiment is conducted on the power station (`NERO`) of Xiaochi (32GB RAM, GPU with 20GB memory, Xeon E5-1620 CPU). 
It is now estimated that the maximum batch size the GPU can handle is 2048 and the GPU is 3.5 times faster than the CPU. 

HPC1/CSD3

### Hyper-parameter optimization 

- Experiment on different loss functions (model), the candidate models include 
`[KG-BERT|TransE|TransD|TransH|TransG|TransM|TransR|DistMult]`

- Experiment on different batch size, which is critical for the final accuracy
- To find out the minimal epoch number needed for a satisfactory model accuracy. 

### Steps
1. Configure CUDA and pytorch so that the training can run on the GPU on `NERO` (DONE)
2. To compare the performance of training with CPU and GPU on `NERO`(GPU is 3.5 times faster than CPU)
3. To test the `Medium` training set and find out the rough training time. It appears the training time required is roughly proportional to the number of triples. (About 1 hour to achieve a satisfactory accuracy on instance-level. )  
4. To test whether it is feasible to run the full-size embedding on `NERO` with the simplest model `TransE` and 100 epochs. If it is feasible, find out the time needed.
5. Try running the training on HPC so see whether there is an improvement. 
