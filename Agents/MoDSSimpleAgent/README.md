# MoDS Simple Agent
This agent is able to access a limited amount of the functionality of [the Model Development Suite (MoDS)](https://cmclinnovations.com/solutions/products/mods/). 

## Deployment
This agent is currently deployed at `https://kg.cmclinnovations.com/mods-agent` however if you require local deployment you can follow the following instructions. 

Login to the CMCL Docker registry using the following command.
```bash
docker login docker.cmclinnovations.com
```
To deploy the MoDS Simple Agent a `.lic` licence file is required to be placed in the [`secrets`](./secrets/) directory. 
Running the following command will spin up the MoDS Simple Agent at port `58085`. 
```bash
docker compose up -d
```

## Usage
Example input files can be found in the [`examples`](./examples) directory.
To submit a job via curl or a web browser use https://kg.cmclinnovations.com/mods-agent/request (remote on KG server) or http://localhost:58085/request (local), with a "`query`" parameter with a value (URL-encoded if working through a browser) similar to these input files.
For example click [here](https://kg.cmclinnovations.com/mods-agent/request?query=%7B%22SimulationType%22%3A%22MOOonly%22%2C%22Algorithms%22%3A%5B%7B%22name%22%3A%22algorithm1%22%2C%22type%22%3A%22GenSurrogateAlg%22%2C%22surrogateToLoad%22%3A%22example-surrogate%22%7D%2C%7B%22name%22%3A%22algorithm2%22%2C%22type%22%3A%22MOO%22%2C%22maxNumberOfResults%22%3A10%2C%22variables%22%3A%5B%7B%22name%22%3A%22var1%22%2C%22type%22%3A%22input%22%7D%2C%7B%22name%22%3A%22var2%22%2C%22type%22%3A%22input%22%7D%2C%7B%22name%22%3A%22var3%22%2C%22type%22%3A%22input%22%7D%2C%7B%22name%22%3A%22var4%22%2C%22type%22%3A%22output%22%2C%22objective%22%3A%22Maximise%22%2C%22minimum%22%3A0.5%2C%22weight%22%3A0.5%7D%2C%7B%22name%22%3A%22var5%22%2C%22type%22%3A%22output%22%2C%22objective%22%3A%22Minimise%22%2C%22maximum%22%3A1.5%2C%22weight%22%3A0.1%7D%2C%7B%22name%22%3A%22var6%22%2C%22type%22%3A%22output%22%2C%22objective%22%3A%22Maximise%22%2C%22minimum%22%3A2.5%2C%22weight%22%3A0.7%7D%5D%7D%5D%7D) to run a the job specified in the [multi-objective optimisation with pregenerated surrogate example](./examples/MOOonly.json).

Some `SimulationType`s will be returned the results of the simulation immediately in a JSON object.
- Multi-Criteria Decision Making (`MCDM`)
- Sensitivity Analysis (`Sensitivity`)

Others will return only return a the "jobID" and the "SimulationType" in a JSON object and run asynchronously.
- High-Dimensional Model Representation Surrogate Generation (`HDMR`)
- Surrogate Generation and Multi-Objective Optimisation (`MOO`)
- Multi-Objective Optimisation with a pregenerated saved surrogate (`MOOonly`)
- Evaluation of a pregenerated saved surrogate (`Evaluate`)
The JSON object returned by this query (URL-encoded if working through a browser) can then be passed as the "`query`" parameter to this URL https://kg.cmclinnovations.com/mods-agent/output/request (remote on KG server) or http://localhost:58085/output/request (local) to retrieve the results when the simulation is complete.

The MoDS Simple Agent supports the loading and saving of surrogates generated.
In the examples an "`example-surrogate`" is loaded using `"surrogateToLoad": "example-surrogate"`. 
This field can be replace with the `jobID` of a previous job where `"saveSurrogate": true`. 

## Development
Add the following files to the [`credentials`](./credentials/) directory.
- `cmcl_repo_username.txt` username for logging into container repo `docker.cmclinnovations.com`
- `cmcl_repo_password.txt` password (or preferably token) for container repo `docker.cmclinnovations.com`
- `wa_repo_username.txt` username for into [TWA repo](https://github.com/cambridge-cares/TheWorldAvatar)
- `wa_repo_password.txt` password (or preferably token) for [TWA repo](https://github.com/cambridge-cares/TheWorldAvatar)