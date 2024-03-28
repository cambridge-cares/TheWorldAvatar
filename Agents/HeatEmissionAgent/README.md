# Heat Emission Agent

The heat emission agent calculates heat emissions of data centres and various types of factories. The properties of these industrial facilities that are required to calculate the heat emissions need to be stored in PostgreSQL tables as explained below. 



The steps required to run the agent for each case are described below:

## Instructions

### 1. Agent Deployment

The agent is designed to be run as part a stack.  


### 1.1 Preparation

#### Maven Repository credentials

This agent is set up to use this [Maven repository](https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/) (in addition to Maven central). You'll need to provide your credentials (github username/personal access token) in single-word text files located like this:
```
./credentials/
        repo_username.txt
        repo_password.txt
```
repo_username.txt should contain your Github username. repo_password.txt should contain your Github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).


#### PostgreSQL database

The data needed to estimate the heat emissions of various types of industrial facilities are as follows:

- Data Centres: Maximum IT capacity in units of megawatts, utilization rate, which is a dimensionless parameter between 0 and 1 and floor area in units of square meters. Their values must be stored in columns called 'maximum_it_capacity', 'utilization_rate' and 'floor_area'. The table must be called 'data_centres'.
- Precision Engineering Plants: Floor area in units of square meters, energy consumption per unit of gross floor area in units of megaJoules per square meters and thermal efficiency, which is a dimensionless quantity between 0 and 1. Their values must be stored in columns called 'floor_area', 'specific_energy_consumption' and 'thermal_efficiency'. The table must be called 'precision_engineering'.
- Printing Plants: Average heat output per printer in units of megawatts. These values must be stored in a column called 'heat_emissions_per_printer'. The table must be called 'printing'.
- Factories in industries other than precision engineering and printing: Specific energy consumption in units of MegaJoules per kilogram, production capacity in units of kilogram per second and thermal efficiency. Units other than kilograms can be used for the production volume as long as it is consistent between the specific energy consumption and production capacity. These values must be stored in columns called 'production_volume', 'specific_energy_consumption' and 'thermal_efficiency'. The table must be called 'factories'.
- Individual Heat Sources in Chemical Plants and Petroleum Refineries: Carbon dioxide emissions in units of tons per year. These values must be stored in a column called 'co2_emissions'. The table name must be called 'jurong_island_city_furniture'.   




##### Stack containers
If the agent is being run as part of a stack, the user can opt to use a namespace located in the stack blazegraph. The procedure for spinning up the stack is described at [stack manager page](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).

#### 1.2 Docker Deployment

- From the same directory location as this README, build the agent's image using the `docker compose build` command. If the container is run as part of a stack, copy the `heatemissionagent.json` file from the `stack-manager-input-config` folder into the `Deploy/stacks/dynamic/stack-manager/inputs/config/services` folder of the stack manager before starting the stack.


### 2. Agent route

The agent has a single API route which requires a POST request. It accepts the following input parameters:

- ```dbName```: The name of the PostgreSQL database from which the factories' properties are queried. 

The following is an example POST request for running the agent. The stack is assumed to have been spun up using the default port 3838:

```
curl -X POST -H "Content-Type: application/json" -d '{"dbName":"postgres"}'  "http://localhost:3838/heatemissionagent/performheatquery"
```




#### Return values 


The agent adds a new column called 'heat_emissions' to each of the tables mentioned above containing the heat emissions of each type of industrial facility in units of megawatts.

The following methods are used to estimate the heat emissions of each type of industrial facility:

- Data Centres:

$$ Q = \frac{1.08*P_{max}U_R + 21.53A_F + 9400}{1.0 \times 10^6} $$

The parameters $Q$, $P_{max}$, $U_R$ and $A_F$ denote the heat emissions rate in megawatts, maximum IT capacity in megawatts, utilization rate and gross floor area in square meters respectively.

- Precision Engineering

$$ Q = A_FS_F(1 - \eta) $$

The parameter $\eta$ denote the thermal efficiency.

- Printing:

$$ Q = N_pH_p $$

The parameters $N_p$ and $H_p$ denote the number of printers and average heat emissions per printer in megawatts.

- Factories not in the precision engineering and printing industries:

$$ Q = begin{cases}
    -VS \text{if } S < 0  
    VS(1 - \eta) & \text{otherwise.}
\end{cases} $$

The parameters $V$ and $S$ denote the production volume in kilograms per second and specific energy consumption in megaJoules per kilogram respectively.

- Individual Heat Sources in Chemical Plants and Petroleum Refineries:

$$ Q = \frac{E_c \times(1 - \eta) \times 1.0 \times 10^{12}}{C_I \times Y_s \times (1.0 \times 10^6)} $$

The parameters $E_c$, $C_I$ and $Y_s$ denote the carbon dioxide emissions rate in units of tons per year, carbon emissions index which is assumed to be 63.0 kg/GJ and number of seconds per year respectively. 