# Heat Emission Agent

The heat emission agent calculates heat emissions of data centres and various types of factories. The properties of these industrial facilities that are required to calculate the heat emissions need to be stored in PostgreSQL tables as explained below. 



The steps required to run the agent are described below:

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
- Chemical Plants: Production volume of main product produced by the plant in units of kilograms per second, specific energy consumption in units of megajoules per kilogram and the thermal efficiency. Their values must be stored in columns called 'production_volume', 'specific_energy_consumption' and 'thermal_efficiency'. The table must be called 'chemicals'.
- Semiconductor Plants: Production volume in units of number of wafers per second, surface area of wafers produced in units of square meters, specific energy consumption in units of megajoules per square meter of wafer produced and thermal efficiency. Their values must be stored in columns called 'production_volume', 'surface_area', 'specific_energy_consumption' and 'thermal_efficiency'. The table must be called 'semiconductors'. 
- Pharmaceutical Plants: Global production volume of the company operating the plant as measured by the value of the products produced in units of US dollars per second, specific energy consumption in units of megajoules per US dollar, thermal efficiency and number of manufacturing facilities operated by the company owning the plant. Their values must be stored in columns called 'revenue', 'energy_consumption_per_unit_revenue', 'thermal_efficiency' and 'number_facilities'. The production volume specified is assumed to be the total production volume of all manufacturing facilities operated by the company. The table must be called 'pharmaceuticals'.
- Food and Beverage Plants: Production volume in units of kilograms per second, specific energy consumption in units of megajoules per kilogram and thermal efficiency. Their values must be stored in columns called 'production_volume', 'specific_energy_consumption' and 'thermal_efficiency'. The table must be called 'food_beverages'. 
- Individual Heat Sources in Chemical Plants and Petroleum Refineries: Carbon dioxide emissions in units of tons per year. These values must be stored in a column called 'co2_emissions'. The table name must be called 'jurong_island_city_furniture'.   




##### Stack containers
If the agent is being run as part of a stack, the user can opt to use a namespace located in the stack blazegraph. The procedure for spinning up the stack is described at [stack manager page](https://github.com/TheWorldAvatar/stack/tree/main/stack-manager).

#### 1.2 Docker Deployment

- From the same directory location as this README, build the agent's image using the `docker compose build` command. If the container is run as part of a stack, copy the `heatemissionagent.json` file from the `stack-manager-input-config` folder into the `Deploy/stacks/dynamic/stack-manager/inputs/config/services` folder of the stack manager before starting the stack.


### 2. Agent route

The agent has a single API route which requires a POST request. It accepts the following input parameters:

- ```dbName```: The name of the PostgreSQL database from which the industrial facilities' properties are queried. 

The following is an example POST request for running the agent. The stack is assumed to have been spun up using the default port 3838:

```
curl -X POST -H "Content-Type: application/json" -d '{"dbName":"postgres"}'  "http://localhost:3838/heatemissionagent/performheatquery"
```




#### Return values 


The agent adds a new column called 'heat_emissions' to each of the tables mentioned above containing the heat emissions of each type of industrial facility in units of megawatts.

The following methods are used to estimate the heat emissions of each type of industrial facility:

#### Data Centres

$$ Q = \frac{1.08P_{max}U_R + 21.53A_F + 9400}{1.0 \times 10^6} $$


The meanings of the symbols in the equation above are as follows:

  - $Q$ : Heat emissions rate in megawatts
  - $P_{max}$ : Maximum IT capacity in megawatts
  - $U_R$ : Utilization rate. This is a dimensionless parameter whose value ranges between 0 and 1.
  - $A_F$ : Gross floor area in square meters. 



#### Precision Engineering

$$ Q = A_FS_F(1 - \eta) $$

The meanings of the symbols in the equation above are as follows:

  - $Q$ : Heat emissions rate in megawatts
  - $A_F$ : Gross floor area in square meters.
  - $S_F$ : Energy consumption rate per unit of gross floor area in megawatts per square meter.
   - $\eta$ : Thermal efficiency. This is a dimensionless parameter whose value ranges between 0 and 1.


#### Printing

$$ Q = N_pH_p $$

The meanings of the symbols in the equation above are as follows:

  - $Q$ : Heat emissions rate in megawatts
  - $N_p$ : Number of printers operated by the printing plant.
  - $H_p$ : Heat emissions rate per printer in units of megawatts.
 
#### Chemicals

$$ Q = \begin{cases}
    -VS(1 - \eta) & \text{if } S < 0  \\
    VS(1 - \eta) & \text{otherwise.}
\end{cases} $$

The meanings of the symbols in the equation above are as follows:

  - $Q$ : Heat emissions rate in megawatts
  - $V$ : Production rate in units of kilograms per second. 
  - $S$ : Specific energy consumption in units of megajoules per unit of product. The precise unit depends on the units of $V$.
  - $\eta$ : Thermal efficiency. This is a dimensionless parameter whose value ranges between 0 and 1.

#### Food and Beverages

$$ Q = VS(1 - \eta) $$

The meanings of the symbols in the equation above are as follows:

  - $Q$ : Heat emissions rate in megawatts
  - $V$ : Production rate in units of kilograms per second.
  - $S$ : Specific energy consumption in units of megajoules per unit of product. The precise unit depends on the units of $V$.
  - $\eta$ : Thermal efficiency. This is a dimensionless parameter whose value ranges between 0 and 1.

#### Pharmaceuticals

$$ Q = \frac{R_gS_r}{N_f}(1 - \eta) $$

The meanings of the symbols in the equation above are as follows:

  - $Q$ : Heat emissions rate in megawatts
  - $R_g$ : Global revenue earned from sales in units of US dollars per second. 
  - $S_r$ : Specific energy consumption in units of megajoules per US dollar of revenue.
  - $N_f$ : Number of manufacturing facilities operated by the company owning the pharmaceutical plant.
  - $\eta$ : Thermal efficiency. This is a dimensionless parameter whose value ranges between 0 and 1.

#### Semiconductors

$$ Q = N_wA_wS_a(1 - \eta) $$

The meanings of the symbols in the equation above are as follows:

  - $Q$ : Heat emissions rate in megawatts
  - $N_w$ : Number of wafers produced by the factory per second. 
  - $A_w$ : Surface area of a single wafer in units of square meters.
  - $S_a$ : Specific energy consumption in units of megajoules per square meter of wafer produced.
  - $\eta$ : Thermal efficiency. This is a dimensionless parameter whose value ranges between 0 and 1.


#### Individual Heat Sources in Chemical Plants and Petroleum Refineries:

$$ Q = \frac{E_c \times(1 - \eta) \times 1.0 \times 10^{12}}{C_I \times Y_s \times (1.0 \times 10^6)} $$


The meanings of the symbols in the equation above are as follows:

  - $Q$ : Heat emissions rate in megawatts
  - $E_c$ : Carbon dioxide emissions rate in units of tons per year
  - $C_I$ : Carbon emissions index which is assumed to be 63.0 kg/GJ
  - $Y_s$ : Number of seconds per year.

