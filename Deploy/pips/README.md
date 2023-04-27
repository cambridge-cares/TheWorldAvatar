# PIPS project
This document is a step-by-step guide for the deployment of PIPS project. The complete set consists of a few docker stacks:
- Knowledge graph ([Blazegraph] and [Fileserver])
- Agents that manage the lab hardware ([VapourtecAgent] and [HPLCAgent])
- Agents that manage the rest of the workflow ([RxnOptGoalAgent], [RxnOptGoalIterAgent], [DoEAgent], [VapourtecScheduleAgent], [HPLCPostProAgent])

For technical details, please refer to each individual folder.


&nbsp;
# Deployment

## Prerequisites
### Software
To ensure robust agent operation, a few settings need to be configured for the Windows machine where the agents are deployed:
- Set WSL memory limit follow https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Environment#wsl-configuration-options
- Change `vpnKitMaxPortIdleTime` in `%APPDATA%\Docker\settings.json` from 300 to 0, see https://github.com/docker/for-win/issues/8861#issuecomment-1305175614

Once done, you may need to restart the docker desktop.

You may also want to install [AutoHotKey] to enable non-disrupted execution of VapourtecAgent. However, **NOTE** that use this at your own risk.

### Hardware

Vapourtec flow chemistry platform:
- Ensure it is properly configured in system configuration
- Ensure all tubings are checked
- Ensure pumps are primed
- Ensure enough solvent are loaded for each pump and autosampler
- Ensure fresh solutions for reaction is loaded
- Ensure waste bottles are emptied

HPLC:
- Ensure the correct HPLC method and sequence is loaded
- Ensure HPLC report is to be generated in the correct format and as separate file after each analysis
- Ensure the retention time information for each peaks is update-to-date


### Data
Triples for the laboratory digital twin:
- Double check if the autosampler sample loop volume setting is the same as the reaction scale in the doe template
  - Change the warning level for the autosampler accordingly - this ideally needs to be more than 1.5 * sample loop volume
- Put lab specific triples (in ttl format) in folder `./triples`, and run command `python upload_triples.py` in terminal to upload all the triples
   > Once uploaded, make sure all triples are reflecting the current state of the hardware, especially the liquid level and chemical amount part for the AutoSampler vials and reagent bottles, you may use below queries to double check:
   ```sparql
   # For liquid level of AutoSampler vials
   PREFIX ocb: <http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#>
   PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
   PREFIX OntoLab: <https://www.theworldavatar.com/kg/ontolab/>
   SELECT ?vial ?sol ?material ?fill ?max ?warn
   WHERE {
   ?vial rdf:type OntoLab:Vial.
   ?vial OntoLab:isFilledWith ?sol.
   ?sol ocb:refersToMaterial ?material.
   ?vial OntoLab:hasFillLevel/om:hasValue/om:hasNumericalValue ?fill.
   ?vial OntoLab:hasMaxLevel/om:hasValue/om:hasNumericalValue ?max.
   ?vial OntoLab:hasWarningLevel/om:hasValue/om:hasNumericalValue ?warn.
   }
   ORDER BY ?material
   ```
   ```sparql
   # For liquid level of ReagentBottle
   PREFIX ocb: <http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#>
   PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
   PREFIX OntoLab: <https://www.theworldavatar.com/kg/ontolab/>
   SELECT ?bottle ?sol ?material ?fill ?max ?warn
   WHERE {
   ?bottle rdf:type OntoLab:ReagentBottle.
   ?bottle OntoLab:isFilledWith ?sol.
   ?sol ocb:refersToMaterial ?material.
   ?bottle OntoLab:hasFillLevel/om:hasValue/om:hasNumericalValue ?fill.
   ?bottle OntoLab:hasMaxLevel/om:hasValue/om:hasNumericalValue ?max.
   ?bottle OntoLab:hasWarningLevel/om:hasValue/om:hasNumericalValue ?warn.
   }
   ORDER BY ?material
   ```


### Other files
- Make sure `email_auth.json` and other auth files are correctly loaded in the `./secrets` folder
- Make a copy of `./env_files/agent.credentials.env.example` as `./env_files/agent.credentials.env.deploy` and populate all the credentials
   > Make sure both the Blazegraph and Fileserver are available and the credentials are correct
- Place both `FCRemote.dll` and `FCRemoteCSV.dll` in this folder (`Deploy/pips`), please contact the developer of this repo if you don't have access to them
- Create `popup.ahk` with below content

  > **NOTE this script might have side effects, please use at YOUR OWN RISK**

   ```ahk
   ; NOTE this script should be used at your own risk.
   #Persistent
   SetTimer, DisablePressureLoss, 1000
   return

   DisablePressureLoss:
   SetControlDelay -1  ; May improve reliability and reduce side effects.
   ControlClick, Button1, Pressure Loss Detection Disabled,,,, NA
   return
   ```
- Check the installed packages with `pip list`, specifically, make sure below packages are installed with the correct version: `chemistry_and_robots==1.4.0`, `py4jps==1.0.34`, `pyderivationagent==1.4.4`, `vapourtecagent` (editable install), `rxngoaloptagent` (editable install), `rxnoptgoaliteragent` (editable install)


## Knowledge graph
It is recommended to deploy them on a Linux server with credentials enabled. To do this, developers need to create and populate files `./secrets/blazegraph_password.txt` and `./secrets/fileserver_password.txt` with a complex password. Then the docker images can be composed up by running below command in the terminal (**NOTE that one needs access to [CMCL Docker Registry] to do this**):
```bash
docker-compose -f "docker-compose.kg.yml" up -d
```

If you hit compatibility issue for docker compose, please remove `name: pips-kg` bit in the docker compose yml file and try again.

## Agents that manage the lab hardware

> NOTE: here the deployment is taking lab1 as an example, please follow the same procedure but using below files when deploying agents in lab2:
> 1. `./env_files/agent.lab1.vapourtec.env.deploy` --> `./env_files/agent.lab2.vapourtec.env.deploy`
> 2. `python vapourtec_agent_lab1.py` --> `python vapourtec_agent_lab2.py`
> 3. `./env_files/agent.lab1.hplc.env.deploy` --> `./env_files/agent.lab2.hplc.env.deploy`
> 4. `docker-compose.lab1.agents.yml` --> `docker-compose.lab2.agents.yml`

It is recommended to deploy them in WSL. To do this, please follow below steps:

1. Vapourtec Agent (21 triples)
   1. Open `FlowCommander` and connect to hardware if this is not already the case
   2. Go through the experiment configuration file
      > If one would like uninterrupted execution, **tick** the `Disable Pressure Loss Detection` box (**NOTE use this at your own risk**)

      > If using an autosampler, make sure to **untick** the `Allow Partial Sample Loop Usage` box
   3. Add one reaction with random condition and delete it
   4. Activate `popup.ahk`
      > **NOTE this script might have side effects, please use at YOUR OWN RISK**
   5. Populate below configurations in the `./env_files/agent.lab1.vapourtec.env.deploy`:
      1. Change `DRY_RUN` to `false`
      2. Populate `VAPOURTEC_IP_ADDRESS` with the value obtained by running the following command from the command line: `echo $(ipconfig.exe | grep 'vEthernet (WSL)' -A4 | cut -d":" -f 2 | tail -n1 | sed -e 's/\s*//g')`
   6. Start VapourtecAgent in terminal by executing `python vapourtec_agent_lab1.py`
2. HPLC Agent (18 triples)
   1. Check that HPLC instrument is ready, i.e. the sequence is loaded
   2. Change `source: "C:\\..."` part in the `docker-compose.lab1.agents.yml` with the folder path where the HPLC reports will be generated
      > If you deploy within WSL, the HPLC report folder path in the Windows host needs to be translated, e.g. `C:\\Chem32\MyProject\Optimisation\Sequence 2023-03-21 18-07-10` --> `/mnt/c/Chem32/MyProject/Optimisation/Sequence 2023-03-21 18-07-10`
   3. Change `DRY_RUN` to `false` in the `./env_files/agent.lab1.hplc.env.deploy`
   4. Start HPLCAgent by:
      ```bash
      docker-compose -f "docker-compose.lab1.agents.yml" up -d
      ```


## Agents that manage the rest of the workflow
By design, these agents can be fully distributed. For simplicity, they can be deployed in the same machine by:
```bash
docker-compose -f "docker-compose.rxn.agents.yml" up -d
```


&nbsp;
# Request optimisation campaign
To start the self-optimisation please follow below steps:
1. Start rog agent in terminal by `python rxn_opt_goal_agent.py`
   > This is a workaround for occasional connection error when firing the goal request via webpage to ROG agent deployed in docker container. However, the same issue is not observed when directly sending goal request as `HTTP POST` from code. This is to be fixed in the next release.
2. Open `http://127.0.0.1:5000/goal` and fill up details for goal request, for details on what to specify, please refer to [RxnOptGoalAgent]
   > Before issuing the goal request, make sure to run a few reactions to let the system stablises


&nbsp;
# Wish list for future development
- Automated fault recovery


&nbsp;
# Authors #

Jiaru Bai (jb2197@cam.ac.uk) (March 2023)

<!-- Links -->
[RxnOptGoalAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/RxnOptGoalAgent
[RxnOptGoalIterAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/RxnOptGoalIterAgent
[DoEAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DoEAgent
[VapourtecScheduleAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/VapourtecScheduleAgent
[VapourtecAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/VapourtecAgent
[HPLCAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HPLCAgent
[HPLCPostProAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HPLCPostProAgent
[Blazegraph]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/db/blazegraph
[Fileserver]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/db/fileserver
[CMCL Docker Registry]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry

[AutoHotKey]: https://www.autohotkey.com/
