package uk.ac.cam.cares.jps.agent.mechanism.sensana;

/**
 * This enumerated list defines the name of important properties of</br>
 * MoDSSensAnaAgent. Some example properties are:</br>
 * - the name of the agent class</br>
 * - the name of the workspace folder on the machine where the agent runs</br>
 * - the name of the workspace folder on HPC where MoDS runs</br>
 * 
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */
public enum Property {
	
	AGENT_WORKSPACE_PARENT_DIR(uk.ac.cam.cares.jps.base.slurm.job.Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName()),
	JOB_NO_OF_CORES_PREFIX("%nprocshared="), //double check
	JOB_MEMORY_PREFIX("%mem="),
	JOB_MEMORY_UNITS("GB"),
	JOB_CHK_POINT_FILE_PREFIX("%Chk="),
	SPECIES_CHARGE_ZERO("0"), // maybe not be used
	SPECIES_MULTIPLICITY("1"), // maybe not be used
	RDF4J_SERVER_URL_FOR_LOCALHOST("http://localhost:8080/rdf4j-server/"),
	
	FUSAKI_URL_FOR_WORLD_AVATAR("http://www.theworldavatar.com/damecoolquestion/agents/query?query="), // maybe need to tailor
	RDF4J_ONTOSPECIES_REPOSITORY_ID("ontospecies"), // change to ontochemexp
	RDF4J_ONTOCHEMEXP_REPOSITORY_ID("ontochemexp"),
	RDF4J_ONTOKIN_REPOSITORY_ID("ontokin"),
	PREFIX_BINDING_ONTOSPECIES("PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> \n"), // prefix ontochemexp
	PREFIX_BINDING_ONTOCHEMEXP("PREFIX OntoChemExp: <http://www.theworldavatar.com/ontology/ontochemexp/OntoChemExp.owl#> \n"),
	PREFIX_BINDING_ONTOKIN("PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#> \n"),
	PREFIX_BINDING_RDFS("PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n"),
	PREFIX_BINDING_RDF("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n"),
	PREFIX_BINDING_MSM("PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>"), // double check
	PREFIX_BINDING_RAM("PREFIX ram: <http://cookingbigdata.com/linkeddata/ccinstances#>"), // double check
	PREFIX_MSM("msm"),
	PREFIX_RAM("ram"),
	MODS_AGENT_IRI("<http://www.theworldavatar.com/kb/agents/Service__DFT.owl#Service>"), // double check
	MODEL_KINETICS("kineticsSRM"),
	MODEL_KINETICS_EXE("../../../../runKineticsSRM.sh"),
	MODEL_KINETICS_SCRIPT("runKineticsSRM.sh"),
	MODEL_KINETICS_OUTPUT("OutputCase00001Cyc0001Info.csv"),
	MODEL_CANTERA("canteraLFS"),
	MODEL_CANTERA_EXE("python"),
	MODEL_CANTERA_SCRIPT("runCanteraLFS.py"),
	MODEL_CANTERA_ARG("../../../../runCanteraLFS.py"),
	MODEL_CANTERA_MULTI_OPT("/home/jb2197/Codes_kinetics/cantera/cantera_LFS_multi.py"),
	MODEL_CANTERA_MIX_AVERAGE_OPT("/home/jb2197/Codes_kinetics/cantera/cantera_LFS_mix_average.py"),
	MODEL_CANTERA_OUTPUT("OutputCase00001Lfs0001Info.csv");
	
	private String propertyName;
	private int value;
	private Property(String propertyName) {
		this.propertyName = propertyName;
	}
	
	public String getPropertyName() {
		return propertyName;
	}
	
	private Property(final int newValue) {
		value = newValue;
	}
	
	public int getValue() {
		return value;
	}
}
