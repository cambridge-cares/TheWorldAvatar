package uk.ac.cam.cares.jps.agent.file_management;

import java.util.ArrayList;

import com.fasterxml.jackson.databind.JsonNode;

import uk.ac.cam.cares.jps.agent.file_management.mods.MoDS;
import uk.ac.cam.cares.jps.agent.file_management.mods.algorithms.Algorithm;
import uk.ac.cam.cares.jps.agent.file_management.mods.algorithms.AlgorithmS;
import uk.ac.cam.cares.jps.agent.file_management.mods.cases.Case;
import uk.ac.cam.cares.jps.agent.file_management.mods.cases.CaseS;
import uk.ac.cam.cares.jps.agent.file_management.mods.files.File;
import uk.ac.cam.cares.jps.agent.file_management.mods.files.FileS;
import uk.ac.cam.cares.jps.agent.file_management.mods.functions.Function;
import uk.ac.cam.cares.jps.agent.file_management.mods.functions.FunctionS;
import uk.ac.cam.cares.jps.agent.file_management.mods.models.Model;
import uk.ac.cam.cares.jps.agent.file_management.mods.models.ModelS;
import uk.ac.cam.cares.jps.agent.file_management.mods.parameters.Detail;
import uk.ac.cam.cares.jps.agent.file_management.mods.parameters.DetailS;
import uk.ac.cam.cares.jps.agent.file_management.mods.parameters.InitialRead;
import uk.ac.cam.cares.jps.agent.file_management.mods.parameters.Parameter;
import uk.ac.cam.cares.jps.agent.file_management.mods.parameters.ParameterS;
import uk.ac.cam.cares.jps.agent.file_management.mods.parameters.WorkingRead;
import uk.ac.cam.cares.jps.agent.file_management.mods.parameters.WorkingWrite;

public class MoDSInputsState {
	public static MoDS mods;
	
	public static DetailS detailS;
	public static Detail detail;
	public static ArrayList<Detail> detailList;
	
	public static AlgorithmS algorithmS;
	public static Algorithm algorithm;
	public static ArrayList<Algorithm> algorithmList;
	
	public static ModelS modelS;
	public static Model model;
	public static ArrayList<Model> modelList;
	
	public static CaseS caseS;
	public static Case caseA;
	public static ArrayList<Case> caseList;
	
	public static FileS fileS;
	public static File file;
	public static ArrayList<File> fileList;
	
	public static FunctionS functionS;
	public static Function function;
	public static ArrayList<Function> functionList;
	
	public static ParameterS parameterS;
	public static Parameter parameter;
	public static ArrayList<Parameter> parameterList;
	
	public static CaseS parameterCaseS;
	public static Case parameterCase;
	public static ArrayList<Case> parameterCaseList;
	
	public static ModelS parameterModelS;
	public static Model parameterModel;
	public static ArrayList<Model> parameterModelList;
	
	public static FileS parameterFileS;
	public static File parameterFile;
	public static ArrayList<File> parameterFileList;
	public static InitialRead initialRead;
	public static WorkingRead workingRead;
	public static WorkingWrite workingWrite;
	
	public static IInitMoDSInputs initMoDSInputs;
	
	public static String modsJsonString;
	
	public static JsonNode modsJsonNode;
	
	public static final String EMPTY = "";
	public static final String HASH = "#";
	public static final String SPACE = " ";
	public static final String COLON = ":";
	public static final String UNDERSCORE = "_";
	public static final String BACKSLASH = "/";
	public static final String HYPHEN = "-";
	public static final String FRONTSLASH = "\\";
	public static final String FLOAT = "float";
	public static final String INTEGER = "integer";
	public static final String STRING = "string";
	
	public static final String FOLDER_ROOT = System.getProperty("user.home");
	public static final String FOLDER_DOCUMENTS = "Documents";
	public static final String FOLDER_JOB_FOLDER = "JobFolder";
	public static final String FOLDER_TEMPORARY = "Temporary";
	public static final String FOLDER_ALL = "All";
	public static final String FOLDER_INITIAL = "Initial";
	public static final String FOLDER_WORKING_DIR = "Working_dir";
	
	public static final String FILE_MODS_PREFIX = "MoDS";
	public static final String FILE_MODS_ACTIVE_SUFFIX = "ActiveInit.csv";
	public static final String FILE_MODS_PASSIVE_SUFFIX = "PassiveANDOut.csv";
	public static final String FILE_KINETICS_INPUTPARAMS = "InputParams.xml";
	public static final String FILE_MECHANISM = "mechanism.xml";
	public static final String FILE_MODEL_EXPDATA_SUFFIX = "Data.csv";
	public static final String FILE_MODS_INPUTS = "MoDS_inputs.xml";
	public static final String FILE_MECHANISM_BASE = "mechanism_base.xml";
	public static final String FILE_MECHANISM_CANTERA = "mechanism_cantera_lfs.xml";
	public static final String FILE_MECHANISM_ELEMENT = "element_data.xml";
	public static final String FILE_CANTERA_LFSSIMULATION = "LFS_simulation.csv";
	public static final String FILE_MODS_SLURM_SCRIPT = "modsslurm_como.sh";
	public static final String FILE_KINETICSSRM_SCRIPT = "runKineticsSRM.sh";
	public static final String FILE_CANTERALFS_SCRIPT = "runCanteraLFS.py";
	public static final String FILE_COMO_CANTERA_CTML = "convert_kinetics_ctml_to_cantera.xslt";
	
	public static final String INITIALISATION_STRING = "DeleteAfterInitialisation";
	public static final String INITIALISATION_STRING_ALGORITHMS = "{\"algorithm\": [{\"name\": \"DeleteAfterInitialisation\"}] }";
	public static final String INITIALISATION_STRING_MODELS = "{\"model\": [{\"name\": \"DeleteAfterInitialisation\"}] }";
	public static final String INITIALISATION_STRING_CASES = "{\"case\": [{\"name\": \"DeleteAfterInitialisation\"}] }";
	public static final String INITIALISATION_STRING_FILES= "{\"file\": [{\"name\": \"DeleteAfterInitialisation\"}] }";
	public static final String INITIALISATION_STRING_FUNCTIONS = "{\"function\": [{\"name\": \"DeleteAfterInitialisation\"}] }";
	public static final String INITIALISATION_STRING_PARAMETERS = "{\"parameter\": [{\"name\": \"DeleteAfterInitialisation\"}] }";	
	
	public static final String NAME_OXIDISER = "Fuel-Ox-Mix";
//	public static final String MODEL_KINETICS = "kineticsSRM";
//	public static final String MODEL_CANTERA = "canteraLFS";
	
	
	public static String jobFolderPath;
	public static String folderTemporaryPath;
	public static String folderAllPath;
	public static String folderInitialPath;
	public static String folderWorkingDirPath;
}
