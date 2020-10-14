package uk.ac.cam.cares.jps.agent.file_management;

import java.util.ArrayList;

import com.fasterxml.jackson.databind.ObjectMapper;

import uk.ac.cam.cares.jps.agent.configuration.AutoMechCalibAgentProperty;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.MoDSMarshaller;
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

public class InitMoDSInputs extends MoDSMarshaller implements IInitMoDSInputs {
	public InitMoDSInputs(AutoMechCalibAgentProperty autoMechCalibAgentProperty) {
		super(autoMechCalibAgentProperty);
	}
	
	public void init() {
		mods = new MoDS();
		
		detailS = new DetailS();
		detail = new Detail();
		detailList = new ArrayList<Detail>();
		
		algorithmS = new AlgorithmS();
		algorithm = new Algorithm();
		algorithmList = new ArrayList<Algorithm>();
		
		modelS = new ModelS();
		model = new Model();
		modelList = new ArrayList<Model>();
		
		caseS = new CaseS();
		caseA = new Case();
		caseList = new ArrayList<Case>();
		
		fileS = new FileS();
		file = new File();
		fileList = new ArrayList<File>();
		
		functionS = new FunctionS();
		function = new Function();
		functionList = new ArrayList<Function>();
		
		parameterS = new ParameterS();
		parameter = new Parameter();
		parameterList = new ArrayList<Parameter>();
		
		parameterCaseS = new CaseS();
		parameterCase = new Case();
		parameterCaseList = new ArrayList<Case>();
		
		parameterModelS = new ModelS();
		parameterModel = new Model();
		parameterModelList = new ArrayList<Model>();
		
		parameterFileS = new FileS();
		parameterFile = new File();
		parameterFileList = new ArrayList<File>();
		initialRead = new InitialRead();
		workingRead = new WorkingRead();
		workingWrite = new WorkingWrite();
		
		modsJsonString = new String();
		
		modsJsonNode = new ObjectMapper().createObjectNode();
	}
}