package com.cmclinnovations.jps.agent.file_management;

import java.util.ArrayList;

import com.cmclinnovations.jps.agent.file_management.marshallr.MoDSMarshaller;
import com.cmclinnovations.jps.agent.file_management.mods.MoDS;
import com.cmclinnovations.jps.agent.file_management.mods.algorithms.Algorithm;
import com.cmclinnovations.jps.agent.file_management.mods.algorithms.AlgorithmS;
import com.cmclinnovations.jps.agent.file_management.mods.cases.Case;
import com.cmclinnovations.jps.agent.file_management.mods.cases.CaseS;
import com.cmclinnovations.jps.agent.file_management.mods.files.File;
import com.cmclinnovations.jps.agent.file_management.mods.files.FileS;
import com.cmclinnovations.jps.agent.file_management.mods.functions.Function;
import com.cmclinnovations.jps.agent.file_management.mods.functions.FunctionS;
import com.cmclinnovations.jps.agent.file_management.mods.models.Model;
import com.cmclinnovations.jps.agent.file_management.mods.models.ModelS;
import com.cmclinnovations.jps.agent.file_management.mods.parameters.Detail;
import com.cmclinnovations.jps.agent.file_management.mods.parameters.DetailS;
import com.cmclinnovations.jps.agent.file_management.mods.parameters.InitialRead;
import com.cmclinnovations.jps.agent.file_management.mods.parameters.Parameter;
import com.cmclinnovations.jps.agent.file_management.mods.parameters.ParameterS;
import com.cmclinnovations.jps.agent.file_management.mods.parameters.WorkingRead;
import com.cmclinnovations.jps.agent.file_management.mods.parameters.WorkingWrite;

public class InitMoDSInputs extends MoDSMarshaller implements IInitMoDSInputs {
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
	}
}