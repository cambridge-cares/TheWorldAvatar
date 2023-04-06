package com.cmclinnovations.mods.modssimpleagent.simulations;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import com.cmclinnovations.mods.api.MoDSAPI;
import com.cmclinnovations.mods.api.MoDSAPI.DataType;
import com.cmclinnovations.mods.api.Options;
import com.cmclinnovations.mods.modssimpleagent.BackendInputFile;
import com.cmclinnovations.mods.modssimpleagent.FileGenerator.FileGenerationException;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Algorithm;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Data;
import com.cmclinnovations.mods.modssimpleagent.datamodels.DataColumn;
import com.cmclinnovations.mods.modssimpleagent.datamodels.InputMetaData;
import com.cmclinnovations.mods.modssimpleagent.datamodels.InputMetaDataRow;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Request;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Variable;
import com.cmclinnovations.mods.modssimpleagent.utils.ListUtils;
import com.google.common.collect.Streams;

import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

class MOOonly extends Simulation {

        public MOOonly(Request request, BackendInputFile inputFile, MoDSBackend modsBackend, InputMetaData inputMetaData)
                        throws IOException {
                super(request, inputFile, modsBackend, inputMetaData);
        }

        @Override
        protected Algorithm getPrimaryAlgorithm() {
                return getAlgorithmOfType("MOO");
        }

        @Override
        protected void populateAlgorithmNodes(List<Variable> variables) {
                populateMOOAlgorithmNode(Simulation.DEFAULT_MOO_ALGORITHM_NAME, getPrimaryAlgorithm().getName(),
                                variables);
                super.populateAlgorithmNodes(variables);
        }

        @Override
        protected void generateFiles() throws FileGenerationException {
                generateInitialFileFromMetaData();
                super.generateFiles();
        }

        @Override
        public Request getResults() {

                String simDir = getModsBackend().getSimDir().toString();
                String algorithmName = Simulation.DEFAULT_MOO_ALGORITHM_NAME;

                if (!MoDSAPI.hasAlgorithmGeneratedOutputFiles(simDir, algorithmName)) {
                        throw new ResponseStatusException(
                                        HttpStatus.NO_CONTENT,
                                        "The multi-objective optimisation job with job '" + getModsBackend().getJobID()
                                                        + "' has not finished yet, has not been run or has failed to run correctly.");
                }

                List<Variable> variables = getPrimaryAlgorithm().getVariables();

                List<String> outputVarNames = MoDSAPI.getReducedYVarIDs(simDir, algorithmName).stream()
                                .map(MoDSAPI::getVarName)
                                .collect(Collectors.toList());

                List<Double> minimaFromData = ListUtils.filterAndSort(getInputMetaData().getRows(), outputVarNames, InputMetaDataRow::getVarName,
                                InputMetaDataRow::getMinimum);

                List<Double> maximaFromData = ListUtils.filterAndSort(getInputMetaData().getRows(), outputVarNames, InputMetaDataRow::getVarName,
                                InputMetaDataRow::getMaximum);

                List<Double> minimaFromAlg = ListUtils.filterAndSort(variables, outputVarNames, Variable::getName,
                                Variable::getMinimum);

                List<Double> maximaFromAlg = ListUtils.filterAndSort(variables, outputVarNames, Variable::getName,
                                Variable::getMaximum);

                List<Double> weightsFromAlg = ListUtils.filterAndSort(variables, outputVarNames, Variable::getName,
                                Variable::getWeight);

                int numResults = getPrimaryAlgorithm().getMaxNumberOfResults();

                List<List<Double>> points = MoDSAPI.getMCDMSimpleWeightedPoints(simDir, algorithmName,
                                ListUtils.replaceNulls(minimaFromAlg, minimaFromData),
                                ListUtils.replaceNulls(maximaFromAlg, maximaFromData),
                                ListUtils.replaceNulls(weightsFromAlg, Collections.nCopies(weightsFromAlg.size(), 1.0)),
                                numResults, new Options().setVarIndexFirst(true)).get(DataType.OutputVariable);

                Data outputValues = new Data(
                                Streams.zip(outputVarNames.stream(), points.stream(), DataColumn::new)
                                                .collect(Collectors.toList()));

                Request results = super.getResults();
                results.setOutputs(outputValues);

                return results;
        }

}
