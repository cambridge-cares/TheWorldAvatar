package com.cmclinnovations.mods.modssimpleagent.simulations;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import com.cmclinnovations.mods.api.MoDSAPI;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Algorithm;
import com.cmclinnovations.mods.modssimpleagent.datamodels.Request;
import com.cmclinnovations.mods.modssimpleagent.datamodels.SensitivityLabels;
import com.cmclinnovations.mods.modssimpleagent.datamodels.SensitivityResult;
import com.cmclinnovations.mods.modssimpleagent.datamodels.SensitivityValues;

@ExtendWith(MockitoExtension.class)
public class SensitivityTest {

    @Test
    void testGetResponse() throws IOException {
        MoDSBackend modsBackendMock = mock(MoDSBackend.class);
        when(modsBackendMock.getSimDir()).thenReturn(Path.of("123"));

        Sensitivity sensitivitySimulation = new Sensitivity(
                Request.builder()
                        .algorithms(List.of(new Algorithm(null, "GenSurrogateAlg", null, null, null, null)))
                        .build(),
                null, modsBackendMock, null, null);

        List<SensitivityLabels> sensitivityLabelsList = List.of(
                new SensitivityLabels(1, List.of("x1", "x2", "x3")),
                new SensitivityLabels(2, List.of("x1 and x2", "x1 and x3", "x2 and x3")));

        List<SensitivityResult> sensitivities = List.of(
                new SensitivityResult("y1", sensitivityLabelsList,
                        List.of(
                                new SensitivityValues(1, List.of(1.1, 1.2, 1.3)),
                                new SensitivityValues(2, List.of(1.12, 1.13, 1.23)))),
                new SensitivityResult("y2", sensitivityLabelsList,
                        List.of(
                                new SensitivityValues(1, List.of(2.1, 2.2, 2.3)),
                                new SensitivityValues(2, List.of(2.12, 2.13, 2.23)))),
                new SensitivityResult("y3", sensitivityLabelsList,
                        List.of(
                                new SensitivityValues(1, List.of(3.1, 3.2, 3.3)),
                                new SensitivityValues(2, List.of(3.12, 3.13, 3.23)))));

        try (MockedStatic<MoDSAPI> api = Mockito.mockStatic(MoDSAPI.class)) {
            api.when(() -> MoDSAPI.getXVarNames(anyString(), anyString()))
                    .thenReturn(List.of("x1", "x2", "x3"));
            api.when(() -> MoDSAPI.getYVarNames(anyString(), anyString()))
                    .thenReturn(List.of("y1", "y2", "y3"));
            api.when(() -> MoDSAPI.getHDMRSensitivities(anyString(), anyString()))
                    .thenReturn(List.of(
                            List.of( // y1
                                    List.of(1.1, 1.2, 1.3), // order 1
                                    List.of(1.12, 1.13, 1.23)), // order 2
                            List.of( // y2
                                    List.of(2.1, 2.2, 2.3), // order 1
                                    List.of(2.12, 2.13, 2.23)), // order 2
                            List.of( // y3
                                    List.of(3.1, 3.2, 3.3), // order 1
                                    List.of(3.12, 3.13, 3.23))) // order 2
                    );
            assertEquals(sensitivities, sensitivitySimulation.getResponse().sensitivities());
        }
    }
}
