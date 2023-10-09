package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;

public record SensitivityResult(String name, List<SensitivityLabels> labels, List<SensitivityValues> values) {
}
