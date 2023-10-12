package com.cmclinnovations.filteragent.utils;

import java.util.List;

import org.apache.commons.lang.StringUtils;

import com.cmclinnovations.filteragent.objects.Substitution;

public class ReplacementUtils {

    private ReplacementUtils() {
    }

    public static String userReplacements(List<Substitution> substitutionRequest, String query) {
        for (Substitution sub : substitutionRequest) {
            query = StringUtils.replace(query, sub.placeholder(), sub.substitution());
        }
        return query;
    }
}
