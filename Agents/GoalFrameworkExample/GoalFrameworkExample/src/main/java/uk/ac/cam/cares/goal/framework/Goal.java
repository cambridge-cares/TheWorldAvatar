package uk.ac.cam.cares.goal.framework;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.json.JSONObject;

import javax.swing.text.html.parser.Entity;


public class Goal {

    private String iri;

    private Entity goalRange; //GoalRange

    private Entity realState; //RealState

    private String rdfType;

    private String agentURL;//isAchievedUsing


    public Goal(String iri, String rdfType) {
        this.iri = iri;
        this.rdfType = rdfType;
        this.goalRange = goalRange;
        this.realState=realState;


    }


    public void setAgentURL(String agentURL) {
        this.agentURL = agentURL;
    }

    public String getAgentURL() {
        return this.agentURL;
    }



}
