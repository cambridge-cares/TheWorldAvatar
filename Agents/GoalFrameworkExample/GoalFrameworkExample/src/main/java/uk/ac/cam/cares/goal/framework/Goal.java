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



public class Goal {

    private String iri;

    private String goalRange; //GoalRange

    private String realState; //RealState

    private String rdfType;

    private String agentURL;//isAchievedUsing


    public Goal(String iri) {
        this.iri = iri;
        this.goalRange = goalRange;
        this.realState=realState;
    }

    public String getIri() {
        return iri;
    }

    public void setIri(String iri) {
        this.iri = iri;
    }

    public String getGoalRange() {
        return goalRange;
    }

    public void setGoalRange(String goalRange) {
        this.goalRange = goalRange;
    }

    public String getRealState() {
        return realState;
    }

    public void setRealState(String realState) {
        this.realState = realState;
    }



    public void setAgentURL(String agentURL) {
        this.agentURL = agentURL;
    }

    public String getAgentURL() {
        return this.agentURL;
    }



}
