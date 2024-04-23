package uk.ac.cam.cares.goal.example;

import javax.servlet.annotation.WebServlet;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.goal.framework.Goal;
import uk.ac.cam.cares.goal.framework.GoalClient;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.util.List;


@WebServlet(urlPatterns = {"/UpdateGoals"})
public class UpdateGoals extends JPSAgent {

   /**
    *  This servlet queries for all Goals
    */
   private static final long serialVersionUID = 1L;
   private static final Logger LOGGER = LogManager.getLogger(UpdateGoals.class);

   private static final String GoalRange_key ="GOALRANGE";
   private static final String RealState_key= "REALSTATE";

   @Override
   public JSONObject processRequestParameters(JSONObject requestParams) {

       Config.initProperties();
       RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
       GoalClient goalClient = new GoalClient(storeClient,InitialiseInstances.goalInstanceBaseURL);

       //Query for all goals
       List<String> goals = goalClient.getAllGoals();


       String res_msg = "Updated goals, retrieved GoalRange, RealState";

       LOGGER.info(res_msg);
       JSONObject response = new JSONObject();

       response.put("status", res_msg);
       return response;
   }
}
