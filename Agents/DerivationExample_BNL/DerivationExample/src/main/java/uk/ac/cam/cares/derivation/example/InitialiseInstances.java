package uk.ac.cam.cares.derivation.example;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import javax.servlet.annotation.WebServlet;

import com.bigdata.rdf.sail.bench.NanoSparqlClient;
import org.json.JSONObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;


@WebServlet(urlPatterns = {"/InitialiseInstances"})
public class InitialiseInstances extends JPSAgent{

		private static final long serialVersionUID = 1L;

		private static final Logger LOGGER = LogManager.getLogger(InitialiseInstances.class);

		private static final String derivationInstanceBaseURL = "http://derivationexample.com/triplestore/repository/";

		@Override
		public JSONObject processRequestParameters (JSONObject requestParams)
		{


			Config.initProperties();
			RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl, Config.kgurl, Config.kguser, Config.kgpassword);
			SparqlClient sparqlClient = new SparqlClient(storeClient);
			DerivationClient devClient = new DerivationClient(storeClient,derivationInstanceBaseURL);


			LOGGER.info("Initialising new instances");

			TimeSeriesClient<Instant> timeSeriesClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
			timeSeriesClient.deleteAll();
			sparqlClient.clearKG();

			LOGGER.info("Instances deleted");

			// record the IRIs of the created instances to link them later
			String input = sparqlClient.createInputData();


			// attach timestamp to input
			devClient.addTimeInstance(input);


            return requestParams;
        }





}