package com.cmclinnovations.emissions;

import java.io.IOException;
import java.net.URISyntaxException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.json.JSONTokener;

import com.cmclinnovations.emissions.objects.Chimney;

import uk.ac.cam.cares.jps.base.agent.DerivationAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationInputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * Takes in one ship IRI as input, queries the ship properties (type and speed
 * for now). These parameters are then passed to the speed load map agent to
 * generate emissions. Reference to convert ship speed into engine speed:
 * http://betterboat.com/average-boat-speed/ assume fastest medium boat max
 * speed= 25knot max rpm= 2500 rpm torque=constant=250Nm then 1knot=100 rpm rpm=
 * https://www.marineinsight.com/shipping-news/worlds-fastest-ship-built-tasmania-christened-argentinas-president/->fastest=58.1
 * knot knot*2500/58.1 roughly 1 ship 33 kg/h 1 boat=
 * 1.1338650741577147e-05*3600 = 0.041 kg/h NO2 (comparison of NO2
 * https://pdfs.semanticscholar.org/1bd2/52f2ae1ede131d0ef84ee21c84a73fb6b374.pdf)
 * 1 boat mass flux=0.0192143028723584 kg/s
 */
@WebServlet(urlPatterns = { "/" })
public class EmissionsAgent extends DerivationAgent {
    private static final Logger LOGGER = LogManager.getLogger(EmissionsAgent.class);
    private QueryClient queryClient;

    @Override
    public void processRequestParameters(DerivationInputs derivationInputs, DerivationOutputs derivationOutputs) {
        String shipIri = derivationInputs.getAllIris().get(0);
        Ship ship = queryClient.getShip(shipIri);

        // convert ship speed to rpm
        double speedRpm = ship.getSpeed() * 2500 / 58.1;
        if (speedRpm > 2500) {
            speedRpm = 2500;
        }
        JSONObject emissions = getSurogateValues(speedRpm, 250);
        Chimney chimney = new Chimney(emissions);
        addEmissionTriples(chimney, derivationOutputs, shipIri);
    }

    private JSONObject getSurogateValues(double speedRpm, double torqueNm) {
        JSONObject result;
        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            URIBuilder url = new URIBuilder(EnvConfig.PYTHON_SERVICE_URL + "/getEmissions");
            url.addParameter("speed", String.valueOf(speedRpm));
            url.addParameter("torque", String.valueOf(torqueNm));

            HttpGet httpGet = new HttpGet(url.build());
            CloseableHttpResponse pyresponse = httpclient.execute(httpGet);
            result = new JSONObject(new JSONTokener(pyresponse.getEntity().getContent()));
        } catch (URISyntaxException | IOException e) {
            LOGGER.error(e.getMessage());
            throw new JPSRuntimeException("Failed to evaluate surrogate values using python", e);
        }

        return result;
    }

    @Override
    public void init() throws ServletException {
        EndpointConfig endpointConfig = new EndpointConfig();
        RemoteStoreClient storeClient = new RemoteStoreClient(endpointConfig.getKgurl(),
                endpointConfig.getKgurl());
        RemoteRDBStoreClient remoteRDBStoreClient = new RemoteRDBStoreClient(endpointConfig.getDburl(),
                endpointConfig.getDbuser(), endpointConfig.getDbpassword());
        super.devClient = new DerivationClient(storeClient, QueryClient.PREFIX);

        queryClient = new QueryClient(storeClient, remoteRDBStoreClient);
    }

    /**
     * adds emissions triples to derivation outputs
     * 
     * @param chimney
     * @param derivationOutputs
     */
    void addEmissionTriples(Chimney chimney, DerivationOutputs derivationOutputs, String shipIri) {
        // common IRIs
        String hasQuantity = QueryClient.OM_STRING + "hasQuantity";
        String hasValue = QueryClient.OM_STRING + "hasValue";
        String hasNumericalValue = QueryClient.OM_STRING + "hasNumericalValue";
        String hasUnit = QueryClient.OM_STRING + "hasUnit";
        String kgs = QueryClient.OM_STRING + "kilogramPerSecond-Time";
        String kgm3 = QueryClient.OM_STRING + "kilogramPerCubicmetre";
        String kelvin = QueryClient.OM_STRING + "kelvin";

        String noxEmission = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.EMISSION);
        String uhcEmission = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.EMISSION);
        String coEmission = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.EMISSION);
        String so2Emission = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.EMISSION);
        String pm10Emission = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.EMISSION);
        String pm25Emission = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.EMISSION);

        String noxId = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX, QueryClient.NO_X);
        String uhcId = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX, QueryClient.UHC);
        String coId = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX, QueryClient.CO);
        String so2Id = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX, QueryClient.SO2);
        String pm10Id = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX, QueryClient.PM10);
        String pm25Id = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX, QueryClient.PM25);

        derivationOutputs.addTriple(noxEmission, QueryClient.HAS_POLLUTANT_ID, noxId);
        derivationOutputs.addTriple(uhcEmission, QueryClient.HAS_POLLUTANT_ID, uhcId);
        derivationOutputs.addTriple(coEmission, QueryClient.HAS_POLLUTANT_ID, coId);
        derivationOutputs.addTriple(so2Emission, QueryClient.HAS_POLLUTANT_ID, so2Id);
        derivationOutputs.addTriple(pm10Emission, QueryClient.HAS_POLLUTANT_ID, pm10Id);
        derivationOutputs.addTriple(pm25Emission, QueryClient.HAS_POLLUTANT_ID, pm25Id);

        // particle density is constant for all sizes (for now and is hardcoded in the
        // python script)
        String particleDensity = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.DENSITY);
        String particleDensityMeasure = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.MEASURE_STRING);

        derivationOutputs.addTriple(particleDensity, hasValue, particleDensityMeasure);
        derivationOutputs.addLiteral(particleDensityMeasure, hasNumericalValue, chimney.getParticleDensity());
        derivationOutputs.addTriple(particleDensityMeasure, hasUnit, kgm3);

        // mixture density, and temperature shared by all gas phase
        String density = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX, QueryClient.DENSITY);
        String densityMeasure = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.MEASURE_STRING);
        String temperature = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.TEMPERATURE);
        String temperatureMeasure = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.MEASURE_STRING);

        derivationOutputs.addTriple(density, hasValue, densityMeasure);
        derivationOutputs.addLiteral(densityMeasure, hasNumericalValue, chimney.getMixtureDensity());
        derivationOutputs.addTriple(densityMeasure, hasUnit, kgm3);
        derivationOutputs.addTriple(temperature, hasValue, temperatureMeasure);
        derivationOutputs.addLiteral(temperatureMeasure, hasNumericalValue, chimney.getMixtureTemperature());
        derivationOutputs.addTriple(temperatureMeasure, hasUnit, kelvin);

        String noxFlow = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.MASS_FLOW);
        String uhcFlow = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.MASS_FLOW);
        String coFlow = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX, QueryClient.MASS_FLOW);
        String so2Flow = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.MASS_FLOW);
        String pm10Flow = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.MASS_FLOW);
        String pm25Flow = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.MASS_FLOW);

        // the derivation outputs class should trim <>
        String noxMeasure = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.MEASURE_STRING);
        String uhcMeasure = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.MEASURE_STRING);
        String coMeasure = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.MEASURE_STRING);
        String so2Measure = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.MEASURE_STRING);
        String pm10Measure = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.MEASURE_STRING);
        String pm25Measure = derivationOutputs.createNewEntityWithBaseUrl(QueryClient.PREFIX,
                QueryClient.MEASURE_STRING);

        // certain pollutants are commented out to speed up aermod agent
        // nox
        derivationOutputs.addTriple(shipIri, QueryClient.EMITS, noxEmission);
        derivationOutputs.addTriple(noxEmission, hasQuantity, density);
        derivationOutputs.addTriple(noxEmission, hasQuantity, temperature);
        derivationOutputs.addTriple(noxEmission, hasQuantity, noxFlow);
        derivationOutputs.addTriple(noxFlow, hasValue, noxMeasure);
        derivationOutputs.addLiteral(noxMeasure, hasNumericalValue,
                chimney.getFlowrateNOx());
        derivationOutputs.addTriple(noxMeasure, hasUnit, kgs);

        // hc
        derivationOutputs.addTriple(shipIri, QueryClient.EMITS, uhcEmission);
        derivationOutputs.addTriple(uhcEmission, hasQuantity, density);
        derivationOutputs.addTriple(uhcEmission, hasQuantity, temperature);
        derivationOutputs.addTriple(uhcEmission, hasQuantity, uhcFlow);
        derivationOutputs.addTriple(uhcFlow, hasValue, uhcMeasure);
        derivationOutputs.addLiteral(uhcMeasure, hasNumericalValue,
                chimney.getFlowrateHC());
        derivationOutputs.addTriple(uhcMeasure, hasUnit, kgs);

        // co
        derivationOutputs.addTriple(shipIri, QueryClient.EMITS, coEmission);
        derivationOutputs.addTriple(coEmission, hasQuantity, density);
        derivationOutputs.addTriple(coEmission, hasQuantity, temperature);
        derivationOutputs.addTriple(coEmission, hasQuantity, coFlow);
        derivationOutputs.addTriple(coFlow, hasValue, coMeasure);
        derivationOutputs.addLiteral(coMeasure, hasNumericalValue,
                chimney.getFlowrateCO());
        derivationOutputs.addTriple(coMeasure, hasUnit, kgs);

        // so2
        derivationOutputs.addTriple(shipIri, QueryClient.EMITS, so2Emission);
        derivationOutputs.addTriple(so2Emission, hasQuantity, density);
        derivationOutputs.addTriple(so2Emission, hasQuantity, temperature);
        derivationOutputs.addTriple(so2Emission, hasQuantity, so2Flow);
        derivationOutputs.addTriple(so2Flow, hasValue, so2Measure);
        derivationOutputs.addLiteral(so2Measure, hasNumericalValue, chimney.getFlowrateSO2());
        derivationOutputs.addTriple(so2Measure, hasUnit, kgs);

        // pm10
        derivationOutputs.addTriple(shipIri, QueryClient.EMITS, pm10Emission);
        derivationOutputs.addTriple(pm10Emission, hasQuantity, particleDensity);
        derivationOutputs.addTriple(pm10Emission, hasQuantity, pm10Flow);
        derivationOutputs.addTriple(pm10Flow, hasValue, pm10Measure);
        derivationOutputs.addLiteral(pm10Measure, hasNumericalValue,
                chimney.getPm10());
        derivationOutputs.addTriple(pm10Measure, hasUnit, kgs);

        // pm25
        derivationOutputs.addTriple(shipIri, QueryClient.EMITS, pm25Emission);
        derivationOutputs.addTriple(pm25Emission, hasQuantity, particleDensity);
        derivationOutputs.addTriple(pm25Emission, hasQuantity, pm25Flow);
        derivationOutputs.addTriple(pm25Flow, hasValue, pm25Measure);
        derivationOutputs.addLiteral(pm25Measure, hasNumericalValue,
                chimney.getPm25());
        derivationOutputs.addTriple(pm25Measure, hasUnit, kgs);
    }
}
