package uk.ac.cam.cares.jsp.integration;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import static uk.ac.cam.cares.jsp.integration.KGObjects.botBuilding;
import static uk.ac.cam.cares.jsp.integration.KGObjects.p_bot;

public class KGAddress {
    private static final Logger LOGGER = LogManager.getLogger(KGObjects.class);
    static String icontactIRI = "http://ontology.eil.utoronto.ca/icontact.owl#";
    static Prefix icontact = SparqlBuilder.prefix("icontact",iri(icontactIRI));
    private String street;
    private String streetNum;
    private String zip_code;
    private String country;
    private String city;
    private String state;
    static Iri hasCity = icontact.iri("hasCity");
    static Iri hasCountry = icontact.iri("hasCountry");
    static Iri hasPostalCode = icontact.iri("hasPostalCode");
    static Iri hasState = icontact.iri("hasState");
    static Iri hasStreet = icontact.iri("hasStreet");
    static Iri hasStreetNum = icontact.iri("hasStreetNumber");
    static String envOnto = "https://www.theworldavatar.com/kg/ontobuiltenv/";
    static Prefix p_env = SparqlBuilder.prefix("env",iri(envOnto));
    static Iri hasAddress = p_env.iri("hasAddress");
    static Iri addressI = icontact.iri("Address");

    KGAddress() {}
    KGAddress(String street, String streetNum, String zip_code, String country, String city, String state) {
        this.street = street;
        this.streetNum = streetNum;
        this.zip_code = zip_code;
        this.country = country;
        this.city = city;
        this.state = state;
    }
    public String getStreet () {
        return this.street;
    }
    public String getStreetNum () {
        return this.streetNum;
    }
    public String getCountry () {
        return this.country;
    }
    public String getCity () {
        return this.city;
    }
    public String getPostalcode () {
        return this.zip_code;
    }
    public String getState () {
        return this.state;
    }
    public KGAddress queryAddress(RemoteStoreClient kgClient, String objectIri) {

        SelectQuery query = Queries.SELECT();

        Variable city = query.var();
        Variable country = query.var();
        Variable postalcode = query.var();
        Variable state = query.var();
        Variable street = query.var();
        Variable streetNum = query.var();
        Variable building = query.var();
        Variable address = query.var();


        GraphPattern[] queryPattern = {building.isA(botBuilding),
                building.has(hasAddress,address),
                address.isA(addressI),
                address.has(hasCity,city).optional(),
                address.has(hasCountry,country).optional(),
                address.has(hasPostalCode,postalcode).optional(),
                address.has(hasState,state).optional(),
                address.has(hasStreet,street).optional(),
                address.has(hasStreetNum,streetNum).optional()

        };
        query.prefix(icontact,p_bot, p_env).where(queryPattern).select(street, streetNum, state, city, country, postalcode);
        JSONArray queryResult = kgClient.executeQuery(query.getQueryString());
        this.street = queryResult.getJSONObject(0).toMap().get(street.getQueryString().substring(1)).toString();
        this.streetNum = queryResult.getJSONObject(0).toMap().get(streetNum.getQueryString().substring(1)).toString();
        this.city = queryResult.getJSONObject(0).toMap().get(city.getQueryString().substring(1)).toString();
        this.country = queryResult.getJSONObject(0).toMap().get(country.getQueryString().substring(1)).toString();
        this.zip_code = queryResult.getJSONObject(0).toMap().get(postalcode.getQueryString().substring(1)).toString();
        this.state = queryResult.getJSONObject(0).toMap().get(state.getQueryString().substring(1)).toString();

        return this;
    }
}
