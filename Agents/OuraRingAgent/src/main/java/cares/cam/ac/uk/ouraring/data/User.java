package cares.cam.ac.uk.ouraring.data;

public class User {
    private String iri;
    private String id;
    private String ouraApiKey;
    private HeartRateData heartRateData;

    public User(String id) {
        this.id = id;
        heartRateData = new HeartRateData();
    }

    public HeartRateData getHeartRateData() {
        return heartRateData;
    }

    public void setOuraApiKey(String ouraApiKey) {
        this.ouraApiKey = ouraApiKey;
    }

    public String getOuraApiKey() {
        return ouraApiKey;
    }

    public String getId() {
        return id;
    }

    public void setIri(String iri) {
        this.iri = iri;
    }

    public String getIri() {
        return iri;
    }
}
