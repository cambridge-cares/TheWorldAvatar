package uk.ac.cam.cares.jps.model;

public class Price {
    String amount;
    String currency;

    public Price(String amount, String currency) {
        this.amount = amount;
        this.currency = currency;
    }

    @Override
    public String toString() {
        return amount + " " + currency;
    }

}
