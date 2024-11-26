package uk.ac.cam.cares.jps.model;

public class Price {
    Double amount;
    String currency;

    public Price(Double amount, String currency) {
        this.amount = amount;
        this.currency = currency;
    }

    @Override
    public String toString() {
        if(amount == 0){
            return "Free";
        }
        return amount + " " + currency;
    }

}
