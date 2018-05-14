package uk.ac.cam.cares.jps.men.entity;


public class Parameters {
	
	static public double INTERNATIONAL_MARKET_PRICE_FACTOR = 1.05;

	// default as in preprint 164; tax per t CO2
	public double carbonTax = 50.;
	public double interestFactor = 1.;
	public double annualCostFactor = 1.;
	// if true then the lowest price of all local sources for a product is used as base price for international market
	// if false then the highest price is used
	public boolean internationalMarketLowestPrice = true;
	// default as in preprint 164
	// the base price for international market is multiplied with this factor
	public double internationalMarketPriceFactor = 1.05;
	
	public Parameters(double carbonTax, double interestFactor, double annualCostFactor, double internationalMarketPriceFactor, boolean internationalMarketLowestPrice) {
		this.carbonTax = carbonTax;
		this.interestFactor = interestFactor;
		this.annualCostFactor = annualCostFactor;
		this.internationalMarketPriceFactor = internationalMarketPriceFactor;
		this.internationalMarketLowestPrice = internationalMarketLowestPrice;
	}
	
	public String toString() {
		return "Parameters[" + carbonTax + ", " + interestFactor + ", " + annualCostFactor + ", " + internationalMarketPriceFactor + ", " + internationalMarketLowestPrice + "]"; 
	}
}
