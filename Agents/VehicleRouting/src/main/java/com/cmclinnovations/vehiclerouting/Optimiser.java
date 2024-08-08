package com.cmclinnovations.vehiclerouting;

import java.util.List;

import com.cmclinnovations.vehiclerouting.objects.Customer;
import com.cmclinnovations.vehiclerouting.objects.Truck;

/**
 * slightly more complex than just finding the nearest
 */
public class Optimiser {
    private List<Customer> customers;
    private List<Truck> trucks;
    private List<List<Customer>> bookedReturnTrip;
    private static final int MAX_DISTANCE = 3000;

    public Optimiser(List<Customer> customers, List<Truck> trucks) {
        this.customers = customers;
        this.trucks = trucks;
    }

    public Customer getNextCustomer(Truck truck) {
        return null;
    }
}
