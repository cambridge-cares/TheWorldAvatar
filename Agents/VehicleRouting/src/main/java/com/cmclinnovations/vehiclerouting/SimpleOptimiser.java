package com.cmclinnovations.vehiclerouting;

import java.util.List;
import java.util.stream.Collectors;

import com.cmclinnovations.vehiclerouting.objects.Customer;
import com.cmclinnovations.vehiclerouting.objects.Customer.CustomerState;
import com.cmclinnovations.vehiclerouting.objects.Distance;
import com.cmclinnovations.vehiclerouting.objects.Truck;

public class SimpleOptimiser {
    private List<Customer> customers;
    private List<Truck> trucks;

    public SimpleOptimiser(List<Customer> customers, List<Truck> trucks) {
        this.customers = customers;
        this.trucks = trucks;
    }

    public Customer getNextCustomer(Truck truck) {
        if (!trucks.contains(truck)) {
            throw new RuntimeException("Provided truck is not in overall list");
        }

        List<Customer> availableCustomers = customers.stream()
                .filter(c -> c.getCustomerState() == CustomerState.REQUESTED_TRUCK)
                .collect(Collectors.toList());

        return Distance.getNearestCustomer(truck, availableCustomers);
    }
}
