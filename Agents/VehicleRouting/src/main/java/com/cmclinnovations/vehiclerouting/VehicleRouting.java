package com.cmclinnovations.vehiclerouting;

import java.io.IOException;
import java.time.Duration;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.cmclinnovations.vehiclerouting.objects.Customer;
import com.cmclinnovations.vehiclerouting.objects.Distance;
import com.cmclinnovations.vehiclerouting.objects.Truck;
import com.cmclinnovations.vehiclerouting.objects.Customer.CustomerState;

@WebServlet(urlPatterns = { "/" })
public class VehicleRouting extends HttpServlet {
    private static final Logger LOGGER = LogManager.getLogger(VehicleRouting.class);
    private static int numTruck = 4;
    private static int depot = 0;

    public static void main(String[] arg) {
        ZonedDateTime currentTime = ZonedDateTime.now();

        List<Truck> trucks = new ArrayList<>();

        for (int i = 0; i < numTruck; i++) {
            trucks.add(new Truck(depot, currentTime));
        }

        List<Customer> customers = initCustomers(currentTime);

        SimpleOptimiser optimiser = new SimpleOptimiser(customers, trucks);
        Optimiser realOptimiser = new Optimiser(customers, trucks);

        while (!customers.stream().allMatch(c -> c.getCustomerState() == CustomerState.ORDER_COMPLETE)) {
            // find new customers for trucks that don't have a customer
            if (customers.stream().anyMatch(c -> c.getCustomerState() == CustomerState.REQUESTED_TRUCK)) {
                for (Truck truck : trucks) {
                    if (!truck.hasCustomer()) {
                        truck.setCustomer(optimiser.getNextCustomer(truck), currentTime);
                    }
                }
            }

            // find truck with the shortest waiting time
            List<Truck> trucksWithCustomers = trucks.stream().filter(Truck::hasCustomer).collect(Collectors.toList());
            List<Truck> trucksWithShortestWaitingTime = new ArrayList<>();
            trucksWithShortestWaitingTime.add(trucksWithCustomers.get(0));

            Duration shortestWaitingTime = trucksWithShortestWaitingTime.get(0).getWaitingTime();

            for (int i = 1; i < trucksWithCustomers.size(); i++) {
                Truck truck = trucksWithCustomers.get(i);

                // no need to bother truck without a customer
                if (!truck.hasCustomer()) {
                    continue;
                }

                if (truck.getWaitingTime().compareTo(shortestWaitingTime) < 0) {
                    trucksWithShortestWaitingTime = new ArrayList<>();
                    trucksWithShortestWaitingTime.add(truck);
                    shortestWaitingTime = truck.getWaitingTime();
                } else if (truck.getWaitingTime().compareTo(shortestWaitingTime) == 0) {
                    trucksWithShortestWaitingTime.add(truck);
                }
            }

            // move forward in time
            currentTime = currentTime.plus(shortestWaitingTime);
            for (Truck truck : trucksWithShortestWaitingTime) {
                // once a truck "served" a customer, hasCustomer will return false
                truck.servedCustomer(currentTime);
            }

            for (Truck truck : trucks) {
                if (truck.hasCustomer()) {
                    truck.subtractWaitingTime(shortestWaitingTime);
                }
            }
        }

        trucks.forEach(truck -> {
            truck.returnToBase();
            System.out.println(truck.getVisitedLocations());
            System.out.println(truck.getVisitedTimes());
        });
    }

    static List<Customer> initCustomers(ZonedDateTime time) {
        int numCustomer = 16;
        List<Customer> customers = new ArrayList<>();
        // hardcoded customers
        for (int i = 0; i < numCustomer; i++) {
            Customer customer = new Customer(i + 1); // i + 1 is the location index in the distance matrix
            customer.setState(Customer.CustomerState.REQUESTED_TRUCK, time);
            customers.add(customer);
        }
        return customers;
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
    }
}
