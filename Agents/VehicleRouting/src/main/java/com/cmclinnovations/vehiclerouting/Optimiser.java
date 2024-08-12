package com.cmclinnovations.vehiclerouting;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.cmclinnovations.vehiclerouting.objects.Customer;
import com.cmclinnovations.vehiclerouting.objects.Distance;
import com.cmclinnovations.vehiclerouting.objects.Customer.CustomerState;
import com.cmclinnovations.vehiclerouting.objects.Truck;

import org.apache.commons.math3.ml.clustering.KMeansPlusPlusClusterer;
import org.apache.commons.math3.ml.clustering.Clusterable;
import org.apache.commons.math3.ml.clustering.CentroidCluster;
import org.apache.commons.math3.ml.distance.EuclideanDistance;

/**
 * slightly more complex than just finding the nearest
 */
public class Optimiser {
    private List<Customer> customers;
    private List<Truck> trucks;
    private List<List<Customer>> optimisedRoutes;

    public Optimiser(List<Customer> customers, List<Truck> trucks) {
        this.customers = customers;
        this.trucks = trucks;
        optimisedRoutes = new ArrayList<>();

        List<Clusterable> points = new ArrayList<>();
        customers.stream().forEach(points::add);

        KMeansPlusPlusClusterer<Clusterable> clusterer = new KMeansPlusPlusClusterer<>(trucks.size(), 10000,
                new EuclideanDistance());

        Collection<CentroidCluster<Clusterable>> clusters = clusterer.cluster(points);

        for (CentroidCluster<Clusterable> cluster : clusters) {
            List<Customer> customerList = cluster.getPoints().stream().map(p -> (Customer) p)
                    .collect(Collectors.toList());

            // this will sort customers in ascending order based on location from base
            Collections.sort(customerList, Comparator.comparing(c -> Distance.getDistance(0, c.getLocation())));
            List<Customer> optimisedRoute = new ArrayList<>();
            if (customerList.size() > 3) {
                // find midpoint between the two closest points
                double[] point1 = customerList.get(0).getPoint();
                double[] point2 = customerList.get(1).getPoint();

                double[] midpoint = { (point1[0] + point2[0]) / 2, (point1[1] + point2[1]) / 2 };

                // create an equation from base to midpoint
                double[] base = { Distance.LOCATIONS[0][0], Distance.LOCATIONS[0][1] };

                // gradient
                double m = (base[1] - midpoint[1]) / (base[0] - midpoint[0]);
                double c = base[1] - m * base[0];

                // points in segment 1, i.e. below line
                List<Customer> customersBelowLine = new ArrayList<>();
                List<Customer> customersAboveLine = new ArrayList<>();

                customerList.stream().forEach(customer -> {
                    double y = m * customer.getPoint()[0] + c;
                    if (customer.getPoint()[1] < y) {
                        customersBelowLine.add(customer);
                    } else {
                        customersAboveLine.add(customer);
                    }
                });

                if (customersBelowLine.isEmpty() || customersAboveLine.isEmpty()) {
                    throw new RuntimeException("Not splitted correctly?");
                }

                // sort based on distance from base
                Collections.sort(customersBelowLine,
                        Comparator.comparing(customer -> Distance.getDistance(0, customer.getLocation())));
                Collections.sort(customersAboveLine,
                        Comparator.comparing(customer -> Distance.getDistance(0, customer.getLocation())));

                optimisedRoute.add(customersBelowLine.get(0));
                customersBelowLine.remove(0);
                while (!customersBelowLine.isEmpty()) {
                    // get the next closest customer within this segment
                    Collections.sort(customersBelowLine,
                            Comparator.comparing(
                                    cus -> Distance.getDistance(
                                            optimisedRoute.get(optimisedRoute.size() - 1).getLocation(),
                                            cus.getLocation())));
                    optimisedRoute.add(customersBelowLine.get(0));
                    customersBelowLine.remove(0);
                }

                // record last point
                Customer lastCustomer = customersAboveLine.get(0);
                customersAboveLine.remove(0);

                while (!customersAboveLine.isEmpty()) {
                    // get the next closest customer within this segment
                    Collections.sort(customersAboveLine,
                            Comparator.comparing(
                                    cus -> Distance.getDistance(
                                            optimisedRoute.get(optimisedRoute.size() - 1).getLocation(),
                                            cus.getLocation())));
                    optimisedRoute.add(customersAboveLine.get(0));
                    customersAboveLine.remove(0);
                }
                optimisedRoute.add(lastCustomer);

            } else if (customerList.size() == 3) {
                optimisedRoute.add(customerList.get(0));
                optimisedRoute.add(customerList.get(2));
                optimisedRoute.add(customerList.get(1));
            } else { // can be 0 - 2
                customerList.forEach(optimisedRoute::add);
            }
            System.out.println(
                    optimisedRoute.stream().map(Customer::getLocation).collect(Collectors.toList()));
            optimisedRoutes.add(optimisedRoute);
        }
    }

    public Customer getNextCustomer(Truck truck) {
        if (!trucks.contains(truck)) {
            throw new RuntimeException("Provided truck is not in overall list");
        }

        return null;
    }
}
