package com.cmclinnovations.vehiclerouting.objects;

import java.time.Duration;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import com.cmclinnovations.vehiclerouting.objects.Customer.CustomerState;

public class Truck {
    private int currentLocation;
    private Duration waitingTime;
    private List<Integer> visitedLocations;
    private List<ZonedDateTime> visitedTimes;
    private Customer customer;
    private boolean hasCustomer;
    private double speed = 1;
    private double totalDistance;

    public Truck(int startingLocation, ZonedDateTime startTime) {
        visitedLocations = new ArrayList<>();
        visitedTimes = new ArrayList<>();

        currentLocation = startingLocation;
        visitedLocations.add(currentLocation);
        visitedTimes.add(startTime);
        totalDistance = 0;
    }

    public int getLocation() {
        return currentLocation;
    }

    public void setCustomer(Customer customer, ZonedDateTime time) {
        this.customer = customer;
        hasCustomer = true;
        customer.setState(CustomerState.HAS_INCOMING_TRUCK, time);

        waitingTime = Duration
                .ofSeconds(Math.round(Distance.getDistance(currentLocation, customer.getLocation()) / speed));

        totalDistance += Distance.getDistance(currentLocation, customer.getLocation());
    }

    public boolean hasCustomer() {
        return hasCustomer;
    }

    public void servedCustomer(ZonedDateTime time) {
        if (!hasCustomer) {
            throw new RuntimeException("Does not have a customer being served");
        }
        currentLocation = customer.getLocation();
        visitedLocations.add(customer.getLocation());
        visitedTimes.add(time);
        customer.setState(CustomerState.ORDER_COMPLETE, time);
        hasCustomer = false;
        customer = null;
    }

    public Duration getWaitingTime() {
        return waitingTime;
    }

    public void subtractWaitingTime(Duration duration) {
        Duration subtractedDuration = waitingTime.minus(duration);

        if (subtractedDuration.isNegative()) {
            throw new RuntimeException("Negative waiting time");
        } else {
            waitingTime = subtractedDuration;
        }
    }

    public void returnToBase() {
        visitedLocations.add(0);

        Duration timeToReturn = Duration.ofSeconds(Math.round(Distance.getDistance(currentLocation, 0) / speed));

        visitedTimes.add(visitedTimes.get(visitedTimes.size() - 1).plus(timeToReturn));

        totalDistance += Distance.getDistance(currentLocation, 0);
    }

    public List<Integer> getVisitedLocations() {
        return visitedLocations;
    }

    public List<LocalTime> getVisitedTimes() {
        return visitedTimes.stream().map(t -> t.toLocalTime()).collect(Collectors.toList());
    }

    public double getTotalDistance() {
        return totalDistance;
    }
}
