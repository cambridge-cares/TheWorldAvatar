package com.cmclinnovations.vehiclerouting.objects;

import java.time.ZonedDateTime;

import org.apache.commons.math3.ml.clustering.Clusterable;

public class Customer implements Clusterable {
    private CustomerState customerState;
    private int locationIndex;
    private ZonedDateTime timeAtRequest;
    private ZonedDateTime timeAtIncomingTruck;
    private ZonedDateTime timeAtOrderCompletion;

    public Customer(int locationIndex) {
        this.locationIndex = locationIndex;
    }

    public void setState(CustomerState customerState, ZonedDateTime time) {
        this.customerState = customerState;
        switch (customerState) {
            case REQUESTED_TRUCK:
                timeAtRequest = time;
                break;
            case HAS_INCOMING_TRUCK:
                timeAtIncomingTruck = time;
                break;
            case ORDER_COMPLETE:
                timeAtOrderCompletion = time;
        }
    }

    public int getLocation() {
        return locationIndex;
    }

    public CustomerState getCustomerState() {
        return customerState;
    }

    public enum CustomerState {
        REQUESTED_TRUCK,
        HAS_INCOMING_TRUCK,
        ORDER_COMPLETE;
    }

    @Override
    public double[] getPoint() {
        double[] doubleArray = { (double) Distance.LOCATIONS[locationIndex][0],
                (double) Distance.LOCATIONS[locationIndex][1] };
        return doubleArray;
    }
}
