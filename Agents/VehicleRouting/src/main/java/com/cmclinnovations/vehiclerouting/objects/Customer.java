package com.cmclinnovations.vehiclerouting.objects;

import java.time.ZonedDateTime;

public class Customer {
    private CustomerState customerState;
    private int location;
    private ZonedDateTime timeAtRequest;
    private ZonedDateTime timeAtIncomingTruck;
    private ZonedDateTime timeAtOrderCompletion;

    public Customer(int location) {
        this.location = location;
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
        return location;
    }

    public CustomerState getCustomerState() {
        return customerState;
    }

    public enum CustomerState {
        REQUESTED_TRUCK,
        HAS_INCOMING_TRUCK,
        ORDER_COMPLETE;
    }
}
