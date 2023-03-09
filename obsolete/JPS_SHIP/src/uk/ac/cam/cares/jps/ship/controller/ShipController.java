package uk.ac.cam.cares.jps.ship.controller;

import javax.persistence.EntityManager;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.ship.listener.LocalEntityManagerFactory;
import uk.ac.cam.cares.jps.ship.model.ShipEntity;

@Path("/ships")
public class ShipController {

    @GET
    @Produces({MediaType.APPLICATION_JSON})
    @Path("{mmsi}")
    public ShipEntity read(@PathParam("mmsi") int mmsi) {
        EntityManager em = LocalEntityManagerFactory.createEntityManager();
        try {
            return em.find(ShipEntity.class, mmsi);
        } catch (Exception e) {
            throw new JPSRuntimeException(e.getMessage());
        } finally {
            em.close();
        }
    }
}
