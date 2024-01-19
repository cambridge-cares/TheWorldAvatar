package uk.ac.cam.cares.jps.ship.model;

import com.fasterxml.jackson.annotation.JsonBackReference;

import javax.persistence.*;

@Entity
@Table(name = "ship_pollution", schema = "public", catalog = "adms_ships")
public class ShipPollutionEntity {

    @Id
    @Column(name = "id", nullable = false)
    @SequenceGenerator( name = "mySeq", sequenceName = "MY_SEQ", allocationSize = 1, initialValue = 1 )
    @GeneratedValue(strategy=GenerationType.IDENTITY, generator="mySeq")
    private int id;
    @Basic
    @Column(name = "chimney_iri", nullable = true, length = -1)
    private String chimneyIri;
    @JsonBackReference
    @OneToOne(fetch=FetchType.LAZY)
    @JoinColumn(name = "ship_mmsi", referencedColumnName = "mmsi", unique=true, nullable=false)
    private ShipEntity shipByShipMmsi;


    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }


    public String getChimneyIri() {
        return chimneyIri;
    }

    public void setChimneyIri(String chimneyIri) {
        this.chimneyIri = chimneyIri;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ShipPollutionEntity that = (ShipPollutionEntity) o;

        if (id != that.id) return false;
        if (chimneyIri != null ? !chimneyIri.equals(that.chimneyIri) : that.chimneyIri != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = id;
        result = 31 * result + (chimneyIri != null ? chimneyIri.hashCode() : 0);
        return result;
    }


    public ShipEntity getShipByShipMmsi() {
        return shipByShipMmsi;
    }

    public void setShipByShipMmsi(ShipEntity shipByShipMmsi) {
        this.shipByShipMmsi = shipByShipMmsi;
    }
}
