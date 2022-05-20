package uk.ac.cam.cares.jps.ship.model;

import com.fasterxml.jackson.annotation.JsonManagedReference;

import javax.persistence.*;
import java.util.Collection;
import java.util.Objects;

@Entity
@Table(name = "ship", schema = "public", catalog = "adms_ships")
public class ShipEntity {
    @Id
    @Column(name = "mmsi", nullable = false)
    private int mmsi;
    @Basic
    @Column(name = "imo")
    private Integer imo;
    @Basic
    @Column(name = "name", length = -1)
    private String name;
    @Basic
    @Column(name = "type", length = -1)
    private String type;
    @Basic
    @Column(name = "y")
    private Integer y;
    @Basic
    @Column(name = "country", length = -1)
    private String country;
    @Basic
    @Column(name = "al")
    private Integer al;
    @Basic
    @Column(name = "gt")
    private Integer gt;
    @Basic
    @Column(name = "aw")
    private Integer aw;
    @JsonManagedReference
    @OneToMany(cascade=CascadeType.ALL, fetch=FetchType.LAZY, mappedBy = "shipByShipMmsi")
    private Collection<ShipDetailsEntity> shipDetailsByMmsi;
    @JsonManagedReference
    @OneToOne(cascade=CascadeType.ALL, fetch=FetchType.LAZY, mappedBy = "shipByShipMmsi")
    private ShipPollutionEntity shipPollutionByMmsi;


    public int getMmsi() {
        return mmsi;
    }

    public void setMmsi(int mmsi) {
        this.mmsi = mmsi;
    }


    public Integer getImo() {
        return imo;
    }

    public void setImo(Integer imo) {
        this.imo = imo;
    }


    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }


    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }


    public Integer getY() {
        return y;
    }

    public void setY(Integer y) {
        this.y = y;
    }


    public String getCountry() {
        return country;
    }

    public void setCountry(String country) {
        this.country = country;
    }


    public Integer getAl() {
        return al;
    }

    public void setAl(Integer al) {
        this.al = al;
    }


    public Integer getGt() {
        return gt;
    }

    public void setGt(Integer gt) {
        this.gt = gt;
    }


    public Integer getAw() {
        return aw;
    }

    public void setAw(Integer aw) {
        this.aw = aw;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ShipEntity that = (ShipEntity) o;

        if (mmsi != that.mmsi) return false;
        if (!Objects.equals(imo, that.imo)) return false;
        if (!Objects.equals(name, that.name)) return false;
        if (!Objects.equals(type, that.type)) return false;
        if (!Objects.equals(y, that.y)) return false;
        if (!Objects.equals(country, that.country)) return false;
        if (!Objects.equals(al, that.al)) return false;
        if (!Objects.equals(gt, that.gt)) return false;
        return Objects.equals(aw, that.aw);
    }

    @Override
    public int hashCode() {
        int result = mmsi;
        result = 31 * result + (imo != null ? imo.hashCode() : 0);
        result = 31 * result + (name != null ? name.hashCode() : 0);
        result = 31 * result + (type != null ? type.hashCode() : 0);
        result = 31 * result + (y != null ? y.hashCode() : 0);
        result = 31 * result + (country != null ? country.hashCode() : 0);
        result = 31 * result + (al != null ? al.hashCode() : 0);
        result = 31 * result + (gt != null ? gt.hashCode() : 0);
        result = 31 * result + (aw != null ? aw.hashCode() : 0);
        return result;
    }


    public Collection<ShipDetailsEntity> getShipDetailsByMmsi() {
        return shipDetailsByMmsi;
    }

    public void setShipDetailsByMmsi(Collection<ShipDetailsEntity> shipDetailsByMmsi) {
        this.shipDetailsByMmsi = shipDetailsByMmsi;
    }


    public ShipPollutionEntity getShipPollutionByMmsi() {
        if (shipPollutionByMmsi == null) {
            shipPollutionByMmsi = new ShipPollutionEntity();
            shipPollutionByMmsi.setShipByShipMmsi(this);
        }
        return shipPollutionByMmsi;
    }

    public void setShipPollutionByMmsi(ShipPollutionEntity shipPollutionByMmsi) {
        this.shipPollutionByMmsi = shipPollutionByMmsi;
    }
}
