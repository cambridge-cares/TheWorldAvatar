package uk.ac.cam.cares.jps.ship.model;

import com.fasterxml.jackson.annotation.JsonManagedReference;

import javax.persistence.*;
import java.util.Collection;
import java.util.Objects;

@Entity
@Table(name = "ship", schema = "public", catalog = "adms_ships")
public class ShipEntity {
    private int mmsi;
    private Integer imo;
    private String name;
    private String type;
    private Integer y;
    private String country;
    private Integer al;
    private Integer gt;
    private Integer aw;
    private Collection<ShipDetailsEntity> shipDetailsByMmsi;

    @Id
    @Column(name = "mmsi")
    public int getMmsi() {
        return mmsi;
    }

    public void setMmsi(int mmsi) {
        this.mmsi = mmsi;
    }

    @Basic
    @Column(name = "imo")
    public Integer getImo() {
        return imo;
    }

    public void setImo(Integer imo) {
        this.imo = imo;
    }

    @Basic
    @Column(name = "name")
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Basic
    @Column(name = "type")
    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    @Basic
    @Column(name = "y")
    public Integer getY() {
        return y;
    }

    public void setY(Integer y) {
        this.y = y;
    }

    @Basic
    @Column(name = "country")
    public String getCountry() {
        return country;
    }

    public void setCountry(String country) {
        this.country = country;
    }

    @Basic
    @Column(name = "al")
    public Integer getAl() {
        return al;
    }

    public void setAl(Integer al) {
        this.al = al;
    }

    @Basic
    @Column(name = "gt")
    public Integer getGt() {
        return gt;
    }

    public void setGt(Integer gt) {
        this.gt = gt;
    }

    @Basic
    @Column(name = "aw")
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

    @JsonManagedReference
    @OneToMany(cascade=CascadeType.ALL, fetch=FetchType.LAZY, mappedBy = "shipByShipMmsi")
    public Collection<ShipDetailsEntity> getShipDetailsByMmsi() {
        return shipDetailsByMmsi;
    }

    public void setShipDetailsByMmsi(Collection<ShipDetailsEntity> shipDetailsByMmsi) {
        this.shipDetailsByMmsi = shipDetailsByMmsi;
    }
}
