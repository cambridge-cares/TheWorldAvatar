package uk.ac.cam.cares.jps.ship.model;

import com.fasterxml.jackson.annotation.JsonBackReference;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Table(name = "ship_details", schema = "public", catalog = "adms_ships")
public class ShipDetailsEntity {
    private int id;
    private String dest;
    private Double ss;
    private Double cu;
    private Integer dw;
    private Double draught;
    private Double lat;
    private Double lon;
    private Integer r;
    private Integer lc;
    private Boolean sl;
    private Integer sc;
    private Integer heading;
    private Integer etaTs;
    private Integer ts;
    private Integer tst;
    private ShipEntity shipByShipMmsi;

    @Id
    @Column(name = "id")
    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    @Basic
    @Column(name = "dest")
    public String getDest() {
        return dest;
    }

    public void setDest(String dest) {
        this.dest = dest;
    }

    @Basic
    @Column(name = "ss")
    public Double getSs() {
        return ss;
    }

    public void setSs(Double ss) {
        this.ss = ss;
    }

    @Basic
    @Column(name = "cu")
    public Double getCu() {
        return cu;
    }

    public void setCu(Double cu) {
        this.cu = cu;
    }

    @Basic
    @Column(name = "dw")
    public Integer getDw() {
        return dw;
    }

    public void setDw(Integer dw) {
        this.dw = dw;
    }

    @Basic
    @Column(name = "draught")
    public Double getDraught() {
        return draught;
    }

    public void setDraught(Double draught) {
        this.draught = draught;
    }

    @Basic
    @Column(name = "lat")
    public Double getLat() {
        return lat;
    }

    public void setLat(Double lat) {
        this.lat = lat;
    }

    @Basic
    @Column(name = "lon")
    public Double getLon() {
        return lon;
    }

    public void setLon(Double lon) {
        this.lon = lon;
    }

    @Basic
    @Column(name = "r")
    public Integer getR() {
        return r;
    }

    public void setR(Integer r) {
        this.r = r;
    }

    @Basic
    @Column(name = "lc")
    public Integer getLc() {
        return lc;
    }

    public void setLc(Integer lc) {
        this.lc = lc;
    }

    @Basic
    @Column(name = "sl")
    public Boolean getSl() {
        return sl;
    }

    public void setSl(Boolean sl) {
        this.sl = sl;
    }

    @Basic
    @Column(name = "sc")
    public Integer getSc() {
        return sc;
    }

    public void setSc(Integer sc) {
        this.sc = sc;
    }

    @Basic
    @Column(name = "heading")
    public Integer getHeading() {
        return heading;
    }

    public void setHeading(Integer heading) {
        this.heading = heading;
    }

    @Basic
    @Column(name = "etats")
    public Integer getEtaTs() {
        return etaTs;
    }

    public void setEtaTs(Integer etaTs) {
        this.etaTs = etaTs;
    }

    @Basic
    @Column(name = "ts")
    public Integer getTs() {
        return ts;
    }

    public void setTs(Integer ts) {
        this.ts = ts;
    }

    @Basic
    @Column(name = "tst")
    public Integer getTst() {
        return tst;
    }

    public void setTst(Integer tst) {
        this.tst = tst;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ShipDetailsEntity that = (ShipDetailsEntity) o;

        if (id != that.id) return false;
        if (!Objects.equals(dest, that.dest)) return false;
        if (!Objects.equals(ss, that.ss)) return false;
        if (!Objects.equals(cu, that.cu)) return false;
        if (!Objects.equals(dw, that.dw)) return false;
        if (!Objects.equals(draught, that.draught)) return false;
        if (!Objects.equals(lat, that.lat)) return false;
        if (!Objects.equals(lon, that.lon)) return false;
        if (!Objects.equals(r, that.r)) return false;
        if (!Objects.equals(lc, that.lc)) return false;
        if (!Objects.equals(sl, that.sl)) return false;
        if (!Objects.equals(sc, that.sc)) return false;
        if (!Objects.equals(heading, that.heading)) return false;
        if (!Objects.equals(etaTs, that.etaTs)) return false;
        if (!Objects.equals(ts, that.ts)) return false;
        return Objects.equals(tst, that.tst);
    }

    @Override
    public int hashCode() {
        int result = id;
        result = 31 * result + (dest != null ? dest.hashCode() : 0);
        result = 31 * result + (ss != null ? ss.hashCode() : 0);
        result = 31 * result + (cu != null ? cu.hashCode() : 0);
        result = 31 * result + (dw != null ? dw.hashCode() : 0);
        result = 31 * result + (draught != null ? draught.hashCode() : 0);
        result = 31 * result + (lat != null ? lat.hashCode() : 0);
        result = 31 * result + (lon != null ? lon.hashCode() : 0);
        result = 31 * result + (r != null ? r.hashCode() : 0);
        result = 31 * result + (lc != null ? lc.hashCode() : 0);
        result = 31 * result + (sl != null ? sl.hashCode() : 0);
        result = 31 * result + (sc != null ? sc.hashCode() : 0);
        result = 31 * result + (heading != null ? heading.hashCode() : 0);
        result = 31 * result + (etaTs != null ? etaTs.hashCode() : 0);
        result = 31 * result + (ts != null ? ts.hashCode() : 0);
        result = 31 * result + (tst != null ? tst.hashCode() : 0);
        return result;
    }

    @JsonBackReference
    @ManyToOne(fetch=FetchType.LAZY)
    @JoinColumn(name = "ship_mmsi", referencedColumnName = "mmsi")
    public ShipEntity getShipByShipMmsi() {
        return shipByShipMmsi;
    }

    public void setShipByShipMmsi(ShipEntity shipByShipMmsi) {
        this.shipByShipMmsi = shipByShipMmsi;
    }
}
