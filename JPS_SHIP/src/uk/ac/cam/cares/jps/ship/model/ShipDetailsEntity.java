package uk.ac.cam.cares.jps.ship.model;

import com.fasterxml.jackson.annotation.JsonBackReference;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Table(name = "ship_details", schema = "public", catalog = "adms_ships")
public class ShipDetailsEntity {
    @Id
    @Column(name = "id", nullable = false)
    @SequenceGenerator( name = "mySeq", sequenceName = "MY_SEQ", allocationSize = 1, initialValue = 1 )
    @GeneratedValue(strategy=GenerationType.IDENTITY, generator="mySeq")
    private int id;
    @Basic
    @Column(name = "dest", length = -1)
    private String dest;
    @Basic
    @Column(name = "ss")
    private Double ss;
    @Basic
    @Column(name = "cu")
    private Double cu;
    @Basic
    @Column(name = "dw")
    private Integer dw;
    @Basic
    @Column(name = "draught")
    private Double draught;
    @Basic
    @Column(name = "lat")
    private Double lat;
    @Basic
    @Column(name = "lon")
    private Double lon;
    @Basic
    @Column(name = "r")
    private Integer r;
    @Basic
    @Column(name = "lc")
    private Integer lc;
    @Basic
    @Column(name = "sl")
    private Boolean sl;
    @Basic
    @Column(name = "sc")
    private Integer sc;
    @Basic
    @Column(name = "heading")
    private Integer heading;
    @Basic
    @Column(name = "etats")
    private Integer etats;
    @Basic
    @Column(name = "ts")
    private Integer ts;
    @Basic
    @Column(name = "tst")
    private Integer tst;
    @JsonBackReference
    @ManyToOne(fetch=FetchType.LAZY)
    @JoinColumn(name = "ship_mmsi", referencedColumnName = "mmsi")
    private ShipEntity shipByShipMmsi;



    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }


    public String getDest() {
        return dest;
    }

    public void setDest(String dest) {
        this.dest = dest;
    }


    public Double getSs() {
        return ss;
    }

    public void setSs(Double ss) {
        this.ss = ss;
    }


    public Double getCu() {
        return cu;
    }

    public void setCu(Double cu) {
        this.cu = cu;
    }


    public Integer getDw() {
        return dw;
    }

    public void setDw(Integer dw) {
        this.dw = dw;
    }


    public Double getDraught() {
        return draught;
    }

    public void setDraught(Double draught) {
        this.draught = draught;
    }


    public Double getLat() {
        return lat;
    }

    public void setLat(Double lat) {
        this.lat = lat;
    }


    public Double getLon() {
        return lon;
    }

    public void setLon(Double lon) {
        this.lon = lon;
    }


    public Integer getR() {
        return r;
    }

    public void setR(Integer r) {
        this.r = r;
    }


    public Integer getLc() {
        return lc;
    }

    public void setLc(Integer lc) {
        this.lc = lc;
    }


    public Boolean getSl() {
        return sl;
    }

    public void setSl(Boolean sl) {
        this.sl = sl;
    }


    public Integer getSc() {
        return sc;
    }

    public void setSc(Integer sc) {
        this.sc = sc;
    }


    public Integer getHeading() {
        return heading;
    }

    public void setHeading(Integer heading) {
        this.heading = heading;
    }


    public Integer getEtats() {
        return etats;
    }

    public void setEtats(Integer etats) {
        this.etats = etats;
    }


    public Integer getTs() {
        return ts;
    }

    public void setTs(Integer ts) {
        this.ts = ts;
    }


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
        result = 31 * result + (ts != null ? ts.hashCode() : 0);
        result = 31 * result + (tst != null ? tst.hashCode() : 0);
        return result;
    }


    public ShipEntity getShipByShipMmsi() {
        return shipByShipMmsi;
    }

    public void setShipByShipMmsi(ShipEntity shipByShipMmsi) {
        this.shipByShipMmsi = shipByShipMmsi;
    }

}
