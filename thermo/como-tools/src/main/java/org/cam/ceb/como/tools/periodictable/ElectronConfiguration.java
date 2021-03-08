/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.periodictable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *
 * @author pb556
 */
public class ElectronConfiguration {

    protected Collection<OrbitalConfiguration> orbitalConfig = new ArrayList<OrbitalConfiguration>();

    public ElectronConfiguration() {
    }

    public ElectronConfiguration(String eConfig) {
        orbitalConfig = parse(eConfig);
    }

    public ElectronConfiguration(Collection<OrbitalConfiguration> eConfig) {
        orbitalConfig = eConfig;
    }

    public ElectronConfiguration(Map<Integer, Integer> s,
            Map<Integer, Integer> p,
            Map<Integer, Integer> d,
            Map<Integer, Integer> f) {
        orbitalConfig = new ArrayList<OrbitalConfiguration>();
        // order and num of electrons
        if (s != null) {
            for (Integer order : s.keySet()) {
                orbitalConfig.add(new OrbitalConfiguration(order, Orbital.s, s.get(order)));
            }
        }
        if (p != null) {
            for (Integer order : p.keySet()) {
                orbitalConfig.add(new OrbitalConfiguration(order, Orbital.p, p.get(order)));
            }
        }
        if (d != null) {
            for (Integer order : d.keySet()) {
                orbitalConfig.add(new OrbitalConfiguration(order, Orbital.d, d.get(order)));
            }
        }
        if (f != null) {
            for (Integer order : f.keySet()) {
                orbitalConfig.add(new OrbitalConfiguration(order, Orbital.f, f.get(order)));
            }
        }
    }

    public Collection<OrbitalConfiguration> getOrbitals(Orbital orbitalType) {
        ArrayList<OrbitalConfiguration> selConfigs = new ArrayList<OrbitalConfiguration>();
        for (OrbitalConfiguration config : orbitalConfig) {
            if (config.getOrbital().equals(orbitalType)) {
                selConfigs.add(config);
            }
        }
        return selConfigs;
    }

    public OrbitalConfiguration getOrbitals(int order, Orbital orbitalType) {
        for (OrbitalConfiguration config : orbitalConfig) {
            if (config.getOrbital().equals(orbitalType) && config.getOrder() == order) {
                return config;
            }
        }
        return null;
    }

    public Collection<OrbitalConfiguration> getOrbitals() {
        return orbitalConfig;
    }

    public OrbitalConfiguration getLastOrbital() {
        // s, d, p, f
        OrbitalConfiguration last = null;
        for (OrbitalConfiguration config : orbitalConfig) {
            if (last == null) {
                last = config;
            } else if (last.getOrbital().getOrder() < config.getOrbital().getOrder()) {
                last = config;
            } else if (last.getOrbital().getOrder() == config.getOrbital().getOrder()) {
                if (last.getOrder() < config.getOrder()) {
                    last = config;
                }
            }
        }
        return last;
    }

    public OrbitalConfiguration getLastOrbital(Orbital o) {
        // s, d, p, f
        OrbitalConfiguration last = null;
        for (OrbitalConfiguration config : orbitalConfig) {
            if (config.getOrbital() == o) {
                if (last == null) {
                    last = config;
                } else if (last.getOrbital().getOrder() < config.getOrbital().getOrder()) {
                    last = config;
                } else if (last.getOrbital().getOrder() == config.getOrbital().getOrder()) {
                    if (last.getOrder() < config.getOrder()) {
                        last = config;
                    }
                }
            }
        }
        return last;
    }

    @Override
    public String toString() {
        return null;
    }

    public static Collection<OrbitalConfiguration> parse(String str) {
        // 1s, 2s, 2p, 3s, 3p, 4s, 3d, 4p, 5s, 4d, 5p, 6s, 4f, 5d, 6p, 7s, 5f, 6d, 7p
        // example: 1s^(2) 2s^(2) 2p^(6)
        String[] items = str.split(" ");
        ArrayList<OrbitalConfiguration> orbitalConfigs = new ArrayList<OrbitalConfiguration>();
        for (String item : items) {
            if (item.trim().isEmpty()) {
                continue;
            } else {
                OrbitalConfiguration c = parseSingleOrbital(item);
                if (c != null) {
                    orbitalConfigs.add(c);
                }
            }
        }
        return orbitalConfigs;
    }

    private static OrbitalConfiguration parseSingleOrbital(String strOrbital) {
        strOrbital = strOrbital.trim();
        Pattern p = Pattern.compile("([1-7])([spdf])\\^\\((\\d+)\\)");
        Matcher m = p.matcher(strOrbital);
        if (m.find()) {
            if (m.groupCount() == 3) {
                return new OrbitalConfiguration(Integer.parseInt(m.group(1)), m.group(2), Integer.parseInt(m.group(3)));
            } else {
                return null;
            }
        }
        return null;
    }
}
