"use client";

import styles from './navbar.module.css';

import { useSelector } from 'react-redux';
import Link from 'next/link';
import KeycloakSession from 'authorisation/keycloak-session';

import { Routes } from 'io/config/routes';
import { selectItem } from 'state/context-menu-slice';
import { UISettings } from 'types/settings';
import IconComponent from 'ui/graphic/icon/icon';
import { navbarItem } from 'ui/interaction/context-menu/context-menu';
import NavbarComponent from './navbar-component';

// Type definition for navbar properties
interface NavbarProps {
  settings: UISettings;
}

/**
 * Represents the top level navigation bar, that loads a number of 
 * custom navbar components.
 */
export default function Navbar(props: Readonly<NavbarProps>) {
  const keycloakEnabled = process.env.KEYCLOAK === 'true';

  // Visibility state of navigation bar
  const navbarState = useSelector(selectItem(navbarItem.name));

  // Do not show if state exists and is disabled
  if (navbarState?.toggled != null && !navbarState.toggled) {
    return null;
  }

  return (
    <div id="navbar" className={styles.navbar}>
      {/* Render navbar logo if set */}
      {props.settings?.branding?.navbarLogo?.length > 0 &&
          // Handle the case where navbarLogo is a list
        <div className={styles["logo-ribbon"]}>
          {
            Array.isArray(props.settings?.branding?.navbarLogo) ? (
              props.settings?.branding?.navbarLogo.map(logo => (
                <Link key={logo} href={Routes.HOME}>
                  <IconComponent
                    icon={logo}
                    classes={styles["logo"]}
                  />
                </Link>
              ))
            ) : (
              // Handle the case where navbarLogo is a string
              <Link href={Routes.HOME}>
                <IconComponent
                  icon={props.settings?.branding?.navbarLogo}
                  classes={styles["logo"]}
                />
              </Link>
            )
          }
        </div>
      }

      {/* Render each component as required */}
      <div className="navbarElements">
        {keycloakEnabled && <KeycloakSession />}
        {props.settings?.modules?.landing &&
          <NavbarComponent
            name="LANDING"
            tooltip="Return to landing page."
            icon="home"
            url={Routes.HOME} />
        }
        {props.settings?.modules?.map &&
          <NavbarComponent
            name="MAP"
            tooltip="Geospatial view."
            icon="public"
            url={Routes.MAP} />
        }
        {props.settings?.modules?.dashboard &&
          <NavbarComponent
            name="DASH"
            tooltip="Analytics view."
            icon="monitoring"
            url={Routes.DASHBOARD} />
        }
        {props.settings?.modules?.help &&
          <NavbarComponent
            name="HELP"
            tooltip="Open help page."
            icon="help"
            url={Routes.HELP} />
        }
        {props.settings?.modules?.registry &&
          <NavbarComponent
            name="REGISTRY"
            tooltip="Open registry."
            icon="contract"
            url={`${process.env.ASSET_PREFIX}/view/${props.settings?.resources?.registry?.data}`} />
        }
        {props.settings?.modules?.scheduler &&
          <NavbarComponent
            name="SCHEDULER"
            tooltip="Open scheduler"
            icon="calendar_month"
            url={`${process.env.ASSET_PREFIX}/view/${props.settings?.resources?.scheduler?.data}`} />
        }
      </div>
    </div>
  );
}