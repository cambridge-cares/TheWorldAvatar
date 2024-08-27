"use client";

import styles from './navbar.module.css';

import { useSelector } from 'react-redux';

import { Routes } from 'io/config/routes';
import { selectItem } from 'state/context-menu-slice';
import IconComponent from 'ui/graphic/icon/icon';
import { navbarItem } from 'ui/interaction/context-menu/context-menu';
import NavbarComponent from './navbar-component';
import Link from 'next/link';
import KeycloakSession from 'authorisation/keycloak-session';
// import KeycloakSession from './keycloak-session';

// Type definition for navbar properties
interface NavbarProps {
  showLanding?: boolean,
  showMap?: boolean,
  showDash?: boolean,
  showHelp?: boolean,
  navbarLogo?: string
}

// Default values for navbar properties
const defaultProps: NavbarProps = {
  showLanding: true,
  showMap: true,
  showDash: true,
  showHelp: true,
  navbarLogo: null
};

/**
 * Represents the top level navigation bar, that loads a number of 
 * custom navbar components.
 */
export default function Navbar(props: Readonly<NavbarProps>) {

  // Visibility state of navigation bar
  const navbarState = useSelector(selectItem(navbarItem.name));

  // Apply defaults to any missing props
  props = { ...defaultProps, ...props };

  // Do not show if state exists and is disabled
  if (navbarState?.toggled != null && !navbarState.toggled) {
    return null;
  }

  return (
    <div id="navbar" className={styles.navbar}>
      {/* Render navbar logo if set */}
      {props.navbarLogo != null &&
        <Link href={Routes.HOME}>
          <div className="navbarLogo">
            <IconComponent
              icon={props.navbarLogo}
            />
          </div>
        </Link>
      }

      {/* Render each component as required */}
      <div className="navbarElements">
        <KeycloakSession />
        {props.showLanding &&
          <NavbarComponent
            name="LANDING"
            tooltip="Return to landing page."
            icon="home"
            url={Routes.HOME} />
        }
        {props.showMap &&
          <NavbarComponent
            name="MAP"
            tooltip="Geospatial view."
            icon="public"
            url={Routes.MAP} />
        }
        {props.showDash &&
          <NavbarComponent
            name="DASH"
            tooltip="Analytics view."
            icon="monitoring"
            url={Routes.DASHBOARD} />
        }
        {props.showHelp &&
          <NavbarComponent
            name="HELP"
            tooltip="Open help page."
            icon="help"
            url={Routes.HELP} />
        }
      </div>


    </div>
  );
}