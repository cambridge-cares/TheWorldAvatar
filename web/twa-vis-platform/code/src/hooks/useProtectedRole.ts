import { useState, useEffect } from 'react';

type Roles = string[]; // Define the Roles type if not already defined

export function useProtectedRole() {
  const [roles, setRoles] = useState<Roles>(null);
  const [authorised, setAuthorised] = useState<boolean>(false);

  useEffect(() => {
    const fetchRoles = async () => {
      try {
        const { roles } = await (await fetch('/api/userinfo')).json() as { roles: Roles };
        setRoles(roles);
      } catch (error) {
        console.error('Error fetching user Info', error);
      }
    };

    fetchRoles();
  }, []);

  useEffect(() => {
    if (roles && roles.includes('protected')) {
      setAuthorised(true);
    } else {
      setAuthorised(false);
    }
  }, [roles]);

  return { authorised };
}

// might modify later to also return the roles list