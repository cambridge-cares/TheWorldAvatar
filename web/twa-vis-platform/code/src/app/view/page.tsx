import { redirect } from 'next/navigation';

import { PathNames } from 'io/config/routes';

/**
 * Redirects back to home page if visited.
 * 
 */
export default function InaccessibleViewPage() {
    redirect(PathNames.HOME);
}