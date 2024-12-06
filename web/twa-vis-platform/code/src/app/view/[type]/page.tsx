import { redirect } from 'next/navigation';

import { Paths } from 'io/config/routes';

/**
 * Redirects back to home page if visited.
 * 
 */
export default function InaccessibleViewPage() {
    redirect(Paths.HOME);
}