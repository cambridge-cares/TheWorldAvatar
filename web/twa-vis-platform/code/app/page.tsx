
import Image from "next/image";
import styles from "./page.module.css";
import ModuleToggle from "@/utils/settings/module-toggle";
import { OptionalPages } from "@/utils/settings/optional-pages";
import LandingPage from "./landing/page";

/**
 * Performs initialisation when the platform is
 * first loaded. Should run on the server.
 */
function initialise() {
    // Read the UI module settings
    ModuleToggle.readModuleSettings();

    // Cache contents of optional static pages
    OptionalPages.loadPages();
}

/**
 * Handles the default route (i.e. "/") to display a home page
 * or redirect to another page.
 * 
 * @returns JSX for default (home) page.
 */
export default function Home() {
    initialise();

    // Check if a landing page is enabled
    // TODO: Rather than loading components for other pages here, it might be wise to check if
    //       redirecting (whilst keeping the same URL) makes more sense.

    let landingToggle: ModuleToggle = ModuleToggle.LANDING;
    if(landingToggle.state) {
        // Enabled, load components for that here
        return LandingPage();
        
    } else {
        // Disabled, load straight into map here
        return (
            <h1>MAP GOES HERE </h1>
        )
    }
    
    // return (
    //     <main className={styles.main}>
    //         <div className={styles.description}>
    //             <p>
    //                 Get started by editing{" "}
    //                 <code className={styles.code}>app/page.tsx</code>
    //                 <br/>
    //                 <br/>
    //                 {/* Value of module setting "landing-page" is: {value} */}
    //             </p>
    //             <div>
    //                 <a
    //                     href="https://vercel.com?utm_source=create-next-app&utm_medium=appdir-template&utm_campaign=create-next-app"
    //                     target="_blank"
    //                     rel="noopener noreferrer"
    //                 >
    //                     By{' '}
    //                     <Image
    //                         src="/vercel.svg"
    //                         alt="Vercel Logo"
    //                         className={styles.vercelLogo}
    //                         width={100}
    //                         height={24}
    //                         priority
    //                     />
    //                 </a>
    //             </div>
    //         </div>

    //         <div className={styles.center}>
    //             <Image
    //                 className={styles.logo}
    //                 src="/next.svg"
    //                 alt="Next.js Logo"
    //                 width={180}
    //                 height={37}
    //                 priority
    //             />
    //         </div>

    //         <div className={styles.grid}>
    //             <a
    //                 href="https://nextjs.org/docs?utm_source=create-next-app&utm_medium=appdir-template&utm_campaign=create-next-app"
    //                 className={styles.card}
    //                 target="_blank"
    //                 rel="noopener noreferrer"
    //             >
    //                 <h2>
    //                     Docs <span>-&gt;</span>
    //                 </h2>
    //                 <p>Find in-depth information about Next.js features and API.</p>
    //             </a>

    //             <a
    //                 href="https://nextjs.org/learn?utm_source=create-next-app&utm_medium=appdir-template&utm_campaign=create-next-app"
    //                 className={styles.card}
    //                 target="_blank"
    //                 rel="noopener noreferrer"
    //             >
    //                 <h2>
    //                     Learn <span>-&gt;</span>
    //                 </h2>
    //                 <p>Learn about Next.js in an interactive course with&nbsp;quizzes!</p>
    //             </a>

    //             <a
    //                 href="https://vercel.com/templates?framework=next.js&utm_source=create-next-app&utm_medium=appdir-template&utm_campaign=create-next-app"
    //                 className={styles.card}
    //                 target="_blank"
    //                 rel="noopener noreferrer"
    //             >
    //                 <h2>
    //                     Templates <span>-&gt;</span>
    //                 </h2>
    //                 <p>Explore the Next.js 13 playground.</p>
    //             </a>

    //             <a
    //                 href="https://vercel.com/new?utm_source=create-next-app&utm_medium=appdir-template&utm_campaign=create-next-app"
    //                 className={styles.card}
    //                 target="_blank"
    //                 rel="noopener noreferrer"
    //             >
    //                 <h2>
    //                     Deploy <span>-&gt;</span>
    //                 </h2>
    //                 <p>
    //                     Instantly deploy your Next.js site to a shareable URL with Vercel.
    //                 </p>
    //             </a>
    //         </div>
    //     </main>
    // )
}
