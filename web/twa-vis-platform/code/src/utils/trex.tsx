"use client";

import Confetti from 'react-confetti';
import React, { PureComponent } from "react";
import { easeInOut, motion} from "framer-motion";

// Incoming properties
type Props = {
    callback: () => void
}

/**
 * Hold onto your butts.
 * 
 * @returns Element for display.
 */
export default class Trex extends PureComponent<Props> {

    // Audio player
    audio = new Audio("/utils/trex.wav");

    // Runs once component is added
    componentDidMount() {
        this.audio.volume = 0.2;
        this.audio.play();
    }

    // Return renderable component.
    render() {
        return (
            <>
                <Confetti
                    style={{
                        width: "100%",
                        height: "100%",
                        zIndex: 99999
                    }}
                    colors={[
                        "#cc1f00",
                        "#991700",
                        "#ccc200",
                        "#999100",
                        "#000000",
                        "#262626"
                    ]}
                />

                <motion.div
                    onAnimationComplete={this.props.callback}
                    animate={{
                        left: "100%"
                    }}
                    transition={{
                        duration: 12,
                        ease: "linear"
                    }}
                    style={{
                        width: "400px",
                        height: "400px",
                        position: "absolute",
                        zIndex: 99999,
                        bottom: "0px",
                        left:"-400px"
                    }}>

                    <motion.div
                        initial={{
                            rotate: -5
                        }}
                        animate={{
                            rotate: 5
                        }}
                        transition={{
                            duration: 1,
                            repeat: 12,
                            repeatType: "reverse",
                            ease: easeInOut
                        }}
                        style={{
                            backgroundImage: "url('/utils/trex.png')",
                            backgroundSize: "cover",
                            width: "400px",
                            height: "400px"
                        }}/>

                </motion.div>
            </>
        );
    }
}