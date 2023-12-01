
import React, { PureComponent } from "react";
import { Animate } from "react-move";

/**
 * Hold onto your butts.
 * 
 * @returns Element for display.
 */
export default class Raptor extends PureComponent {
    state = {
        open: false
    }

    handleClick = () => {
        this.setState({ open: !this.state.open })
    }

    render() {
        return (
            <div onClick={this.handleClick}>
                <Animate
                    start={() => ({
                        x: 0,
                    })}

                    update={() => ({
                        x: [this.state.open ? 300 : 0],
                        timing: { duration: 750 },
                    })}>

                    {(state) => {
                        const { x } = state;
                        
                        return (
                            <img
                                src="/img/utils/raptor.png"
                                style={{
                                    width: "250px",
                                    height: "250px",
                                    position: "absolute",
                                    bottom: "0px",
                                    right: `${x}px`
                                }}
                            />
                        );
                    }}
                </Animate>
            </div>
        );
    }
}