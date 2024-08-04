import React from 'react';
import { ReactFlow, Background, Controls, MiniMap } from '@xyflow/react';
import '@xyflow/react/dist/style.css';

const initialNodes = [
    { id: '1', data: { label: 'Node 1' }, position: { x: 250, y: 5 } },
    { id: '2', data: { label: 'Node 2' }, position: { x: 100, y: 100 } },
    { id: '3', data: { label: 'Node 3' }, position: { x: 400, y: 100 } },
];

const initialEdges = [
    { id: 'e1-2', source: '1', target: '2', animated: true },
    { id: 'e1-3', source: '1', target: '3', animated: true },
];

export default function Flow() {
    return (
        <div style={{ width: '100vw', height: '100vh' }}>
            <ReactFlow nodes={initialNodes} edges={initialEdges} />
            <MiniMap />
            <Controls />
            <Background />
        </div>
    );
}


