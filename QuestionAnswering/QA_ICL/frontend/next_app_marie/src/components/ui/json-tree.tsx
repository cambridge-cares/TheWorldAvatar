'use client'

import React from 'react'
import * as ReactJSONTree from 'react-json-tree'

const THEME = {
  scheme: 'rjv-default',
  author: 'mac gainor',
  //transparent main background
  base00: 'rgba(0, 0, 0, 0)',
  base01: 'rgb(245, 245, 245)',
  base02: 'rgb(235, 235, 235)',
  base03: '#93a1a1',
  base04: 'rgba(0, 0, 0, 0.3)',
  base05: '#586e75',
  base06: '#073642',
  base07: '#002b36',
  base08: '#d33682',
  base09: '#cb4b16',
  base0A: '#dc322f',
  base0B: '#859900',
  base0C: '#6c71c4',
  base0D: '#586e75',
  base0E: '#2aa198',
  base0F: '#268bd2',
}

export const JSONTree = ({
  ...props
}: React.ComponentProps<typeof ReactJSONTree.JSONTree>) => {
  return <ReactJSONTree.JSONTree theme={THEME} {...props} />
}
