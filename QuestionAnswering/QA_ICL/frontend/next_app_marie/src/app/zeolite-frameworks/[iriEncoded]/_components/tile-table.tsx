'use client'

import { createColumnHelper } from '@tanstack/react-table'

import { TileNum } from '@/lib/model/ontozeolite'
import { DataTable } from '@/components/ui/data-table'

const COL_HELPER = createColumnHelper<TileNum>()
const COLS = [
  COL_HELPER.accessor('tile.tile_code', { header: 'Tile code' }),
  COL_HELPER.accessor('tile.signature', { header: 'Signature' }),
  COL_HELPER.accessor('value', { header: 'Number of tiles' }),
  COL_HELPER.accessor('tile.vertex_num', { header: 'Number of vertices' }),
  COL_HELPER.accessor('tile.edge_num', { header: 'Number of edges' }),
  COL_HELPER.accessor('tile.face_num', { header: 'Number of faces' }),
]

export const TileTable = ({ tileNums }: { tileNums: TileNum[] }) => {
  return <DataTable columns={COLS} data={tileNums} bordered numbered />
}
