import { DataItem } from '@/lib/model/qa'

import {
  Accordion,
  AccordionContent,
  AccordionItem,
  AccordionTrigger,
} from '@/components/ui/accordion'
import { DataTableRecursive } from '@/components/ui/data-table-recursive'
import { JSONTree } from '@/components/ui/json-tree'

export interface QAResponseDataDivProps
  extends React.HTMLAttributes<HTMLDivElement> {
  qaResponseData: DataItem[]
}

export const QAResponseDataDiv = ({
  qaResponseData,
  ...props
}: QAResponseDataDivProps) => (
  <div {...props}>
    <h2 className='text-xl font-semibold text-blue-500'>Retrieved data</h2>
    <Accordion
      type='multiple'
      defaultValue={qaResponseData.map((_, idx) => idx.toString())}
    >
      {qaResponseData.map((item, idx) => {
        let headerText, component
        if (item.type === 'document_collection') {
          headerText = 'JSON data'
          component = (
            <JSONTree
              data={item.data}
              shouldExpandNodeInitially={() => false}
            />
          )
        } else if (item.type === 'table') {
          headerText = 'Tabular data'
          component = (
            <DataTableRecursive
              columns={item.columns}
              data={item.data}
              numbered
              paginated
              bordered
              scrollable
            />
          )
        }
        return headerText && component ? (
          <AccordionItem key={idx} value={idx.toString()}>
            <AccordionTrigger>{headerText}</AccordionTrigger>
            <AccordionContent className='py-2'>{component}</AccordionContent>
          </AccordionItem>
        ) : (
          <></>
        )
      })}
    </Accordion>
  </div>
)
