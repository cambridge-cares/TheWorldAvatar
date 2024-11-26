import { ReloadIcon } from '@radix-ui/react-icons'

export default function ZeoMaterialLoading() {
  return (
    <div className='w-full h-screen flex items-center justify-center'>
      <ReloadIcon className='animate-spin' />
    </div>
  )
}
