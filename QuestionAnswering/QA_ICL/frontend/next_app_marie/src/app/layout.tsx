import type { Metadata } from 'next'
import { Inter } from 'next/font/google'
import './globals.css'
import { Sidebar, Header, Footer } from '@/components/layout'
import { Toaster } from '@/components/ui/toaster'

const inter = Inter({ subsets: ['latin'] })

export const metadata: Metadata = {
  title: 'Marie',
  description: 'A Question-Answering Application for Chemistry',
}

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode
}>) {
  return (
    <html lang='en'>
      <body className={inter.className}>
        <Header className='py-2 w-full fixed z-10 border-b' />
        <div className='w-full flex justify-center items-center'>
          <div className='max-w-7xl w-full mt-20 flex items-start justify-between'>
            <Sidebar className='py-4 pl-4 pr-8 h-[calc(100vh-5rem)] sticky top-[5rem] border-r' />
            <div className='min-h-[calc(100vh-5rem)] w-full flex flex-col justify-between'>
              {children}
              <Footer />
            </div>
            <Toaster />
          </div>
        </div>
      </body>
    </html>
  )
}
