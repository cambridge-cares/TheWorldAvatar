'use client'

export default function imageLoader({ src, height, width, quality }) {
    return `${process.env.ASSET_PREFIX}${src}?w=${width || 25}&h=${height|| 25}&q=${quality || 75}`
}

// default height and width of 25px