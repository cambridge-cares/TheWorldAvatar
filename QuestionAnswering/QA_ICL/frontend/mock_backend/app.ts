import express, { Request } from "express"
import cors from "cors"
import { readdirSync, readFileSync } from "fs"
import * as path from "path"

const HTTP_VERBS: ("get" | "post")[] = ["get", "post"]
const PORT = 5000


const app = express()
app.use(express.json())
app.use(cors<Request>())

app.use((req, res, next) => {
    console.log(new Date().toISOString(), "Incoming request:")
    console.log({
        url: req.originalUrl,
        method: req.method,
        body: req.body
    })
    next()
})

app.get('/', (_, res) => {
    res.send('Hello World!')
})

readdirSync("data", { withFileTypes: true }).filter(dirent => dirent.isDirectory())
    .forEach(dirent =>
        HTTP_VERBS.forEach((verb) => {
            const filepath = path.join("data", dirent.name, verb + ".json")
            console.log(`Locating ${filepath}...`)

            let data
            try {
                data = readFileSync(filepath, "utf8")
                console.log("File exists")
            } catch {
                console.log("File does not exists\n")
                return
            }

            let obj
            try {
                obj = JSON.parse(data)
            } catch (err) {
                console.log(`${filepath} is an invalid JSON file.`)
                throw err
            }

            const route = "/" + dirent.name
            console.log(`Registering mock data at path ${route}\n`)
            app[verb](route, (_, res) => res.json(obj))
        })
    )

app.listen(PORT, () => {
    console.log(`Example app listening on port ${PORT}`)
})