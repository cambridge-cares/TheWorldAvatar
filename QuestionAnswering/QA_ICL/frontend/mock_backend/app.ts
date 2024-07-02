import express, { Request, Response } from "express"
import cors from "cors"
import { readdirSync, readFileSync } from "fs"
import * as path from "path"

const PORT = 5000
function sleep(ms: number) {
    return new Promise((resolve) => {
        setTimeout(resolve, ms);
    });
}

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
    .forEach(dir =>
        readdirSync(path.join("data", dir.name), { withFileTypes: true })
            .filter(dirent => dirent.isFile() && dirent.name.endsWith(".json"))
            .forEach(file => {
                const [verb, contentType] = file.name.split(".")
                const filepath = path.join("data", dir.name, file.name)
                const data = readFileSync(filepath, "utf8")

                let obj
                try {
                    obj = JSON.parse(data)
                } catch (err) {
                    console.log(`${filepath} is an invalid JSON file.`)
                    throw err
                }

                const callback = contentType === "json"
                    ? async (_: Request, res: Response) => {
                        await sleep(2000);
                        return res.json(obj)
                    } : (_: Request, res: Response) => {
                        res.setHeader('Cache-Control', 'no-cache');
                        res.setHeader('Content-Type', 'text/event-stream');
                        res.setHeader('Connection', 'keep-alive');
                        res.setHeader('X-Accel-Buffering', 'no')
                        res.flushHeaders(); // flush the headers to establish SSE with client

                        let idx = 0;
                        let intervalId = setInterval(() => {
                            idx++;
                            if (idx >= obj.length) {
                                clearInterval(intervalId);
                                res.end();
                                return;
                            }
                            res.write(`data: ${JSON.stringify(obj[idx])}\n\n`);
                        }, 10);

                        res.on('close', () => {
                            console.log('Client drops connection');
                            clearInterval(intervalId);
                            res.end();
                        });
                    }

                const route = "/" + dir.name
                if ((verb === "get") || (verb === "post")) {
                    console.log(`Registering mock data at path ${route}\n`)
                    app[verb](route, callback)
                } else {
                    console.log(`${verb} is an invalid HTTP verb.`)
                    throw Error()
                }
            })
    )

app.listen(PORT, () => {
    console.log(`Example app listening on port ${PORT}`)
})