# _Installation instructions:_

###### **on Windows**:

`git pull into C:\JPS-AWS-git` (on more nested directories windows will complain about too long paths)

`mvn clean install `
`.\run.bat`

call http://localhost:8084/watcher with POST request:

`{"watch":"...file location goes here...",
"callback":"...iri to call back when chages are detected..."}`
