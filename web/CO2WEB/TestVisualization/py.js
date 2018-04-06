let a = [1,2,3,4,5,6,7]
let b = [8,9,10,11,12,13,14]

let MAX = 3
function  divide() {
    let results = []
    while(a.length >= MAX){
        let ta = a.splice(0, MAX)
        let tb = b.splice(0, MAX)
         results.push({a:ta,b:tb})
    }
    let ta = a.splice(0)
    let tb = b.splice(0)
    results.push({a:ta,b:tb})
    
    return results
}

console.log(divide())