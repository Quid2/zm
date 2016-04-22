// node test.js
var c = require('./sha3')
console.log(c.shake_128("abc",256));
console.log(c.shake_128([97,98,99],256));
console.log(c.shake_128.array([97,98,99],256));
console.log(c.shake_128.array([],256));
console.log(c.sha3_256.array([],256));
