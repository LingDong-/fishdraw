# fishdraw

*procedurally generated fish drawings*

![](samples/000020.svg)

- generates all sorts of weird fishes
- outputs polylines (supported format svg, json, csv, etc.)
- full procedural generation, single file no dependencies
- plotter-centric
- export drawing animation:

![](samples/animated.svg)

## usage

basic

```
node fishdraw.js > output.svg
```

specify seed (from a string) and output format:

```
node fishdraw.js --seed "Biggus fishus" --format smil > output.svg
```

- the seed string is used as the name of the fish (printed in the drawing). If unspecified, a random pseudo-Latin name will be auto generated.
- format options: `svg` (regular svg), `smil` (animated svg), `csv` (each polyline on a comma-separated line) and `json`.

use as JS library:

```js
const {fish,generate_params} = require('./fishdraw.js');
let polylines = fish(generate_params());
console.log(polylines);
```


## gallery

![](samples/000000.svg)
![](samples/000001.svg)
![](samples/000002.svg)
![](samples/000003.svg)
![](samples/000004.svg)
![](samples/000005.svg)
![](samples/000006.svg)
![](samples/000007.svg)
![](samples/000008.svg)
![](samples/000009.svg)
![](samples/000010.svg)
![](samples/000011.svg)
![](samples/000012.svg)
![](samples/000013.svg)
![](samples/000014.svg)
![](samples/000015.svg)
![](samples/000016.svg)
![](samples/000017.svg)
![](samples/000018.svg)
![](samples/000019.svg)
![](samples/000021.svg)
![](samples/000022.svg)
![](samples/000023.svg)
![](samples/000024.svg)
![](samples/000025.svg)
![](samples/000026.svg)
![](samples/000027.svg)
![](samples/000028.svg)
![](samples/000029.svg)
![](samples/000030.svg)
![](samples/000031.svg)
![](samples/000032.svg)
