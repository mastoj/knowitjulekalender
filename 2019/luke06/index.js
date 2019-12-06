var fs = require('fs'),
PNG = require('pngjs').PNG;

fs.createReadStream('../luke06.png')
  .pipe(new PNG())
  .on('parsed', function() {

    let prevR = this.data[0]
    let prevG = this.data[1]
    let prevB = this.data[2]
    
    for (var y = 0; y < this.height; y++) {
        for (var x = 0; x < this.width; x++) {
            if(!(y == 0 && x == 0)) {
                console.log(`R: ${prevR}; G: ${prevG}; B: ${prevB}; x: ${x}; y: ${y}`)
                var idx = (this.width * y + x) << 2;
                console.log(`Rin: ${this.data[idx]}; Gin: ${this.data[idx+1]}; Bin: ${this.data[idx+2]}`)
                const nextR = this.data[idx];
                const nextG = this.data[idx];
                const nextB = this.data[idx];
                // invert color
                this.data[idx] =  this.data[idx] ^ prevR;
                this.data[idx+1] = this.data[idx+1] ^ prevG;
                this.data[idx+2] = this.data[idx+2] ^ prevB;
                console.log(`Rout: ${this.data[idx]}; Gout: ${this.data[idx+1]}; Bout: ${this.data[idx+2]}`)
                prevR = nextR;
                prevG = nextG;
                prevB = nextB;
            }
        }
    }

    this.pack().pipe(fs.createWriteStream('out.png'));
});