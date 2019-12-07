const fs = require("fs");
const pngjs = require("pngjs");

const getPngIndex = (imageWidth, x, y) => (imageWidth * y + x) << 2;

fs.createReadStream("../luke06.png")
  .pipe(
    new pngjs.PNG({
      filterType: 4,
    }),
  )
  .on("parsed", function() {
    let current = this.data[
      getPngIndex(this.width, this.width - 1, this.height - 1)
    ];

    for (let y = this.height - 2; y > 0; y--) {
      for (let x = this.width - 2; x > 0; x--) {
        let prev = getPngIndex(this.width, x, y);

        // xor withprev
        this.data[current] = this.data[current] ^ this.data[prev];
        this.data[current + 1] = this.data[current + 1] ^ this.data[prev + 1];
        this.data[current + 2] = this.data[current + 2] ^ this.data[prev + 2];

        current = prev;
      }
    }

    this.pack().pipe(fs.createWriteStream("knowit-day6.png"));
  });