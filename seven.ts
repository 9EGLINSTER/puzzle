const payload: Record<number, number[]> = {
  0: [57],
  1: [69, 70, 76, 73, 78, 83, 84, 69, 82],
};

function decode(chunks: Record<number, number[]>): string {
  return Object.keys(chunks)
    .sort((a, b) => Number(a) - Number(b))
    .flatMap((key) => chunks[Number(key)])
    .reduce((acc, code) => acc + String.fromCharCode(code), "");
}

const pipeline: Array<(input: string) => string> = [
  (s) => s.split("").reverse().join(""),
  (s) => s.toUpperCase(),
  (s) => s.split("").reverse().join(""),
];

const result = pipeline.reduce((val, fn) => fn(val), decode(payload));

console.log(result);