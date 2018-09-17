type position = {
  x:int,
  y:int,
}

let addP = (p1, p2) => {
  x: p1.x + p2.x,
  y: p1.y + p2.y
};

let (+&) = addP;
