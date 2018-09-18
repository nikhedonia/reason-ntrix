open Array;

type cells = 
  | Empty
  | Block
  | Shadow;

let isEmpty = cell => switch(cell) {
  | Empty => true
  | _ => false
}

let asCells = matrix => {
  matrix |> map( rows =>
    rows |> map( cells =>
      switch(cells) {
        | 0 => Empty
        | _ => Block
      }))};

type orientations =
 | O
 | R
 | H
 | L;

type oriented = {
  o: orientations,
  g: array(array(cells))
};


let _I = asCells([|
  [|0,0,0,0|],
  [|1,1,1,1|],
  [|0,0,0,0|],
  [|0,0,0,0|],
|]);


let _B = asCells([|
  [|1, 1|],
  [|1, 1|],
|]);


let _S = asCells([|
  [|0, 1, 1|],
  [|1, 1, 0|],
  [|0, 0, 0|],
|]);


let _Z = asCells([|
  [|1, 1, 0|],
  [|0, 1, 1|],
  [|0, 0, 0|],
|]);

let _L = asCells([| 
  [|0, 0, 1|],
  [|1, 1, 1|],
  [|0, 0, 0|],
|]);


let _J = asCells([| 
  [|1, 0, 0|],
  [|1, 1, 1|],
  [|0, 0, 0|],
|]);


let _T = asCells([| 
  [|0, 1, 0|],
  [|1, 1, 1|],
  [|0, 0, 0|],
|]);

let rotateRight = (board:array(array(cells))) => {  
  let w = board[0] |> length
  let h = board |> length
  make_matrix(
    w, h, Empty
  ) |> mapi( (i, rows) =>
    rows |> mapi( (j, _) => {
      board[w-j-1][i]  
    }))
}

let pieces = [|
  {o:R, g: rotateRight(_I)},
  {o:O, g: _B},
  {o:O, g: _S},
  {o:O, g: _Z},
  {o:O, g: _L},
  {o:O, g: _J},
  {o:O, g: _T}
|];


let transition = (o, r) =>
  switch (o, r) {
    | (O, R) => R
    | (O, L) => L
    | (R, R) => H
    | (H, R) => L
    | (H, L) => R
    | (L, R) => O
    | (L, L) => H
    | _ => O
  }
