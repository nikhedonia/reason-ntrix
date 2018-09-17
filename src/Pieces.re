open Array;

type orientations =
 | O
 | R
 | H
 | L

type oriented = {
  o: orientations,
  g: array(array(int))
};


let _I = [|
  [|0,0,0,0|],
  [|1,1,1,1|],
  [|0,0,0,0|],
  [|0,0,0,0|],
|];


let _B = [|
  [|1, 1|],
  [|1, 1|],
|];


let _S = [|
  [|0, 1, 1|],
  [|1, 1, 0|],
  [|0, 0, 0|],
|];


let _Z = [|
  [|1, 1, 0|],
  [|0, 1, 1|],
  [|0, 0, 0|],
|];

let _L = [| 
  [|0, 0, 1|],
  [|1, 1, 1|],
  [|0, 0, 0|],
|];


let _J = [| 
  [|1, 0, 0|],
  [|1, 1, 1|],
  [|0, 0, 0|],
|];


let _T = [| 
  [|0, 1, 0|],
  [|1, 1, 1|],
  [|0, 0, 0|],
|];

let rotateRight = (board) => {
  let w = board[0] |> length
  let h = board |> length
  make_matrix(
    w, h, 0
  ) |> mapi( (i, rows) =>
    rows |> mapi( (j, _) => {
      board[w-j-1][i]  
    }))
}

let pieces = [|
  {o:R, g:rotateRight(_I)},
  {o:O, g:_B},
  {o:O, g:_S},
  {o:O, g:_Z},
  {o:O, g:_L},
  {o:O, g:_J},
  {o:O, g:_T}
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
