open Array;
open Position;
open Pieces;

let createBoard = (n, m) => make_matrix(m, n, 0);


let merge = (board, piece, {x, y}) => {
  board |> mapi( (i, rows) => {
    rows |> mapi( (j, cell) => {
      if (
        y <= i && i < y+(piece|>length) && 
        x <= j && j < x+(piece[i-y]|>length) &&  
        piece[i-y][j-x] > 0
      ) {
         piece[i-y][j-x]; 
      } else {
        cell;
      }
    })
  })
};

let overlap = (board, piece, {x, y}) => {
 let w = board[0]|>length;
 let h = board|>length;
 piece 
   |> mapi( (i, rows) => {
    rows |> mapi( (j, cell) : bool => {
      (cell>0) && (
        ( (i+y < h) && 
          (j+x < w) && 
          (j+x >= 0) && 
          (i+y >= 0) && 
          (board[i+y][j+x]>0)) || 
        (j+x >= w) ||
        (j+x < 0) ||
        (i+y >= h)
      )
    }) |> fold_left((a, b) => a||b, false) 
  }) |> fold_left((a, b) => a||b, false); 
};

type maybe('a) =
  | None
  | Some('a)
;

let rec findOffset = (board, rotated, pos, offsets) => switch(offsets) {
  | [] => None
  | [head, ...tail] => {
    if( overlap(board, rotated, pos +& head) ) {
      findOffset(board, rotated, pos, tail)
    } else
      Some(head)
  }
};

let rec findGhost = (board, piece, pos) => {
  switch (overlap(board, piece, pos)) {
    | true => pos +& {x:0, y:-1}
    | false => findGhost(board, piece, pos +& {x:0, y: +1})
  }
};


let materialize = (board, piece, pos) => {
  let ghostPos = findGhost(board, piece, pos +& {x:0, y:1});
  let ghost = piece |> map(rows =>
    rows |> map(cell => switch(cell) { 
      | 0 => 0
      | _ => 2
    })
  );

  board 
    -> merge(ghost, ghostPos)
    -> merge(piece, pos);
};

let eliminate = board => {
  let w = board[0]|>length;
  let next = board |> fold_left( (rows, row) => {
    let count = row 
      |> fold_left( (a, b) => switch (b>0) { 
        | true => a+1
        | false => a
      },  0);

    switch ( w == count ) {
      | true => rows
      | false => concat([rows, [|row|]])
    }
  }, [||]);

  let removed = (board|>length) - (next|>length);
  
  (removed, concat([
    createBoard(w, removed),
    next
  ]));
};
