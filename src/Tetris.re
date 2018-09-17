open Array;
open ReactDOMRe;
open Pieces;
let component = ReasonReact.statelessComponent("Tetris");

let createBoard = (n, m) => make_matrix(m, n, 0);



type boardT = array(array(int));




let pieces = Pieces.pieces;






type pos = {
  x: int,
  y: int
};

let merge = (board: boardT, piece: boardT, {x, y}: pos) : boardT => {
  board |> mapi( (i, rows) => {
    rows |> mapi( (j, cell) => {
      if (
        y <= i && i < y+(piece|>length) && 
        x <= j && j < x+(piece[i-y]|>length) &&  
        piece[i-y][j-x] > 0
      ) {
         piece[i-y][j-x] 
      } else {
        cell
      }
    })
  })
}

let overlap = (board: boardT, piece: boardT, {x, y}: pos): bool => {
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
  }) |> fold_left((a, b) => a||b, false)  
}


let gridStyle = Style.make(
  ~borderCollapse= "collapse",
  ~emptyCells= "show",
  ~border= "solid 1px black",
());

let cellStyle = Style.make(
  ~background= "white",
  ~color="transparent",
  ~border= "solid 1px #333",
  ~fontSize="5",
  ~minWidth= "1vh",
  ~minHeight= "1vh",
());


type actions = 
  | MoveLeft
  | MoveRight
  | Rotate
  | Tick
  | Drop
  | GameOver
  | ()

type state = {
  board: boardT,
  lines: int,
  lastAction: actions,
  piece: Pieces.oriented,
  pos: pos
};

let addP = (p1: pos, p2: pos): pos => {
  x: p1.x + p2.x,
  y: p1.y + p2.y
};

let makeMove = (s: state, step: pos) => {
  if( !overlap(s.board, s.piece.g, addP(s.pos, step) ) ) {
   addP(s.pos, step) 
  } else {
    s.pos
  }
}

let wallKickTest = (dim, f, t) =>
  switch (dim, f, t) {
    | (3, O, R) => [{x:0, y:0}, {x:-1, y:0}, {x:-1, y:+1}, {x:0, y:-2}, {x:-1, y:-2}]
    | (3, R, O) => [{x:0, y:0}, {x:+1, y:0}, {x:+1, y:-1}, {x:0, y:+2}, {x:+1, y:+2}]
    | (3, R, H) => [{x:0, y:0}, {x:+1, y:0}, {x:+1, y:-1}, {x:0, y:+2}, {x:+1, y:+2}]
    | (3, H, R) => [{x:0, y:0}, {x:-1, y:0}, {x:-1, y:+1}, {x:0, y:-2}, {x:-1, y:-2}]
    | (3, H, L) => [{x:0, y:0}, {x:+1, y:0}, {x:+1, y:+1}, {x:0, y:-2}, {x:+1, y:-2}]
    | (3, L, H) => [{x:0, y:0}, {x:-1, y:0}, {x:-1, y:-1}, {x:0, y:+2}, {x:-1, y:+2}]
    | (3, L, O) => [{x:0, y:0}, {x:-1, y:0}, {x:-1, y:-1}, {x:0, y:+2}, {x:-1, y:+2}]
    | (3, O, L) => [{x:0, y:0}, {x:+1, y:0}, {x:+1, y:+1}, {x:0, y:-2}, {x:+1, y:-2}]

    | (4, O, R) => [{x:0, y:0}, {x:-2, y:0}, {x:+1, y:+0}, {x:-2, y:-1}, {x:+1, y:+2}]
    | (4, R, O) => [{x:0, y:0}, {x:+2, y:0}, {x:-1, y:-0}, {x:+2, y:+1}, {x:-1, y:-2}]
    | (4, R, H) => [{x:0, y:0}, {x:-1, y:0}, {x:+2, y:-0}, {x:-1, y:+2}, {x:+2, y:-1}]
    | (4, H, R) => [{x:0, y:0}, {x:+1, y:0}, {x:-2, y:+0}, {x:+1, y:-2}, {x:-2, y:+1}]
    | (4, H, L) => [{x:0, y:0}, {x:+2, y:0}, {x:-1, y:+0}, {x:+2, y:+1}, {x:-1, y:-2}]
    | (4, L, H) => [{x:0, y:0}, {x:-2, y:0}, {x:+1, y:-0}, {x:-2, y:-1}, {x:+1, y:+2}]
    | (4, L, O) => [{x:0, y:0}, {x:+1, y:0}, {x:-2, y:-0}, {x:+1, y:-2}, {x:-2, y:+1}]
    | (4, O, L) => [{x:0, y:0}, {x:-1, y:0}, {x:+2, y:+0}, {x:-1, y:+2}, {x:+2, y:-1}]
    
    | _ => []
  };


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

type maybe('a) =
  | None
  | Some('a)

let rec findOffset = (board, rotated, pos, offsets) => switch(offsets) {
  | [] => None
  | [head, ...tail] => {
    if( overlap(board, rotated, addP(pos, head)) ) {
      findOffset(board, rotated, pos, tail)
    } else
      Some(head)
  }
}

let rec findGhost = (board, piece, position) => {
  switch (overlap(board, piece, position)) {
    | true => addP(position, {x:0, y:-1})
    | false => findGhost(board, piece, addP(position, {x:0, y: +1}))
  }
}


let materialize = (data) => {
  let ghostPos = findGhost(data.board, data.piece.g, addP(data.pos, {x:0, y:1}));
  let ghost = data.piece.g |> map(rows =>
    rows |> map(cell => switch(cell) { 
      | 0 => 0
      | _ => 2
    })
  );

  data.board 
    -> merge(ghost, ghostPos)
    -> merge(data.piece.g, data.pos)
}



let rotate = (s:state) => { 
  let rotated = rotateRight(s.piece.g);
  let next = transition(s.piece.o, R);
  let tests = wallKickTest(s.piece.g|>length, s.piece.o, next);
  let p = findOffset(s.board, rotated, s.pos, tests) 
  switch (p) {
    | None => {...s, lastAction: Rotate}
    | Some(offset) => {
      Js.log(offset);
      Js.log(tests);
      {
        ...s,
        lastAction: Rotate,
        piece: {g:rotated, o:next},
        pos: addP(s.pos, offset)
      }
    }  
  }
}

let moveDown = s => {
  ...s,
  lastAction: Tick,
  pos: makeMove(s, {x:0, y:+1}),
} 

let eliminate = board => {
  let w = board[0]|>length;
  let next = board |> fold_left( (rows, row) => 
    if( w != (row |> fold_left( (a, b) => {
      if (b>0){ 
        a+1; 
      }else {
        a;
      }
    }, 0))) 
      concat([rows, [|row|]])
    else rows
  , [||]);

  let removed = (board|>length) - (next|>length);
  
  (removed, concat([
    createBoard(w, removed),
    next
  ]));
}

let stateReducer = (s: state, a: actions): state => {
  switch (s.lastAction, a) {
    | (GameOver, _) => s
    | (_, Rotate) => rotate(s)
    | (_, MoveLeft) => {
      ...s,
      lastAction: MoveLeft,
      pos: makeMove(s, {x:-1, y:0})
    }
    | (_, MoveRight) => {
      ...s,
      lastAction: MoveRight,
      pos: makeMove(s, {x:+1, y:0})
    }
    | (Tick|Drop, Tick) => {
      if (overlap(s.board, s.piece.g, addP(s.pos, {x:0, y:+1}))) {
        let (removed, board) = eliminate(
          merge(s.board, s.piece.g, s.pos)
        );
        
        let nextPiece = pieces[Random.int(pieces|>length)];
        let nextPos = {x:4, y:0};

        {
          board,
          lastAction: if(overlap(board, nextPiece.g, nextPos))
            GameOver
          else Tick,
          lines: s.lines+removed,
          piece: nextPiece,
          pos: nextPos
        }
      } else moveDown(s)
    }
    | (_, Tick) => moveDown(s)
    | (_,_) => s
  }
}




let store = Reductive.Store.create(
  ~reducer=stateReducer,
  ~preloadedState={
    piece: pieces[0],
    lastAction: (),
    lines: 0,
    pos: {x:4, y:0},
    board: createBoard(10, 20)
  },
());

let cellColor = cell => switch(cell) {
  | 0 => "#000"
  | 2 => "#222"
  | _ => "#fff"
}


let make = (~data, _children) => {
  ...component,
    render: _self =>
      <div>
      <table style={
        gridStyle
      }> <tbody>(
        ReasonReact.array(
         materialize(data) 
          |> mapi( (i, rows) => 
          <tr key=string_of_int(i)>( ReasonReact.array(
              rows |> mapi( (j, cell) =>
              <td key=string_of_int(j) style={Style.combine(
                  cellStyle,
                  Style.make(~background= cellColor(cell) ,())
              )}>(ReasonReact.string("+"))</td>
              )) )</tr>
          )
        )) </tbody> </table>
      <div> 
        <b>(ReasonReact.string(string_of_int(data.lines)))</b>
        (switch(data.lastAction){ 
          | GameOver => <b>(ReasonReact.string("You lost"))</b> 
          | _ => <b/> 
         })
      </div>
      </div>
}
